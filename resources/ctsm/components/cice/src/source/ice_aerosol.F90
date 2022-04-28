!  SVN:$Id: ice_aerosol.F90 744 2013-09-27 22:53:24Z eclare $
!=======================================================================

! Aerosol tracer within sea ice
!
! authors Marika Holland, NCAR
!         David Bailey, NCAR

      module ice_aerosol

      use ice_kinds_mod
      use ice_constants
      use ice_fileunits, only: nu_diag
      use ice_restart_shared, only: lenstr, restart_dir, restart_file, &
                             pointer_file, runtype
      use ice_communicate, only: my_task, master_task
      use ice_exit, only: abort_ice

      implicit none

      private
      public :: init_aerosol, faero_default, update_aerosol, write_restart_aero

      logical (kind=log_kind), public :: & 
         restart_aero      ! if .true., read aerosol tracer restart file

!=======================================================================

      contains

!=======================================================================

!  Initialize ice aerosol tracer (call prior to reading restart data)

      subroutine init_aerosol

      use ice_domain_size, only: n_aero
      use ice_state, only: trcrn, nt_aero

      if (trim(runtype) == 'continue') restart_aero = .true.
      if (restart_aero) then
         call read_restart_aero
      else
         trcrn(:,:,nt_aero:nt_aero+4*n_aero-1,:,:) = c0
      endif

      end subroutine init_aerosol

!=======================================================================

! constant values for atmospheric aerosols
!
! authors: Elizabeth Hunke, LANL

      subroutine faero_default

      use ice_flux, only: faero_atm

      faero_atm(:,:,1,:) = 1.e-15_dbl_kind ! W/m^2 s
      faero_atm(:,:,2,:) = 1.e-13_dbl_kind
      faero_atm(:,:,3,:) = 1.e-11_dbl_kind

      end subroutine faero_default

!=======================================================================

! read atmospheric aerosols
!
! authors: Elizabeth Hunke, LANL

      subroutine faero_data

      use ice_calendar, only: month, mday, istep, sec
      use ice_domain_size, only: max_blocks
      use ice_blocks, only: nx_block, ny_block
      use ice_flux, only: faero_atm
      use ice_forcing, only: interp_coeff_monthly, read_clim_data_nc, interpolate_data

#ifdef ncdf 
      ! local parameters

      real (kind=dbl_kind), dimension(nx_block,ny_block,2,max_blocks), &
         save :: &
         aero1_data    , & ! field values at 2 temporal data points
         aero2_data    , & ! field values at 2 temporal data points
         aero3_data        ! field values at 2 temporal data points

      character (char_len_long) :: & 
         aero_file,   &   ! netcdf filename
         fieldname        ! field name in netcdf file

      integer (kind=int_kind) :: & 
         ixm,ixp     , & ! record numbers for neighboring months
         maxrec      , & ! maximum record number
         recslot     , & ! spline slot for current record
         midmonth        ! middle day of month

      logical (kind=log_kind) :: readm

    !-------------------------------------------------------------------
    ! monthly data 
    !
    ! Assume that monthly data values are located in the middle of the 
    ! month.
    !-------------------------------------------------------------------

      midmonth = 15  ! data is given on 15th of every month
!      midmonth = fix(p5 * real(daymo(month)))  ! exact middle

      ! Compute record numbers for surrounding months
      maxrec = 12
      ixm  = mod(month+maxrec-2,maxrec) + 1
      ixp  = mod(month,         maxrec) + 1
      if (mday >= midmonth) ixm = 99  ! other two points will be used
      if (mday <  midmonth) ixp = 99

      ! Determine whether interpolation will use values 1:2 or 2:3
      ! recslot = 2 means we use values 1:2, with the current value (2)
      !  in the second slot
      ! recslot = 1 means we use values 2:3, with the current value (2)
      !  in the first slot
      recslot = 1                             ! latter half of month
      if (mday < midmonth) recslot = 2        ! first half of month

      ! Find interpolation coefficients
      call interp_coeff_monthly (recslot)

      ! Read 2 monthly values 
      readm = .false.
      if (istep==1 .or. (mday==midmonth .and. sec==0)) readm = .true.

!      aero_file = trim(atm_data_dir)//'faero.nc'   
      aero_file = '/usr/projects/climate/eclare/DATA/gx1v3/faero.nc'   

      fieldname='faero_atm001'
      call read_clim_data_nc (readm, 0,  ixm, month, ixp, &
                              aero_file, fieldname, aero1_data, &
                              field_loc_center, field_type_scalar)

      fieldname='faero_atm002'
      call read_clim_data_nc (readm, 0,  ixm, month, ixp, &
                              aero_file, fieldname, aero2_data, &
                              field_loc_center, field_type_scalar)

      fieldname='faero_atm003'
      call read_clim_data_nc (readm, 0,  ixm, month, ixp, &
                              aero_file, fieldname, aero3_data, &
                              field_loc_center, field_type_scalar)

      call interpolate_data (aero1_data, faero_atm(:,:,1,:)) ! W/m^2 s
      call interpolate_data (aero2_data, faero_atm(:,:,2,:))
      call interpolate_data (aero3_data, faero_atm(:,:,3,:))

      where (faero_atm(:,:,:,:) > 1.e20) faero_atm(:,:,:,:) = c0

#endif

      end subroutine faero_data

!=======================================================================

!  Increase aerosol in ice or snow surface due to deposition
!  and vertical cycling

      subroutine update_aerosol (nx_block, ny_block,  &
                                dt,       icells,     &
                                indxi,    indxj,      &
                                meltt,    melts,      &
                                meltb,    congel,     &
                                snoice,               &
                                fsnow,                &
                                trcrn,                &
                                aice_old,             &
                                vice_old, vsno_old,   &
                                vicen, vsnon, aicen,  &
                                faero_atm, faero_ocn)

      use ice_domain_size, only: max_ntrcr, nilyr, nslyr, n_aero, max_aero
      use ice_state, only: nt_aero 
      use ice_shortwave, only: hi_ssl, hs_ssl

      integer (kind=int_kind), intent(in) :: &
         nx_block, ny_block, & ! block dimensions
         icells                ! number of cells with ice present

      integer (kind=int_kind), dimension (nx_block*ny_block), &
         intent(in) :: &
         indxi, indxj     ! compressed indices for cells with ice

      real (kind=dbl_kind), intent(in) :: &
         dt                    ! time step

      real (kind=dbl_kind), dimension(nx_block,ny_block), &
         intent(in) :: &
         meltt,    & ! thermodynamic melt/growth rates
         melts,    &
         meltb,    &
         congel,   &
         snoice,   &
         fsnow,    &
         vicen,    & ! ice volume (m)
         vsnon,    & ! snow volume (m)
         aicen,    & ! ice area fraction
         aice_old, & ! values prior to thermodynamic changes
         vice_old, &
         vsno_old 

      real (kind=dbl_kind), dimension(nx_block,ny_block,max_aero), &
         intent(in) :: &
         faero_atm   ! aerosol deposition rate (W/m^2 s)

      real (kind=dbl_kind), dimension(nx_block,ny_block,max_aero), &
         intent(inout) :: &
         faero_ocn   ! aerosol flux to ocean (W/m^2 s)

      real (kind=dbl_kind), dimension(nx_block,ny_block,max_ntrcr), &
         intent(inout) :: &
         trcrn       ! ice/snow tracer array

      !  local variables

      integer (kind=int_kind) :: i, j, ij, k, n

      real (kind=dbl_kind) :: &
         dzssl,  dzssl_new,      & ! snow ssl thickness
         dzint,  dzint_new,      & ! snow interior thickness
         dzssli, dzssli_new,     & ! ice ssl thickness
         dzinti, dzinti_new,     & ! ice interior thickness
         dznew,                  & ! tracks thickness changes
         hs, hi,                 & ! snow/ice thickness (m)
         dhs_evap, dhi_evap,     & ! snow/ice thickness change due to evap
         dhs_melts, dhi_meltt,   & ! ... due to surface melt
         dhs_snoice, dhi_snoice, & ! ... due to snow-ice formation
         dhi_congel, dhi_meltb,  & ! ... due to bottom growth, melt
         hslyr, hilyr,           & ! snow, ice layer thickness (m)
         hslyr_old, hilyr_old,   & ! old snow, ice layer thickness (m)
         hs_old, hi_old,         & ! old snow, ice thickness (m)
         sloss1, sloss2,         & ! aerosol mass loss (kg/m^2)
         ar                        ! 1/aicen(i,j)

      real (kind=dbl_kind), dimension(max_aero) :: &
         kscav, kscavsi   , & ! scavenging by melt water
         aerotot, aerotot0, & ! for conservation check
         focn_old             ! for conservation check

      real (kind=dbl_kind), dimension(max_aero,2) :: &
         aerosno,  aeroice, & ! kg/m^2
         aerosno0, aeroice0   ! for diagnostic prints

      data kscav   / .03_dbl_kind, .20_dbl_kind,&
           .02_dbl_kind,.02_dbl_kind,.01_dbl_kind,.01_dbl_kind /
      data kscavsi / .03_dbl_kind, .20_dbl_kind,&
           .02_dbl_kind,.02_dbl_kind,.01_dbl_kind,.01_dbl_kind /

      ! loop over grid cells with ice at beginning of time step
      do ij = 1, icells
         i = indxi(ij)
         j = indxj(ij)

    !-------------------------------------------------------------------
    ! initialize
    !-------------------------------------------------------------------
         focn_old(:) = faero_ocn(i,j,:)
         aerosno (:,:) = c0
         aeroice (:,:) = c0
         aerosno0(:,:) = c0
         aeroice0(:,:) = c0

         hs_old    = vsno_old(i,j)/aice_old(i,j)
         hi_old    = vice_old(i,j)/aice_old(i,j)
         hslyr_old = hs_old/real(nslyr,kind=dbl_kind)
         hilyr_old = hi_old/real(nilyr,kind=dbl_kind)

         dzssl  = min(hslyr_old/c2, hs_ssl)
         dzssli = min(hilyr_old/c2, hi_ssl)
         dzint  = hs_old - dzssl
         dzinti = hi_old - dzssli

         if (aicen(i,j) > c0) then
            ar = c1/aicen(i,j)
            hs = vsnon(i,j)*ar
            hi = vicen(i,j)*ar
         else ! ice disappeared during time step
            hs = vsnon(i,j)/aice_old(i,j)
            hi = vicen(i,j)/aice_old(i,j)
         endif

         dhs_melts  = -melts(i,j)
         dhi_snoice = snoice(i,j)
         dhs_snoice = dhi_snoice*rhoi/rhos
         dhi_meltt  = -meltt(i,j)
         dhi_meltb  = -meltb(i,j)
         dhi_congel = congel(i,j)

         dhs_evap = hs - (hs_old + dhs_melts - dhs_snoice &
                                 + fsnow(i,j)/rhos*dt)
         dhi_evap = hi - (hi_old + dhi_meltt + dhi_meltb &
                                 + dhi_congel + dhi_snoice)

         ! trcrn(nt_aero) has units kg/m^3
         do k=1,n_aero
            aerosno (k,:) = &
               trcrn(i,j,nt_aero+(k-1)*4  :nt_aero+(k-1)*4+1)*vsno_old(i,j)
            aeroice (k,:) = &
               trcrn(i,j,nt_aero+(k-1)*4+2:nt_aero+(k-1)*4+3)*vice_old(i,j)
            aerosno0(k,:) = aerosno(k,:)
            aeroice0(k,:) = aeroice(k,:)
            aerotot0(k) = aerosno(k,2) + aerosno(k,1) &
                        + aeroice(k,2) + aeroice(k,1)
         enddo

    !-------------------------------------------------------------------
    ! evaporation
    !-------------------------------------------------------------------
         dzint  = dzint  + min(dzssl  + dhs_evap, c0)
         dzinti = dzinti + min(dzssli + dhi_evap, c0)
         dzssl  = max(dzssl  + dhs_evap, c0)
         dzssli = max(dzssli + dhi_evap, c0)

    !-------------------------------------------------------------------
    ! basal ice growth
    !-------------------------------------------------------------------
         dzinti = dzinti + dhi_congel

    !-------------------------------------------------------------------
    ! surface snow melt
    !-------------------------------------------------------------------
         if (-dhs_melts > puny) then
            do k = 1, n_aero
               sloss1 = c0
               sloss2 = c0
               if (dzssl > puny)  &
                  sloss1 = kscav(k)*aerosno(k,1)  &
                                   *min(-dhs_melts,dzssl)/dzssl
               aerosno(k,1) = aerosno(k,1) - sloss1
               if (dzint > puny)  &
                  sloss2 = kscav(k)*aerosno(k,2) &
                                   *max(-dhs_melts-dzssl,c0)/dzint
               aerosno(k,2) = aerosno(k,2) - sloss2
               faero_ocn(i,j,k) = faero_ocn(i,j,k) + (sloss1+sloss2)/dt
            enddo  ! n_aero

            ! update snow thickness
            dzint=dzint+min(dzssl+dhs_melts, c0)
            dzssl=max(dzssl+dhs_melts, c0)

            if ( dzssl <= puny ) then ! ssl melts away
               aerosno(:,2) = aerosno(:,1) + aerosno(:,2)
               aerosno(:,1) = c0
               dzssl = max(dzssl, c0)
            endif
            if (dzint <= puny ) then  ! all snow melts away
               aeroice(:,1) = aeroice(:,1) &
                                + aerosno(:,1) + aerosno(:,2)
               aerosno(:,:) = c0
               dzint = max(dzint, c0)
            endif
         endif

    !-------------------------------------------------------------------
    ! surface ice melt
    !-------------------------------------------------------------------
         if (-dhi_meltt > puny) then
            do k = 1, n_aero
               sloss1 = c0
               sloss2 = c0
               if (dzssli > puny)  &
                  sloss1 = kscav(k)*aeroice(k,1)  &
                                   *min(-dhi_meltt,dzssli)/dzssli
               aeroice(k,1) = aeroice(k,1) - sloss1
               if (dzinti > puny)  &
                  sloss2 = kscav(k)*aeroice(k,2)  &
                                   *max(-dhi_meltt-dzssli,c0)/dzinti
               aeroice(k,2) = aeroice(k,2) - sloss2
               faero_ocn(i,j,k) = faero_ocn(i,j,k) + (sloss1+sloss2)/dt
            enddo

            dzinti = dzinti + min(dzssli+dhi_meltt, c0)
            dzssli = max(dzssli+dhi_meltt, c0)
            if (dzssli <= puny) then   ! ssl ice melts away
               do k = 1, n_aero
                  aeroice(k,2) = aeroice(k,1) + aeroice(k,2)
                  aeroice(k,1) = c0
               enddo
               dzssli = max(dzssli, c0)
            endif
            if (dzinti <= puny) then   ! all ice melts away
               do k = 1, n_aero
                  faero_ocn(i,j,k) = faero_ocn(i,j,k)  &
                                   + (aeroice(k,1)+aeroice(k,2))/dt
                  aeroice(k,:)=c0
               enddo
               dzinti = max(dzinti, c0)
            endif
         endif

    !-------------------------------------------------------------------
    ! basal ice melt.  Assume all aero lost in basal melt
    !-------------------------------------------------------------------
         if (-dhi_meltb > puny) then
            do k=1,n_aero
               sloss1=c0
               sloss2=c0
               if (dzssli > puny)  &
                  sloss1 = max(-dhi_meltb-dzinti, c0)  &
                           *aeroice(k,1)/dzssli
               aeroice(k,1) = aeroice(k,1) - sloss1
               if (dzinti > puny)  &
                  sloss2 = min(-dhi_meltb, dzinti)  &
                           *aeroice(k,2)/dzinti
               aeroice(k,2) = aeroice(k,2) - sloss2
               faero_ocn(i,j,k) = faero_ocn(i,j,k) + (sloss1+sloss2)/dt
            enddo

            dzssli = dzssli + min(dzinti+dhi_meltb, c0)
            dzinti = max(dzinti+dhi_meltb, c0)           
         endif

    !-------------------------------------------------------------------
    ! snowfall
    !-------------------------------------------------------------------
         if (fsnow(i,j) > c0) dzssl = dzssl + fsnow(i,j)/rhos*dt

    !-------------------------------------------------------------------
    ! snow-ice formation
    !-------------------------------------------------------------------
         if (dhs_snoice > puny) then
            do k = 1, n_aero
               sloss1 = c0
               sloss2 = c0
               if (dzint > puny)  &
                  sloss2 = min(dhs_snoice, dzint)  &
                           *aerosno(k,2)/dzint
               aerosno(k,2) = aerosno(k,2) - sloss2
               if (dzssl > puny)  &
                  sloss1 = max(dhs_snoice-dzint, c0)  &
                           *aerosno(k,1)/dzssl
               aerosno(k,1) = aerosno(k,1) - sloss1
               aeroice(k,1) = aeroice(k,1) &
                                + (c1-kscavsi(k))*(sloss2+sloss1)
               faero_ocn(i,j,k) = faero_ocn(i,j,k) &
                                + kscavsi(k)*(sloss2+sloss1)/dt
            enddo
            dzssl  = dzssl - max(dhs_snoice-dzint, c0)
            dzint  = max(dzint-dhs_snoice, c0)
            dzssli = dzssli + dhi_snoice
         endif

    !-------------------------------------------------------------------
    ! aerosol deposition
    !-------------------------------------------------------------------
         if (aicen(i,j) > c0) then
            hs = vsnon(i,j) * ar
         else
            hs = c0
         endif
         if (hs > hs_min) then    ! should this really be hs_min or 0? 
                                  ! should use same hs_min value as in radiation
            do k=1,n_aero
               aerosno(k,1) = aerosno(k,1) &
                                + faero_atm(i,j,k)*dt*aicen(i,j)
            enddo
         else
            do k=1,n_aero
               aeroice(k,1) = aeroice(k,1) &
                                + faero_atm(i,j,k)*dt*aicen(i,j)
            enddo
         endif

    !-------------------------------------------------------------------
    ! redistribute aerosol within vertical layers
    !-------------------------------------------------------------------
         if (aicen(i,j) > c0) then
            hs = vsnon(i,j)  * ar     ! new snow thickness
            hi = vicen(i,j)  * ar     ! new ice thickness
         else
            hs = c0
            hi = c0
         endif
         if (dzssl <= puny) then   ! nothing in SSL
            do k=1,n_aero
               aerosno(k,2) = aerosno(k,2) + aerosno(k,1)
               aerosno(k,1) = c0
            enddo
         endif
         if (dzint <= puny) then   ! nothing in Snow Int
            do k = 1, n_aero
               aeroice(k,1) = aeroice(k,1) + aerosno(k,2)
               aerosno(k,2) = c0
            enddo
         endif
         if (dzssli <= puny) then  ! nothing in Ice SSL
            do k = 1, n_aero
               aeroice(k,2) = aeroice(k,2) + aeroice(k,1)
               aeroice(k,1) = c0
            enddo
         endif

         if (dzinti <= puny) then  ! nothing in Ice INT
            do k = 1, n_aero
               faero_ocn(i,j,k) = faero_ocn(i,j,k) &
                                + (aeroice(k,1)+aeroice(k,2))/dt
               aeroice(k,:)=c0
            enddo
         endif

         hslyr      = hs/real(nslyr,kind=dbl_kind)
         hilyr      = hi/real(nilyr,kind=dbl_kind)
         dzssl_new  = min(hslyr/c2, hs_ssl)
         dzssli_new = min(hilyr/c2, hi_ssl)
         dzint_new  = hs - dzssl_new
         dzinti_new = hi - dzssli_new

         if (hs > hs_min) then
            do k = 1, n_aero
               dznew = min(dzssl_new-dzssl, c0)
               sloss1 = c0
               if (dzssl > puny) &
                  sloss1 = dznew*aerosno(k,1)/dzssl ! not neccesarily a loss
                  dznew = max(dzssl_new-dzssl, c0)
               if (dzint > puny) &
                  sloss1 = sloss1 + aerosno(k,2)*dznew/dzint
               aerosno(k,1) = aerosno(k,1) + sloss1 
               aerosno(k,2) = aerosno(k,2) - sloss1
            enddo
         else
            aeroice(:,1) = aeroice(:,1)  &
                             + aerosno(:,1) + aerosno(:,2)
            aerosno(:,:) = c0
         endif

         if (vicen(i,j) > puny) then ! may want a limit on hi instead?
            do k = 1, n_aero
               sloss2 = c0
               dznew = min(dzssli_new-dzssli, c0)
               if (dzssli > puny) & 
                  sloss2 = dznew*aeroice(k,1)/dzssli
                  dznew = max(dzssli_new-dzssli, c0)
               if (dzinti > puny) & 
                  sloss2 = sloss2 + aeroice(k,2)*dznew/dzinti
               aeroice(k,1) = aeroice(k,1) + sloss2 
               aeroice(k,2) = aeroice(k,2) - sloss2
            enddo
         else
            faero_ocn(i,j,:) = faero_ocn(i,j,:) + (aeroice(:,1)+aeroice(:,2))/dt
            aeroice(:,:) = c0
         endif

    !-------------------------------------------------------------------
    ! check conservation
    !-------------------------------------------------------------------
         do k = 1, n_aero
            aerotot(k) = aerosno(k,2) + aerosno(k,1) &
                       + aeroice(k,2) + aeroice(k,1)
            if ((aerotot(k)-aerotot0(k)) &
                 - (   faero_atm(i,j,k)*aicen(i,j) &
                    - (faero_ocn(i,j,k)-focn_old(k)) )*dt  > puny) then
               
               write(nu_diag,*) 'aerosol tracer:  ',k
               write(nu_diag,*) 'aerotot-aerotot0 ',aerotot(k)-aerotot0(k) 
               write(nu_diag,*) 'faero_atm-faero_ocn      ', &
               (faero_atm(i,j,k)*aicen(i,j)-(faero_ocn(i,j,k)-focn_old(k)))*dt
            endif
         enddo

    !-------------------------------------------------------------------
    ! reload tracers
    !-------------------------------------------------------------------
         if (vicen(i,j) > puny) &
            aeroice(:,:) = aeroice(:,:)/vicen(i,j)
         if (vsnon(i,j) > puny) &
            aerosno(:,:) = aerosno(:,:)/vsnon(i,j)
         do k = 1, n_aero
            do n = 1,2
               trcrn(i,j,nt_aero+(k-1)*4+n-1)=aerosno(k,n)
               trcrn(i,j,nt_aero+(k-1)*4+n+1)=aeroice(k,n)
            enddo
         enddo

    !-------------------------------------------------------------------
    ! check for negative values
    !-------------------------------------------------------------------
         if (trcrn(i,j,nt_aero  ) < -puny .or. &
             trcrn(i,j,nt_aero+1) < -puny .or. &
             trcrn(i,j,nt_aero+2) < -puny .or. &
             trcrn(i,j,nt_aero+3) < -puny) then

            write(nu_diag,*) 'MH aerosol negative in aerosol code'
            write(nu_diag,*) 'MH INT neg in aerosol my_task = ',&
                              my_task, &
                              ' printing point = ',n, &
                              ' i and j = ',i,j
            write(nu_diag,*) 'MH Int Neg aero snowssl= '    ,aerosno0(1,1)
            write(nu_diag,*) 'MH Int Neg aero new snowssl= ',aerosno (1,1)
            write(nu_diag,*) 'MH Int Neg aero snowint= '    ,aerosno0(1,2)
            write(nu_diag,*) 'MH Int Neg aero new snowint= ',aerosno (1,2)
            write(nu_diag,*) 'MH Int Neg aero ice_ssl= '    ,aeroice0(1,1)
            write(nu_diag,*) 'MH Int Neg aero new ice_ssl= ',aeroice (1,1)
            write(nu_diag,*) 'MH Int Neg aero ice_int= '    ,aeroice0(1,2)
            write(nu_diag,*) 'MH Int Neg aero new ice_int= ',aeroice (1,2)
            write(nu_diag,*) 'MH Int Neg aero aicen= '      ,aicen   (i,j)
            write(nu_diag,*) 'MH Int Neg aero vicen= '      ,vicen   (i,j)
            write(nu_diag,*) 'MH Int Neg aero vsnon= '      ,vsnon   (i,j)
            write(nu_diag,*) 'MH Int Neg aero viceold= '    ,vice_old(i,j)
            write(nu_diag,*) 'MH Int Neg aero vsnoold= '    ,vsno_old(i,j)
            write(nu_diag,*) 'MH Int Neg aero melts= '      ,melts   (i,j)
            write(nu_diag,*) 'MH Int Neg aero meltt= '      ,meltt   (i,j)
            write(nu_diag,*) 'MH Int Neg aero meltb= '      ,meltb   (i,j)
            write(nu_diag,*) 'MH Int Neg aero congel= '     ,congel  (i,j)
            write(nu_diag,*) 'MH Int Neg aero snoice= '     ,snoice  (i,j)
            write(nu_diag,*) 'MH Int Neg aero evap sno?= '  ,dhs_evap
            write(nu_diag,*) 'MH Int Neg aero evap ice?= '  ,dhi_evap
            write(nu_diag,*) 'MH Int Neg aero fsnow= '      ,fsnow   (i,j)
            write(nu_diag,*) 'MH Int Neg aero faero_atm= '  ,faero_atm(i,j,1)
            write(nu_diag,*) 'MH Int Neg aero faero_ocn= '  ,faero_ocn(i,j,1)

            trcrn(i,j,nt_aero  ) = max(trcrn(i,j,nt_aero  ), c0)
            trcrn(i,j,nt_aero+1) = max(trcrn(i,j,nt_aero+1), c0)
            trcrn(i,j,nt_aero+2) = max(trcrn(i,j,nt_aero+2), c0)
            trcrn(i,j,nt_aero+3) = max(trcrn(i,j,nt_aero+3), c0)
         endif
      enddo

      end subroutine update_aerosol

!=======================================================================
!---! these subroutines write/read Fortran unformatted data files ..
!=======================================================================

! Dumps all values needed for restarting
!
! authors Elizabeth Hunke, LANL (original version)
!         David Bailey, NCAR
!         Marika Holland, NCAR

      subroutine write_restart_aero()

      use ice_domain_size, only: ncat, n_aero
      use ice_restart, only: write_restart_field
      use ice_state, only: trcrn, nt_aero
      use ice_fileunits, only: nu_dump_aero

      ! local variables

      integer (kind=int_kind) :: &
         k                    ! loop indices

      logical (kind=log_kind) :: diag

      character (len=3)       :: nchar

      !-----------------------------------------------------------------

      if (my_task == master_task) write(nu_diag,*) 'write_restart_aero (aerosols)'

      diag = .true.

      do k = 1, n_aero
       write(nchar,'(i3.3)') k
       call write_restart_field(nu_dump_aero,0, &
            trcrn(:,:,nt_aero  +(k-1)*4,:,:),'ruf8','aerosnossl'//nchar, &
            ncat,diag)
       call write_restart_field(nu_dump_aero,0, &
            trcrn(:,:,nt_aero+1+(k-1)*4,:,:),'ruf8','aerosnoint'//nchar, &
            ncat,diag)
       call write_restart_field(nu_dump_aero,0, &
            trcrn(:,:,nt_aero+2+(k-1)*4,:,:),'ruf8','aeroicessl'//nchar, &
            ncat,diag)
       call write_restart_field(nu_dump_aero,0, &
            trcrn(:,:,nt_aero+3+(k-1)*4,:,:),'ruf8','aeroiceint'//nchar, &
            ncat,diag)
      enddo

      end subroutine write_restart_aero

!=======================================================================

! Reads all values needed for an ice aerosol restart
!
! authors Elizabeth Hunke, LANL (original version)
!         David Bailey, NCAR
!         Marika Holland, NCAR

      subroutine read_restart_aero()

      use ice_communicate, only: my_task, master_task
      use ice_domain_size, only: n_aero, ncat
      use ice_restart, only: read_restart_field
      use ice_state, only: trcrn, nt_aero
      use ice_fileunits, only: nu_restart_aero

      ! local variables

      integer (kind=int_kind) :: &
         k                    ! loop indices

      logical (kind=log_kind) :: &
         diag

      character (len=3)       :: nchar

      !-----------------------------------------------------------------

      if (my_task == master_task) write(nu_diag,*) 'read_restart_aero (aerosols)'

      diag = .true.

      do k = 1, n_aero
       write(nchar,'(i3.3)') k
       call read_restart_field(nu_restart_aero,0, &
            trcrn(:,:,nt_aero  +(k-1)*4,:,:),'ruf8','aerosnossl'//trim(nchar), &
            ncat,diag,field_type=field_type_scalar,field_loc=field_loc_center)
       call read_restart_field(nu_restart_aero,0, &
            trcrn(:,:,nt_aero+1+(k-1)*4,:,:),'ruf8','aerosnoint'//trim(nchar), &
            ncat,diag,field_type=field_type_scalar,field_loc=field_loc_center)
       call read_restart_field(nu_restart_aero,0, &
            trcrn(:,:,nt_aero+2+(k-1)*4,:,:),'ruf8','aeroicessl'//trim(nchar), &
            ncat,diag,field_type=field_type_scalar,field_loc=field_loc_center)
       call read_restart_field(nu_restart_aero,0, &
            trcrn(:,:,nt_aero+3+(k-1)*4,:,:),'ruf8','aeroiceint'//trim(nchar), &
            ncat,diag,field_type=field_type_scalar,field_loc=field_loc_center)
      enddo

      end subroutine read_restart_aero

!=======================================================================

      end module ice_aerosol

!=======================================================================
