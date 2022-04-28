!  SVN:$Id: ice_meltpond_lvl.F90 897 2015-01-22 01:15:53Z tcraig $
!=======================================================================

! Level-ice meltpond parameterization
!
! This meltpond parameterization was developed for use with the delta-
! Eddington radiation scheme, and only affects the radiation budget in
! the model.  That is, although the pond volume is tracked, that liquid
! water is not used elsewhere in the model for mass budgets or other
! physical processes.
!
! authors Elizabeth Hunke (LANL)
!         David Hebert (NRL Stennis)
!         Olivier Lecomte (Univ. Louvain)

      module ice_meltpond_lvl

      use ice_kinds_mod
      use ice_constants
      use ice_blocks, only: nx_block, ny_block
      use ice_domain_size, only: max_blocks, ncat

      implicit none

      private
      public :: init_meltponds_lvl, compute_ponds_lvl, &
                write_restart_pond_lvl, read_restart_pond_lvl

      logical (kind=log_kind), public :: & 
         restart_pond_lvl    ! if .true., read meltponds restart file

      character (len=char_len), public :: &
         frzpnd              ! pond refreezing parameterization

      real (kind=dbl_kind), public :: &
         dpscale, &          ! alter e-folding time scale for flushing 
         rfracmin, &         ! minimum retained fraction of meltwater
         rfracmax, &         ! maximum retained fraction of meltwater
         pndaspect, &        ! ratio of pond depth to pond fraction
         hs1                 ! tapering parameter for snow on pond ice

      real (kind=dbl_kind), public, &
         dimension (nx_block,ny_block,ncat,max_blocks) :: &
         dhsn, &      ! depth difference for snow on sea ice and pond ice
         ffracn       ! fraction of fsurfn used to melt ipond

!=======================================================================

      contains

!=======================================================================

!  Initialize melt ponds.

      subroutine init_meltponds_lvl(nx_block, ny_block, ncat, &
                                    apnd, hpnd, ipnd, dhsn)

      integer(kind=int_kind), intent(in) :: &
             nx_block , &
             ny_block , &
             ncat

      real(kind=dbl_kind), dimension(nx_block,ny_block,ncat), &
             intent(out) :: &
             apnd , & ! melt pond area fraction
             hpnd , & ! melt pond depth
             ipnd , & ! melt pond refrozen lid thickness
             dhsn     ! depth difference for snow on sea ice and pond ice

      apnd(:,:,:) = c0
      hpnd(:,:,:) = c0
      ipnd(:,:,:) = c0
      dhsn(:,:,:) = c0
      ffracn(:,:,:,:) = c0

      end subroutine init_meltponds_lvl

!=======================================================================

      subroutine compute_ponds_lvl(nx_block,ny_block,   &
                                   ilo, ihi, jlo, jhi,  &
                                   dt,    hi_min,       &
                                   dpscale, frzpnd,     &
                                   pndaspect,           &
                                   rfrac, meltt, melts, &
                                   frain, Tair,  fsurfn,&
                                   dhs,   ffrac,        &
                                   aicen, vicen, vsnon, &
                                   qicen, sicen,        &
                                   Tsfcn, alvl,         &
                                   apnd,  hpnd,  ipnd)

      use ice_constants, only: viscosity_dyn
      use ice_domain_size, only: nilyr
      use ice_therm_shared, only: ktherm

      integer (kind=int_kind), intent(in) :: &
         nx_block, ny_block, & ! block dimensions
         ilo,ihi,jlo,jhi       ! beginning and end of physical domain

      real (kind=dbl_kind), intent(in) :: &
         dt,       & ! time step (s)  
         hi_min,   & ! minimum ice thickness allowed for thermo (m)
         dpscale,  & ! alter e-folding time scale for flushing 
         pndaspect   ! ratio of pond depth to pond fraction

      character (len=char_len), intent(in) :: &
         frzpnd      ! pond refreezing parameterization

      real (kind=dbl_kind), dimension(nx_block,ny_block), &
         intent(in) :: &
         Tsfcn, &    ! surface temperature (C)
         alvl,  &    ! fraction of level ice
         rfrac, &    ! water fraction retained for melt ponds
         meltt, &    ! top melt rate (m/s)
         melts, &    ! snow melt rate (m/s)
         frain, &    ! rainfall rate (kg/m2/s)
         Tair,  &    ! air temperature (K)
         fsurfn,&    ! atm-ice surface heat flux  (W/m2)
         aicen, &    ! ice area fraction
         vicen, &    ! ice volume (m)
         vsnon       ! snow volume (m)

      real (kind=dbl_kind), dimension(nx_block,ny_block), &
         intent(inout) :: &
         apnd, hpnd, ipnd

      real (kind=dbl_kind), dimension (nx_block,ny_block,nilyr), intent(in) :: &
         qicen, &  ! ice layer enthalpy (J m-3)
         sicen     ! salinity (ppt)   

      real (kind=dbl_kind), dimension(nx_block,ny_block), &
         intent(in) :: &
         dhs       ! depth difference for snow on sea ice and pond ice

      real (kind=dbl_kind), dimension(nx_block,ny_block), &
         intent(out) :: &
         ffrac     ! fraction of fsurfn over pond used to melt ipond

      ! local temporary variables

      real (kind=dbl_kind), dimension(nx_block,ny_block) :: &
         volpn     ! pond volume per unit area (m)

      real (kind=dbl_kind), dimension (nilyr) :: &
         Tmlt      ! melting temperature (C)

      integer (kind=int_kind), dimension (nx_block*ny_block) :: &
         indxi, indxj     ! compressed indices for cells with ice melting

      integer (kind=int_kind) :: i, j, ij, icells ! indices

      real (kind=dbl_kind) :: &
         hi                     , & ! ice thickness (m)
         hs                     , & ! snow depth (m)
         dTs                    , & ! surface temperature diff for freeze-up (C)
         Tp                     , & ! pond freezing temperature (C)
         Ts                     , & ! surface air temperature (C)
         apondn                 , & ! local pond area 
         hpondn                 , & ! local pond depth (m)
         dvn                    , & ! change in pond volume (m)
         hlid, alid             , & ! refrozen lid thickness, area
         dhlid                  , & ! change in refrozen lid thickness
         bdt                    , & ! 2 kice dT dt / (rhoi Lfresh)
         alvl_tmp               , & ! level ice fraction of ice area
         draft, deltah, pressure_head, perm, drain ! for permeability

      real (kind=dbl_kind), parameter :: &
         Td       = c2          , & ! temperature difference for freeze-up (C)
         rexp     = p01             ! pond contraction scaling

      !-----------------------------------------------------------------
      ! Initialize 
      !-----------------------------------------------------------------

      do j = 1, ny_block
      do i = 1, nx_block
         volpn(i,j) = hpnd(i,j) &
                    * aicen(i,j) * alvl(i,j) * apnd(i,j)
         ffrac(i,j) = c0
      enddo
      enddo

      !-----------------------------------------------------------------
      ! Identify grid cells where ponds can be
      !-----------------------------------------------------------------

      icells = 0
      do j = jlo, jhi
      do i = ilo, ihi
         if (aicen(i,j)*alvl(i,j) > puny**2) then
            icells = icells + 1
            indxi(icells) = i
            indxj(icells) = j
         endif
      enddo                     ! i
      enddo                     ! j

      do ij = 1, icells
         i = indxi(ij)
         j = indxj(ij)

         hi = vicen(i,j)/aicen(i,j)
         hs = vsnon(i,j)/aicen(i,j)
         alvl_tmp = alvl(i,j)

         if (hi < hi_min) then

            !-----------------------------------------------------------
            ! Remove ponds on thin ice
            !-----------------------------------------------------------

            apondn = c0
            hpondn = c0
            volpn (i,j) = c0
            hlid = c0

         else

            !-----------------------------------------------------------
            ! initialize pond area as fraction of ice
            !-----------------------------------------------------------
            apondn = apnd(i,j)*alvl_tmp

            !-----------------------------------------------------------
            ! update pond volume
            !-----------------------------------------------------------
            ! add melt water
            dvn = rfrac(i,j)/rhofresh*(meltt(i,j)*rhoi &
                +                      melts(i,j)*rhos &
                +                      frain(i,j)*  dt)*aicen(i,j)

            ! shrink pond volume under freezing conditions
            if (trim(frzpnd) == 'cesm') then
               Tp = Timelt - Td
               dTs = max(Tp - Tsfcn(i,j),c0)
               dvn = dvn - volpn(i,j) * (c1 - exp(rexp*dTs/Tp))

            else 
               ! trim(frzpnd) == 'hlid' Stefan approximation
               ! assumes pond is fresh (freezing temperature = 0 C)
               ! and ice grows from existing pond ice
               hlid = ipnd(i,j)
               if (dvn == c0) then ! freeze pond
                  Ts = Tair(i,j) - Tffresh
                  if (Ts < c0) then
                 ! if (Ts < -c2) then ! as in meltpond_cesm
                     bdt = -c2*Ts*kice*dt/(rhoi*Lfresh)
                     dhlid = p5*sqrt(bdt)                  ! open water freezing
                     if (hlid > dhlid) dhlid = p5*bdt/hlid ! existing ice
                     dhlid = min(dhlid, hpnd(i,j)*rhofresh/rhoi)
                     hlid = hlid + dhlid
                  else
                     dhlid = c0 ! to account for surface inversions
                  endif
               else ! convert refrozen pond ice back to water
                  dhlid = max(fsurfn(i,j)*dt / (rhoi*Lfresh), c0) ! > 0
                  dhlid = -min(dhlid, hlid) ! < 0
                  hlid = max(hlid + dhlid, c0)
                  if (hs - dhs(i,j) < puny) then ! pond ice is snow-free
                     ffrac(i,j) = c1 ! fraction of fsurfn over pond used to melt ipond
                     if (fsurfn(i,j) > puny) &
                        ffrac(i,j) = min(-dhlid*rhoi*Lfresh/(dt*fsurfn(i,j)), c1)
                  endif
               endif
               alid = apondn * aicen(i,j)
               dvn = dvn - dhlid*alid*rhoi/rhofresh
            endif

            volpn(i,j) = volpn(i,j) + dvn

            !-----------------------------------------------------------
            ! update pond area and depth
            !-----------------------------------------------------------
            if (volpn(i,j) <= c0) then
               volpn(i,j) = c0
               apondn = c0
            endif

            if (apondn*aicen(i,j) > puny) then ! existing ponds
               apondn = max(c0, min(alvl_tmp, &
                            apondn + 0.5*dvn/(pndaspect*apondn*aicen(i,j))))
               hpondn = c0
               if (apondn > puny) &
                  hpondn = volpn(i,j)/(apondn*aicen(i,j))

            elseif (alvl_tmp*aicen(i,j) > c10*puny) then ! new ponds
               apondn = min (sqrt(volpn(i,j)/(pndaspect*aicen(i,j))), alvl_tmp)
               hpondn = pndaspect * apondn

            else           ! melt water runs off deformed ice      
               apondn = c0
               hpondn = c0
            endif
            apondn = max(apondn, c0)

            ! limit pond depth to maintain nonnegative freeboard
            hpondn = min(hpondn, ((rhow-rhoi)*hi - rhos*hs)/rhofresh)

            ! fraction of grid cell covered by ponds
            apondn = apondn * aicen(i,j)

            volpn(i,j) = hpondn*apondn
            if (volpn(i,j) <= c0) then
               volpn(i,j) = c0
               apondn = c0
               hpondn = c0
               hlid = c0
            endif

            !-----------------------------------------------------------
            ! drainage due to permeability (flushing)
            ! setting dpscale = 0 turns this off
            ! NOTE this uses the initial salinity and melting T profiles
            !-----------------------------------------------------------

            if (ktherm /= 2 .and. hpondn > c0 .and. dpscale > puny) then
               draft = (rhos*hs + rhoi*hi)/rhow + hpondn
               deltah = hpondn + hi - draft
               pressure_head = gravit * rhow * max(deltah, c0)
               Tmlt(:) = -sicen(i,j,:) * depressT
               call brine_permeability(nilyr, qicen(i,j,:), &
                    vicen(i,j), sicen(i,j,:), Tmlt, perm)
               drain = perm*pressure_head*dt / (viscosity_dyn*hi) * dpscale
               deltah = min(drain, hpondn)
               dvn = -deltah*apondn
               volpn(i,j) = volpn(i,j) + dvn
               apondn = max(c0, min(apondn &
                          + 0.5*dvn/(pndaspect*apondn), alvl_tmp*aicen(i,j)))
               hpondn = c0
               if (apondn > puny) hpondn = volpn(i,j)/apondn
            endif

         endif

         !-----------------------------------------------------------
         ! Reload tracer array
         !-----------------------------------------------------------

         hpnd(i,j) = hpondn
         apnd(i,j) = apondn / (aicen(i,j)*alvl_tmp)
         if (trim(frzpnd) == 'hlid') ipnd(i,j) = hlid

      enddo

      end subroutine compute_ponds_lvl

!=======================================================================

! determine the liquid fraction of brine in the ice and the permeability

      subroutine brine_permeability(nilyr, qicen, vicen, salin, Tmlt, perm)

      use ice_therm_shared, only: calculate_Tin_from_qin

      integer (kind=int_kind), intent(in) :: &
         nilyr     ! number of ice layers

      real (kind=dbl_kind), dimension(nilyr), intent(in) :: &
         qicen, &  ! enthalpy for each ice layer (J m-3)
         salin, &  ! salinity (ppt)   
         Tmlt      ! melting temperature (C)
    
      real (kind=dbl_kind), intent(in) :: &
         vicen     ! ice volume (m)
    
      real (kind=dbl_kind), intent(out) :: &
         perm      ! permeability (m^2)

      ! local variables

      real (kind=dbl_kind) ::   &
         Sbr       ! brine salinity

      real (kind=dbl_kind), dimension(nilyr) ::   &
         Tin, &    ! ice temperature (C)
         phi       ! liquid fraction

      integer (kind=int_kind) :: k
    
      !-----------------------------------------------------------------
      ! Compute ice temperatures from enthalpies using quadratic formula
      !-----------------------------------------------------------------

      do k = 1,nilyr
         Tin(k) = calculate_Tin_from_qin(qicen(k),Tmlt(k))
      enddo

      !-----------------------------------------------------------------
      ! brine salinity and liquid fraction
      !-----------------------------------------------------------------

      do k = 1,nilyr
         Sbr = c1/(1.e-3_dbl_kind - depressT/Tin(k)) ! Notz thesis eq 3.6
         phi(k) = salin(k)/Sbr ! liquid fraction
         if (phi(k) < 0.05) phi(k) = c0 ! impermeable
      enddo

      !-----------------------------------------------------------------
      ! permeability
      !-----------------------------------------------------------------

      perm = 3.0e-8_dbl_kind * (minval(phi))**3
    
      end subroutine brine_permeability
  
!=======================================================================
!
! Dumps all values needed for restarting
!
! authors Elizabeth C. Hunke, LANL

      subroutine write_restart_pond_lvl()

      use ice_domain_size, only: ncat
      use ice_fileunits, only: nu_dump_pond
      use ice_flux, only: fsnow
      use ice_state, only: trcrn, nt_apnd, nt_hpnd, nt_ipnd
      use ice_restart, only: write_restart_field

      ! local variables

      logical (kind=log_kind) :: diag

      diag = .true.

      call write_restart_field(nu_dump_pond,0, trcrn(:,:,nt_apnd,:,:),'ruf8', &
                               'apnd',ncat,diag)
      call write_restart_field(nu_dump_pond,0, trcrn(:,:,nt_hpnd,:,:),'ruf8', &
                               'hpnd',ncat,diag)
      call write_restart_field(nu_dump_pond,0, trcrn(:,:,nt_ipnd,:,:),'ruf8', &
                               'ipnd',ncat,diag)
      call write_restart_field(nu_dump_pond,0, fsnow(:,:,          :),'ruf8', &
                               'fsnow',1,diag)
      call write_restart_field(nu_dump_pond,0,  dhsn(:,:,        :,:),'ruf8', &
                               'dhs',ncat,diag)
      call write_restart_field(nu_dump_pond,0,ffracn(:,:,        :,:),'ruf8', &
                               'ffrac',ncat,diag)

      end subroutine write_restart_pond_lvl

!=======================================================================

! Reads all values needed for a meltpond volume restart
!
! authors Elizabeth C. Hunke, LANL

      subroutine read_restart_pond_lvl()

      use ice_domain_size, only: ncat
      use ice_communicate, only: my_task, master_task
      use ice_fileunits, only: nu_diag, nu_restart_pond 
      use ice_flux, only: fsnow
      use ice_state, only: trcrn, nt_apnd, nt_hpnd, nt_ipnd
      use ice_restart, only: read_restart_field

      ! local variables

      logical (kind=log_kind) :: &
         diag

      diag = .true.

      if (my_task == master_task) write(nu_diag,*) 'min/max level-ice ponds'

      call read_restart_field(nu_restart_pond,0, trcrn(:,:,nt_apnd,:,:),'ruf8', &
                              'apnd',ncat,diag,field_loc_center,field_type_scalar)
      call read_restart_field(nu_restart_pond,0, trcrn(:,:,nt_hpnd,:,:),'ruf8', &
                              'hpnd',ncat,diag,field_loc_center,field_type_scalar)
      call read_restart_field(nu_restart_pond,0, trcrn(:,:,nt_ipnd,:,:),'ruf8', &
                              'ipnd',ncat,diag,field_loc_center,field_type_scalar)
      call read_restart_field(nu_restart_pond,0, fsnow(:,:,          :),'ruf8', &
                              'fsnow',1,diag,field_loc_center,field_type_scalar)
      call read_restart_field(nu_restart_pond,0,  dhsn(:,:,        :,:),'ruf8', &
                              'dhs',ncat,diag,field_loc_center,field_type_scalar)
      call read_restart_field(nu_restart_pond,0,ffracn(:,:,        :,:),'ruf8', &
                              'ffrac',ncat,diag,field_loc_center,field_type_scalar)

      end subroutine read_restart_pond_lvl

!=======================================================================

      end module ice_meltpond_lvl

!=======================================================================
