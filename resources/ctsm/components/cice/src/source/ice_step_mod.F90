!  SVN:$Id: ice_step_mod.F90 915 2015-02-08 02:50:33Z tcraig $
!=======================================================================
!
!  Contains CICE component driver routines common to all drivers.
!
!  authors Elizabeth C. Hunke, LANL
!          Philip W. Jones, LANL
!          William H. Lipscomb, LANL
!
! 2008 ECH: created module by moving subroutines from drivers/cice4/

      module ice_step_mod

      use ice_constants
      use ice_kinds_mod
      implicit none
      private

      public :: step_therm1, step_therm2, step_dynamics, &
                prep_radiation, step_radiation, post_thermo

!=======================================================================

      contains

!=======================================================================
!
! Scales radiation fields computed on the previous time step.
!
! authors: Elizabeth Hunke, LANL

      subroutine prep_radiation (dt, iblk)

      use ice_blocks, only: block, get_block, nx_block, ny_block
      use ice_communicate, only: my_task
      use ice_domain, only: blocks_ice
      use ice_domain_size, only: ncat, nilyr, nslyr
      use ice_fileunits, only: nu_diag
      use ice_flux, only: scale_factor, swvdr, swvdf, swidr, swidf, &
          alvdr_ai, alvdf_ai, alidr_ai, alidf_ai, fswfac, coszen, &
          alvdr_init, alvdf_init, alidr_init, alidf_init
      use ice_shortwave, only: fswsfcn, fswintn, fswthrun, fswpenln, &
                               Sswabsn, Iswabsn
      use ice_state, only: aice, aicen
      use ice_timers, only: ice_timer_start, ice_timer_stop, timer_sw

      real (kind=dbl_kind), intent(in) :: &
         dt      ! time step

      integer (kind=int_kind), intent(in) :: &
         iblk    ! block index

      ! local variables

      integer (kind=int_kind) :: &
         i, j, ij    , & ! horizontal indices
         k           , & ! vertical index       
         ilo,ihi,jlo,jhi, & ! beginning and end of physical domain
         n               ! thickness category index

      integer (kind=int_kind) :: &
         icells          ! number of cells with aicen > puny

      integer (kind=int_kind), dimension(nx_block*ny_block) :: &
         indxi, indxj    ! indirect indices for cells with aicen > puny

      real (kind=dbl_kind) :: netsw 

      type (block) :: &
         this_block      ! block information for current block

      call ice_timer_start(timer_sw,iblk)      ! shortwave

         this_block = get_block(blocks_ice(iblk),iblk)         
         ilo = this_block%ilo
         ihi = this_block%ihi
         jlo = this_block%jlo
         jhi = this_block%jhi

      !-----------------------------------------------------------------
      ! Compute netsw scaling factor (new netsw / old netsw)
      !-----------------------------------------------------------------

         do j = jlo, jhi
         do i = ilo, ihi
            alvdr_init(i,j,iblk) = alvdr_ai(i,j,iblk)
            alvdf_init(i,j,iblk) = alvdf_ai(i,j,iblk)
            alidr_init(i,j,iblk) = alidr_ai(i,j,iblk)
            alidf_init(i,j,iblk) = alidf_ai(i,j,iblk)
            if (aice(i,j,iblk) > c0 .and. scale_factor(i,j,iblk) > puny) then
               netsw = swvdr(i,j,iblk)*(c1 - alvdr_ai(i,j,iblk)) &
                     + swvdf(i,j,iblk)*(c1 - alvdf_ai(i,j,iblk)) &
                     + swidr(i,j,iblk)*(c1 - alidr_ai(i,j,iblk)) &
                     + swidf(i,j,iblk)*(c1 - alidf_ai(i,j,iblk))
               scale_factor(i,j,iblk) = netsw / scale_factor(i,j,iblk)
            else
               scale_factor(i,j,iblk) = c1
            endif
            fswfac(i,j,iblk) = scale_factor(i,j,iblk) ! for history 
         enddo               ! i
         enddo               ! j

         do n = 1, ncat

      !-----------------------------------------------------------------
      ! Identify cells with nonzero ice area
      !-----------------------------------------------------------------
           
            icells = 0
            do j = jlo, jhi
            do i = ilo, ihi
               if (aicen(i,j,n,iblk) > puny) then
                  icells = icells + 1
                  indxi(icells) = i
                  indxj(icells) = j
               endif
            enddo               ! i
            enddo               ! j

      !-----------------------------------------------------------------
      ! Scale absorbed solar radiation for change in net shortwave
      !-----------------------------------------------------------------

            do ij = 1, icells
               i = indxi(ij)
               j = indxj(ij)

               fswsfcn(i,j,n,iblk)  = scale_factor(i,j,iblk)*fswsfcn (i,j,n,iblk)
               fswintn(i,j,n,iblk)  = scale_factor(i,j,iblk)*fswintn (i,j,n,iblk)
               fswthrun(i,j,n,iblk) = scale_factor(i,j,iblk)*fswthrun(i,j,n,iblk)
               do k = 1,nilyr+1
                  fswpenln(i,j,k,n,iblk) &
                                    = scale_factor(i,j,iblk)*fswpenln(i,j,k,n,iblk)
               enddo       !k

               do k=1,nslyr
                  Sswabsn(i,j,k,n,iblk) = &
                       scale_factor(i,j,iblk)*Sswabsn(i,j,k,n,iblk)
               enddo
               do k=1,nilyr
                  Iswabsn(i,j,k,n,iblk) = &
                       scale_factor(i,j,iblk)*Iswabsn(i,j,k,n,iblk)
               enddo
            enddo
         enddo                  ! ncat

      call ice_timer_stop(timer_sw,iblk)     ! shortwave

      end subroutine prep_radiation

!=======================================================================
!
! Driver for updating ice and snow internal temperatures and
! computing thermodynamic growth rates and coupler fluxes.
!
! authors: William H. Lipscomb, LANL

      subroutine step_therm1 (dt, iblk)

      use ice_aerosol
      use ice_isotope
      use ice_age, only: increment_age
      use ice_atmo, only: calc_strair, &
          atmbndy, atmo_boundary_const, atmo_boundary_layer, &
          formdrag, neutral_drag_coeffs, &
          Cdn_ocn, Cdn_ocn_skin, Cdn_ocn_floe, Cdn_ocn_keel, Cd_atm, &
          Cdn_atm, Cdn_atm_skin, Cdn_atm_floe, Cdn_atm_rdg, Cdn_atm_pond, &
          hfreebd, hdraft, hridge, distrdg, hkeel, dkeel, lfloe, dfloe
      use ice_blocks, only: block, get_block, nx_block, ny_block
      use ice_calendar, only: yday, istep1
      use ice_communicate, only: my_task
      use ice_domain, only: blocks_ice
      use ice_domain_size, only: ncat, nilyr, max_iso
      use ice_exit, only: abort_ice
      use ice_fileunits, only: nu_diag
      use ice_flux, only: frzmlt, sst, Tf, strocnxT, strocnyT, rside, &
          meltsn, melttn, meltbn, congeln, snoicen, dsnown, uatm, vatm, &
          wind, rhoa, potT, Qa, zlvl, strax, stray, flatn, fsensn, fsurfn, fcondtopn, &
          flw, fsnow, fpond, sss, mlt_onset, frz_onset, faero_atm, faero_ocn, &
          fiso_atm, fiso_ocn, Qa_iso, Qref_iso, fiso_evap, fiso_rain, &
          frain, Tair, coszen, strairxT, strairyT, fsurf, fcondtop, fsens, &
          flat, fswabs, flwout, evap, evapi, evaps, Tref, Qref, Uref, fresh, fsalt, fhocn, &
          fswthru, meltt, melts, meltb, meltl, congel, snoice, fcondbot, fcondbotn, &
          set_sfcflux, merge_fluxes, send_i2x_per_cat, fswthrun_ai, Tbot, Tsnice, &
          HDO_ocn, H2_16O_ocn, H2_18O_ocn
      use ice_firstyear, only: update_FYarea
      use ice_grid, only: lmask_n, lmask_s, TLAT, TLON
      use ice_itd, only: hi_min
      use ice_meltpond_cesm, only: compute_ponds_cesm
      use ice_meltpond_lvl, only: compute_ponds_lvl, ffracn, dhsn, &
          rfracmin, rfracmax, dpscale, pndaspect, frzpnd
      use ice_meltpond_topo, only: compute_ponds_topo
      use ice_shortwave, only: fswsfcn, fswintn, fswthrun, &
                               Sswabsn, Iswabsn, shortwave
      use ice_state, only: aice, aicen, aice_init, aicen_init, vicen_init, &
          vice, vicen, vsno, vsnon, ntrcr, trcrn, &
          nt_apnd, nt_hpnd, nt_ipnd, nt_alvl, nt_vlvl, nt_Tsfc, &
          tr_iage, nt_iage, tr_FY, nt_FY, tr_aero, tr_iso, &
          tr_pond, tr_pond_cesm, tr_pond_lvl, &
          nt_qice, nt_sice, tr_pond_topo, uvel, vvel
      use ice_therm_shared, only: calc_Tsfc
      use ice_therm_vertical, only: frzmlt_bottom_lateral, thermo_vertical
      use ice_timers, only: ice_timer_start, ice_timer_stop, timer_ponds
!jd - Blowing snow modification
      use ice_snowphys, only: blowingsnow,snowphys_snowfonice, &
           snowfonicen, snow2ocnn, snowfonice, snow2ocn
!jd

      real (kind=dbl_kind), intent(in) :: &
         dt      ! time step

      integer (kind=int_kind), intent(in) :: &
         iblk    ! block index

      ! local variables

      integer (kind=int_kind) :: &
         i, j, ij    , & ! horizontal indices
         ilo,ihi,jlo,jhi, & ! beginning and end of physical domain
         n               ! thickness category index

      integer (kind=int_kind) :: &
         icells          ! number of cells with aicen > puny

      integer (kind=int_kind), dimension(nx_block*ny_block) :: &
         indxi, indxj    ! indirect indices for cells with aicen > puny

      ! 2D coupler variables (computed for each category, then aggregated)
      real (kind=dbl_kind), dimension (nx_block,ny_block) :: &
         fswabsn     , & ! shortwave absorbed by ice          (W/m^2)
         flwoutn     , & ! upward LW at surface               (W/m^2)
         evapn       , & ! flux of vapor, atmos to ice   (kg m-2 s-1)
         evapin      , & ! flux of vapor over ice   (kg m-2 s-1)
         evapsn      , & ! flux of vapor over snow   (kg m-2 s-1)
         freshn      , & ! flux of water, ice to ocean     (kg/m^2/s)
         fsaltn      , & ! flux of salt, ice to ocean      (kg/m^2/s)
         fhocnn      , & ! fbot corrected for leftover energy (W/m^2)
         strairxn    , & ! air/ice zonal  stress,             (N/m^2)
         strairyn    , & ! air/ice meridional stress,         (N/m^2)
         Cd_atm_n,& ! drag coefficient ratio
         Trefn       , & ! air tmp reference level                (K)
         Urefn       , & ! air speed reference level            (m/s)
         Qrefn           ! air sp hum reference level         (kg/kg)

      real (kind=dbl_kind), dimension (nx_block,ny_block,max_iso) :: &
         Qrefn_iso   , & ! air sp hum reference level         (kg/kg)
         fiso_evapn  , & ! flux of vapor, atmos to ice   (kg m-2 s-1)
         fiso_ocnn       ! flux of water, ice to ocean     (kg/m^2/s)

      ! other local variables
      real (kind=dbl_kind), dimension (nx_block,ny_block) :: &
         Tbotn       , & ! ice bottom surface temperature (deg C)
         Tsnicen      , & ! snow-ice interface temperature (deg C)
         fbot        , & ! ice-ocean heat flux at bottom surface (W/m^2)
         shcoef      , & ! transfer coefficient for sensible heat
         lhcoef          ! transfer coefficient for latent heat

      real (kind=dbl_kind), dimension (nx_block,ny_block) :: &
         vsnon_init  , & ! for aerosol mass budget
         rfrac           ! water fraction retained for melt ponds

      real (kind=dbl_kind) :: &
         raice       , & ! 1/aice
         pond            ! water retained in ponds (m)

      type (block) :: &
         this_block      ! block information for current block

      logical (kind=log_kind) :: &
         l_stop          ! if true, abort the model

      integer (kind=int_kind) :: &
         istop, jstop    ! indices of grid cell where model aborts 

      real (kind=dbl_kind), dimension (nx_block,ny_block) :: &
         worka, workb, fsnown

      l_stop = .false.

         this_block = get_block(blocks_ice(iblk),iblk)         
         ilo = this_block%ilo
         ihi = this_block%ihi
         jlo = this_block%jlo
         jhi = this_block%jhi

      !-----------------------------------------------------------------
      ! Save the ice area passed to the coupler (so that history fields
      !  can be made consistent with coupler fields).
      ! Save the initial ice area and volume in each category.
      !-----------------------------------------------------------------

         do j = 1, ny_block
         do i = 1, nx_block
            aice_init (i,j,  iblk) = aice (i,j,  iblk)
         enddo
         enddo

         do n = 1, ncat
         do j = 1, ny_block
         do i = 1, nx_block
            aicen_init(i,j,n,iblk) = aicen(i,j,n,iblk)
            vicen_init(i,j,n,iblk) = vicen(i,j,n,iblk)
         enddo
         enddo
         enddo

#ifdef CICE_IN_NEMO
       !---------------------------------------------------------------
       ! Scale frain and fsnow by ice concentration as these fields
       ! are supplied by NEMO multiplied by ice concentration
       !---------------------------------------------------------------
 
         do j = 1, ny_block
         do i = 1, nx_block

            if (aice_init(i,j,iblk) > puny) then
               raice           = c1 / aice_init(i,j,iblk)
               frain(i,j,iblk) = frain(i,j,iblk)*raice
               fsnow(i,j,iblk) = fsnow(i,j,iblk)*raice
            else
               frain(i,j,iblk) = c0
               fsnow(i,j,iblk) = c0
            endif

         enddo
         enddo
#endif

      !-----------------------------------------------------------------
      ! Adjust frzmlt to account for ice-ocean heat fluxes since last
      !  call to coupler.
      ! Compute lateral and bottom heat fluxes.
      !-----------------------------------------------------------------

         call frzmlt_bottom_lateral                                      &
                                (nx_block,           ny_block,           &
                                 ilo, ihi,           jlo, jhi,           &
                                 ntrcr,              dt,                 &
                                 aice  (:,:,  iblk), frzmlt(:,:,  iblk), &
                                 vicen (:,:,:,iblk), vsnon (:,:,:,iblk), &
                                 trcrn (:,:,1:ntrcr,:,iblk),             &
                                 sst   (:,:,  iblk), Tf    (:,:,  iblk), &
                                 strocnxT(:,:,iblk), strocnyT(:,:,iblk), &
                                 Tbotn,               fbot,               &
                                 rside (:,:,  iblk), Cdn_ocn (:,:,iblk) )

      !-----------------------------------------------------------------
      ! Update the neutral drag coefficients to account for form drag
      ! Oceanic and atmospheric drag coefficients
      !-----------------------------------------------------------------


         if (formdrag) then

            call neutral_drag_coeffs &
                       (nx_block,       ny_block,                      &
                        ilo, ihi,       jlo, jhi,                      &
                        trcrn (:,:,nt_apnd,:,iblk),                    &
                        trcrn (:,:,nt_hpnd,:,iblk),                    &
                        trcrn (:,:,nt_ipnd,:,iblk),                    &
                        trcrn (:,:,nt_alvl,:,iblk),                    &
                        trcrn (:,:,nt_vlvl,:,iblk),                    &
                        aice        (:,:,iblk), vice        (:,:,iblk),&
                        vsno        (:,:,iblk), aicen     (:,:,:,iblk),&
                        vicen     (:,:,:,iblk), vsnon     (:,:,:,iblk),&
                        Cdn_ocn     (:,:,iblk), Cdn_ocn_skin(:,:,iblk),&
                        Cdn_ocn_floe(:,:,iblk), Cdn_ocn_keel(:,:,iblk),&
                        Cdn_atm     (:,:,iblk), Cdn_atm_skin(:,:,iblk),&
                        Cdn_atm_floe(:,:,iblk), Cdn_atm_pond(:,:,iblk),&
                        Cdn_atm_rdg (:,:,iblk), hfreebd     (:,:,iblk),&
                        hdraft      (:,:,iblk), hridge      (:,:,iblk),&
                        distrdg     (:,:,iblk), hkeel       (:,:,iblk),&
                        dkeel       (:,:,iblk), lfloe       (:,:,iblk),&
                        dfloe       (:,:,iblk), ncat)
         endif 


         do n = 1, ncat

            meltsn(:,:,n,iblk)  = c0
            melttn(:,:,n,iblk)  = c0
            meltbn(:,:,n,iblk)  = c0
            congeln(:,:,n,iblk) = c0
            snoicen(:,:,n,iblk) = c0
            dsnown(:,:,n,iblk) = c0 
!            Tsf_icen(:,:,n,iblk) = c0
!jd
            if (trim(blowingsnow)=='lecomte2013') then
               snowfonicen(:,:,n,iblk) = c0
            else
               snowfonicen(:,:,n,iblk) = c1
            end if
            fsnown(:,:) = fsnow(:,:,iblk)
            snow2ocnn(:,:,n,iblk) = c0
!jd
           
      !-----------------------------------------------------------------
      ! Identify cells with nonzero ice area
      !-----------------------------------------------------------------

            icells = 0
            indxi = 0
            indxj = 0
            do j = jlo, jhi
            do i = ilo, ihi
               if (aicen(i,j,n,iblk) > puny) then
                  icells = icells + 1
                  indxi(icells) = i
                  indxj(icells) = j
               endif
            enddo               ! i
            enddo               ! j

            if ((calc_Tsfc .or. calc_strair) .and. icells > 0) then 

      !-----------------------------------------------------------------
      ! Atmosphere boundary layer calculation; compute coefficients
      ! for sensible and latent heat fluxes.
      !
      ! NOTE: The wind stress is computed here for later use if 
      !       calc_strair = .true.   Otherwise, the wind stress
      !       components are set to the data values.
      !-----------------------------------------------------------------

               if (trim(atmbndy) == 'constant') then
                   call atmo_boundary_const &
                                   (nx_block,      ny_block,        &
                                    'ice',          icells,         &
                                    indxi,          indxj,          &
                                    uatm(:,:,iblk), vatm(:,:,iblk), &
                                    wind(:,:,iblk), rhoa(:,:,iblk), &
                                    strairxn,       strairyn,       &
                                    trcrn(:,:,nt_Tsfc,n,iblk),      &
                                    potT(:,:,iblk), Qa  (:,:,iblk), &
                                    worka,          workb,          &
                                    lhcoef,         shcoef,         &
                                    Cdn_atm(:,:,iblk))
               else ! default
                   call atmo_boundary_layer & 
                                  (nx_block,       ny_block,       &
                                   'ice',          icells,         &
                                   indxi,          indxj,          &
                                   trcrn(:,:,nt_Tsfc,n,iblk),      &
                                   potT(:,:,iblk),                 &
                                   uatm(:,:,iblk), vatm(:,:,iblk), &
                                   wind(:,:,iblk), zlvl(:,:,iblk), &
                                   Qa  (:,:,iblk), rhoa(:,:,iblk), &
                                   strairxn,       strairyn,       &
                                   Trefn,          Qrefn,          &
                                   worka,          workb,          &
                                   lhcoef,         shcoef,         &
                                   Cdn_atm(:,:,iblk),              &
                                   Cd_atm_n,                &
                                   Qa_iso = Qa_iso(:,:,:,iblk),    &
                                   Qref_iso = Qrefn_iso,           &
                                   uice=uvel(:,:,iblk),            &
                                   vice=vvel(:,:,iblk),            &
                                   Uref=Urefn                      )
               endif ! atmbndy

            else

               ! Initialize for safety
               Trefn (:,:)  = c0
               Qrefn (:,:)  = c0
               Qrefn_iso(:,:,:)  = c0
               Urefn (:,:)  = c0
               lhcoef(:,:)  = c0
               shcoef(:,:)  = c0

            endif   ! calc_Tsfc or calc_strair

            if (.not.(calc_strair)) then
#ifndef CICE_IN_NEMO
               ! Set to data values (on T points)
               strairxn(:,:) = strax(:,:,iblk)
               strairyn(:,:) = stray(:,:,iblk)
#else
               ! NEMO wind stress is supplied on u grid, multipied 
               ! by ice concentration and set directly in evp, so
               ! strairxT/yT = 0. Zero u-components here for safety.
               strairxn(:,:) = c0
               strairyn(:,:) = c0
#endif
            endif

      !-----------------------------------------------------------------
      ! Update ice age
      ! This is further adjusted for freezing in the thermodynamics.
      ! Melting does not alter the ice age.
      !-----------------------------------------------------------------

            if (tr_iage) then
               call increment_age (nx_block, ny_block,      &
                                   dt, icells,              &
                                   indxi, indxj,            &
                                   trcrn(:,:,nt_iage,n,iblk))
            endif
            if (tr_FY) then
               call update_FYarea (nx_block, ny_block,      &
                                   dt, icells,              &
                                   indxi, indxj,            &
                                   lmask_n(:,:,iblk),       &
                                   lmask_s(:,:,iblk),       &
                                   yday,                    &
                                   trcrn(:,:,nt_FY,n,iblk))
            endif

      !-----------------------------------------------------------------      
      ! Calculate amount of snow blowing directly into the ocean from this
      ! cathegory.  (jd) First part, split 
      !-----------------------------------------------------------------
            if (trim(blowingsnow)=='lecomte2013') then
               call snowphys_snowfonice(icells,                  &
                                        indxi, indxj,            &
                                        snowfonicen(:,:,n,iblk), &
                                        aice(:,:,iblk))
               do ij = 1, icells
                  i = indxi(ij)
                  j = indxj(ij)
                  fsnown(i,j)=fsnow(i,j,iblk)*snowfonicen(i,j,n,iblk)
                  snow2ocnn(i,j,n,iblk)=fsnow(i,j,iblk) - fsnown(i,j)
               end do

            end if
      !-----------------------------------------------------------------
      ! Vertical thermodynamics: Heat conduction, growth and melting.
      !----------------------------------------------------------------- 

            if (.not.(calc_Tsfc)) then

               ! If not calculating surface temperature and fluxes, set 
               ! surface fluxes (flatn, fsurfn, and fcondtopn) to be used 
               ! in thickness_changes
 
               ! hadgem routine sets fluxes to default values in ice-only mode
               call set_sfcflux(nx_block,  ny_block,  &
                                n,         iblk,      &
                                icells,               & 
                                indxi,     indxj,     &
                                aicen    (:,:,n,iblk),&
                                flatn    (:,:,n,iblk),&
                                fsensn   (:,:,n,iblk),&
                                fsurfn   (:,:,n,iblk),&
                                fcondtopn(:,:,n,iblk) )
            endif

            vsnon_init(:,:) = vsnon(:,:,n,iblk)

            call thermo_vertical(nx_block,           ny_block,            &
                                dt,                  icells,              &
                                indxi,               indxj,               &
                                aicen(:,:,n,iblk),                        &
                                trcrn(:,:,:,n,iblk),                      &
                                vicen(:,:,n,iblk),   vsnon(:,:,n,iblk),   &
                                flw    (:,:,iblk),   potT (:,:,iblk),     &
                                Qa     (:,:,iblk),   rhoa (:,:,iblk),     &
!jd                                fsnow  (:,:,iblk),   fpond (:,:,iblk),    &
                                fsnown  (:,:),   fpond (:,:,iblk),    &
                                fbot,                Tbotn,                &
                                Tsnicen, &
                                sss  (:,:,iblk),                          &
                                lhcoef,              shcoef,              &
                                fswsfcn(:,:,n,iblk), fswintn(:,:,n,iblk), &
                                Sswabsn(:,:,:,n,iblk),                    &
                                Iswabsn(:,:,:,n,iblk),                    &
                                fsurfn(:,:,n,iblk),                       &
                                fcondtopn(:,:,n,iblk),                    &
                                fcondbotn(:,:,n,iblk),                    &
                                fsensn(:,:,n,iblk),  flatn(:,:,n,iblk),   &
                                flwoutn,                                  &
                                evapn,                                    &
                                evapin,              evapsn,              &
                                freshn,              &
                                fsaltn,              fhocnn,              &
                                melttn(:,:,n,iblk),  meltsn(:,:,n,iblk),  &
                                meltbn(:,:,n,iblk),                       &
                                congeln(:,:,n,iblk), snoicen(:,:,n,iblk), &
                                mlt_onset(:,:,iblk), frz_onset(:,:,iblk), &
                                yday,                l_stop,              &
                                istop,               jstop,               &
                                dsnown(:,:,n,iblk))

         if (l_stop) then
            write (nu_diag,*) 'istep1, my_task, iblk =', &
                               istep1, my_task, iblk
            write (nu_diag,*) 'category n = ', n
            write (nu_diag,*) 'Global block:', this_block%block_id
            if (istop > 0 .and. jstop > 0) then
               write(nu_diag,*) 'Global i and j:', &
                                this_block%i_glob(istop), &
                                this_block%j_glob(jstop) 
               write(nu_diag,*) 'Lat, Lon:', &
                                TLAT(istop,jstop,iblk)*rad_to_deg, &
                                TLON(istop,jstop,iblk)*rad_to_deg
               write(nu_diag,*) 'aice:', &
                                aice(istop,jstop,iblk)
               write(nu_diag,*) 'n: ',n, 'aicen: ', &
                                aicen(istop,jstop,n,iblk)
            endif
            call abort_ice ('ice: Vertical thermo error')
         endif

      !-----------------------------------------------------------------
      ! Total absorbed shortwave radiation
      !-----------------------------------------------------------------
         do j = 1, ny_block
         do i = 1, nx_block
            fswabsn(i,j) = fswsfcn (i,j,n,iblk) &
                         + fswintn (i,j,n,iblk) &
                         + fswthrun(i,j,n,iblk)
         enddo
         enddo

      !-----------------------------------------------------------------
      ! Snowfall and energy to the ocean
      !-----------------------------------------------------------------

         do ij = 1,icells
            i = indxi(ij)
            j = indxj(ij)
            fhocnn(i,j) = fhocnn(i,j) &
                 - Lfresh*snow2ocnn(i,j,n,iblk)
            freshn(i,j) = freshn(i,j) + snow2ocnn(i,j,n,iblk)
         enddo

      !-----------------------------------------------------------------
      ! Aerosol update
      !-----------------------------------------------------------------
         if (tr_aero .and. icells > 0) then

               call update_aerosol (nx_block, ny_block,                  &
                                    dt, icells,                          &
                                    indxi, indxj,                        &
                                    melttn(:,:,n,iblk),                  &
                                    meltsn(:,:,n,iblk),                  &
                                    meltbn(:,:,n,iblk),                  &
                                    congeln(:,:,n,iblk),                 &
                                    snoicen(:,:,n,iblk),                 &
!jd                                    fsnow(:,:,iblk),                     &
                                    fsnown(:,:),                         &
                                    trcrn(:,:,:,n,iblk),                 &
                                    aicen_init(:,:,n,iblk),              &
                                    vicen_init(:,:,n,iblk),              &
                                    vsnon_init(:,:),                     &
                                    vicen(:,:,n,iblk),                   &
                                    vsnon(:,:,n,iblk),                   &
                                    aicen(:,:,n,iblk),                   &
                                    faero_atm(:,:,:,iblk),               &
                                    faero_ocn(:,:,:,iblk))
         endif

      !-----------------------------------------------------------------
      ! Isotope update
      !-----------------------------------------------------------------
         if (tr_iso .and. icells > 0) then

               call update_isotope (nx_block, ny_block, &
                                    dt, icells, &
                                    indxi, indxj, &
                                    melttn(:,:,n,iblk), &
                                    meltsn(:,:,n,iblk), &
                                    meltbn(:,:,n,iblk), &
                                    congeln(:,:,n,iblk), &
                                    snoicen(:,:,n,iblk), &
                                    evapn,                 &
!jd                                    fsnow(:,:,iblk), &
                                    fsnown(:,:), &
                                    Qrefn_iso(:,:,:),                &
                                    trcrn(:,:,:,n,iblk), &
                                    aicen_init(:,:,n,iblk), &
                                    vicen_init(:,:,n,iblk), &
                                    vsnon_init(:,:), &
                                    vicen(:,:,n,iblk), &
                                    vsnon(:,:,n,iblk), &
                                    aicen(:,:,n,iblk), &
                                    fiso_atm(:,:,:,iblk), &
                                    fiso_evapn(:,:,:),              &
                                    fiso_ocnn(:,:,:),               &
                                    HDO_ocn(:,:,iblk), &
                                    H2_16O_ocn(:,:,iblk), &
                                    H2_18O_ocn(:,:,iblk))
         else
             fiso_evapn(:,:,:) = c0
             fiso_ocnn(:,:,:) = c0
             Qrefn_iso(:,:,:)  = c0
         endif

      !-----------------------------------------------------------------
      ! Melt ponds
      ! If using tr_pond_cesm, the full calculation is performed here.
      ! If using tr_pond_topo, the rest of the calculation is done after
      ! the surface fluxes are merged, below.
      !-----------------------------------------------------------------

         call ice_timer_start(timer_ponds,iblk)

         if (tr_pond) then

            if (tr_pond_cesm) then
               rfrac(:,:) = rfracmin + (rfracmax-rfracmin) * aicen(:,:,n,iblk) 
               call compute_ponds_cesm(nx_block,  ny_block,                    &
                                       ilo, ihi,  jlo, jhi,                    &
                                       dt,        hi_min,                      &
                                       pndaspect, rfrac,                       &
                                       melttn(:,:,n,iblk),                     &
                                       meltsn(:,:,n,iblk), frain(:,:,iblk),    &
                                       aicen (:,:,n,iblk), vicen (:,:,n,iblk), &
                                       vsnon (:,:,n,iblk),                     &
                                       trcrn(:,:,nt_Tsfc,n,iblk),              &
                                       trcrn(:,:,nt_apnd,n,iblk),              &
                                       trcrn(:,:,nt_hpnd,n,iblk))

            elseif (tr_pond_lvl) then
               rfrac(:,:) = rfracmin + (rfracmax-rfracmin) * aicen(:,:,n,iblk)
               call compute_ponds_lvl(nx_block,  ny_block,                        &
                                      ilo, ihi,  jlo, jhi,                        &
                                      dt,        hi_min,                          &
                                      dpscale,   frzpnd,                          &
                                      pndaspect, rfrac,                           &
                                      melttn(:,:,n,iblk), meltsn(:,:,n,iblk),     &
                                      frain (:,:,iblk),   Tair  (:,:,iblk),       &
                                      fsurfn(:,:,n,iblk),                         &
                                      dhsn  (:,:,n,iblk), ffracn(:,:,n,iblk),     &
                                      aicen (:,:,n,iblk), vicen (:,:,n,iblk),     &
                                      vsnon (:,:,n,iblk),                         &
                                      trcrn (:,:,nt_qice:nt_qice+nilyr-1,n,iblk), &
                                      trcrn (:,:,nt_sice:nt_sice+nilyr-1,n,iblk), &
                                      trcrn (:,:,nt_Tsfc,n,iblk),                 &
                                      trcrn (:,:,nt_alvl,n,iblk),                 &
                                      trcrn (:,:,nt_apnd,n,iblk),                 &
                                      trcrn (:,:,nt_hpnd,n,iblk),                 &
                                      trcrn (:,:,nt_ipnd,n,iblk))

            elseif (tr_pond_topo .and. icells > 0) then
               do ij = 1, icells
                  i = indxi(ij)
                  j = indxj(ij)

                  ! collect liquid water in ponds
                  ! assume salt still runs off

                  rfrac(i,j) = rfracmin + (rfracmax-rfracmin) * aicen(i,j,n,iblk)
                  pond = rfrac(i,j)/rhofresh * (melttn(i,j,n,iblk)*rhoi &
                       +                        meltsn(i,j,n,iblk)*rhos &
                       +                        frain (i,j,iblk)*dt)

                  ! if pond does not exist, create new pond over full ice area
                  ! otherwise increase pond depth without changing pond area
                  if (trcrn(i,j,nt_apnd,n,iblk) < puny) then
                      trcrn(i,j,nt_hpnd,n,iblk) = c0
                      trcrn(i,j,nt_apnd,n,iblk) = c1
                  endif 
                  trcrn(i,j,nt_hpnd,n,iblk) = (pond &
                  + trcrn(i,j,nt_hpnd,n,iblk)*trcrn(i,j,nt_apnd,n,iblk)) &
                                            / trcrn(i,j,nt_apnd,n,iblk)
                  fpond(i,j,iblk) = fpond(i,j,iblk) &
                                  + pond * aicen(i,j,n,iblk) ! m
               enddo
            endif

         endif
         call ice_timer_stop(timer_ponds,iblk)

      !-----------------------------------------------------------------
      ! Increment area-weighted fluxes.
      !-----------------------------------------------------------------

         call merge_fluxes (nx_block,           ny_block,             &
                            icells,                                   &
                            indxi,              indxj,                &
                            aicen_init(:,:,n,iblk),                   &
                            flw(:,:,iblk),      coszen(:,:,iblk),     &
                            strairxn,           strairyn,             &
                            Cd_atm_n,                          &
                            fsurfn(:,:,n,iblk), fcondtopn(:,:,n,iblk),&
                            fcondbotn(:,:,n,iblk),&
                            fsensn(:,:,n,iblk), flatn(:,:,n,iblk),    &
                            fswabsn,            flwoutn,              &
                            evapn,                                    &
                            evapin,             evapsn,               &
                            Trefn,              Qrefn,                &
                            Tbotn,              Tsnicen,               &
                            freshn,             fsaltn,               &
                            fhocnn,             fswthrun(:,:,n,iblk), &
                            strairxT(:,:,iblk), strairyT  (:,:,iblk), &
                            Cd_atm(:,:,iblk),                  &
                            fsurf   (:,:,iblk), fcondtop  (:,:,iblk), &
                            fcondbot  (:,:,iblk), &
                            fsens   (:,:,iblk), flat      (:,:,iblk), &
                            fswabs  (:,:,iblk), flwout    (:,:,iblk), &
                            evap    (:,:,iblk),                       &
                            evapi   (:,:,iblk), evaps     (:,:,iblk), &
                            Tref    (:,:,iblk), Qref      (:,:,iblk), &
                            Tbot    (:,:,iblk), Tsnice     (:,:,iblk), &
                            fresh   (:,:,iblk), fsalt     (:,:,iblk), &
                            fhocn   (:,:,iblk), fswthru   (:,:,iblk), &
                            melttn(:,:,n,iblk), meltsn  (:,:,n,iblk), &
                            meltbn(:,:,n,iblk), congeln (:,:,n,iblk), &
                            snoicen(:,:,n,iblk),                      &
                            meltt   (:,:,iblk),  melts   (:,:,iblk),  &
                            meltb   (:,:,iblk),                       &
                            congel  (:,:,iblk),  snoice  (:,:,iblk),  &
!jd
                            snow2ocnn(:,:,n,iblk), snow2ocn(:,:,iblk),  &
                            snowfonicen(:,:,n,iblk), snowfonice(:,:,iblk),  &
!jd
                            Uref=Uref(:,:,iblk), Urefn=Urefn,         &
                            Qref_iso=Qref_iso(:,:,:,iblk), &
                            Qrefn_iso=Qrefn_iso(:,:,:),&
                            fiso_evap=fiso_evap(:,:,:,iblk), &
                            fiso_evapn=fiso_evapn(:,:,:),&
                            fiso_ocn=fiso_ocn(:,:,:,iblk), &
                            fiso_ocnn=fiso_ocnn(:,:,:))

      !-----------------------------------------------------------------
      ! handle per-category i2x fields, no merging
      !-----------------------------------------------------------------

            if (send_i2x_per_cat) then
               do ij = 1, icells
                  i = indxi(ij)
                  j = indxj(ij)

                  fswthrun_ai(i,j,n,iblk) = fswthrun(i,j,n,iblk)*aicen_init(i,j,n,iblk)
               enddo ! ij
            endif

         enddo                  ! ncat

      !-----------------------------------------------------------------
      ! Calculate ponds from the topographic scheme
      !-----------------------------------------------------------------
         call ice_timer_start(timer_ponds,iblk)
         if (tr_pond_topo) then
            call compute_ponds_topo(nx_block, ny_block,                        &
                                    ilo, ihi, jlo, jhi,                        &
                                    dt,                                        &
                                    aice (:,:,iblk), aicen(:,:,:,iblk),        &
                                    vice (:,:,iblk), vicen(:,:,:,iblk),        &
                                    vsno (:,:,iblk), vsnon(:,:,:,iblk),        &
                                    potT (:,:,iblk), meltt(:,:,  iblk),        &
                                    fsurf(:,:,iblk), fpond(:,:,  iblk),        &
                                    trcrn(:,:,nt_Tsfc,:,iblk), Tf(:,:,  iblk), &
                                    trcrn(:,:,nt_qice:nt_qice+nilyr-1,:,iblk), &
                                    trcrn(:,:,nt_sice:nt_sice+nilyr-1,:,iblk), &
                                    trcrn(:,:,nt_apnd,:,iblk),                 &
                                    trcrn(:,:,nt_hpnd,:,iblk),                 &
                                    trcrn(:,:,nt_ipnd,:,iblk))
         endif
         call ice_timer_stop(timer_ponds,iblk)

      end subroutine step_therm1

!=======================================================================
! Driver for thermodynamic changes not needed for coupling:
! transport in thickness space, lateral growth and melting.
!
! author: William H. Lipscomb, LANL

      subroutine step_therm2 (dt, iblk)

      use ice_blocks, only: block, get_block, nx_block, ny_block
      use ice_calendar, only: istep1, yday
      use ice_communicate, only: my_task
      use ice_domain, only: blocks_ice
      use ice_domain_size, only: ncat
      use ice_exit, only: abort_ice
      use ice_flux, only: fresh, frain, fiso_rain, fpond, frzmlt, frazil, frz_onset, &
          update_ocn_f, fsalt, Tf, sss, salinz, fhocn, faero_ocn, fiso_ocn, rside, &
          meltl, frazil_diag, HDO_ocn, H2_16O_ocn, H2_18O_ocn
      use ice_fileunits, only: nu_diag
      use ice_grid, only: tmask
      use ice_itd, only: cleanup_itd, kitd, aggregate_area, reduce_area
      use ice_therm_itd, only: lateral_melt, linear_itd, add_new_ice
      use ice_zbgc_shared, only: ocean_bio, flux_bio
      use ice_state, only: aice, aicen, aice0, ntrcr, trcr_depend, &
          aicen_init, vicen_init, trcrn, vicen, vsnon, nbtrcr, tr_aero, tr_iso, &
          tr_pond_topo
      use ice_therm_shared, only: heat_capacity
      use ice_therm_vertical, only: phi_init, dSin0_frazil
      use ice_timers, only: ice_timer_start, ice_timer_stop, timer_catconv
      use ice_zbgc_shared, only: first_ice

      real (kind=dbl_kind), intent(in) :: &
         dt      ! time step

      integer (kind=int_kind), intent(in) :: &
         iblk    ! block index

      ! local variables

      integer (kind=int_kind) :: &
         ilo,ihi,jlo,jhi, & ! beginning and end of physical domain
         i, j

      integer (kind=int_kind) :: &
         icells          ! number of ice/ocean cells 

      integer (kind=int_kind), dimension(nx_block*ny_block) :: &
         indxi, indxj    ! indirect indices for ice/ocean cells

      type (block) :: &
         this_block      ! block information for current block

      logical (kind=log_kind) :: &
         l_stop          ! if true, abort model

      integer (kind=int_kind) :: &
         istop, jstop    ! indices of grid cell where model aborts

      l_stop = .false.
      
         this_block = get_block(blocks_ice(iblk),iblk)         
         ilo = this_block%ilo
         ihi = this_block%ihi
         jlo = this_block%jlo
         jhi = this_block%jhi

      !-----------------------------------------------------------------
      ! Let rain drain through to the ocean.
      !-----------------------------------------------------------------

         do j = 1, ny_block
         do i = 1, nx_block
            fresh     (i,j,iblk) = fresh(i,j,iblk)       &
                                 + frain(i,j,iblk)*aice(i,j,iblk)
            fiso_ocn(i,j,:,iblk) = fiso_ocn(i,j,:,iblk)  &
                                 + fiso_rain(i,j,:,iblk)*aice(i,j,iblk)
         enddo
         enddo

      !-----------------------------------------------------------------
      ! Given thermodynamic growth rates, transport ice between
      ! thickness categories.
      !-----------------------------------------------------------------

         call ice_timer_start(timer_catconv,iblk)    ! category conversions

      !-----------------------------------------------------------------
      ! Compute fractional ice area in each grid cell.
      !-----------------------------------------------------------------
         call aggregate_area (nx_block,          ny_block, &
                              aicen(:,:,:,iblk),           &
                              aice (:,:,  iblk), aice0(:,:,iblk))

         if (kitd == 1) then
      !-----------------------------------------------------------------
      ! Identify grid cells with ice.
      !-----------------------------------------------------------------

            icells = 0
            do j = jlo,jhi
            do i = ilo,ihi
               if (aice(i,j,iblk) > puny) then
                  icells = icells + 1
                  indxi(icells) = i
                  indxj(icells) = j
               endif
            enddo
            enddo

            if (icells > 0) then

            call linear_itd (nx_block, ny_block,             &
                             icells,   indxi, indxj,         &
                             ntrcr,    trcr_depend(1:ntrcr), &
                             aicen_init(:,:,:,iblk),         &
                             vicen_init(:,:,:,iblk),         &
                             aicen     (:,:,:,iblk),         &
                             trcrn     (:,:,1:ntrcr,:,iblk), & 
                             vicen     (:,:,:,iblk),         &
                             vsnon     (:,:,:,iblk),         &
                             aice      (:,:,  iblk),         &
                             aice0     (:,:,  iblk),         &
                             fpond     (:,:,  iblk),         &
                             l_stop,                         &
                             istop,    jstop)
          
            if (l_stop) then
               write (nu_diag,*) 'istep1, my_task, iblk =', &
                                  istep1, my_task, iblk
               write (nu_diag,*) 'Global block:', this_block%block_id
               if (istop > 0 .and. jstop > 0) &
                    write(nu_diag,*) 'Global i and j:', &
                                     this_block%i_glob(istop), &
                                     this_block%j_glob(jstop) 
               call abort_ice ('ice: Linear ITD error')
            endif

            endif ! icells

         endif  ! kitd = 1

         call ice_timer_stop(timer_catconv,iblk)    ! category conversions

      !-----------------------------------------------------------------
      ! Add frazil ice growing in leads.
      !-----------------------------------------------------------------

         ! identify ice-ocean cells
         icells = 0
         do j = jlo, jhi
         do i = ilo, ihi
            if (tmask(i,j,iblk)) then
               icells = icells + 1
               indxi(icells) = i
               indxj(icells) = j
            endif
         enddo               ! i
         enddo               ! j
            
         call add_new_ice (nx_block,              ny_block, &
                           ntrcr,                 icells,   &
                           indxi,                 indxj,    &
                           dt,                              &
                           aicen     (:,:,:,iblk),          &
                           trcrn     (:,:,1:ntrcr,:,iblk),  &
                           vicen     (:,:,:,iblk),          &
                           aice0     (:,:,  iblk),          &
                           aice      (:,:,  iblk),          &
                           frzmlt    (:,:,  iblk),          &
                           frazil    (:,:,  iblk),          &
                           frazil_diag(:,:,  iblk),         &
                           frz_onset (:,:,  iblk), yday,    &
                           update_ocn_f,                    &
                           fresh     (:,:,  iblk),          &
                           fsalt     (:,:,  iblk),          &
                           Tf        (:,:,  iblk),          &
                           sss       (:,:,  iblk),          &
                           salinz    (:,:,:,iblk),          &
                           phi_init, dSin0_frazil,          &
                           fiso_ocn  (:,:,:, iblk),         &
                           HDO_ocn   (:,:,iblk),            &
                           H2_16O_ocn   (:,:,iblk),         &
                           H2_18O_ocn   (:,:,iblk),         &
                           nbtrcr,                          &
                           flux_bio  (:,:,1:nbtrcr,iblk),   &
                           ocean_bio (:,:,1:nbtrcr,iblk),   &
                           l_stop,                          &
                           istop                 , jstop)

         if (l_stop) then
            write (nu_diag,*) 'istep1, my_task, iblk =', &
                               istep1, my_task, iblk
            write (nu_diag,*) 'Global block:', this_block%block_id
            if (istop > 0 .and. jstop > 0) &
                 write(nu_diag,*) 'Global i and j:', &
                                  this_block%i_glob(istop), &
                                  this_block%j_glob(jstop) 
            call abort_ice ('ice: add_new_ice error')
         endif

      !-----------------------------------------------------------------
      ! Melt ice laterally.
      !-----------------------------------------------------------------

         call lateral_melt (nx_block, ny_block,     &
                            ilo, ihi, jlo, jhi,     &
                            dt,                     &
                            fpond     (:,:,  iblk), &
                            fresh     (:,:,  iblk), &
                            fsalt     (:,:,  iblk), &    
                            fhocn     (:,:,  iblk), &
                            faero_ocn (:,:,:,iblk), &
                            fiso_ocn  (:,:,:,iblk), &
                            rside     (:,:,  iblk), &
                            meltl     (:,:,  iblk), &
                            aicen     (:,:,:,iblk), &
                            vicen     (:,:,:,iblk), &
                            vsnon     (:,:,:,iblk), &
                            trcrn     (:,:,:,:,iblk))

      !-----------------------------------------------------------------
      ! For the special case of a single category, adjust the area and
      ! volume (assuming that half the volume change decreases the
      ! thickness, and the other half decreases the area).  
      !-----------------------------------------------------------------

!echmod: test this
         if (ncat==1) &
             call reduce_area (nx_block, ny_block,     &
                               ilo, ihi, jlo, jhi,     &
                               tmask     (:,:,  iblk), &
                               aicen     (:,:,1,iblk), &
                               vicen     (:,:,1,iblk), &
                               aicen_init(:,:,1,iblk), &
                               vicen_init(:,:,1,iblk))
         
      !-----------------------------------------------------------------
      ! ITD cleanup: Rebin thickness categories if necessary, and remove
      !  categories with very small areas.
      !-----------------------------------------------------------------

         call cleanup_itd (nx_block,             ny_block,             &
                           ilo, ihi,             jlo, jhi,             &
                           dt,                   ntrcr,                &
                           aicen   (:,:,:,iblk),                       &
                           trcrn (:,:,1:ntrcr,:,iblk),                 &
                           vicen   (:,:,:,iblk), vsnon (:,:,  :,iblk), &
                           aice0   (:,:,  iblk), aice      (:,:,iblk), &
                           trcr_depend(1:ntrcr), fpond     (:,:,iblk), &
                           fresh   (:,:,  iblk), fsalt     (:,:,iblk), &
                           fhocn   (:,:,  iblk),                       &
                           faero_ocn(:,:,:,iblk),tr_aero,              &
                           fiso_ocn (:,:,:,iblk),tr_iso,               &
                           tr_pond_topo,         heat_capacity,        &
                           nbtrcr,               first_ice(:,:,:,iblk),&
                           flux_bio(:,:,1:nbtrcr,iblk),                &
                           l_stop,                                     &
                           istop,                jstop)

         if (l_stop) then
            write (nu_diag,*) 'istep1, my_task, iblk =', &
                               istep1, my_task, iblk
            write (nu_diag,*) 'Global block:', this_block%block_id
            if (istop > 0 .and. jstop > 0) &
                 write(nu_diag,*) 'Global i and j:', &
                                  this_block%i_glob(istop), &
                                  this_block%j_glob(jstop) 
            call abort_ice ('ice: ITD cleanup error in step_therm2')
         endif

      end subroutine step_therm2

!=======================================================================
!
! finalize thermo updates
!
! authors: Elizabeth Hunke, LANL

      subroutine post_thermo (dt)

      use ice_blocks, only: nx_block, ny_block
      use ice_domain, only: nblocks
      use ice_flux, only: daidtt, dvidtt, dagedtt
      use ice_grid, only: tmask
      use ice_itd, only: aggregate
      use ice_state, only: aicen, trcrn, vicen, vsnon, ntrcr, &
                           aice,  trcr,  vice,  vsno, aice0, trcr_depend, &
                           bound_state, tr_iage, nt_iage
      use ice_timers, only: ice_timer_start, ice_timer_stop, timer_bound

      real (kind=dbl_kind), intent(in) :: &
         dt      ! time step

      integer (kind=int_kind) :: & 
         iblk        , & ! block index 
         i,j             ! horizontal indices

      !-------------------------------------------------------------------
      ! Ghost cell updates for state variables.
      !-------------------------------------------------------------------

      call ice_timer_start(timer_bound)
      call bound_state (aicen, trcrn, &
                        vicen, vsnon)
      call ice_timer_stop(timer_bound)

      !$OMP PARALLEL DO PRIVATE(iblk,i,j)
      do iblk = 1, nblocks

      !-----------------------------------------------------------------
      ! Aggregate the updated state variables (includes ghost cells). 
      !----------------------------------------------------------------- 
 
         call aggregate (nx_block,          ny_block,             &
                         aicen(:,:,:,iblk),                       &
                         trcrn(:,:,1:ntrcr,:,iblk),               &
                         vicen(:,:,:,iblk), vsnon(:,:,  :,iblk),  &
                         aice (:,:,  iblk),                       &
                         trcr (:,:,1:ntrcr,  iblk),               &
                         vice (:,:,  iblk), vsno (:,:,    iblk),  &
                         aice0(:,:,  iblk), tmask(:,:,    iblk),  &
                         ntrcr, trcr_depend(1:ntrcr)) 

      !-----------------------------------------------------------------
      ! Compute thermodynamic area and volume tendencies.
      !-----------------------------------------------------------------

         do j = 1, ny_block
         do i = 1, nx_block
            daidtt(i,j,iblk) = (aice(i,j,iblk) - daidtt(i,j,iblk)) / dt
            dvidtt(i,j,iblk) = (vice(i,j,iblk) - dvidtt(i,j,iblk)) / dt
            if (tr_iage) then
               if (trcr(i,j,nt_iage,iblk) > c0) &
                  dagedtt(i,j,iblk)= (trcr(i,j,nt_iage,iblk)-dagedtt(i,j,iblk)-dt)/dt
            endif
         enddo
         enddo

      enddo ! iblk
      !$OMP END PARALLEL DO

      end subroutine post_thermo

!=======================================================================
!
! Run one time step of dynamics, horizontal transport, and ridging.
! NOTE: The evp and transport modules include boundary updates, so
!       they cannot be done inside a single block loop.  Ridging
!       and cleanup, on the other hand, are single-column operations. 
!       They are called with argument lists inside block loops
!       to increase modularity.
!
! authors: William H. Lipscomb, LANL

      subroutine step_dynamics (dt, ndtd)

      use ice_blocks, only: block, get_block, nx_block, ny_block
      use ice_calendar, only: istep
      use ice_domain, only: blocks_ice, nblocks
      use ice_domain_size, only: nslyr
      use ice_dyn_evp, only: evp
      use ice_dyn_eap, only: eap
      use ice_dyn_shared, only: kdyn
!jd      use ice_flux, only: daidtd, dvidtd, init_history_dyn, dagedtd
      use ice_flux, only: daidtd, dvidtd, dvsdtd, init_history_dyn, dagedtd
      use ice_grid, only: tmask
      use ice_itd, only: aggregate
      use ice_state, only: nt_qsno, trcrn, vsnon, aicen, vicen, ntrcr, &
          aice, trcr, vice, vsno, aice0, trcr_depend, bound_state, tr_iage, nt_iage
      use ice_timers, only: ice_timer_start, ice_timer_stop, timer_column, &
          timer_ridge, timer_bound
      use ice_transport_driver, only: advection, transport_upwind, transport_remap

      real (kind=dbl_kind), intent(in) :: &
         dt      ! time step

      integer (kind=int_kind), intent(in) :: &
         ndtd    ! number of dynamics subcycles

      ! local variables

      type (block) :: &
         this_block      ! block information for current block

      integer (kind=int_kind) :: & 
         iblk        , & ! block index 
         i,j         , & ! horizontal indices
         ilo,ihi,jlo,jhi ! beginning and end of physical domain

      call init_history_dyn     ! initialize dynamic history variables

      !-----------------------------------------------------------------
      ! Elastic-viscous-plastic ice dynamics
      !-----------------------------------------------------------------

      if (kdyn == 1) call evp (dt)
      if (kdyn == 2) call eap (dt)

      !-----------------------------------------------------------------
      ! Horizontal ice transport
      !-----------------------------------------------------------------

      if (advection == 'upwind') then
         call transport_upwind (dt)    ! upwind
      else
         call transport_remap (dt)     ! incremental remapping
      endif

      !-----------------------------------------------------------------
      ! Ridging
      !-----------------------------------------------------------------

      call ice_timer_start(timer_column)
      call ice_timer_start(timer_ridge)

      !$OMP PARALLEL DO PRIVATE(iblk)
      do iblk = 1, nblocks
         call step_ridge (dt, ndtd, iblk)
      enddo                     ! iblk
      !$OMP END PARALLEL DO

      call ice_timer_stop(timer_ridge)

      !-------------------------------------------------------------------
      ! Ghost cell updates for state variables.
      !-------------------------------------------------------------------

      call ice_timer_start(timer_bound)
      call bound_state (aicen, trcrn, &
                        vicen, vsnon)
      call ice_timer_stop(timer_bound)

      !$OMP PARALLEL DO PRIVATE(iblk,i,j,ilo,ihi,jlo,jhi,this_block)
      do iblk = 1, nblocks

      !-----------------------------------------------------------------
      ! Aggregate the updated state variables (includes ghost cells). 
      !----------------------------------------------------------------- 
 
         call aggregate (nx_block,          ny_block,             &
                         aicen(:,:,:,iblk),                       &
                         trcrn(:,:,1:ntrcr,:,iblk),               &
                         vicen(:,:,:,iblk), vsnon(:,:,  :,iblk),  &
                         aice (:,:,  iblk),                       &
                         trcr (:,:,1:ntrcr,  iblk),               &
                         vice (:,:,  iblk), vsno (:,:,    iblk),  &
                         aice0(:,:,  iblk), tmask(:,:,    iblk),  &
                         ntrcr, trcr_depend(1:ntrcr)) 

      !-----------------------------------------------------------------
      ! Compute dynamic area and volume tendencies.
      !-----------------------------------------------------------------

         this_block = get_block(blocks_ice(iblk),iblk)         
         ilo = this_block%ilo
         ihi = this_block%ihi
         jlo = this_block%jlo
         jhi = this_block%jhi

         do j = jlo,jhi
         do i = ilo,ihi
            dvidtd(i,j,iblk) = (vice(i,j,iblk) - dvidtd(i,j,iblk)) /dt
!jd
            dvsdtd(i,j,iblk) = (vsno(i,j,iblk) - dvsdtd(i,j,iblk)) /dt
!jd
            daidtd(i,j,iblk) = (aice(i,j,iblk) - daidtd(i,j,iblk)) /dt
            if (tr_iage) &
               dagedtd(i,j,iblk)= (trcr(i,j,nt_iage,iblk)-dagedtd(i,j,iblk))/dt
         enddo
         enddo

      enddo
      !$OMP END PARALLEL DO

      call ice_timer_stop(timer_column)

      end subroutine step_dynamics

!=======================================================================
!
! Computes sea ice mechanical deformation
!
! authors: William H. Lipscomb, LANL
!          Elizabeth C. Hunke, LANL

      subroutine step_ridge (dt, ndtd, iblk)

      use ice_blocks, only: block, get_block, nx_block, ny_block
      use ice_calendar, only: istep1
      use ice_communicate, only: my_task
      use ice_domain, only: blocks_ice
      use ice_exit, only: abort_ice
      use ice_fileunits, only: nu_diag
      use ice_flux, only: rdg_conv, rdg_shear, dardg1dt, dardg2dt, &
          dvirdgdt, opening, fpond, fresh, fhocn, faero_ocn, fiso_ocn, &
          aparticn, krdgn, aredistn, vredistn, dardg1ndt, dardg2ndt, &
          dvirdgndt, araftn, vraftn, fsalt
      use ice_grid, only: tmask
      use ice_itd, only: cleanup_itd
      use ice_mechred, only: ridge_ice
      use ice_state, only: ntrcr, aicen, trcrn, vicen, vsnon, aice0, &
          trcr_depend, aice, tr_aero, tr_iso, tr_pond_topo, nbtrcr
      use ice_therm_shared, only: heat_capacity
      use ice_zbgc_shared, only: flux_bio, first_ice

      real (kind=dbl_kind), intent(in) :: &
         dt      ! time step

      integer (kind=int_kind), intent(in) :: &
         ndtd, & ! number of dynamics subcycles
         iblk            ! block index

      ! local variables

      real (kind=dbl_kind) :: &
         dtt      ! thermo time step

      type (block) :: &
         this_block      ! block information for current block

      integer (kind=int_kind) :: & 
         i,j         , & ! horizontal indices
         ilo,ihi,jlo,jhi ! beginning and end of physical domain

      integer (kind=int_kind) :: &
         icells          ! number of cells with aicen > puny

      integer (kind=int_kind), dimension(nx_block*ny_block) :: &
         indxi, indxj    ! indirect indices for cells with aicen > puny

      logical (kind=log_kind) :: &
         l_stop          ! if true, abort model

      integer (kind=int_kind) :: &
         istop, jstop    ! indices of grid cell where model aborts

         l_stop = .false.

         this_block = get_block(blocks_ice(iblk), iblk)
         ilo = this_block%ilo
         ihi = this_block%ihi
         jlo = this_block%jlo
         jhi = this_block%jhi

      !-----------------------------------------------------------------
      ! Identify ice-ocean cells.
      ! Note:  We can not define icells here using aice>puny because
      !        aice has not yet been updated since the transport (and
      !        it may be out of whack, which the ridging helps fix).-ECH
      !-----------------------------------------------------------------
           
         icells = 0
         do j = jlo, jhi
         do i = ilo, ihi
            if (tmask(i,j,iblk)) then
               icells = icells + 1
               indxi(icells) = i
               indxj(icells) = j
            endif
         enddo               ! i
         enddo               ! j

         if (icells > 0) then

         call ridge_ice (nx_block,             ny_block,                 &
                         dt,                   ndtd,                     &
                         ntrcr,                icells,                   &
                         indxi,                indxj,                    &
                         rdg_conv(:,:,  iblk), rdg_shear (:,:,  iblk),   &
                         aicen   (:,:,:,iblk),                           &
                         trcrn     (:,:,1:ntrcr,:,iblk),                 &
                         vicen   (:,:,:,iblk), vsnon     (:,:,:,iblk),   &
                         aice0   (:,:,  iblk),                           &
                         trcr_depend(1:ntrcr), l_stop,                   &
                         istop,                jstop,                    &   
                         dardg1dt(:,:,iblk),   dardg2dt  (:,:,iblk),     &
                         dvirdgdt(:,:,iblk),   opening   (:,:,iblk),     &
                         fpond   (:,:,iblk),                             &
                         fresh   (:,:,iblk),   fhocn     (:,:,iblk),     &
                         faero_ocn(:,:,:,iblk),                          &
                         fiso_ocn(:,:,:,iblk),                           &
                         aparticn(:,:,:,iblk), krdgn     (:,:,:,iblk),   &
                         aredistn(:,:,:,iblk), vredistn  (:,:,:,iblk),   &
                         dardg1ndt(:,:,:,iblk),dardg2ndt (:,:,:,iblk),   &
                         dvirdgndt(:,:,:,iblk),                          &
                         araftn   (:,:,:,iblk),vraftn   (:,:,:,iblk))

         if (l_stop) then
            write (nu_diag,*) 'istep1, my_task, iblk =', &
                               istep1, my_task, iblk
            write (nu_diag,*) 'Global block:', this_block%block_id
            if (istop > 0 .and. jstop > 0) &
                 write(nu_diag,*) 'Global i and j:', &
                                  this_block%i_glob(istop), &
                                  this_block%j_glob(jstop) 
            call abort_ice ('ice: Ridging error')
         endif

         endif
    
      !-----------------------------------------------------------------
      ! ITD cleanup: Rebin thickness categories if necessary, and remove
      !  categories with very small areas.
      !-----------------------------------------------------------------

         dtt = dt * ndtd  ! for proper averaging over thermo timestep
         call cleanup_itd (nx_block,             ny_block,             &
                           ilo, ihi,             jlo, jhi,             &
                           dtt,                  ntrcr,                &
                           aicen   (:,:,:,iblk),                       &
                           trcrn (:,:,1:ntrcr,:,iblk),                 &
                           vicen   (:,:,:,iblk), vsnon (:,:,  :,iblk), &
                           aice0   (:,:,  iblk), aice      (:,:,iblk), &
                           trcr_depend(1:ntrcr), fpond     (:,:,iblk), &
                           fresh   (:,:,  iblk), fsalt     (:,:,iblk), &
                           fhocn   (:,:,  iblk),                       &
                           faero_ocn(:,:,:,iblk),tr_aero,              &
                           fiso_ocn (:,:,:,iblk),tr_iso,               &
                           tr_pond_topo,         heat_capacity,        &
                           nbtrcr,               first_ice(:,:,:,iblk),&
                           flux_bio(:,:,1:nbtrcr,iblk),                &
                           l_stop,                                     &
                           istop,                jstop)

         if (l_stop) then
            write (nu_diag,*) 'istep1, my_task, iblk =', &
                               istep1, my_task, iblk
            write (nu_diag,*) 'Global block:', this_block%block_id
            if (istop > 0 .and. jstop > 0) &
                 write(nu_diag,*) 'Global i and j:', &
                                  this_block%i_glob(istop), &
                                  this_block%j_glob(jstop) 
            call abort_ice ('ice: ITD cleanup error in step_ridge')
         endif

      end subroutine step_ridge

!=======================================================================
!
! Computes radiation fields
!
! authors: William H. Lipscomb, LANL
!          David Bailey, NCAR
!          Elizabeth C. Hunke, LANL

      subroutine step_radiation (dt, iblk)

      use ice_blocks, only: block, get_block, nx_block, ny_block
      use ice_domain, only: blocks_ice, nblocks
      use ice_domain_size, only: ncat
      use ice_flux, only: swvdr, swvdf, swidr, swidf, coszen, fsnow
      use ice_grid, only: TLAT, TLON, tmask
      use ice_meltpond_lvl, only: ffracn, dhsn
      use ice_meltpond_topo, only: hp1 
      use ice_shortwave, only: fswsfcn, fswintn, fswthrun, fswpenln, &
                               Sswabsn, Iswabsn, shortwave, &
                               albicen, albsnon, albpndn, &
                               alvdrn, alidrn, alvdfn, alidfn, &
                               run_dedd, shortwave_ccsm3, apeffn, &
                               snowfracn
      use ice_state, only: aicen, vicen, vsnon, trcrn, nt_Tsfc, &
                           nt_apnd, nt_ipnd, nt_hpnd, tr_pond_topo 
      use ice_timers, only: ice_timer_start, ice_timer_stop, timer_sw
      use ice_therm_shared, only: calc_Tsfc

      real (kind=dbl_kind), intent(in) :: &
         dt      ! time step

      integer (kind=int_kind), intent(in) :: &
         iblk            ! block index

      ! local variables

      integer (kind=int_kind) :: &
         i, j,            & ! horizontal indices
         ilo,ihi,jlo,jhi, & ! beginning and end of physical domain
         n                  ! thickness category index

      type (block) :: &
         this_block      ! block information for current block

      call ice_timer_start(timer_sw,iblk)      ! shortwave

      ! Initialize
      do n = 1, ncat
      do j = 1, ny_block
      do i = 1, nx_block
         alvdrn(i,j,n,iblk) = c0
         alidrn(i,j,n,iblk) = c0
         alvdfn(i,j,n,iblk) = c0
         alidfn(i,j,n,iblk) = c0
         fswsfcn(i,j,n,iblk) = c0
         fswintn(i,j,n,iblk) = c0
         fswthrun(i,j,n,iblk) = c0
      enddo   ! i
      enddo   ! j
      enddo   ! ncat
      fswpenln(:,:,:,:,iblk) = c0
      Iswabsn(:,:,:,:,iblk) = c0
      Sswabsn(:,:,:,:,iblk) = c0

      this_block = get_block(blocks_ice(iblk),iblk)         
      ilo = this_block%ilo
      ihi = this_block%ihi
      jlo = this_block%jlo
      jhi = this_block%jhi


      if (calc_Tsfc) then
        if (trim(shortwave) == 'dEdd') then ! delta Eddington
 
          call run_dEdd(ilo, ihi, jlo, jhi,                            &
                       aicen(:,:,:,iblk),     vicen(:,:,:,iblk),       &
                       vsnon(:,:,:,iblk),     trcrn(:,:,:,:,iblk),     &
                       TLAT(:,:,iblk),        TLON(:,:,iblk),          &
                       tmask(:,:,iblk),                                & 
                       swvdr(:,:,iblk),       swvdf(:,:,iblk),         &
                       swidr(:,:,iblk),       swidf(:,:,iblk),         &
                       coszen(:,:,iblk),      fsnow(:,:,iblk),         &
                       alvdrn(:,:,:,iblk),    alvdfn(:,:,:,iblk),      &
                       alidrn(:,:,:,iblk),    alidfn(:,:,:,iblk),      &
                       fswsfcn(:,:,:,iblk),   fswintn(:,:,:,iblk),     &
                       fswthrun(:,:,:,iblk),  fswpenln(:,:,:,:,iblk),  &
                       Sswabsn(:,:,:,:,iblk), Iswabsn(:,:,:,:,iblk),   &
                       albicen(:,:,:,iblk),   albsnon(:,:,:,iblk),     &
                       albpndn(:,:,:,iblk),   apeffn(:,:,:,iblk),      &
                       snowfracn(:,:,:,iblk), &
                       dhsn(:,:,:,iblk),      ffracn(:,:,:,iblk))
         
        else  ! .not. dEdd

          call shortwave_ccsm3(nx_block, ny_block,                      &
                              ilo, ihi, jlo, jhi,                       &
                              aicen(:,:,:,iblk),   vicen(:,:,:,iblk),   &
                              vsnon(:,:,:,iblk),                        &
                              trcrn(:,:,nt_Tsfc,:,iblk),                &
                              swvdr(:,:,  iblk),   swvdf(:,:,  iblk),   &
                              swidr(:,:,  iblk),   swidf(:,:,  iblk),   &
                              alvdrn(:,:,:,iblk),  alidrn(:,:,:,iblk),  &
                              alvdfn(:,:,:,iblk),  alidfn(:,:,:,iblk),  &
                              fswsfcn(:,:,:,iblk), fswintn(:,:,:,iblk), &
                              fswthrun(:,:,:,iblk),                     &
                              fswpenln(:,:,:,:,iblk),                   &
                              Iswabsn(:,:,:,:,iblk),                    &
                              Sswabsn(:,:,:,:,iblk),                    &
                              albicen(:,:,:,iblk), albsnon(:,:,:,iblk), &
                              coszen(:,:,iblk))
        endif   ! shortwave

      else    ! .not. calc_Tsfc

      ! Calculate effective pond area for HadGEM

      if (tr_pond_topo) then
         do n = 1, ncat
           apeffn(:,:,n,iblk) = c0 
           do j = 1, ny_block
             do i = 1, nx_block
               if (aicen(i,j,n,iblk) > puny) then
               ! Lid effective if thicker than hp1
                 if (trcrn(i,j,nt_apnd,n,iblk)*aicen(i,j,n,iblk) > puny .and. &
                     trcrn(i,j,nt_ipnd,n,iblk) < hp1) then
                     apeffn(i,j,n,iblk) = trcrn(i,j,nt_apnd,n,iblk)
                 else
                   apeffn(i,j,n,iblk) = c0
                 endif
                 if (trcrn(i,j,nt_apnd,n,iblk) < puny) apeffn(i,j,n,iblk) = c0
               endif
             enddo 
           enddo
         enddo  ! ncat
 
      endif ! tr_pond_topo

        ! Initialize for safety
        do n = 1, ncat
          do j = 1, ny_block
            do i = 1, nx_block
              alvdrn(i,j,n,iblk) = c0
              alidrn(i,j,n,iblk) = c0
              alvdfn(i,j,n,iblk) = c0
              alidfn(i,j,n,iblk) = c0
              fswsfcn(i,j,n,iblk) = c0
              fswintn(i,j,n,iblk) = c0
              fswthrun(i,j,n,iblk) = c0
            enddo   ! i
          enddo   ! j
        enddo   ! ncat
        Iswabsn(:,:,:,:,iblk) = c0
        Sswabsn(:,:,:,:,iblk) = c0

      endif    ! calc_Tsfc

      call ice_timer_stop(timer_sw,iblk)     ! shortwave

      end subroutine step_radiation

!=======================================================================

      end module ice_step_mod

!=======================================================================
