!  SVN:$Id: ice_therm_bl99.F90 923 2015-03-02 20:33:57Z tcraig $
!=========================================================================
!
! Update ice and snow internal temperatures
! using Bitz and Lipscomb 1999 thermodynamics
!
! authors: William H. Lipscomb, LANL
!          C. M. Bitz, UW
!          Elizabeth C. Hunke, LANL
!
! 2012: Split from ice_therm_vertical.F90

      module ice_therm_bl99

      use ice_kinds_mod
      use ice_domain_size, only: nilyr, nslyr, max_ntrcr, n_aero, ncat
      use ice_constants
      use ice_fileunits, only: nu_diag
      use ice_therm_shared, only: conduct, calc_Tsfc, ferrmax, l_brine, hfrazilmin
      implicit none
      save

      private
      public :: surface_fluxes, temperature_changes

      real (kind=dbl_kind), parameter :: &
         betak   = 0.13_dbl_kind, & ! constant in formula for k (W m-1 ppt-1)
         kimin   = 0.10_dbl_kind    ! min conductivity of saline ice (W m-1 deg-1)

!=======================================================================

      contains

!=======================================================================
!
! Compute new surface temperature and internal ice and snow
! temperatures.  Include effects of salinity on sea ice heat
! capacity in a way that conserves energy (Bitz and Lipscomb, 1999).
!
! New temperatures are computed iteratively by solving a tridiagonal
! system of equations; heat capacity is updated with each iteration.
! Finite differencing is backward implicit.
!
! See Bitz, C.M., and W.H. Lipscomb, 1999:
! An energy-conserving thermodynamic model of sea ice,
! J. Geophys. Res., 104, 15,669-15,677.
!
! authors William H. Lipscomb, LANL
!         C. M. Bitz, UW

      subroutine temperature_changes (nx_block, ny_block, &
                                      my_task,  istep1,   &
                                      dt,       icells,   & 
                                      indxi,    indxj,    &
                                      rhoa,     flw,      &
                                      potT,     Qa,       &
                                      shcoef,   lhcoef,   &
                                      fswsfc,   fswint,   &
                                      Sswabs,   Iswabs,   &
                                      hilyr,    hslyr,    &
                                      zqin,     zTin,     &
                                      zqsn,     zTsn,     &
                                      zSin,               &
                                      Tsf,      Tbot,     &
                                      fsensn,   flatn,    &
                                      flwoutn,  fsurfn,   &
                                      fcondtopn,fcondbotn, &
                                      einit,    l_stop,   &
                                      istop,    jstop)

      use ice_therm_shared, only: surface_heat_flux, dsurface_heat_flux_dTsf

      integer (kind=int_kind), intent(in) :: &
         nx_block, ny_block, & ! block dimensions
         my_task     , & ! task number (diagnostic only)
         istep1      , & ! time step index (diagnostic only)
         icells          ! number of cells with aicen > puny

      real (kind=dbl_kind), intent(in) :: &
         dt              ! time step

      integer (kind=int_kind), dimension(nx_block*ny_block), &
         intent(in) :: &
         indxi, indxj    ! compressed indices for cells with aicen > puny

      real (kind=dbl_kind), dimension (nx_block,ny_block), &
         intent(in) :: &
         rhoa        , & ! air density (kg/m^3)
         flw         , & ! incoming longwave radiation (W/m^2)
         potT        , & ! air potential temperature  (K)
         Qa          , & ! specific humidity (kg/kg)
         shcoef      , & ! transfer coefficient for sensible heat
         lhcoef      , & ! transfer coefficient for latent heat
         Tbot            ! ice bottom surface temperature (deg C)

      real (kind=dbl_kind), dimension (nx_block,ny_block), &
         intent(inout) :: &
         fswsfc      , & ! SW absorbed at ice/snow surface (W m-2)
         fswint          ! SW absorbed in ice interior below surface (W m-2)

      real (kind=dbl_kind), dimension (icells), intent(in) :: &
         hilyr       , & ! ice layer thickness (m)
         hslyr       , & ! snow layer thickness (m)
         einit           ! initial energy of melting (J m-2)

      real (kind=dbl_kind), dimension (nx_block,ny_block,nslyr), &
         intent(inout) :: &
         Sswabs          ! SW radiation absorbed in snow layers (W m-2)

      real (kind=dbl_kind), dimension (nx_block,ny_block,nilyr), &
         intent(inout) :: &
         Iswabs          ! SW radiation absorbed in ice layers (W m-2)

      real (kind=dbl_kind), dimension (nx_block,ny_block), intent(inout):: &
         fsurfn      , & ! net flux to top surface, excluding fcondtopn
         fcondtopn   , & ! downward cond flux at top surface (W m-2)
         fcondbotn   , & ! downward cond flux at bottom surface (W m-2)
         fsensn      , & ! surface downward sensible heat (W m-2)
         flatn       , & ! surface downward latent heat (W m-2)
         flwoutn         ! upward LW at surface (W m-2)


      real (kind=dbl_kind), dimension (icells), &
         intent(inout):: &
         Tsf             ! ice/snow surface temperature, Tsfcn

      real (kind=dbl_kind), dimension (icells,nilyr), &
         intent(inout) :: &
         zqin        , & ! ice layer enthalpy (J m-3)
         zTin        , & ! internal ice layer temperatures
         zSin            ! internal ice layer salinities

      real (kind=dbl_kind), dimension (icells,nslyr), &
         intent(inout) :: &
         zqsn        , & ! snow layer enthalpy (J m-3)
         zTsn            ! internal snow layer temperatures

      logical (kind=log_kind), intent(inout) :: &
         l_stop          ! if true, print diagnostics and abort model

      integer (kind=int_kind), intent(inout) :: &
         istop, jstop    ! i and j indices of cell where model fails

      ! local variables

      integer (kind=int_kind), parameter :: &
         nitermax = 100, & ! max number of iterations in temperature solver
         nmat = nslyr + nilyr + 1  ! matrix dimension

      real (kind=dbl_kind), parameter :: &
         Tsf_errmax = 5.e-4_dbl_kind ! max allowed error in Tsf
                                     ! recommend Tsf_errmax < 0.01 K

      integer (kind=int_kind) :: &
         i, j        , & ! horizontal indices
         ij, m       , & ! horizontal indices, combine i and j loops
         k           , & ! ice layer index
         niter           ! iteration counter in temperature solver

      integer (kind=int_kind) :: &
         isolve          ! number of cells with temps not converged

      integer (kind=int_kind), dimension (icells) :: &
         indxii, indxjj  ! compressed indices for cells not converged

      integer (kind=int_kind), dimension (icells) :: &
         indxij          ! compressed 1D index for cells not converged

      logical (kind=log_kind), dimension (icells) :: &
         l_snow      , & ! true if snow temperatures are computed
         l_cold          ! true if surface temperature is computed

      real (kind=dbl_kind), dimension (:), allocatable :: &
         Tsf_start   , & ! Tsf at start of iteration
         dTsf        , & ! Tsf - Tsf_start
         dTi1        , & ! Ti1(1) - Tin_start(1)
         dfsurf_dT   , & ! derivative of fsurf wrt Tsf
         avg_Tsi     , & ! = 1. if new snow/ice temps avg'd w/starting temps
         enew            ! new energy of melting after temp change (J m-2)

      real (kind=dbl_kind), dimension (icells) :: &
         dTsf_prev   , & ! dTsf from previous iteration
         dTi1_prev   , & ! dTi1 from previous iteration
         dfsens_dT   , & ! deriv of fsens wrt Tsf (W m-2 deg-1)
         dflat_dT    , & ! deriv of flat wrt Tsf (W m-2 deg-1)
         dflwout_dT  , & ! deriv of flwout wrt Tsf (W m-2 deg-1)
         dt_rhoi_hlyr, & ! dt/(rhoi*hilyr)
         einex       , & ! excess energy from dqmat to ocean
         ferr            ! energy conservation error (W m-2)

      real (kind=dbl_kind), dimension (icells,nilyr) :: &
         Tin_init    , & ! zTin at beginning of time step
         Tin_start   , & ! zTin at start of iteration
         dTmat       , & ! zTin - matrix solution before limiting
         dqmat       , & ! associated enthalpy difference
         Tmlts           ! melting temp, -depressT * salinity

      real (kind=dbl_kind), dimension (icells,nslyr) :: &
         Tsn_init    , & ! zTsn at beginning of time step
         Tsn_start   , & ! zTsn at start of iteration
         etas            ! dt / (rho * cp * h) for snow layers

      real (kind=dbl_kind), dimension (:,:), allocatable :: &
         etai        , & ! dt / (rho * cp * h) for ice layers
         sbdiag      , & ! sub-diagonal matrix elements
         diag        , & ! diagonal matrix elements
         spdiag      , & ! super-diagonal matrix elements
         rhs         , & ! rhs of tri-diagonal matrix equation
         Tmat            ! matrix output temperatures

      real (kind=dbl_kind), dimension(icells,nilyr+nslyr+1):: &
         kh              ! effective conductivity at interfaces (W m-2 deg-1)

      real (kind=dbl_kind) :: &
         ci          , & ! specific heat of sea ice (J kg-1 deg-1)
         avg_Tsf     , & ! = 1. if Tsf averaged w/Tsf_start, else = 0.
         Iswabs_tmp  , & ! energy to melt through fraction frac of layer
         Sswabs_tmp  , & ! same for snow
         dswabs      , & ! difference in swabs and swabs_tmp
         frac        , & ! fraction of layer that can be melted through
         dTemp           ! minimum temperature difference for absorption

      logical (kind=log_kind), dimension (icells) :: &
         converged      ! = true when local solution has converged

      logical (kind=log_kind) :: &
         all_converged  ! = true when all cells have converged

      logical (kind=log_kind) , dimension (icells,nilyr) :: &
         reduce_kh        ! reduce conductivity when T exceeds Tmlt

      !-----------------------------------------------------------------
      ! Initialize
      !-----------------------------------------------------------------

      all_converged   = .false.

      do ij = 1, icells

         converged (ij) = .false.
         l_snow    (ij) = .false.
         l_cold    (ij) = .true.
         dTsf_prev (ij) = c0
         dTi1_prev (ij) = c0
         dfsens_dT (ij) = c0
         dflat_dT  (ij) = c0
         dflwout_dT(ij) = c0  
         einex     (ij) = c0
         dt_rhoi_hlyr(ij) = dt / (rhoi*hilyr(ij))  ! hilyr > 0
         if (hslyr(ij) > hs_min/real(nslyr,kind=dbl_kind)) &
            l_snow(ij) = .true.
      enddo                     ! ij

      do k = 1, nslyr
         do ij = 1, icells
            Tsn_init (ij,k) = zTsn(ij,k) ! beginning of time step
            Tsn_start(ij,k) = zTsn(ij,k) ! beginning of iteration
            if (l_snow(ij)) then
               etas(ij,k) = dt/(rhos*cp_ice*hslyr(ij))
            else
               etas(ij,k) = c0
            endif
         enddo                  ! ij
      enddo                     ! k

      do k = 1, nilyr
         do ij = 1, icells
            Tin_init (ij,k) =  zTin(ij,k)   ! beginning of time step
            Tin_start(ij,k) =  zTin(ij,k)   ! beginning of iteration
            Tmlts    (ij,k) = -zSin(ij,k) * depressT
         enddo
      enddo

      !-----------------------------------------------------------------
      ! Compute thermal conductivity at interfaces (held fixed during
      !  subsequent iterations).
      ! Ice and snow interfaces are combined into one array (kh) to
      !  simplify the logic.
      !-----------------------------------------------------------------

      call conductivity (nx_block, ny_block,         &
                         l_snow,   icells,           &
                         indxi,    indxj,    indxij, &
                         hilyr,    hslyr,            &
                         zTin,     kh,       zSin)

      !-----------------------------------------------------------------
      ! Check for excessive absorbed solar radiation that may result in
      ! temperature overshoots. Convergence is particularly difficult
      ! if the starting temperature is already very close to the melting 
      ! temperature and extra energy is added.   In that case, or if the
      ! amount of energy absorbed is greater than the amount needed to
      ! melt through a given fraction of a layer, we put the extra 
      ! energy into the surface.
      ! NOTE: This option is not available if the atmosphere model
      !       has already computed fsurf.  (Unless we adjust fsurf here)
      !-----------------------------------------------------------------
!mclaren: Should there be an if calc_Tsfc statement here then?? 

#ifdef CESMCOUPLED
      frac = c1
      dTemp = p01
#else
      frac = 0.9_dbl_kind
      dTemp = 0.02_dbl_kind
#endif
      do k = 1, nilyr
         do ij = 1, icells
            i = indxi(ij)
            j = indxj(ij)

            Iswabs_tmp = c0 ! all Iswabs is moved into fswsfc

            if (Tin_init(ij,k) <= Tmlts(ij,k) - dTemp) then
               if (l_brine) then
                  ci = cp_ice - Lfresh * Tmlts(ij,k) / (Tin_init(ij,k)**2)
                  Iswabs_tmp = min(Iswabs(i,j,k), &
                     frac*(Tmlts(ij,k)-Tin_init(ij,k))*ci/dt_rhoi_hlyr(ij))
               else
                  ci = cp_ice
                  Iswabs_tmp = min(Iswabs(i,j,k), &
                                   frac*(-Tin_init(ij,k))*ci/dt_rhoi_hlyr(ij))
               endif
            endif
            if (Iswabs_tmp < puny) Iswabs_tmp = c0

            dswabs = min(Iswabs(i,j,k) - Iswabs_tmp, fswint(i,j))

            fswsfc(i,j)   = fswsfc(i,j) + dswabs
            fswint(i,j)   = fswint(i,j) - dswabs
            Iswabs(i,j,k) = Iswabs_tmp

         enddo
      enddo

#ifdef CESMCOUPLED
      frac = 0.9_dbl_kind
#endif
      do k = 1, nslyr
         do ij = 1, icells
            if (l_snow(ij)) then
               i = indxi(ij)
               j = indxj(ij)

               Sswabs_tmp = c0
               if (Tsn_init(ij,k) <= -dTemp) then
                  Sswabs_tmp = min(Sswabs(i,j,k), &
                          -frac*Tsn_init(ij,k)/etas(ij,k))
               endif
               if (Sswabs_tmp < puny) Sswabs_tmp = c0

               dswabs = min(Sswabs(i,j,k) - Sswabs_tmp, fswint(i,j))

               fswsfc(i,j)   = fswsfc(i,j) + dswabs
               fswint(i,j)   = fswint(i,j) - dswabs
               Sswabs(i,j,k) = Sswabs_tmp

            endif
         enddo
      enddo

      !-----------------------------------------------------------------
      ! Solve for new temperatures.
      ! Iterate until temperatures converge with minimal energy error.
      !-----------------------------------------------------------------

      do niter = 1, nitermax

      !-----------------------------------------------------------------
      ! Identify cells, if any, where calculation has not converged.
      !-----------------------------------------------------------------

         if (all_converged) then  ! thermo calculation is done
            exit
         else                     ! identify cells not yet converged
            isolve = 0
            do ij = 1, icells
               i = indxi(ij)
               j = indxj(ij)
               if (.not.converged(ij)) then
                  isolve = isolve + 1
                  indxii(isolve) = i
                  indxjj(isolve) = j
                  indxij(isolve) = ij
               endif
            enddo               ! ij
         endif

      !-----------------------------------------------------------------
      ! Allocate and initialize
      !-----------------------------------------------------------------

         allocate(   sbdiag(isolve,nilyr+nslyr+1))
         allocate(     diag(isolve,nilyr+nslyr+1))
         allocate(   spdiag(isolve,nilyr+nslyr+1))
         allocate(      rhs(isolve,nilyr+nslyr+1))
         allocate(     Tmat(isolve,nilyr+nslyr+1))
         allocate(     etai(isolve,nilyr))
         allocate(Tsf_start(isolve))
         allocate(     dTsf(isolve))
         allocate(dfsurf_dT(isolve))
         allocate(  avg_Tsi(isolve))
         allocate(     enew(isolve))
         allocate(     dTi1(isolve))

         all_converged = .true.

         do ij = 1, isolve
            m = indxij(ij)
            converged(m)  = .true.
            dfsurf_dT(ij) = c0
            avg_Tsi  (ij) = c0
            enew     (ij) = c0
            einex    (m)  = c0
         enddo

      !-----------------------------------------------------------------
      ! Update specific heat of ice layers.
      ! To ensure energy conservation, the specific heat is a function of
      ! both the starting temperature and the (latest guess for) the
      ! final temperature.
      !-----------------------------------------------------------------

         do k = 1, nilyr
            do ij = 1, isolve
               m = indxij(ij)
               i = indxii(ij)
               j = indxjj(ij)

               if (l_brine) then
                  ci = cp_ice - Lfresh*Tmlts(m,k) /  &
                                (zTin(m,k)*Tin_init(m,k))
               else
                  ci = cp_ice
               endif
               etai(ij,k) = dt_rhoi_hlyr(m) / ci

            enddo
         enddo

         if (calc_Tsfc) then

      !-----------------------------------------------------------------
      ! Update radiative and turbulent fluxes and their derivatives
      ! with respect to Tsf.
      !-----------------------------------------------------------------

!DIR$ CONCURRENT !Cray
!cdir nodep      !NEC
!ocl novrec      !Fujitsu
          do ij = 1, isolve
            i = indxii(ij)
            j = indxjj(ij)
            m = indxij(ij)

         ! surface heat flux
         call surface_heat_flux(Tsf    (m),   fswsfc(i,j), &
                                rhoa   (i,j), flw   (i,j), &
                                potT   (i,j), Qa    (i,j), &
                                shcoef (i,j), lhcoef(i,j), &
                                flwoutn(i,j), fsensn(i,j), &
                                flatn  (i,j), fsurfn(i,j))

         ! derivative of heat flux with respect to surface temperature
         call dsurface_heat_flux_dTsf(Tsf      (m),   fswsfc    (i,j), &
                                      rhoa     (i,j), flw       (i,j), &
                                      potT     (i,j), Qa        (i,j), &
                                      shcoef   (i,j), lhcoef    (i,j), &
                                      dfsurf_dT(ij),  dflwout_dT(m),   &
                                      dfsens_dT(m),   dflat_dT  (m))

      !-----------------------------------------------------------------
      ! Compute conductive flux at top surface, fcondtopn.
      ! If fsurfn < fcondtopn and Tsf = 0, then reset Tsf to slightly less
      !  than zero (but not less than -puny).
      !-----------------------------------------------------------------

            if (l_snow(m)) then
               fcondtopn(i,j) = kh(m,1) * (Tsf(m) - zTsn(m,1))
            else
               fcondtopn(i,j) = kh(m,1+nslyr) * (Tsf(m) - zTin(m,1))
            endif

            if ( Tsf(m) >= c0 .and. fsurfn(i,j) < fcondtopn(i,j)) &
                 Tsf(m) = -puny 


      !-----------------------------------------------------------------
      ! Save surface temperature at start of iteration
      !-----------------------------------------------------------------

            Tsf_start(ij) = Tsf(m)

            if (Tsf(m) < c0) then
               l_cold(m) = .true.
            else
               l_cold(m) = .false.
            endif

          enddo                  ! ij

      !-----------------------------------------------------------------
      ! Compute elements of tridiagonal matrix.
      !-----------------------------------------------------------------

            call get_matrix_elements_calc_Tsfc &
                                  (nx_block, ny_block,         &
                                   isolve,   icells,           &
                                   indxii,   indxjj,   indxij, &
                                   l_snow,   l_cold,           &
                                   Tsf,      Tbot,             &
                                   fsurfn,   dfsurf_dT,        &
                                   Tin_init, Tsn_init,         &
                                   kh,       Sswabs,           &
                                   Iswabs,                     &
                                   etai,     etas,             &
                                   sbdiag,   diag,             &
                                   spdiag,   rhs)

         else
            call get_matrix_elements_know_Tsfc &
                                  (nx_block, ny_block,         &
                                   isolve,   icells,           &
                                   indxii,   indxjj,   indxij, &
                                   l_snow,   Tbot,             &
                                   Tin_init, Tsn_init,         &
                                   kh,       Sswabs,           &
                                   Iswabs,                     &
                                   etai,     etas,             &
                                   sbdiag,   diag,             &
                                   spdiag,   rhs,              &
                                   fcondtopn)
         endif  ! calc_Tsfc

      !-----------------------------------------------------------------
      ! Solve tridiagonal matrix to obtain the new temperatures.
      !-----------------------------------------------------------------

         call tridiag_solver (nx_block, ny_block, &
                              isolve,   icells,   &
                              indxii,   indxjj,   &
                              nmat,     sbdiag,   &
                              diag,     spdiag,   &
                              rhs,      Tmat)

      !-----------------------------------------------------------------
      ! Determine whether the computation has converged to an acceptable
      ! solution.  Five conditions must be satisfied:
      !
      !    (1) Tsf <= 0 C.
      !    (2) Tsf is not oscillating; i.e., if both dTsf(niter) and
      !        dTsf(niter-1) have magnitudes greater than puny, then
      !        dTsf(niter)/dTsf(niter-1) cannot be a negative number
      !        with magnitude greater than 0.5.  
      !    (3) abs(dTsf) < Tsf_errmax
      !    (4) If Tsf = 0 C, then the downward turbulent/radiative 
      !        flux, fsurfn, must be greater than or equal to the downward
      !        conductive flux, fcondtopn.
      !    (5) The net energy added to the ice per unit time must equal 
      !        the net change in internal ice energy per unit time,
      !        withinic the prescribed error ferrmax.
      !
      ! For briny ice (the standard case), zTsn and zTin are limited
      !  to prevent them from exceeding their melting temperatures.
      !  (Note that the specific heat formula for briny ice assumes
      !  that T < Tmlt.)  
      ! For fresh ice there is no limiting, since there are cases
      !  when the only convergent solution has zTsn > 0 and/or zTin > 0.
      !  Above-zero temperatures are then reset to zero (with melting 
      !  to conserve energy) in the thickness_changes subroutine.
      !-----------------------------------------------------------------

         if (calc_Tsfc) then

!DIR$ CONCURRENT !Cray
!cdir nodep      !NEC
!ocl novrec      !Fujitsu
          do ij = 1, isolve
            m = indxij(ij)

      !-----------------------------------------------------------------
      ! Reload Tsf from matrix solution
      !-----------------------------------------------------------------

            if (l_cold(m)) then
               if (l_snow(m)) then
                  Tsf(m) = Tmat(ij,1)
               else
                  Tsf(m) = Tmat(ij,1+nslyr)
               endif
            else                ! melting surface
               Tsf(m) = c0
            endif

      !-----------------------------------------------------------------
      ! Initialize convergence flag (true until proven false), dTsf,
      !  and temperature-averaging coefficients.
      ! Average only if test 1 or 2 fails.
      ! Initialize energy.
      !-----------------------------------------------------------------

            dTsf(ij) = Tsf(m) - Tsf_start(ij)
            avg_Tsf  = c0

      !-----------------------------------------------------------------
      ! Condition 1: check for Tsf > 0
      ! If Tsf > 0, set Tsf = 0, then average zTsn and zTin to force
      ! internal temps below their melting temps.
      !-----------------------------------------------------------------

            if (Tsf(m) > puny) then
               Tsf(m) = c0
               dTsf(ij) = -Tsf_start(ij)
               if (l_brine) avg_Tsi(ij) = c1   ! avg with starting temp
               converged(m) = .false.
               all_converged = .false.

      !-----------------------------------------------------------------
      ! Condition 2: check for oscillating Tsf
      ! If oscillating, average all temps to increase rate of convergence.
      !-----------------------------------------------------------------

            elseif (niter > 1 &                ! condition (2)
              .and. Tsf_start(ij) <= -puny &
              .and. abs(dTsf(ij)) > puny &
              .and. abs(dTsf_prev(m)) > puny &
              .and. -dTsf(ij)/(dTsf_prev(m)+puny*puny) > p5) then

               if (l_brine) then ! average with starting temp
                  avg_Tsf  = c1    
                  avg_Tsi(ij) = c1
               endif
               dTsf(ij) = p5 * dTsf(ij)
               converged(m) = .false.
               all_converged = .false.
            endif

!!!            dTsf_prev(m) = dTsf(ij)

      !-----------------------------------------------------------------
      ! If condition 2 failed, average new surface temperature with
      !  starting value.
      !-----------------------------------------------------------------
            Tsf(m)  = Tsf(m) &
                      + avg_Tsf * p5 * (Tsf_start(ij) - Tsf(m))

          enddo  ! ij

         endif   ! calc_Tsfc

         do k = 1, nslyr
!DIR$ CONCURRENT !Cray
!cdir nodep      !NEC
!ocl novrec      !Fujitsu
            do ij = 1, isolve
               m = indxij(ij)

      !-----------------------------------------------------------------
      ! Reload zTsn from matrix solution
      !-----------------------------------------------------------------

               if (l_snow(m)) then
                  zTsn(m,k) = Tmat(ij,k+1)
               else
                  zTsn(m,k) = c0
               endif
               if (l_brine) zTsn(m,k) = min(zTsn(m,k), c0)

      !-----------------------------------------------------------------
      ! If condition 1 or 2 failed, average new snow layer
      !  temperatures with their starting values.
      !-----------------------------------------------------------------
               zTsn(m,k) = zTsn(m,k) &
                         + avg_Tsi(ij)*p5*(Tsn_start(m,k)-zTsn(m,k))

      !-----------------------------------------------------------------
      ! Compute zqsn and increment new energy.
      !-----------------------------------------------------------------
               zqsn(m,k) = -rhos * (Lfresh - cp_ice*zTsn(m,k))
               enew(ij)  = enew(ij) + hslyr(m) * zqsn(m,k)

               Tsn_start(m,k) = zTsn(m,k) ! for next iteration

            enddo               ! ij
         enddo                  ! nslyr

         dTmat(:,:) = c0
         dqmat(:,:) = c0
         reduce_kh(:,:) = .false.
         do k = 1, nilyr
!DIR$ CONCURRENT !Cray
!cdir nodep      !NEC
!ocl novrec      !Fujitsu
            do ij = 1, isolve
               m = indxij(ij)

      !-----------------------------------------------------------------
      ! Reload zTin from matrix solution
      !-----------------------------------------------------------------

               zTin(m,k) = Tmat(ij,k+1+nslyr)

               if (l_brine .and. zTin(m,k) > Tmlts(m,k) - puny) then
                  dTmat(m,k) = zTin(m,k) - Tmlts(m,k)
                  dqmat(m,k) = rhoi * dTmat(m,k) &
                             * (cp_ice - Lfresh * Tmlts(m,k)/zTin(m,k)**2)
! use this for the case that Tmlt changes by an amount dTmlt=Tmltnew-Tmlt(k)
!                             + rhoi * dTmlt &
!                             * (cp_ocn - cp_ice + Lfresh/zTin(m,k))
                  zTin(m,k) = Tmlts(m,k)
                  reduce_kh(m,k) = .true.
               endif

      !-----------------------------------------------------------------
      ! Condition 2b: check for oscillating zTin(1)
      ! If oscillating, average all ice temps to increase rate of convergence.
      !-----------------------------------------------------------------

               if (k==1 .and. .not.calc_Tsfc) then
                  dTi1(ij) = zTin(m,k) - Tin_start(m,k)

                  if (niter > 1 &                    ! condition 2b    
                      .and. abs(dTi1(ij)) > puny &
                      .and. abs(dTi1_prev(m)) > puny &
                      .and. -dTi1(ij)/(dTi1_prev(m)+puny*puny) > p5) then

                     if (l_brine) avg_Tsi(ij) = c1
                     dTi1(ij) = p5 * dTi1(ij)
                     converged(m) = .false.
                     all_converged = .false.
                  endif
                  dTi1_prev(m) = dTi1(ij)
               endif   ! k = 1 .and. calc_Tsfc = F

      !-----------------------------------------------------------------
      ! If condition 1 or 2 failed, average new ice layer
      !  temperatures with their starting values.
      !-----------------------------------------------------------------
               zTin(m,k) = zTin(m,k) &
                         + avg_Tsi(ij)*p5*(Tin_start(m,k)-zTin(m,k))

      !-----------------------------------------------------------------
      ! Compute zqin and increment new energy.
      !-----------------------------------------------------------------
               if (l_brine) then
                  zqin(m,k) = -rhoi * (cp_ice*(Tmlts(m,k)-zTin(m,k)) &
                                      + Lfresh*(c1-Tmlts(m,k)/zTin(m,k)) &
                                      - cp_ocn*Tmlts(m,k))
               else
                  zqin(m,k) = -rhoi * (-cp_ice*zTin(m,k) + Lfresh)
               endif
               enew(ij) = enew(ij) + hilyr(m) * zqin(m,k)
               einex(m) = einex(m) + hilyr(m) * dqmat(m,k)

               Tin_start(m,k) = zTin(m,k) ! for next iteration

            enddo               ! ij
         enddo                  ! nilyr

         if (calc_Tsfc) then

!DIR$ CONCURRENT !Cray
!cdir nodep      !NEC
!ocl novrec      !Fujitsu
          do ij = 1, isolve
            i = indxii(ij)
            j = indxjj(ij)
            m = indxij(ij)

      !-----------------------------------------------------------------
      ! Condition 3: check for large change in Tsf
      !-----------------------------------------------------------------

            if (abs(dTsf(ij)) > Tsf_errmax) then
               converged(m) = .false.
               all_converged = .false.
            endif

      !-----------------------------------------------------------------
      ! Condition 4: check for fsurfn < fcondtopn with Tsf >= 0
      !-----------------------------------------------------------------

            fsurfn(i,j) = fsurfn(i,j) + dTsf(ij)*dfsurf_dT(ij)
            if (l_snow(m)) then
               fcondtopn(i,j) = kh(m,1) * (Tsf(m)-zTsn(m,1))
            else
               fcondtopn(i,j) = kh(m,1+nslyr) * (Tsf(m)-zTin(m,1))
            endif

            if (Tsf(m) >= c0 .and. fsurfn(i,j) < fcondtopn(i,j)) then
               converged(m) = .false.
               all_converged = .false.
            endif

            dTsf_prev(m) = dTsf(ij)

          enddo                  ! ij
         endif                   ! calc_Tsfc

      !-----------------------------------------------------------------
      ! Condition 5: check for energy conservation error
      ! Change in internal ice energy should equal net energy input.
      !-----------------------------------------------------------------

         do ij = 1, isolve
            i = indxii(ij)
            j = indxjj(ij)
            m = indxij(ij)

            fcondbotn(i,j) = kh(m,1+nslyr+nilyr) * &
                          (zTin(m,nilyr) - Tbot(i,j))

            ! Flux extra energy out of the ice
            fcondbotn(i,j) = fcondbotn(i,j) + einex(m)/dt 

            ferr(m) = abs( (enew(ij)-einit(m))/dt &
                    - (fcondtopn(i,j) - fcondbotn(i,j) + fswint(i,j)) )

            ! factor of 0.9 allows for roundoff errors later
            if (ferr(m) > 0.9_dbl_kind*ferrmax) then         ! condition (5)

               converged(m) = .false.
               all_converged = .false.

               ! reduce conductivity for next iteration
               do k = 1, nilyr
                  if (reduce_kh(m,k) .and. dqmat(m,k) > c0) then
                     frac = max(0.5*(c1-ferr(m)/abs(fcondtopn(i,j)-fcondbotn(i,j))),p1)
!                     frac = p1
                     kh(m,k+nslyr+1) = kh(m,k+nslyr+1) * frac
                     kh(m,k+nslyr)   = kh(m,k+nslyr+1)
                  endif
               enddo

            endif               ! ferr 
         enddo                  ! ij

         deallocate(sbdiag)
         deallocate(diag)
         deallocate(spdiag)
         deallocate(rhs)
         deallocate(Tmat)
         deallocate(etai)
         deallocate(Tsf_start)
         deallocate(dTsf)
         deallocate(dfsurf_dT)
         deallocate(avg_Tsi)
         deallocate(enew)
         deallocate(dTi1)

      enddo                     ! temperature iteration niter

      if (.not.all_converged) then

         do ij = 1, icells
            i = indxi(ij)
            j = indxj(ij)

      !-----------------------------------------------------------------
      ! Check for convergence failures.
      !-----------------------------------------------------------------
            if (.not.converged(ij)) then
               write(nu_diag,*) 'Thermo iteration does not converge,', &
                                'istep1, my_task, i, j:', &
                                 istep1, my_task, i, j
               write(nu_diag,*) 'Ice thickness:',  hilyr(ij)*nilyr
               write(nu_diag,*) 'Snow thickness:', hslyr(ij)*nslyr
               write(nu_diag,*) 'dTsf, Tsf_errmax:',dTsf_prev(ij), &
                                 Tsf_errmax
               write(nu_diag,*) 'Tsf:', Tsf(ij)
               write(nu_diag,*) 'fsurf:', fsurfn(i,j)
               write(nu_diag,*) 'fcondtop, fcondbotn, fswint', &
                                 fcondtopn(i,j), fcondbotn(i,j), fswint(i,j)
               write(nu_diag,*) 'fswsfc', fswsfc(i,j)
               write(nu_diag,*) 'Iswabs',(Iswabs(i,j,k),k=1,nilyr)
               write(nu_diag,*) 'Flux conservation error =', ferr(ij)
               write(nu_diag,*) 'Initial snow temperatures:'
               write(nu_diag,*) (Tsn_init(ij,k),k=1,nslyr)
               write(nu_diag,*) 'Initial ice temperatures:'
               write(nu_diag,*) (Tin_init(ij,k),k=1,nilyr)
               write(nu_diag,*) 'Matrix ice temperature diff:'
               write(nu_diag,*) (dTmat(ij,k),k=1,nilyr)
               write(nu_diag,*) 'dqmat*hilyr/dt:'
               write(nu_diag,*) (hilyr(ij)*dqmat(ij,k)/dt,k=1,nilyr)
               write(nu_diag,*) 'Final snow temperatures:'
               write(nu_diag,*) (zTsn(ij,k),k=1,nslyr)
               write(nu_diag,*) 'Matrix ice temperature diff:'
               write(nu_diag,*) (dTmat(ij,k),k=1,nilyr)
               write(nu_diag,*) 'dqmat*hilyr/dt:'
               write(nu_diag,*) (hilyr(ij)*dqmat(ij,k)/dt,k=1,nilyr)
               write(nu_diag,*) 'Final ice temperatures:'
               write(nu_diag,*) (zTin(ij,k),k=1,nilyr)
               write(nu_diag,*) 'Ice melting temperatures:'
               write(nu_diag,*) (Tmlts(ij,k),k=1,nilyr)
               write(nu_diag,*) 'Ice bottom temperature:', Tbot(i,j)
               write(nu_diag,*) 'dT initial:'
               write(nu_diag,*) (Tmlts(ij,k)-Tin_init(ij,k),k=1,nilyr)
               write(nu_diag,*) 'dT final:'
               write(nu_diag,*) (Tmlts(ij,k)-zTin(ij,k),k=1,nilyr)
               write(nu_diag,*) 'zSin'
               write(nu_diag,*) (zSin(ij,k),k=1,nilyr)
               l_stop = .true.
               istop = i
               jstop = j
               return
            endif
         enddo                  ! ij
      endif                     ! all_converged

      if (calc_Tsfc) then
!DIR$ CONCURRENT !Cray
!cdir nodep      !NEC
!ocl novrec      !Fujitsu
         do ij = 1, icells
            i = indxi(ij)
            j = indxj(ij)

            ! update fluxes that depend on Tsf
            flwoutn(i,j) = flwoutn(i,j) + dTsf_prev(ij) * dflwout_dT(ij)
            fsensn(i,j)  = fsensn(i,j)  + dTsf_prev(ij) * dfsens_dT(ij)
            flatn(i,j)   = flatn(i,j)   + dTsf_prev(ij) * dflat_dT(ij)

         enddo                     ! ij
      endif                        ! calc_Tsfc

      end subroutine temperature_changes

!=======================================================================
!
! Compute thermal conductivity at interfaces (held fixed during
!  the subsequent iteration).
!
! NOTE: Ice conductivity must be >= kimin
!
! authors William H. Lipscomb, LANL
!         C. M. Bitz, UW

      subroutine conductivity (nx_block, ny_block,         &
                               l_snow,   icells,           &
                               indxi,    indxj,    indxij, &
                               hilyr,    hslyr,            &
                               zTin,      kh,       zSin)

      integer (kind=int_kind), intent(in) :: &
         nx_block, ny_block, & ! block dimensions
         icells          ! number of cells with aicen > puny

      logical (kind=log_kind), dimension(icells), &
         intent(in) :: &
         l_snow          ! true if snow temperatures are computed

      integer (kind=int_kind), dimension(nx_block*ny_block), &
         intent(in) :: &
         indxi, indxj    ! compressed indices for cells with aicen > puny

      integer (kind=int_kind), dimension (icells), &
         intent(in) :: &
         indxij          ! compressed 1D index for cells not converged

      real (kind=dbl_kind), dimension (icells), intent(in) :: &
         hilyr       , & ! ice layer thickness (same for all ice layers)
         hslyr           ! snow layer thickness (same for all snow layers)

      real (kind=dbl_kind), dimension (icells,nilyr), &
         intent(in) :: &
         zTin         , & ! internal ice layer temperatures
         zSin             ! internal ice layer salinities

      real (kind=dbl_kind), dimension (icells,nilyr+nslyr+1), &
         intent(out) :: &
         kh              ! effective conductivity at interfaces (W m-2 deg-1)

      ! local variables

      integer (kind=int_kind) :: &
         ij          , & ! horizontal index, combines i and j loops
         k               ! vertical index

      real (kind=dbl_kind), dimension (icells,nilyr) :: &
         kilyr           ! thermal cond at ice layer midpoints (W m-1 deg-1)

      real (kind=dbl_kind), dimension (icells,nslyr) :: &
         kslyr           ! thermal cond at snow layer midpoints (W m-1 deg-1)

      ! interior snow layers (simple for now, but may be fancier later)
      do k = 1, nslyr
         do ij = 1, icells
            kslyr(ij,k) = ksno
         enddo
      enddo                     ! nslyr

      ! interior ice layers
      if (conduct == 'MU71') then
         ! Maykut and Untersteiner 1971 form (with Wettlaufer 1991 constants)
         do k = 1, nilyr
!DIR$ CONCURRENT !Cray
!cdir nodep      !NEC
!ocl novrec      !Fujitsu
            do ij = 1, icells
               kilyr(ij,k) = kice + betak*zSin(ij,k)/min(-puny,zTin(ij,k))
               kilyr(ij,k) = max (kilyr(ij,k), kimin)
            enddo
         enddo                     ! nilyr
      else
         ! Pringle et al JGR 2007 'bubbly brine'
         do k = 1, nilyr
!DIR$ CONCURRENT !Cray
!cdir nodep      !NEC
!ocl novrec      !Fujitsu
            do ij = 1, icells
               kilyr(ij,k) = (2.11_dbl_kind - 0.011_dbl_kind*zTin(ij,k) &
                            + 0.09_dbl_kind*zSin(ij,k)/min(-puny,zTin(ij,k))) &
                            * rhoi / 917._dbl_kind
               kilyr(ij,k) = max (kilyr(ij,k), kimin)
            enddo
         enddo                     ! nilyr
      endif ! conductivity

      ! top snow interface, top and bottom ice interfaces
      do ij = 1, icells
         ! top of snow layer; top surface of top ice layer
         if (l_snow(ij)) then
            kh(ij,1)       = c2 * kslyr(ij,1) / hslyr(ij)
            kh(ij,1+nslyr) = c2 * kslyr(ij,nslyr) * kilyr(ij,1) / &
                             ( kslyr(ij,nslyr)*hilyr(ij) +  &
                               kilyr(ij,1    )*hslyr(ij) )
         else
            kh(ij,1)       = c0
            kh(ij,1+nslyr) = c2 * kilyr(ij,1) / hilyr(ij)
         endif

         ! bottom surface of bottom ice layer
         kh(ij,1+nslyr+nilyr) = c2 * kilyr(ij,nilyr) / hilyr(ij)

      enddo                     ! ij

      ! interior snow interfaces

      if (nslyr > 1) then
         do k = 2, nslyr
            do ij = 1, icells
               if (l_snow(ij)) then
                  kh(ij,k) = c2 * kslyr(ij,k-1) * kslyr(ij,k) / &
                            ((kslyr(ij,k-1) + kslyr(ij,k))*hslyr(ij))
               else
                  kh(ij,k) = c0
               endif
            enddo                  ! ij
         enddo                     ! nilyr
      endif ! nslyr > 1

      ! interior ice interfaces
      do k = 2, nilyr
         do ij = 1, icells
            kh(ij,k+nslyr) = c2 * kilyr(ij,k-1) * kilyr(ij,k) / &
                            ((kilyr(ij,k-1) + kilyr(ij,k))*hilyr(ij))
         enddo                  ! ij
      enddo                     ! nilyr

      end subroutine conductivity

!=======================================================================
!
! Compute radiative and turbulent fluxes and their derivatives
! with respect to Tsf.
!
! authors William H. Lipscomb, LANL
!         C. M. Bitz, UW

      subroutine surface_fluxes (nx_block,   ny_block,          &
                                 isolve,     icells,            &
                                 indxii,     indxjj,    indxij, &
                                 Tsf,        fswsfc,            &
                                 rhoa,       flw,               &
                                 potT,       Qa,                &
                                 shcoef,     lhcoef,            &
                                 flwoutn,    fsensn,            &
                                 flatn,      fsurfn,            &
                                 dflwout_dT, dfsens_dT,         &
                                 dflat_dT,   dfsurf_dT)

      use ice_therm_shared, only: surface_heat_flux, dsurface_heat_flux_dTsf

      integer (kind=int_kind), intent(in) :: &
         nx_block, ny_block, & ! block dimensions
         isolve            , & ! number of cells with temps not converged
         icells                ! number of cells with ice present

      integer (kind=int_kind), dimension(icells), &
         intent(in) :: &
         indxii, indxjj  ! compressed indices for cells not converged

      integer (kind=int_kind), dimension (icells) :: &
         indxij          ! compressed 1D index for cells not converged

      real (kind=dbl_kind), dimension (icells), intent(in) :: &
         Tsf             ! ice/snow surface temperature, Tsfcn

      real (kind=dbl_kind), dimension (nx_block,ny_block), intent(in) :: &
         fswsfc      , & ! SW absorbed at ice/snow surface (W m-2)
         rhoa        , & ! air density (kg/m^3)
         flw         , & ! incoming longwave radiation (W/m^2)
         potT        , & ! air potential temperature  (K)
         Qa          , & ! specific humidity (kg/kg)
         shcoef      , & ! transfer coefficient for sensible heat
         lhcoef          ! transfer coefficient for latent heat

      real (kind=dbl_kind), dimension (nx_block,ny_block), &
         intent(inout) :: &
         fsensn      , & ! surface downward sensible heat (W m-2)
         flatn       , & ! surface downward latent heat (W m-2)
         flwoutn     , & ! upward LW at surface (W m-2)
         fsurfn          ! net flux to top surface, excluding fcondtopn

      real (kind=dbl_kind), dimension (icells), &
         intent(inout) :: &
         dfsens_dT   , & ! deriv of fsens wrt Tsf (W m-2 deg-1)
         dflat_dT    , & ! deriv of flat wrt Tsf (W m-2 deg-1)
         dflwout_dT      ! deriv of flwout wrt Tsf (W m-2 deg-1)

      real (kind=dbl_kind), dimension (isolve), &
         intent(inout) :: &
         dfsurf_dT       ! derivative of fsurfn wrt Tsf

      ! local variables

      integer (kind=int_kind) :: &
         i, j        , & ! horizontal indices
         ij, m           ! horizontal indices, combine i and j loops

!DIR$ CONCURRENT !Cray
!cdir nodep      !NEC
!ocl novrec      !Fujitsu
      do ij = 1, isolve
         i = indxii(ij)         ! NOTE: not indxi and indxj
         j = indxjj(ij)
         m = indxij(ij)

         ! surface heat flux
         call surface_heat_flux(Tsf    (m),   fswsfc(i,j), &
                                rhoa   (i,j), flw   (i,j), &
                                potT   (i,j), Qa    (i,j), &
                                shcoef (i,j), lhcoef(i,j), &
                                flwoutn(i,j), fsensn(i,j), &
                                flatn  (i,j), fsurfn(i,j))

         ! derivative of heat flux with respect to surface temperature
         call dsurface_heat_flux_dTsf(Tsf      (m),   fswsfc    (i,j), &
                                      rhoa     (i,j), flw       (i,j), &
                                      potT     (i,j), Qa        (i,j), &
                                      shcoef   (i,j), lhcoef    (i,j), &
                                      dfsurf_dT(ij),  dflwout_dT(m),   &
                                      dfsens_dT(m),   dflat_dT  (m))
      enddo                     ! ij

      end subroutine surface_fluxes

!=======================================================================
!
! Compute terms in tridiagonal matrix that will be solved to find
!  the new vertical temperature profile
! This routine is for the case in which Tsfc is being computed.
!
! authors William H. Lipscomb, LANL
!         C. M. Bitz, UW
!
! March 2004 by William H. Lipscomb for multiple snow layers
! April 2008 by E. C. Hunke, divided into two routines based on calc_Tsfc 

      subroutine get_matrix_elements_calc_Tsfc &
                                     (nx_block, ny_block,         &
                                      isolve,   icells,           &
                                      indxii,   indxjj,   indxij, &
                                      l_snow,   l_cold,           &
                                      Tsf,      Tbot,             &
                                      fsurfn,   dfsurf_dT,        &
                                      Tin_init, Tsn_init,         &
                                      kh,       Sswabs,           &
                                      Iswabs,                     &
                                      etai,     etas,             &
                                      sbdiag,   diag,             &
                                      spdiag,   rhs)

      integer (kind=int_kind), intent(in) :: &
         nx_block, ny_block, & ! block dimensions
         isolve            , & ! number of cells with temps not converged
         icells                ! number of cells with aicen > puny

      integer (kind=int_kind), dimension(icells), &
         intent(in) :: &
         indxii, indxjj  ! compressed indices for cells not converged

      integer (kind=int_kind), dimension (icells), &
         intent(in) :: &
         indxij          ! compressed 1D index for cells not converged

      logical (kind=log_kind), dimension (icells), &
         intent(in) :: &
         l_snow      , & ! true if snow temperatures are computed
         l_cold          ! true if surface temperature is computed

      real (kind=dbl_kind), dimension (icells), intent(in) :: &
         Tsf             ! ice/snow top surface temp (deg C)

      real (kind=dbl_kind), dimension (nx_block,ny_block), intent(in) :: &
         fsurfn      , & ! net flux to top surface, excluding fcondtopn (W/m^2)
         Tbot            ! ice bottom surface temperature (deg C)

      real (kind=dbl_kind), dimension (isolve), intent(in) :: &
         dfsurf_dT       ! derivative of fsurf wrt Tsf

      real (kind=dbl_kind), dimension (isolve,nilyr), &
         intent(in) :: &
         etai            ! dt / (rho*cp*h) for ice layers

      real (kind=dbl_kind), dimension (icells,nilyr), &
         intent(in) :: &
         Tin_init        ! ice temp at beginning of time step

      real (kind=dbl_kind), dimension (nx_block,ny_block,nslyr), &
         intent(in) :: &
         Sswabs          ! SW radiation absorbed in snow layers (W m-2)

      real (kind=dbl_kind), dimension (nx_block,ny_block,nilyr), &
         intent(in) :: &
         Iswabs          ! absorbed SW flux in ice layers

      real (kind=dbl_kind), dimension (icells,nslyr), &
         intent(in) :: &
         etas        , & ! dt / (rho*cp*h) for snow layers
         Tsn_init        ! snow temp at beginning of time step
                         ! Note: no absorbed SW in snow layers

      real (kind=dbl_kind), dimension (icells,nslyr+nilyr+1), &
         intent(in) :: &
         kh              ! effective conductivity at layer interfaces

      real (kind=dbl_kind), dimension (isolve,nslyr+nilyr+1), &
         intent(inout) :: &
         sbdiag      , & ! sub-diagonal matrix elements
         diag        , & ! diagonal matrix elements
         spdiag      , & ! super-diagonal matrix elements
         rhs             ! rhs of tri-diagonal matrix eqn.

      ! local variables

      integer (kind=int_kind) :: &
         i, j        , & ! horizontal indices
         ij, m       , & ! horizontal indices, combine i and j loops
         k, ki, kr       ! vertical indices and row counters

      !-----------------------------------------------------------------
      ! Initialize matrix elements.
      ! Note: When we do not need to solve for the surface or snow
      !       temperature, we solve dummy equations with solution T = 0.
      !       Ice layers are fully initialized below.
      !-----------------------------------------------------------------

      do k = 1, nslyr+1
         do ij = 1, isolve
            sbdiag(ij,k) = c0
            diag  (ij,k) = c1
            spdiag(ij,k) = c0
            rhs   (ij,k) = c0
         enddo
      enddo
            
      !-----------------------------------------------------------------
      ! Compute matrix elements
      !
      ! Four possible cases to solve:
      !   (1) Cold surface (Tsf < 0), snow present
      !   (2) Melting surface (Tsf = 0), snow present
      !   (3) Cold surface (Tsf < 0), no snow
      !   (4) Melting surface (Tsf = 0), no snow
      !-----------------------------------------------------------------

         do ij = 1, isolve
            i = indxii(ij)
            j = indxjj(ij)
            m = indxij(ij)

      !-----------------------------------------------------------------
      ! Tsf equation for case of cold surface (with or without snow)
      !-----------------------------------------------------------------
            if (l_cold(m)) then
               if (l_snow(m)) then
                  k = 1
               else                ! no snow
                  k = 1 + nslyr
               endif
               kr = k

               sbdiag(ij,kr) = c0
               diag  (ij,kr) = dfsurf_dT(ij) - kh(m,k)
               spdiag(ij,kr) = kh(m,k)
               rhs   (ij,kr) = dfsurf_dT(ij)*Tsf(m) - fsurfn(i,j)
            endif                  ! l_cold

      !-----------------------------------------------------------------
      ! top snow layer
      !-----------------------------------------------------------------
!           k = 1
!           kr = 2

            if (l_snow(m)) then
               if (l_cold(m)) then
                  sbdiag(ij,2) = -etas(m,1) * kh(m,1)
                  spdiag(ij,2) = -etas(m,1) * kh(m,2)
                  diag  (ij,2) = c1 &
                                + etas(m,1) * (kh(m,1) + kh(m,2))
                  rhs   (ij,2) = Tsn_init(m,1) &
                                + etas(m,1) * Sswabs(i,j,1)
               else                ! melting surface
                  sbdiag(ij,2) = c0
                  spdiag(ij,2) = -etas(m,1) * kh(m,2)
                  diag  (ij,2) = c1 &
                                + etas(m,1) * (kh(m,1) + kh(m,2))
                  rhs   (ij,2) = Tsn_init(m,1) &
                                + etas(m,1)*kh(m,1)*Tsf(m) &
                                + etas(m,1) * Sswabs(i,j,1)
               endif               ! l_cold
            endif                  ! l_snow

         enddo                    ! ij

      !-----------------------------------------------------------------
      ! remaining snow layers
      !-----------------------------------------------------------------

      if (nslyr > 1) then

         do k = 2, nslyr
            kr = k + 1

            do ij = 1, isolve
               i = indxii(ij)
               j = indxjj(ij)
               m = indxij(ij)

               if (l_snow(m)) then
                  sbdiag(ij,kr) = -etas(m,k) * kh(m,k)
                  spdiag(ij,kr) = -etas(m,k) * kh(m,k+1)
                  diag  (ij,kr) = c1 &
                               + etas(m,k) * (kh(m,k) + kh(m,k+1))
                  rhs   (ij,kr) = Tsn_init(m,k) &
                               + etas(m,k) * Sswabs(i,j,k)
               endif
            enddo               ! ij
         enddo                  ! nslyr

      endif                     ! nslyr > 1


      if (nilyr > 1) then

      !-----------------------------------------------------------------
      ! top ice layer
      !-----------------------------------------------------------------

         ki = 1
         k  = ki + nslyr
         kr = k + 1

            do ij = 1, isolve
               i = indxii(ij)
               j = indxjj(ij)
               m = indxij(ij)

               if (l_snow(m) .or. l_cold(m)) then
                  sbdiag(ij,kr) = -etai(ij,ki) * kh(m,k)
                  spdiag(ij,kr) = -etai(ij,ki) * kh(m,k+1)
                  diag  (ij,kr) = c1 &
                                 + etai(ij,ki) * (kh(m,k) + kh(m,k+1))
                  rhs   (ij,kr) = Tin_init(m,ki) &
                                 + etai(ij,ki)*Iswabs(i,j,ki)
               else    ! no snow, warm surface
                  sbdiag(ij,kr) = c0
                  spdiag(ij,kr) = -etai(ij,ki) * kh(m,k+1)
                  diag  (ij,kr) = c1 &
                                 + etai(ij,ki) * (kh(m,k) + kh(m,k+1))
                  rhs   (ij,kr) = Tin_init(m,ki) &
                                 + etai(ij,ki)*Iswabs(i,j,ki) &
                                 + etai(ij,ki)*kh(m,k)*Tsf(m)
               endif
            enddo    ! ij

      !-----------------------------------------------------------------
      ! bottom ice layer
      !-----------------------------------------------------------------

         ki = nilyr
         k  = ki + nslyr
         kr = k + 1
      
         do ij = 1, isolve
            i = indxii(ij)
            j = indxjj(ij)
            m = indxij(ij)
            sbdiag(ij,kr) = -etai(ij,ki) * kh(m,k)
            spdiag(ij,kr) = c0
            diag  (ij,kr) = c1  &
                           + etai(ij,ki) * (kh(m,k) + kh(m,k+1))
            rhs   (ij,kr) = Tin_init(m,ki) &
                           + etai(ij,ki)*Iswabs(i,j,ki) &
                           + etai(ij,ki)*kh(m,k+1)*Tbot(i,j)
         enddo                   ! ij
      
      else         ! nilyr = 1

      !-----------------------------------------------------------------
      ! single ice layer
      !-----------------------------------------------------------------

         ki = 1
         k  = ki + nslyr
         kr = k + 1

            do ij = 1, isolve
               i = indxii(ij)
               j = indxjj(ij)
               m = indxij(ij)

               if (l_snow(m) .or. l_cold(m)) then
                  sbdiag(ij,kr) = -etai(ij,ki) * kh(m,k)
                  spdiag(ij,kr) = c0
                  diag  (ij,kr) = c1                                 &
                                 + etai(ij,ki) * (kh(m,k) + kh(m,k+1))
                  rhs   (ij,kr) = Tin_init(m,ki)                     &
                                 + etai(ij,ki) * Iswabs(i,j,ki)      &
                                 + etai(ij,ki) * kh(m,k+1)*Tbot(i,j)
               else   ! no snow, warm surface
                  sbdiag(ij,kr) = c0
                  spdiag(ij,kr) = c0
                  diag  (ij,kr) = c1                                 &
                                 + etai(ij,ki) * (kh(m,k) + kh(m,k+1))
                  rhs   (ij,kr) = Tin_init(m,ki)                     &
                                 + etai(ij,ki) * Iswabs(i,j,ki)      &
                                 + etai(ij,ki) * kh(m,k)*Tsf(m)      &
                                 + etai(ij,ki) * kh(m,k+1)*Tbot(i,j)
               endif
            enddo                  ! ij
 
      endif        ! nilyr > 1

      !-----------------------------------------------------------------
      ! interior ice layers
      !-----------------------------------------------------------------

      do ki = 2, nilyr-1
           
         k  = ki + nslyr
         kr = k + 1
         do ij = 1, isolve
            i = indxii(ij)
            j = indxjj(ij)
            m = indxij(ij)

            sbdiag(ij,kr) = -etai(ij,ki) * kh(m,k)
            spdiag(ij,kr) = -etai(ij,ki) * kh(m,k+1)
            diag  (ij,kr) = c1 &
                           + etai(ij,ki) * (kh(m,k) + kh(m,k+1))
            rhs   (ij,kr) = Tin_init(m,ki) &
                           + etai(ij,ki)*Iswabs(i,j,ki)
         enddo                  ! ij
      enddo                     ! nilyr

      end subroutine get_matrix_elements_calc_Tsfc

!=======================================================================
!
! Compute terms in tridiagonal matrix that will be solved to find
!  the new vertical temperature profile
! This routine is for the case in which Tsfc is already known.
!
! authors William H. Lipscomb, LANL
!         C. M. Bitz, UW
!
! March 2004 by William H. Lipscomb for multiple snow layers
! April 2008 by E. C. Hunke, divided into two routines based on calc_Tsfc 

      subroutine get_matrix_elements_know_Tsfc &
                                     (nx_block, ny_block,         &
                                      isolve,   icells,           &
                                      indxii,   indxjj,   indxij, &
                                      l_snow,   Tbot,             &
                                      Tin_init, Tsn_init,         &
                                      kh,       Sswabs,           &
                                      Iswabs,                     &
                                      etai,     etas,             &
                                      sbdiag,   diag,             &
                                      spdiag,   rhs,              &
                                      fcondtopn)

      integer (kind=int_kind), intent(in) :: &
         nx_block, ny_block, & ! block dimensions
         isolve            , & ! number of cells with temps not converged
         icells                ! number of cells with aicen > puny

      integer (kind=int_kind), dimension(icells), &
         intent(in) :: &
         indxii, indxjj  ! compressed indices for cells not converged

      integer (kind=int_kind), dimension (icells), &
         intent(in) :: &
         indxij          ! compressed 1D index for cells not converged

      logical (kind=log_kind), dimension (icells), &
         intent(in) :: &
         l_snow          ! true if snow temperatures are computed

      real (kind=dbl_kind), dimension (nx_block,ny_block), intent(in) :: &
         Tbot            ! ice bottom surface temperature (deg C)

      real (kind=dbl_kind), dimension (isolve,nilyr), &
         intent(in) :: &
         etai            ! dt / (rho*cp*h) for ice layers

      real (kind=dbl_kind), dimension (icells,nilyr), &
         intent(in) :: &
         Tin_init        ! ice temp at beginning of time step

      real (kind=dbl_kind), dimension (nx_block,ny_block,nslyr), &
         intent(in) :: &
         Sswabs          ! SW radiation absorbed in snow layers (W m-2)

      real (kind=dbl_kind), dimension (nx_block,ny_block,nilyr), &
         intent(in) :: &
         Iswabs          ! absorbed SW flux in ice layers

      real (kind=dbl_kind), dimension (icells,nslyr), &
         intent(in) :: &
         etas        , & ! dt / (rho*cp*h) for snow layers
         Tsn_init        ! snow temp at beginning of time step
                         ! Note: no absorbed SW in snow layers

      real (kind=dbl_kind), dimension (icells,nslyr+nilyr+1), &
         intent(in) :: &
         kh              ! effective conductivity at layer interfaces

      real (kind=dbl_kind), dimension (isolve,nslyr+nilyr+1), &
         intent(inout) :: &
         sbdiag      , & ! sub-diagonal matrix elements
         diag        , & ! diagonal matrix elements
         spdiag      , & ! super-diagonal matrix elements
         rhs             ! rhs of tri-diagonal matrix eqn.

      real (kind=dbl_kind), dimension (nx_block,ny_block), intent(in),  &
         optional :: &
         fcondtopn       ! conductive flux at top sfc, positive down (W/m^2)

      ! local variables

      integer (kind=int_kind) :: &
         i, j        , & ! horizontal indices
         ij, m       , & ! horizontal indices, combine i and j loops
         k, ki, kr       ! vertical indices and row counters

      !-----------------------------------------------------------------
      ! Initialize matrix elements.
      ! Note: When we do not need to solve for the surface or snow
      !       temperature, we solve dummy equations with solution T = 0.
      !       Ice layers are fully initialized below.
      !-----------------------------------------------------------------

      do k = 1, nslyr+1
         do ij = 1, isolve
            sbdiag(ij,k) = c0
            diag  (ij,k) = c1
            spdiag(ij,k) = c0
            rhs   (ij,k) = c0
         enddo
      enddo
            
      !-----------------------------------------------------------------
      ! Compute matrix elements
      !
      ! Four possible cases to solve:
      !   (1) Cold surface (Tsf < 0), snow present
      !   (2) Melting surface (Tsf = 0), snow present
      !   (3) Cold surface (Tsf < 0), no snow
      !   (4) Melting surface (Tsf = 0), no snow
      !-----------------------------------------------------------------

      !-----------------------------------------------------------------
      ! top snow layer
      !-----------------------------------------------------------------
!        k = 1
!        kr = 2

         do ij = 1, isolve
            i = indxii(ij)
            j = indxjj(ij)
            m = indxij(ij)

            if (l_snow(m)) then
               sbdiag(ij,2) = c0
               spdiag(ij,2) = -etas(m,1) * kh(m,2)
               diag  (ij,2) = c1                                 &
                             + etas(m,1) * kh(m,2)
               rhs   (ij,2) = Tsn_init(m,1)                      &
                             + etas(m,1) * Sswabs(i,j,1)         &
                             + etas(m,1) * fcondtopn(i,j)
            endif   ! l_snow
         enddo   ! ij

      !-----------------------------------------------------------------
      ! remaining snow layers
      !-----------------------------------------------------------------

      if (nslyr > 1) then

         do k = 2, nslyr
            kr = k + 1

            do ij = 1, isolve
               i = indxii(ij)
               j = indxjj(ij)
               m = indxij(ij)

               if (l_snow(m)) then
                  sbdiag(ij,kr) = -etas(m,k) * kh(m,k)
                  spdiag(ij,kr) = -etas(m,k) * kh(m,k+1)
                  diag  (ij,kr) = c1 &
                               + etas(m,k) * (kh(m,k) + kh(m,k+1))
                  rhs   (ij,kr) = Tsn_init(m,k) &
                               + etas(m,k) * Sswabs(i,j,k)
               endif
            enddo               ! ij
         enddo                  ! nslyr

      endif                     ! nslyr > 1


      if (nilyr > 1) then

      !-----------------------------------------------------------------
      ! top ice layer
      !-----------------------------------------------------------------

         ki = 1
         k  = ki + nslyr
         kr = k + 1

            do ij = 1, isolve
               i = indxii(ij)
               j = indxjj(ij)
               m = indxij(ij)

               if (l_snow(m)) then

                  sbdiag(ij,kr) = -etai(ij,ki) * kh(m,k)
                  spdiag(ij,kr) = -etai(ij,ki) * kh(m,k+1)
                  diag  (ij,kr) = c1                                &
                                 + etai(ij,ki) * (kh(m,k) + kh(m,k+1))
                  rhs   (ij,kr) = Tin_init(m,ki)                    &
                                 + etai(ij,ki) * Iswabs(i,j,ki)
               else                  
                  sbdiag(ij,kr) = c0
                  spdiag(ij,kr) = -etai(ij,ki) * kh(m,k+1)
                  diag  (ij,kr) = c1                                &
                                 + etai(ij,ki) * kh(m,k+1)
                  rhs   (ij,kr) = Tin_init(m,ki)                    &
                                 + etai(ij,ki) * Iswabs(i,j,ki)       &
                                 + etai(ij,ki) * fcondtopn(i,j)
               endif  ! l_snow
            enddo   ! ij

      !-----------------------------------------------------------------
      ! bottom ice layer
      !-----------------------------------------------------------------

         ki = nilyr
         k  = ki + nslyr
         kr = k + 1
      
         do ij = 1, isolve
            i = indxii(ij)
            j = indxjj(ij)
            m = indxij(ij)
            sbdiag(ij,kr) = -etai(ij,ki) * kh(m,k)
            spdiag(ij,kr) = c0
            diag  (ij,kr) = c1  &
                           + etai(ij,ki) * (kh(m,k) + kh(m,k+1))
            rhs   (ij,kr) = Tin_init(m,ki) &
                           + etai(ij,ki)*Iswabs(i,j,ki) &
                           + etai(ij,ki)*kh(m,k+1)*Tbot(i,j)

         enddo                   ! ij
      
      else         ! nilyr = 1

      !-----------------------------------------------------------------
      ! single ice layer
      !-----------------------------------------------------------------

         ki = 1
         k  = ki + nslyr
         kr = k + 1

            do ij = 1, isolve
               i = indxii(ij)
               j = indxjj(ij)
               m = indxij(ij)

               if (l_snow(m)) then
                  sbdiag(ij,kr) = -etai(ij,ki) * kh(m,k)
                  spdiag(ij,kr) = c0
                  diag  (ij,kr) = c1                                 &
                                 + etai(ij,ki) * (kh(m,k) + kh(m,k+1))
                  rhs   (ij,kr) = Tin_init(m,ki)                     &
                                 + etai(ij,ki) * Iswabs(i,j,ki)      &
                                 + etai(ij,ki) * kh(m,k+1)*Tbot(i,j)
               else
                  sbdiag(ij,kr) = c0
                  spdiag(ij,kr) = c0
                  diag  (ij,kr) = c1                                 &
                                 + etai(ij,ki) * kh(m,k+1)
                  rhs   (ij,kr) = Tin_init(m,ki)                     &
                                 + etai(ij,ki) * Iswabs(i,j,ki)      &
                                 + etai(ij,ki) * fcondtopn(i,j)      &
                                 + etai(ij,ki) * kh(m,k+1)*Tbot(i,j)
               endif
            enddo                     ! ij

      endif        ! nilyr > 1

      !-----------------------------------------------------------------
      ! interior ice layers
      !-----------------------------------------------------------------

      do ki = 2, nilyr-1
           
         k  = ki + nslyr
         kr = k + 1
         do ij = 1, isolve
            i = indxii(ij)
            j = indxjj(ij)
            m = indxij(ij)

            sbdiag(ij,kr) = -etai(ij,ki) * kh(m,k)
            spdiag(ij,kr) = -etai(ij,ki) * kh(m,k+1)
            diag  (ij,kr) = c1 &
                           + etai(ij,ki) * (kh(m,k) + kh(m,k+1))
            rhs   (ij,kr) = Tin_init(m,ki) &
                           + etai(ij,ki)*Iswabs(i,j,ki)

         enddo                  ! ij
      enddo                     ! nilyr

      end subroutine get_matrix_elements_know_Tsfc

!=======================================================================
!
! Tridiagonal matrix solver--used to solve the implicit vertical heat
! equation in ice and snow
!
! authors William H. Lipscomb, LANL
!         C. M. Bitz, UW

      subroutine tridiag_solver (nx_block, ny_block, &
                                 isolve,   icells,   &
                                 indxii,   indxjj,   &
                                 nmat,     sbdiag,   &
                                 diag,     spdiag,   &
                                 rhs,      xout)

      integer (kind=int_kind), intent(in) :: &
         nx_block, ny_block, & ! block dimensions
         isolve            , & ! number of cells with temps not converged
         icells                ! number of cells with aicen > puny

      integer (kind=int_kind), dimension(icells), &
         intent(in) :: &
         indxii, indxjj  ! compressed indices for cells not converged

      integer (kind=int_kind), intent(in) :: &
         nmat            ! matrix dimension

      real (kind=dbl_kind), dimension (isolve,nmat), &
           intent(in) :: &
         sbdiag      , & ! sub-diagonal matrix elements
         diag        , & ! diagonal matrix elements
         spdiag      , & ! super-diagonal matrix elements
         rhs             ! rhs of tri-diagonal matrix eqn.

      real (kind=dbl_kind), dimension (isolve,nmat), &
           intent(inout) :: &
         xout            ! solution vector

      ! local variables

      integer (kind=int_kind) :: &
         ij          , & ! horizontal index, combines i and j loops
         k               ! row counter

      real (kind=dbl_kind), dimension (isolve) :: &
         wbeta           ! temporary matrix variable

      real (kind=dbl_kind), dimension(isolve,nilyr+nslyr+1):: &
         wgamma          ! temporary matrix variable

!DIR$ CONCURRENT !Cray
!cdir nodep      !NEC
!ocl novrec      !Fujitsu
      do ij = 1, isolve
         wbeta(ij) = diag(ij,1)
         xout(ij,1) = rhs(ij,1) / wbeta(ij)
      enddo                     ! ij

      do k = 2, nmat
!DIR$ CONCURRENT !Cray
!cdir nodep      !NEC
!ocl novrec      !Fujitsu
         do ij = 1, isolve
            wgamma(ij,k) = spdiag(ij,k-1) / wbeta(ij)
            wbeta(ij) = diag(ij,k) - sbdiag(ij,k)*wgamma(ij,k)
            xout(ij,k) = (rhs(ij,k) - sbdiag(ij,k)*xout(ij,k-1)) &
                         / wbeta(ij)
         enddo                  ! ij
      enddo                     ! k

      do k = nmat-1, 1, -1
!DIR$ CONCURRENT !Cray
!cdir nodep      !NEC
!ocl novrec      !Fujitsu
         do ij = 1, isolve
            xout(ij,k) = xout(ij,k) - wgamma(ij,k+1)*xout(ij,k+1)
         enddo                  ! ij
      enddo                     ! k

      end subroutine tridiag_solver

!=======================================================================

      end module ice_therm_bl99

!=======================================================================
