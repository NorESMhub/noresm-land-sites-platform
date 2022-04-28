!  SVN:$Id: ice_therm_0layer.F90 703 2013-08-20 21:14:30Z eclare $
!=========================================================================
!
! Update ice and snow internal temperatures
! using zero-layer thermodynamics
!
! authors: Alison McLaren, UK MetOffice
!          Elizabeth C. Hunke, LANL
!
! 2012: Split from ice_therm_vertical.F90

      module ice_therm_0layer

      use ice_kinds_mod
      use ice_domain_size, only: nilyr, nslyr, max_ntrcr
      use ice_constants
      use ice_fileunits, only: nu_diag
      use ice_therm_bl99, only: surface_fluxes

      implicit none

      private
      public :: zerolayer_temperature

!=======================================================================

      contains

!=======================================================================
!
! Compute new surface temperature using zero layer model of Semtner
! (1976).
!
! New temperatures are computed iteratively by solving a
! surface flux balance equation (i.e. net surface flux from atmos
! equals conductive flux from the top to the bottom surface).
!
! author:  Alison McLaren, Met Office
!         (but largely taken from temperature_changes)

      subroutine zerolayer_temperature(nx_block, ny_block, &
                                       my_task,  istep1,   &
                                       dt,       icells,   & 
                                       indxi,    indxj,    &
                                       rhoa,     flw,      &
                                       potT,     Qa,       &
                                       shcoef,   lhcoef,   &
                                       fswsfc,             &
                                       hilyr,    hslyr,    &
                                       Tsf,      Tbot,     &
                                       fsensn,   flatn,    &
                                       flwoutn,  fsurfn,   &
                                       fcondtopn,fcondbotn, &
                                       l_stop,             &
                                       istop,    jstop)

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

      real (kind=dbl_kind), dimension (nx_block,ny_block), intent(in) :: &
         rhoa        , & ! air density (kg/m^3)
         flw         , & ! incoming longwave radiation (W/m^2)
         potT        , & ! air potential temperature  (K)
         Qa          , & ! specific humidity (kg/kg)
         shcoef      , & ! transfer coefficient for sensible heat
         lhcoef      , & ! transfer coefficient for latent heat
         Tbot        , & ! ice bottom surface temperature (deg C)
         fswsfc          ! SW absorbed at ice/snow surface (W m-2)

      real (kind=dbl_kind), dimension (icells), intent(in) :: &
         hilyr       , & ! ice layer thickness (m)
         hslyr           ! snow layer thickness (m)

      real (kind=dbl_kind), dimension (nx_block,ny_block), intent(inout):: &
         fsensn      , & ! surface downward sensible heat (W m-2)
         flatn       , & ! surface downward latent heat (W m-2)
         flwoutn     , & ! upward LW at surface (W m-2)
         fsurfn      , & ! net flux to top surface, excluding fcondtopn
         fcondtopn   , & ! downward cond flux at top surface (W m-2)
         fcondbotn       ! downward cond flux at bottom surface (W m-2)

      real (kind=dbl_kind), dimension (icells), &
         intent(inout):: &
         Tsf             ! ice/snow surface temperature, Tsfcn

      logical (kind=log_kind), intent(inout) :: &
         l_stop          ! if true, print diagnostics and abort model

      integer (kind=int_kind), intent(inout) :: &
         istop, jstop    ! i and j indices of cell where model fails

      ! local variables

      logical (kind=log_kind), parameter :: &
         l_zerolayerchecks = .true.

      integer (kind=int_kind), parameter :: &
         nitermax = 50   ! max number of iterations in temperature solver

      real (kind=dbl_kind), parameter :: &
         Tsf_errmax = 5.e-4_dbl_kind ! max allowed error in Tsf
                                     ! recommend Tsf_errmax < 0.01 K

      integer (kind=int_kind) :: &
         i, j        , & ! horizontal indices
         ij, m       , & ! horizontal indices, combine i and j loops
         niter           ! iteration counter in temperature solver

      integer (kind=int_kind) :: &
         isolve          ! number of cells with temps not converged

      integer (kind=int_kind), dimension (icells) :: &
         indxii, indxjj  ! compressed indices for cells not converged

      integer (kind=int_kind), dimension (icells) :: &
         indxij          ! compressed 1D index for cells not converged

      real (kind=dbl_kind), dimension (:), allocatable :: &
         Tsf_start   , & ! Tsf at start of iteration
         dTsf        , & ! Tsf - Tsf_start
         dfsurf_dT       ! derivative of fsurfn wrt Tsf

      real (kind=dbl_kind), dimension (icells) :: &
         dTsf_prev   , & ! dTsf from previous iteration
         dfsens_dT   , & ! deriv of fsens wrt Tsf (W m-2 deg-1)
         dflat_dT    , & ! deriv of flat wrt Tsf (W m-2 deg-1)
         dflwout_dT      ! deriv of flwout wrt Tsf (W m-2 deg-1)

      real (kind=dbl_kind), dimension (:), allocatable :: &
         kh          , & ! effective conductivity
         diag        , & ! diagonal matrix elements
         rhs             ! rhs of tri-diagonal matrix equation

      real (kind=dbl_kind) :: &
         heff        , & ! effective ice thickness (m)
                         ! ( hice + hsno*kseaice/ksnow)
         kratio      , & ! ratio of ice and snow conductivies
         avg_Tsf         ! = 1. if Tsf averaged w/Tsf_start, else = 0.

      logical (kind=log_kind), dimension (icells) :: &
         converged      ! = true when local solution has converged

      logical (kind=log_kind) :: &
         all_converged  ! = true when all cells have converged

      !-----------------------------------------------------------------
      ! Initialize
      !-----------------------------------------------------------------

      all_converged   = .false.

      do ij = 1, icells

         converged (ij) = .false.

         dTsf_prev (ij) = c0

      enddo                     ! ij
      
      !-----------------------------------------------------------------
      ! Solve for new temperatures.
      ! Iterate until temperatures converge with minimal temperature
      ! change.
      !-----------------------------------------------------------------

      do niter = 1, nitermax

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

         allocate(     diag(isolve))
         allocate(      rhs(isolve))
         allocate(       kh(isolve))
         allocate(Tsf_start(isolve))
         allocate(     dTsf(isolve))
         allocate(dfsurf_dT(isolve))

      !-----------------------------------------------------------------
      ! Update radiative and turbulent fluxes and their derivatives
      ! with respect to Tsf.
      !-----------------------------------------------------------------

         call surface_fluxes (nx_block,    ny_block,          &
                              isolve,      icells,            &
                              indxii,      indxjj,    indxij, &
                              Tsf,         fswsfc,            &
                              rhoa,        flw,               &
                              potT,        Qa,                &
                              shcoef,      lhcoef,            &
                              flwoutn,     fsensn,            &
                              flatn,       fsurfn,            &
                              dflwout_dT,  dfsens_dT,         &
                              dflat_dT,    dfsurf_dT)

      !-----------------------------------------------------------------
      ! Compute effective ice thickness (includes snow) and thermal 
      ! conductivity 
      !-----------------------------------------------------------------

         kratio = kseaice/ksno
 
         do ij = 1, isolve
             m = indxij(ij)
   
             heff = hilyr(m) + kratio * hslyr(m)
             kh(ij) = kseaice / heff
         enddo                     ! ij


!DIR$ CONCURRENT !Cray
!cdir nodep      !NEC
!ocl novrec      !Fujitsu
         do ij = 1, isolve
            i = indxii(ij)
            j = indxjj(ij)
            m = indxij(ij)

      !-----------------------------------------------------------------
      ! Compute conductive flux at top surface, fcondtopn.
      ! If fsurfn < fcondtopn and Tsf = 0, then reset Tsf to slightly less
      !  than zero (but not less than -puny).
      !-----------------------------------------------------------------

            fcondtopn(i,j) = kh(ij) * (Tsf(m) - Tbot(i,j))

            if (fsurfn(i,j) < fcondtopn(i,j)) &
                 Tsf(m) = min (Tsf(m), -puny)

      !-----------------------------------------------------------------
      ! Save surface temperature at start of iteration
      !-----------------------------------------------------------------

            Tsf_start(ij) = Tsf(m)

         enddo                  ! ij

      !-----------------------------------------------------------------
      ! Solve surface balance equation to obtain the new temperatures.
      !-----------------------------------------------------------------

         do ij = 1, isolve
            i = indxii(ij)
            j = indxjj(ij)
            m = indxij(ij)

            diag(ij)  = dfsurf_dT(ij) - kh(ij)
            rhs(ij)   = dfsurf_dT(ij)*Tsf(m) - fsurfn(i,j)   &
                        - kh(ij)*Tbot(i,j)
            Tsf(m)  = rhs(ij) / diag(ij)

         enddo

      !-----------------------------------------------------------------
      ! Determine whether the computation has converged to an acceptable
      ! solution.  Four conditions must be satisfied:
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
      !-----------------------------------------------------------------

         ! initialize global convergence flag
         all_converged = .true.

!DIR$ CONCURRENT !Cray
!cdir nodep      !NEC
!ocl novrec      !Fujitsu
         do ij = 1, isolve
            m = indxij(ij)

      !-----------------------------------------------------------------
      ! Initialize convergence flag (true until proven false), dTsf,
      !  and temperature-averaging coefficients.
      ! Average only if test 1 or 2 fails.
      ! Initialize energy.
      !-----------------------------------------------------------------

            converged(m) = .true.
            dTsf(ij) = Tsf(m) - Tsf_start(ij)
            avg_Tsf      = c0

      !-----------------------------------------------------------------
      ! Condition 1: check for Tsf > 0
      ! If Tsf > 0, set Tsf = 0 and leave converged=.true.
      !-----------------------------------------------------------------

            if (Tsf(m) > puny) then
               Tsf(m) = c0
               dTsf(ij) = -Tsf_start(ij)

      !-----------------------------------------------------------------
      ! Condition 2: check for oscillating Tsf
      ! If oscillating, average all temps to increase rate of convergence.
      ! It is possible that this may never occur.
      !-----------------------------------------------------------------

            elseif (niter > 1 &                ! condition (2)
              .and. Tsf_start(ij) <= -puny &
              .and. abs(dTsf(ij)) > puny &
              .and. abs(dTsf_prev(m)) > puny &
              .and. -dTsf(ij)/(dTsf_prev(m)+puny*puny) > p5) then

               avg_Tsf  = c1  ! average with starting temp  
               dTsf(ij) = p5 * dTsf(ij)
               converged(m) = .false.
               all_converged = .false.
            endif

      !-----------------------------------------------------------------
      ! If condition 2 failed, average new surface temperature with
      !  starting value.
      !-----------------------------------------------------------------
            Tsf(m)  = Tsf(m) &
                      + avg_Tsf * p5 * (Tsf_start(ij) - Tsf(m))

         enddo  ! ij

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
      ! Condition 4: check for fsurfn < fcondtopn with Tsf > 0
      !-----------------------------------------------------------------

            fsurfn(i,j) = fsurfn(i,j) + dTsf(ij)*dfsurf_dT(ij)
            fcondtopn(i,j) = kh(ij) * (Tsf(m)-Tbot(i,j))

            if (Tsf(m) > -puny .and. fsurfn(i,j) < fcondtopn(i,j)) then
               converged(m) = .false.
               all_converged = .false.
            endif

            fcondbotn(i,j) = fcondtopn(i,j)

            dTsf_prev(m) = dTsf(ij)

         enddo                  ! ij

         deallocate(diag)
         deallocate(rhs)
         deallocate(kh)
         deallocate(Tsf_start)
         deallocate(dTsf)
         deallocate(dfsurf_dT)

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
               write(nu_diag,*) 'fsurfn:', fsurfn(i,j)
               write(nu_diag,*) 'fcondtopn, fcondbotn', &
                                 fcondtopn(i,j), fcondbotn(i,j)
               l_stop = .true.
               istop = i
               jstop = j
               return
            endif
         enddo                  ! ij
      endif                     ! all_converged

      !-----------------------------------------------------------------
      ! Check that if Tsfc < 0, then fcondtopn = fsurfn
      !-----------------------------------------------------------------

      if (l_zerolayerchecks) then
         do ij = 1, icells
            i = indxi(ij)
            j = indxj(ij)
            
            if (Tsf(ij) < c0 .and. & 
                  abs(fcondtopn(i,j)-fsurfn(i,j)) > puny) then

               write(nu_diag,*) 'fcondtopn does not equal fsurfn,', &
                                'istep1, my_task, i, j:', &
                                 istep1, my_task, i, j
               write(nu_diag,*) 'Tsf=',Tsf(ij)
               write(nu_diag,*) 'fcondtopn=',fcondtopn(i,j)
               write(nu_diag,*) 'fsurfn=',fsurfn(i,j)
               l_stop = .true.
               istop = i
               jstop = j
               return
            endif
         enddo                  ! ij
      endif                     ! l_zerolayerchecks


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

      end subroutine zerolayer_temperature

!=======================================================================

      end module ice_therm_0layer

!=======================================================================
