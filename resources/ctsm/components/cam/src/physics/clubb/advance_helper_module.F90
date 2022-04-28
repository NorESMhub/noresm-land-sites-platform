!-------------------------------------------------------------------------
! $Id: advance_helper_module.F90 7381 2014-11-11 23:59:39Z schemena@uwm.edu $
!===============================================================================
module advance_helper_module

! Description:
!   This module contains helper methods for the advance_* modules.
!------------------------------------------------------------------------

  implicit none

  public :: &
    set_boundary_conditions_lhs, &
    set_boundary_conditions_rhs, &
    calc_stability_correction,   &
    calc_brunt_vaisala_freq_sqd

  private ! Set Default Scope

  contains

  !---------------------------------------------------------------------------
  subroutine set_boundary_conditions_lhs( diag_index, low_bound, high_bound, lhs, &
                                      diag_index2, low_bound2, high_bound2 )

  ! Description:
  !   Sets the boundary conditions for a left-hand side LAPACK matrix.
  !
  ! References:
  !   none
  !---------------------------------------------------------------------------

    use clubb_precision, only: &
      core_rknd ! Variable(s)

    implicit none

    ! Exernal 
    intrinsic :: present

    ! Input Variables
    integer, intent(in) :: &
      diag_index, low_bound, high_bound ! boundary indexes for the first variable

    ! Input / Output Variables
    real( kind = core_rknd ), dimension(:,:), intent(inout) :: &
      lhs ! left hand side of the LAPACK matrix equation

    ! Optional Input Variables
    integer, intent(in), optional :: &
      diag_index2, low_bound2, high_bound2 ! boundary indexes for the second variable

    ! --------------------- BEGIN CODE ----------------------

    if ( ( present( low_bound2 ) .or. present( high_bound2 ) ) .and. &
         ( .not. present( diag_index2 ) ) ) then

      stop "Boundary index provided without diag_index."

    end if

    ! Set the lower boundaries for the first variable
    lhs(:,low_bound) = 0.0_core_rknd
    lhs(diag_index,low_bound) = 1.0_core_rknd

    ! Set the upper boundaries for the first variable
    lhs(:,high_bound) = 0.0_core_rknd
    lhs(diag_index,high_bound) = 1.0_core_rknd

    ! Set the lower boundaries for the second variable, if it is provided
    if ( present( low_bound2 ) ) then

      lhs(:,low_bound2) = 0.0_core_rknd
      lhs(diag_index2,low_bound2) = 1.0_core_rknd

    end if

    ! Set the upper boundaries for the second variable, if it is provided
    if ( present( high_bound2 ) ) then

      lhs(:,high_bound2) = 0.0_core_rknd
      lhs(diag_index2,high_bound2) = 1.0_core_rknd

    end if

    return
  end subroutine set_boundary_conditions_lhs

  !--------------------------------------------------------------------------
  subroutine set_boundary_conditions_rhs( &
               low_value, low_bound, high_value, high_bound, &
               rhs, &
               low_value2, low_bound2, high_value2, high_bound2 )

  ! Description:
  !   Sets the boundary conditions for a right-hand side LAPACK vector.
  !
  ! References:
  !   none
  !---------------------------------------------------------------------------

    use clubb_precision, only: &
      core_rknd ! Variable(s)

    implicit none

    ! Exernal 
    intrinsic :: present

    ! Input Variables

    ! The values for the first variable
    real( kind = core_rknd ), intent(in) :: low_value, high_value

    ! The bounds for the first variable
    integer, intent(in) :: low_bound, high_bound

    ! Input / Output Variables

    ! The right-hand side vector
    real( kind = core_rknd ), dimension(:), intent(inout) :: rhs

    ! Optional Input Variables

    ! The values for the second variable
    real( kind = core_rknd ), intent(in), optional :: low_value2, high_value2

    ! The bounds for the second variable
    integer, intent(in), optional :: low_bound2, high_bound2


    ! -------------------- BEGIN CODE ------------------------

    ! Stop execution if a boundary was provided without a value
    if ( (present( low_bound2 ) .and. (.not. present( low_value2 ))) .or. &
         (present( high_bound2 ) .and. (.not. present( high_value2 ))) ) then

      stop "Boundary condition provided without value."

    end if

    ! Set the lower and upper bounds for the first variable
    rhs(low_bound) = low_value
    rhs(high_bound) = high_value

    ! If a lower bound was given for the second variable, set it
    if ( present( low_bound2 ) ) then
      rhs(low_bound2) = low_value2
    end if

    ! If an upper bound was given for the second variable, set it
    if ( present( high_bound2 ) ) then
      rhs(high_bound2) = high_value2
    end if

    return
  end subroutine set_boundary_conditions_rhs

  !===============================================================================
  function calc_stability_correction( thlm, Lscale, em, exner, rtm, rcm, p_in_Pa, cloud_frac ) &
    result ( stability_correction )
  !
  ! Description:
  !   Stability Factor
  !
  ! References:
  !
  !--------------------------------------------------------------------

    use parameters_tunable, only: &
      lambda0_stability_coef ! Variable(s)

    use constants_clubb, only: &
      zero    ! Constant(s)

    use grid_class, only:  &
      gr, & ! Variable(s)
      zt2zm    ! Procedure(s)

    use clubb_precision, only:  &
      core_rknd ! Variable(s)

    implicit none

    ! Input Variables
    real( kind = core_rknd ), intent(in), dimension(gr%nz) :: &
      Lscale,          & ! Turbulent mixing length                   [m]
      em,              & ! Turbulent Kinetic Energy (TKE)            [m^2/s^2]
      thlm,            & ! th_l (thermo. levels)                     [K]
      exner,           & ! Exner function                            [-]
      rtm,             & ! total water mixing ratio, r_t             [kg/kg]
      rcm,             & ! cloud water mixing ratio, r_c             [kg/kg]
      p_in_Pa,         & ! Air pressure                              [Pa]
      cloud_frac         ! Cloud fraction                            [-]

    ! Result
    real( kind = core_rknd ), dimension(gr%nz) :: &
      stability_correction

    real( kind = core_rknd ), dimension(gr%nz) :: &
      brunt_vaisala_freq_sqd, & !  []
      lambda0_stability

    !------------ Begin Code --------------
    brunt_vaisala_freq_sqd = calc_brunt_vaisala_freq_sqd( thlm, exner, rtm, rcm, p_in_Pa, &
                                                          cloud_frac )
    lambda0_stability = merge( lambda0_stability_coef, zero, brunt_vaisala_freq_sqd > zero )

    stability_correction = 1.0_core_rknd &
      + min( lambda0_stability * brunt_vaisala_freq_sqd * zt2zm( Lscale )**2 / em, 3.0_core_rknd )

    return
  end function calc_stability_correction

  !===============================================================================
  function calc_brunt_vaisala_freq_sqd( thlm, exner, rtm, rcm, p_in_Pa, cloud_frac ) &
    result( brunt_vaisala_freq_sqd )

  ! Description:
  !   Calculate the Brunt-Vaisala frequency squared, N^2.

  ! References:
  !   ?
  !-----------------------------------------------------------------------

    use clubb_precision, only: &
      core_rknd ! Konstant

    use constants_clubb, only: &
      grav, & ! Constant(s)
      cloud_frac_min, &
      Lv, Cp, Rd, ep, &
      one

    use parameters_model, only: &
      T0 ! Variable!

    use grid_class, only: &
      gr,     & ! Variable
      ddzt,   &  ! Procedure(s)
      zt2zm

    use T_in_K_module, only: &
      thlm2T_in_K ! Procedure

    use saturation, only: &
      sat_mixrat_liq ! Procedure

    use model_flags, only: &
      l_brunt_vaisala_freq_moist ! Variable

    implicit none

    ! Input Variables
    real( kind = core_rknd ), dimension(gr%nz), intent(in) :: &
      thlm,    &  ! th_l (thermo. levels)              [K]
      exner,   &  ! Exner function                     [-]
      rtm,     &  ! total water mixing ratio, r_t      [kg/kg]
      rcm,     &  ! cloud water mixing ratio, r_c      [kg/kg]
      p_in_Pa, &  ! Air pressure                       [Pa]
      cloud_frac  ! Cloud fraction                     [-]

    ! Output Variables
    real( kind = core_rknd ), dimension(gr%nz) :: &
      brunt_vaisala_freq_sqd ! Brunt-Vaisala frequency squared, N^2 [1/s^2]

    ! Local Variables
    real( kind = core_rknd ), dimension(gr%nz) :: &
      T_in_K, T_in_K_zm, rsat, rsat_zm, thm, thm_zm, ddzt_thlm, &
      ddzt_thm, ddzt_rsat, ddzt_rtm

    integer :: k

  !---------------------------------------------------------------------
    !----- Begin Code -----
    ddzt_thlm = ddzt( thlm )

    if ( l_brunt_vaisala_freq_moist ) then
      ! These parameters are needed to compute the moist Brunt-Vaisala
      ! frequency.
      T_in_K = thlm2T_in_K( thlm, exner, rcm )
      T_in_K_zm = zt2zm( T_in_K )
      rsat = sat_mixrat_liq( p_in_Pa, T_in_K )
      rsat_zm = zt2zm( rsat )
      ddzt_rsat = ddzt( rsat )
      thm = thlm + Lv/(Cp*exner) * rcm
      thm_zm = zt2zm( thm )
      ddzt_thm = ddzt( thm )
      ddzt_rtm = ddzt( rtm )
    end if

    do k=1, gr%nz

      if ( .not. l_brunt_vaisala_freq_moist .or. cloud_frac(k) < cloud_frac_min ) then

        ! Dry Brunt-Vaisala frequency
        brunt_vaisala_freq_sqd(k) = ( grav / T0 ) * ddzt_thlm(k)

      else ! l_brunt_vaisala_freq_moist .and. cloud_frac(k) >= cloud_frac_min

        ! In-cloud Brunt-Vaisala frequency. This is Eq. (36) of Durran and Klemp (1982)
        brunt_vaisala_freq_sqd(k) = &
          grav * ( ((one + Lv*rsat_zm(k) / (Rd*T_in_K_zm(k))) / &
            (one + ep*(Lv**2)*rsat_zm(k)/(Cp*Rd*T_in_K_zm(k)**2))) * &
            ( (one/thm_zm(k) * ddzt_thm(k)) + (Lv/(Cp*T_in_K_zm(k)))*ddzt_rsat(k)) - &
            ddzt_rtm(k) )

      end if

    end do ! k=1, gr%nz

    return
  end function calc_brunt_vaisala_freq_sqd

end module advance_helper_module
