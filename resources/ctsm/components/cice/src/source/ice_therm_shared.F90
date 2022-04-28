!  SVN:$Id: ice_therm_shared.F90 726 2013-09-17 14:58:52Z eclare $
!=========================================================================
!
! Shared thermo variables, subroutines
!
! authors: Elizabeth C. Hunke, LANL

      module ice_therm_shared

      use ice_kinds_mod
      use ice_domain_size, only: ncat, nilyr, nslyr, max_ntrcr

      implicit none

      private
      public :: calculate_Tin_from_qin, &
                surface_heat_flux, dsurface_heat_flux_dTsf

      integer (kind=int_kind), public :: &
         ktherm          ! type of thermodynamics
                         ! 0 = 0-layer approximation
                         ! 1 = Bitz and Lipscomb 1999
                         ! 2 = mushy layer theory

      real (kind=dbl_kind), dimension(nilyr+1), public :: &
         Tmlt            ! melting temperature
                         ! nilyr + 1 index is for bottom surface

      real (kind=dbl_kind), parameter, public :: &
         ferrmax = 1.0e-3_dbl_kind    ! max allowed energy flux error (W m-2)
                                      ! recommend ferrmax < 0.01 W m-2

      real (kind=dbl_kind), parameter, public :: &
         Tmin = -100.0_dbl_kind ! min allowed internal temperature (deg C)

      character (char_len), public :: &
         conduct         ! 'MU71' or 'bubbly'

      logical (kind=log_kind), public :: &
         l_brine         ! if true, treat brine pocket effects

      logical (kind=log_kind), public :: &
         heat_capacity, &! if true, ice has nonzero heat capacity
                         ! if false, use zero-layer thermodynamics
         calc_Tsfc       ! if true, calculate surface temperature
                         ! if false, Tsfc is computed elsewhere and
                         ! atmos-ice fluxes are provided to CICE

      real (kind=dbl_kind), parameter, public :: &
         hfrazilmin = 0.05_dbl_kind ! min thickness of new frazil ice (m)

!=======================================================================

      contains

!=======================================================================
!
!  Compute the internal ice temperatures from enthalpy using
!  quadratic formula

      function calculate_Tin_from_qin (qin, Tmltk) &
               result(Tin)

      use ice_constants

      real (kind=dbl_kind), intent(in) :: &
         qin   , &              ! enthalpy
         Tmltk                  ! melting temperature at one level

      real (kind=dbl_kind) :: &
         Tin                 ! internal temperature

      ! local variables

      real (kind=dbl_kind) :: &
         aa1,bb1,cc1         ! quadratic solvers

      if (l_brine) then
         aa1 = cp_ice
         bb1 = (cp_ocn-cp_ice)*Tmltk - qin/rhoi - Lfresh 
         cc1 = Lfresh * Tmltk
         Tin =  min((-bb1 - sqrt(bb1*bb1 - c4*aa1*cc1)) /  &
                         (c2*aa1),Tmltk)

      else                ! fresh ice
         Tin = (Lfresh + qin/rhoi) / cp_ice
      endif
 
      end function calculate_Tin_from_qin

!=======================================================================
! Surface heat flux
!=======================================================================

! heat flux into ice
    
      subroutine surface_heat_flux(Tsf,     fswsfc, &
                                   rhoa,    flw,    &
                                   potT,    Qa,     &
                                   shcoef,  lhcoef, &
                                   flwoutn, fsensn, &
                                   flatn,   fsurfn)

      use ice_constants, only:  c1, Tffresh, TTTice, qqqice, &
          stefan_boltzmann, emissivity
    
      ! input surface temperature
      real(kind=dbl_kind), intent(in) :: &
         Tsf             ! ice/snow surface temperature (C)
    
      ! input variables
      real(kind=dbl_kind), intent(in) :: &
         fswsfc      , & ! SW absorbed at ice/snow surface (W m-2)
         rhoa        , & ! air density (kg/m^3)
         flw         , & ! incoming longwave radiation (W/m^2)
         potT        , & ! air potential temperature  (K)
         Qa          , & ! specific humidity (kg/kg)
         shcoef      , & ! transfer coefficient for sensible heat
         lhcoef          ! transfer coefficient for latent heat
    
      ! output
      real(kind=dbl_kind), intent(out) :: &
         fsensn      , & ! surface downward sensible heat (W m-2)
         flatn       , & ! surface downward latent heat (W m-2)
         flwoutn     , & ! upward LW at surface (W m-2)
         fsurfn          ! net flux to top surface, excluding fcondtopn
    
      ! local variables
      real(kind=dbl_kind) :: &
         TsfK        , & ! ice/snow surface temperature (K)
         Qsfc        , & ! saturated surface specific humidity (kg/kg)
         qsat        , & ! the saturation humidity of air (kg/m^3)
         flwdabs     , & ! downward longwave absorbed heat flx (W/m^2)
         tmpvar          ! 1/TsfK
    
      ! ice surface temperature in Kelvin
      TsfK = Tsf + Tffresh
!      TsfK = max(Tsf + Tffresh, c1)
      tmpvar = c1/TsfK
    
      ! saturation humidity
      qsat    = qqqice * exp(-TTTice*tmpvar)
      Qsfc    = qsat / rhoa
    
      ! longwave radiative flux
      flwdabs =  emissivity * flw
      flwoutn = -emissivity * stefan_boltzmann * TsfK**4
    
      ! downward latent and sensible heat fluxes
      fsensn = shcoef * (potT - TsfK)
      flatn  = lhcoef * (Qa - Qsfc)
    
      ! combine fluxes
      fsurfn = fswsfc + flwdabs + flwoutn + fsensn + flatn

      end subroutine surface_heat_flux

  !=======================================================================
  
      subroutine dsurface_heat_flux_dTsf(Tsf,     fswsfc, &
                                         rhoa,    flw,    &
                                         potT,    Qa,     &
                                         shcoef,  lhcoef, &
                                         dfsurfn_dTsf, dflwoutn_dTsf, &
                                         dfsensn_dTsf, dflatn_dTsf)
    
      use ice_constants, only:  c1, c4, Tffresh, TTTice, qqqice, &
          stefan_boltzmann, emissivity
    
      ! input surface temperature
      real(kind=dbl_kind), intent(in) :: &
         Tsf               ! ice/snow surface temperature (C)
    
      ! input variables
      real(kind=dbl_kind), intent(in) :: &
         fswsfc        , & ! SW absorbed at ice/snow surface (W m-2)
         rhoa          , & ! air density (kg/m^3)
         flw           , & ! incoming longwave radiation (W/m^2)
         potT          , & ! air potential temperature  (K)
         Qa            , & ! specific humidity (kg/kg)
         shcoef        , & ! transfer coefficient for sensible heat
         lhcoef            ! transfer coefficient for latent heat
    
      ! output
      real(kind=dbl_kind), intent(out) :: &
         dfsurfn_dTsf      ! derivative of net flux to top surface, excluding fcondtopn
    
      real(kind=dbl_kind), intent(out) :: &
         dflwoutn_dTsf , & ! derivative of longwave flux wrt surface temperature
         dfsensn_dTsf  , & ! derivative of sensible heat flux wrt surface temperature
         dflatn_dTsf       ! derivative of latent heat flux wrt surface temperature
    
      ! local variables
      real(kind=dbl_kind) :: &
         TsfK          , & ! ice/snow surface temperature (K)
         dQsfc_dTsf    , & ! saturated surface specific humidity (kg/kg)
         qsat          , & ! the saturation humidity of air (kg/m^3)
         tmpvar            ! 1/TsfK
    
      ! ice surface temperature in Kelvin
!      TsfK = max(Tsf + Tffresh, c1)
      TsfK = Tsf + Tffresh
      tmpvar = c1/TsfK
    
      ! saturation humidity
      qsat          = qqqice * exp(-TTTice*tmpvar)
      dQsfc_dTsf    = TTTice * tmpvar * tmpvar * (qsat / rhoa)
    
      ! longwave radiative flux
      dflwoutn_dTsf = -emissivity * stefan_boltzmann * c4*TsfK**3
    
      ! downward latent and sensible heat fluxes
      dfsensn_dTsf = -shcoef
      dflatn_dTsf  = -lhcoef * dQsfc_dTsf
    
      ! combine fluxes
      dfsurfn_dTsf = dflwoutn_dTsf + dfsensn_dTsf + dflatn_dTsf
    
      end subroutine dsurface_heat_flux_dTsf

!=======================================================================

      end module ice_therm_shared

!=======================================================================
