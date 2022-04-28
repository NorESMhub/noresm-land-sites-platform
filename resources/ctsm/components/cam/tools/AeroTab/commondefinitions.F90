
module commondefinitions

!---------------------------------------------------------------------------------
! Module for aerosol hygroscopicities, mass densities and dry size parameters 
! which are common in AeroTab and CAM5-Oslo.
!---------------------------------------------------------------------------------

  use shr_kind_mod, only: r8 => shr_kind_r8
  implicit none


   !Define some aerosol types and their properties..
   integer, parameter, public :: N_AEROSOL_TYPES      = 5
   integer, parameter, public :: AEROSOL_TYPE_SULFATE = 1
   integer, parameter, public :: AEROSOL_TYPE_BC      = 2
   integer, parameter, public :: AEROSOL_TYPE_OM      = 3
   integer, parameter, public :: AEROSOL_TYPE_DUST    = 4
   integer, parameter, public :: AEROSOL_TYPE_SALT    = 5

   !NUMBERS BELOW ARE ESSENTIAL TO CALCULATE HYGROSCOPICITY AND THEREFORE INDIRECT EFFECT!
   !These numbers define the "hygroscopicity parameter" Numbers are selected so that they give reasonable hygroscipity
   !note that changing numbers individually changes the hygroscopicity!
   !Hygroscopicity is defined in Abdul-Razzak and S. Ghan: (B in their eqn 4) 
   !A parameterization of aerosol activation 2. Multiple aerosol types, JGR, vol 105, noD5, pp 6837
   !http://onlinelibrary.wiley.com/doi/10.1029/1999JD901161/abstract
   !   
   !Further note that changing any of these numbers without changing aerotab will lead to 
   !inconsistencies in the simulation since Aerotab tabulates hygroscopical growth!
   !
   !Main reference for numbers chosen: Ghan et al MIRAGE paper (JRG, vol 106, D6, pp 5295), 2001
   !References: 
   !SULFATE : Using same numbers as MIRAGE paper (ammonium sulfate)
   !BC      : Does not really matter as long as soluble mass fraction is small
   !          However, numbers below reproduces values from MIRAGE paper 
   !OM      : Soluble mass fraction tuned to give B of MIRAGE Paper 
   !DUST    : The numbers give B of ~ 0.07 (high end of Kohler, Kreidenweis et al, GRL, vol 36, 2009.
   !                                   (10% as soluble mass fraction seems reasonable) 
   !                                   (see also Osada et al, Atmospheric Research, vol 124, 2013, pp 101
   !SEA SALT: Soluble mass fraction tuned to give consistent values for (r/r0) at 99% when using the parametrization in
   !          Koepke, Hess, Schult and Shettle: Max-Plack-Institut fur Meteorolgie, report No. 243 "GLOBAL AEROSOL DATA SET"
   !          These values give "B" of 1.20 instead of 1.16 in MIRAGE paper.

   character(len=8),public,  dimension(N_AEROSOL_TYPES)  :: aerosol_type_name = &
                                 (/"SULFATE ", "BC      ","OM      ", "DUST    ", "SALT    " /)
   real(r8), public, dimension(N_AEROSOL_TYPES)  :: aerosol_type_density =               &
                                 (/1769.0_r8, 1800.0_r8,  1500.0_r8, 2600.0_r8,  2200.0_r8 /)  !kg/m3
   real(r8), public, dimension(N_AEROSOL_TYPES)  :: aerosol_type_molecular_weight =      &
                                 (/132.0_r8,  12.0_r8,    168.2_r8,  135.0_r8,   58.44_r8  /)  !kg/kmol
   real(r8), public, dimension(N_AEROSOL_TYPES)  :: aerosol_type_osmotic_coefficient =   &
                                 (/0.7_r8,    1.111_r8,     1.0_r8,    1.0_r8,     1.0_r8    /)  ![-]
   real(r8), public, dimension(N_AEROSOL_TYPES)  :: aerosol_type_soluble_mass_fraction = & 
                                 (/1.0_r8,    1.67e-7_r8, 0.8725_r8, 0.1_r8,     0.885_r8  /)  ![-]
   real(r8), public, dimension(N_AEROSOL_TYPES)  :: aerosol_type_number_of_ions =        &
                                 (/3.0_r8,    1.0_r8,     1.0_r8,    2.0_r8,     2.0_r8    /)  ![-]

!  Define lognormal size parameters for each size mode (dry, at point of emission/production): 
   integer, public, parameter :: nmodes  = 14
   integer, public, parameter :: nbmodes = 10 
   !Number median radius of background emissions THESE DO NOT ASSUME IMPLICIT GROWTH!! 
   real(r8), parameter, public, dimension(0:nmodes) :: originalNumberMedianRadius =   &
                      1.e-6_r8* (/ 0.0626_r8,                                            &   !0
                                   0.0118_r8, 0.024_r8, 0.04_r8,  0.04_r8, 0.075_r8, &   !1-5
                                   0.22_r8,   0.63_r8,   0.0475_r8, 0.30_r8, 0.75_r8, &   !6-10    ! Salter SS
                                   0.0118_r8, 0.024_r8, 0.04_r8,  0.04_r8    /)          !11-14

   !Standard deviation sigma of background size distributions
   real(r8), parameter, public, dimension(0:nmodes) :: originalSigma =        &
                                 (/1.6_r8,                                    &   !0
                                   1.8_r8, 1.8_r8, 1.8_r8, 1.8_r8, 1.59_r8,   &   !1-5
                                   1.59_r8, 2.0_r8, 2.1_r8, 1.72_r8, 1.60_r8, &   !6-10   ! Salter SS
                                   1.8_r8, 1.8_r8, 1.8_r8, 1.8_r8  /)             !11-14

end module
