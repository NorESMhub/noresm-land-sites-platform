module const

!-----------------------------------------------------------------------------
!Module containing subroutines constants, koagsub and parmix and declaration
!of the variables required by them. Updated with one internally mixed and one
!externally mixed OC mode November 2004. 
!Updated with extra variables for SOA by Alf Kirkevåg May 2013, and
!with explicit equations for diffusion variables for SOA and H2SO4 in July 2015
!(moved to condtend.F90).
!-----------------------------------------------------------------------------

   use shr_kind_mod, only: r8 => shr_kind_r8
!   use aerosoldef, only: nmodes
   use commondefinitions, only: nmodes
   use physconst,    only: pi
!
implicit none
!
public
save

    real(r8), parameter:: smallNumber = 1.e-100_r8
   
    !Essential size distribution parameters
    real(r8), parameter :: rTabMin = 1.e-9_r8   ![m] smallest lookup table size
    real(r8), parameter :: rTabMax = 20.e-6_r8  ![m] largest lookup table size
    integer,  parameter :: nBinsTab = 44        ![nbr] number of tabulated bins


!cak: diff, th and mfv for H2SO4 and SOA are now calculated in condtend.F90


   !Smallest particle which can receive aquous chemistry mass
   real(r8), parameter :: rMinAquousChemistry = 0.05e-6_r8
   real(r8) nk(0:nmodes,nbinsTab)               !dN/dlogr for modes
   real(r8) normnk(0:nmodes,nbinsTab)           !dN for modes (sums to one over size range)

	real(r8) rBinEdge(nBinsTab+1)
	real(r8) rBinMidpoint(nBinsTab)

!soa
!        real(r8) :: rrr1to3 (3,16)	!TS: Modal radius array, mode 1 - 3	 
!        real(r8) :: sss1to3 (3,16)	!TS: Standard deviation array, Mode 1 -3
!        real(r8) :: calog1to3(3,16)	!TS: Array for reading catot from file
!        real(r8) :: rk1to3 (3,16)	!TS: Array for reading modal radius from file
!        real(r8) :: stdv1to3 (3,16)	!TS: Array for reading std. dev. from file
!soa
        real(r8) :: rrr1to3 (3,16,6)	!TS: Modal radius array, mode 1 - 3
        real(r8) :: sss1to3 (3,16,6)	!TS: Standard deviation array, Mode 1 -3
        real(r8) :: calog1to3(3,96)	!TS: Array for reading catot from file
	real(r8) :: rk1to3 (3,96)	!TS: Array for reading modal radius from file
        real(r8) :: stdv1to3 (3,96)	!TS: Array for reading std. dev. from file
        real(r8) :: fraclog1to3 (3,96)	!TS: Same as frac4, but for initlogn.F90 
!soa

        real(r8) :: rrr4 (16,6,6)	!TS: Modal radius array, mode 4	 	
        real(r8) :: sss4 (16,6,6)	!TS: Modal radius array, mode 4	 
        real(r8) :: calog4(576)	        !TS: Same as catot4, but for initlogn.F90
        real(r8) :: fraclog4 (576)	!TS: Same as frac4, but for initlogn.F90 
        real(r8) :: fraqlog4 (576)	!TS: Same as fraq4, but for initlogn.F90
        real(r8) :: rk4 (576)		!TS: Array for reading modal radius from file
        real(r8) :: stdv4 (576)	        !TS: Array for reading std. dev. from file

        real(r8) :: rrr (5:10,6,6,6,6)	!TS: Modal radius array, mode 5 - 10
        real(r8) :: sss (5:10,6,6,6,6)	!TS: Standard deviation array, mode 5 - 10
        real(r8) :: calog (5:10,1296)   !TS: Same as catot, but for initlogn.F90
        real(r8) :: fraclog5to10 (5:10,1296) !TS: Same as frac5to10, but for initlogn.F90 
        real(r8) :: fabclog5to10 (5:10,1296) !TS: Same as fabc5to10, but for initlogn.F90 
        real(r8) :: fraqlog5to10 (5:10,1296) !TS: Same as fraq5to10, but for initlogn.F90 
        real(r8) :: rk5to10 (5:10,1296)      !TS: Array for reading modal radius from file
        real(r8) :: stdv5to10 (5:10,1296)    !TS: Array for reading std. dev. from file


   real(r8), parameter :: sq2pi = 1._r8/sqrt(2.0_r8*pi)
   real(r8), dimension(0:nmodes) :: volumeToNumber               !m3 ==> #
   real(r8), dimension(0:nmodes) :: numberToSurface              !# ==> m2


end module const




