!  SVN:$Id: ice_orbital.F90 700 2013-08-15 19:17:39Z eclare $
!=======================================================================

! Orbital parameters computed from date
! author:  Bruce P. Briegleb, NCAR 
!
! 2006: Converted to free source form (F90) by Elizabeth Hunke

      module ice_orbital

      use ice_kinds_mod

      implicit none
      private
      public :: init_orbit, compute_coszen

      integer (kind=int_kind) :: iyear_AD  ! Year to calculate orbit for
 
      real(kind=dbl_kind),public :: eccen  !Earth's orbital eccentricity
      real(kind=dbl_kind),public :: obliqr !Earth's obliquity in radians
      real(kind=dbl_kind),public :: lambm0 !Mean longitude of perihelion at the
                                    !vernal equinox (radians)
      real(kind=dbl_kind),public :: mvelpp !Earth's moving vernal equinox longitude
                                    !of perihelion + pi (radians)
      real(kind=dbl_kind) :: obliq  ! obliquity in degrees
      real(kind=dbl_kind) :: mvelp  ! moving vernal equinox long
      real(kind=dbl_kind) :: delta  ! solar declination angle in radians
      real(kind=dbl_kind) :: eccf   ! earth orbit eccentricity factor

      logical(kind=log_kind) :: log_print ! Flags print of status/error
 
!=======================================================================
 
      contains
 
!=======================================================================

! Uses share routines to compute orbital parameters
! for the specified date.
!
! author:  Bruce P. Briegleb, NCAR 

      subroutine init_orbit

      use shr_orb_mod, only: shr_orb_params

      iyear_AD  = 1950
      log_print = .false.   ! if true, write out orbital parameters

      call shr_orb_params( iyear_AD , eccen  , obliq , mvelp     , &
                           obliqr   , lambm0 , mvelpp, log_print )
 
      end subroutine init_orbit
 
!=======================================================================

! Uses orbital and lat/lon info to compute cosine solar zenith angle
! for the specified date.
!
! author:  Bruce P. Briegleb, NCAR 

      subroutine compute_coszen (nx_block, ny_block, &
                                 icells,             &
                                 indxi,    indxj,    &
                                 tlat,     tlon,     &
                                 coszen,   dt)

      use ice_calendar, only: yday, sec, calendar_type, nextsw_cday, days_per_year
      use ice_constants, only: c0, c2, p5, pi, secday
      use shr_orb_mod, only: shr_orb_decl
!+tht
#ifdef CESMCOUPLED
      use shr_orb_mod, only: shr_orb_cosz
#endif
!-tht

      integer (kind=int_kind), intent(in) :: &
         nx_block, ny_block, & ! block dimensions
         icells                ! number of ice-covered grid cells

      integer (kind=int_kind), dimension (nx_block*ny_block) :: &
         indxi, indxj          ! indices for ice-covered cells
 
      real (kind=dbl_kind), dimension(nx_block,ny_block), intent(in) :: &
         tlat, tlon          ! latitude and longitude (radians)

      real (kind=dbl_kind), dimension(nx_block,ny_block), intent(out) :: &
         coszen              ! cosine solar zenith angle 
                             ! negative for sun below horizon
 
      real (kind=dbl_kind), intent(in) :: &
         dt                  ! thermodynamic time step

      ! local variables

      real (kind=dbl_kind) :: ydayp1 ! day of year plus one time step
 
      integer (kind=int_kind) :: &
         i   , & ! domain longitude index
         j   , & ! domain latitude index
         ij      ! horizontal index, combines i and j loops
 
 
! Solar declination for next time step
 
#ifdef CESMCOUPLED
      if (calendar_type == "GREGORIAN") then
         ydayp1 = min(nextsw_cday, real(days_per_year,kind=dbl_kind))
      else
         ydayp1 = nextsw_cday
      endif

      !--- update coszen when nextsw_cday valid
      if (ydayp1 > -0.5_dbl_kind) then
#else
      ydayp1 = yday + sec/secday
#endif
 
      call shr_orb_decl(ydayp1, eccen, mvelpp, lambm0, &
                        obliqr, delta, eccf)

      coszen(:,:) = c0  ! sun at horizon

!+tht
#ifdef CESMCOUPLED
!DIR$ CONCURRENT !Cray
!cdir nodep      !NEC
!ocl novrec      !Fujitsu
      do ij = 1, icells
         i = indxi(ij)
         j = indxj(ij)
!+tht outlined again 
          coszen(i,j) = shr_orb_cosz(ydayp1, &
                                     tlat(i,j),tlon(i,j),delta,dt)
      enddo

      endif
#else
!DIR$ CONCURRENT !Cray
!cdir nodep      !NEC
!ocl novrec      !Fujitsu
      do ij = 1, icells
         i = indxi(ij)
         j = indxj(ij)
!lipscomb - function inlined to improve vector efficiency
!         coszen(i,j) = shr_orb_cosz(ydayp1, &
!                                    tlat(i,j),tlon(i,j),delta)
         coszen(i,j) = sin(tlat(i,j))*sin(delta) &
                     + cos(tlat(i,j))*cos(delta) &
                      *cos((sec/secday-p5)*c2*pi + tlon(i,j)) !cos(hour angle)
      enddo
#endif
!-tht

      end subroutine compute_coszen
 
!=======================================================================
 
      end module ice_orbital
 
!=======================================================================
