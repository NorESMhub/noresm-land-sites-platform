module oxi_diurnal_var

use chem_mods,         only : nfs
use physconst,         only : pi
use mo_chem_utls,      only : get_inv_ndx
use ppgrid,            only : pcols, pver
use phys_grid,         only: get_rlat_all_p, get_rlon_all_p
use shr_kind_mod,      only: r8 => shr_kind_r8
implicit none
private
save


public :: & 
  set_diurnal_invariants

private :: &
  sunrisesetxx , srisesetxx


  integer, pointer  :: id_oh,id_no3,id_ho2
  logical :: inv_oh,inv_ho2,inv_no3

contains


  subroutine set_diurnal_invariants(invariants,dtc,ncol,lchnk,inv_oh,inv_ho2,id_oh,id_ho2, inv_no3, id_no3) !++IH: added ,inv_no3, id_no3

   use chem_mods, only :  nfs
   use time_manager,      only : get_curr_date


   real(r8), intent(in)      :: dtc ! Time step
   integer, intent(in)       :: ncol
   integer,  intent(in)      :: lchnk      ! chunk id
   logical, intent(in)       :: inv_oh, inv_ho2, inv_no3  !++IH: added inv_no3
   integer, intent(in)       :: id_oh, id_ho2, id_no3     !++IH: added id_no3
   real(r8), intent(inout)   :: invariants(ncol,pver,nfs)




   integer :: i                            ! column index
   integer :: k                            ! height index
   integer :: iriseset                     ! sunrise/set flag
   integer :: day, mon, yr, jyr         ! date stuff
   integer :: j                            ! working var
   integer :: ncsec                        ! time stuff

   real(r8) :: deglat, deglon              ! lat and long (degrees)
   real(r8) :: solardec                    ! solar declination (degrees)
   real(r8) :: sum                         ! working vars
   real(r8) :: trise, tset                 ! sunrise and set times (h then d)
   real(r8) :: tlight                      ! amount of daylight (d)
   real(r8) :: trisej, tsetj               ! working vars
   real(r8) :: t1, t2, ta, tb              ! working vars
   real(r8) :: rlats(pcols), rlons(pcols)  ! latitude & longitude (radians)
   real(r8) :: fdiurn_oxid     
   real(r8) :: fdiurn_no3oxid              !++IH



   call get_curr_date(yr, mon, day, ncsec)
   call get_rlat_all_p( lchnk, ncol, rlats )
   call get_rlon_all_p( lchnk, ncol, rlons )

!   jyr = mod( yr, 100 ) + 1900
!   if (jyr < 1950) jyr = jyr + 100
!   if (jyr > 2049) jyr = jyr - 100
   jyr=2000  
! Assume the daily cycle to follow year 2000. The subroutine is
!   at any rate only valid between 1950 and 2050, so important years e.g. 1850
!   is out of boundary 


   do i=1,ncol

   fdiurn_oxid=1._r8
   fdiurn_no3oxid=1._r8  !++IH

      deglat = rlats(i)*180._r8/pi
      deglat = max( -89.9999_r8, min( +89.9999_r8, deglat ) )
      deglon = rlons(i)*180._r8/pi

! get sunrise and sunset times in UTC hours
      call sunrisesetxx( deglon, deglat, jyr, mon, day,   &
                iriseset, trise, tset, solardec )

! convert rise/set times to days
! compute tlight = amount of daylight
! handle case of all day or night
      if (iriseset > 0) then
         trise = trise/24._r8
         tset  = tset/24._r8
         tlight = tset - trise
         if (tlight < 0._r8) then
            tset = tset + 1.0_r8
            tlight = tlight + 1._r8
         end if
      else
         trise = 0._r8
         if (abs(deglat+solardec) .ge. 90._r8) then
            tset = 1._r8
         else
            tset = 0._r8
         end if
         tlight = tset - trise  !length of light period in a day
      end if

! if all day or all night (or very close to it), set fdiurn = 1.0
! Also in periods with all night, we put the mean value for all night steps
      if ((tlight .ge. 0.99_r8) .or. (tlight .le. 0.01_r8)) then
         fdiurn_oxid = 1._r8
         fdiurn_no3oxid = 1._r8  !++IH
! otherwise determine overlap between current timestep and daylight times
! to account for all overlap possibilities, need to try this 
! with rise/set times shifted by +/- 1 day 
      else  !==> There is diurnal cycle
         t1 = ncsec/86400._r8          !start of timestep (days)
         t2 = t1 + dtc/86400._r8       !end of timestep (days)
         sum = 0._r8
         do j = -1, 1
            trisej = trise + dfloat(j)  !one day before sunrise, sunrise, one day after runrise
            tsetj  = trisej + tlight    !time of sunset given "j"
            ta = max( t1, trisej )      !start or sunrise (if later)
            tb = min( t2, tsetj )       !end of step or sunset (if earlier)
            sum = sum + max( tb-ta, 0._r8 )

         end do

         !sum is length of timestep (in days) which has light
         !"sum"/(t1-t2) is fraction of timestep which has light
         !"tlight is fraction of day which has light
         !So if fraction of dt is higher than avg fraction during day ==> increase oxidants
         !   if fraction of dt is lower than  avg fraction during day ==> decrease oxidants

         !++IH
         if (inv_oh .or. inv_ho2) then
         !--IH
            fdiurn_oxid = max(1.0e-3_r8, sum/(t2-t1)/tlight)
         !++IH
         end if
         if (inv_no3) then
            fdiurn_no3oxid = max(1.0e-3_r8, (1._r8 - (sum/(t2-t1))) / (1._r8 - tlight))
         ! (1._r8 - (sum/(t2-t1))) is the fraction of timestep WITHOUT light
         ! (1._r8 - tlight) is the fraction of day WITHOUT light
         end if 
         !--IH
      end if

  if (inv_oh) then
      do k=1,pver
         invariants(i,k,id_oh)=invariants(i,k,id_oh)*fdiurn_oxid
      end do
    end if

    if (inv_ho2) then
      do k=1,pver
        invariants(i,k,id_ho2)=invariants(i,k,id_ho2)*fdiurn_oxid
      end do
    end if
    
    !++IH
    if (inv_no3) then
      do k=1,pver
        invariants(i,k,id_no3)=invariants(i,k,id_no3)*fdiurn_no3oxid
      end do
    end if
    !--IH

   end do  ! i= 1,ncol   
   end   subroutine set_diurnal_invariants


!--------------------------------------------------------------------
	subroutine sunrisesetxx( xlong, ylat, iyear, imonth, iday, &
     		iflag, trise, tset, solardec )
!
!   provides interface to subr srisesetxx without use of common blocks
!
!  input parameters
!	xlong - longitude in degrees (east longitudes are positive)
!	ylat  - latitude in degrees (north latitudes are positive)
!	iyear - year
!	imonth - month
!	iday - day
!  output parameters
!	iflag - status flag
!	    +1 - OK and there is a sunrise and sunset
!	     0 - OK but no sunrise or sunset
!	    -1 = input parameters (date or position) are bad
!	trise - time of sunrise in UT hours
!	tset  - time of sunset  in UT hours
!	solardec - apparent solar declination in degrees
!
!   written 17-aug-93 by r.c.easter
!   Rewritten into fortran 90 by Ø Seland




!   arguments


	real(r8) ,intent(in) :: xlong
	real(r8) ,intent(in) :: ylat
	integer  ,intent(in) :: iyear
	integer  ,intent(in) :: imonth
	integer  ,intent(in) :: iday
	integer  ,intent(out) :: iflag
	real(r8) ,intent(out) :: trise
	real(r8) ,intent(out) :: tset
	real(r8) ,intent(out) :: solardec
! local
	real(r8) sunrise, sunset, ap_dec
	real(r8) :: xlongb
	integer  :: iriseset,i

!   need xlong between -180 and +180
	xlongb = xlong
!	do 1000 i = 1, 10
	    if (xlongb .lt. -180.) then
		xlongb = xlongb + 360._r8
	    else if (xlongb .gt. 180._r8) then
		xlongb = xlongb - 360._r8
!	    else
!		goto 1050
	    end if
!1000	continue
!1050	continue
	
	call srisesetxx( iyear, imonth, iday, ylat, xlongb, &
        iriseset,sunrise, sunset, ap_dec)

	iflag = iriseset
	if (iflag .eq. 0) then
	    iflag = 1
	    if (abs(sunrise+100_r8) .le. 0.01_r8) iflag = 0
	end if
	trise = sunrise
	tset  = sunset
	solardec = ap_dec

	end subroutine sunrisesetxx



!c***************************************************************************
	subroutine srisesetxx(iyear, month, iday, rlat, rlong, &
        iriseset,sunrise, sunset,ap_dec)


	integer  ,intent(in) :: iyear
	integer  ,intent(in) :: month
	integer  ,intent(in) :: iday
	real(r8) ,intent(in) :: rlat
	real(r8) ,intent(in) :: rlong
	integer  ,intent(out) :: iriseset
	real(r8) ,intent(out) :: sunrise
	real(r8) ,intent(out) :: sunset
	real(r8) ,intent(out) :: ap_dec


!local
	integer :: jday

        integer ,dimension(12) :: iimonth,iimonthleap
        logical                :: leapyr

!c math definitions. 
! 	real(r8),parameter  :: twopi = 6.2831853071795864_r8
 	real(r8),parameter  :: twopi = 2._r8*pi
	real(r8), parameter :: deg_rad = 0.017453292519943295_r8
	real(r8), parameter  :: rad_deg = 57.295779513082323_r8
! local variables
        
	real(r8) :: mean_anomaly, mean_longitude, mean_obliquity
        real(r8) :: year

        real(r8) :: delta_years,delta_days,days_j2000
        real(r8) :: cent_j2000,f_mean_anomaly,f_mean_longitude
        real(r8) :: ecliptic_long,f_ap_ra, ap_ra,f_gmst0h
        real(r8) :: gmst0h,rlat_r,tan_lat,tan_dec,tangterm
        real(r8) :: timeterm

	data iimonth /0,31,59,90,120,151,181,212,243,273,304,334/
	data iimonthleap /0,31,60,91,121,152,182,213,244,274,305,335/
        leapyr = .false.

!!	common / sundataxx_cmn / jday, iriseset, 
!     +		sunrise, sunset, rloc_timehrs, ap_dec
!c--------------------------------------------------------------------------
!c "sunriseset.c" contains the integer function sunriseset() for calculating
!c  the rising and setting times of the Sun as seen from a place on Earth on a
!c  specific date.
!c
!c  Version 1.0 - April 6, 1992.
!c  (This code was adapted from "solarpos.c" Version 3.1.)
!c
!c  sunriseset() employs the low precision formulas for the Sun's coordinates
!c  given in the "Astronomical Almanac" of 1990 to compute the Sun's apparent
!c  right ascension, apparent declination, and Greenwich mean sidereal time at
!c  0 hours Universal Time, and then the rising and setting times of the Sun.
!c  The "Astronomical Almanac" (A. A.) states a precision of 0.01 degree for the
!c  apparent coordinates between the years 1950 and 2050.
!c
!c  The following assumptions and simplifications are made:
!c  -> diurnal parallax is ignored, resulting in 0 to 9 arc seconds error in
!c     apparent position.
!c  -> diurnal aberration is also ignored, resulting in 0 to 0.02 second error
!c     in right ascension and 0 to 0.3 arc second error in declination.
!c  -> geodetic site coordinates are used, without correction for polar motion
!c     (maximum amplitude of 0.3 arc second) and local gravity anomalies.
!c  -> the formulas ignore atmospheric refraction, semi-diameter, and changes
!c     in right ascension and declination over the course of a day; the
!c     accuracies of sunrise and sunset are about 2 and 7 minutes for latitude
!c     and longitude of 0 degrees, but accuracy degrades significantly for high
!c     latitudes.
!c
!c
!c  The necessary input parameters are:
!c  -> the UT date, specified in one of three ways:
!c       1) year, month, day.fraction
!c       2) year, daynumber.fraction
!c       3) days.fraction elapsed since January 0, 1900.
!c  Note: in GChM application, only specification #1 is currently valid
!c  -> site geodetic (geographic) latitude and longitude.
!c
!c  Refer to the function declaration for the parameter type specifications and
!c  formats.
!c
!c  sunriseset() returns -1 if an input parameter is out of bounds, or 0 if
!c  values were written to the locations specified by the output parameters.
!c  Sunrise and sunset times are in UT hours; if there is no sunrise or sunset
!c  the values are -1.0.
!c
!c  Author: Nels Larson
!c          Pacific Northwest Lab.
!c          P.O. Box 999
!c          Richland, WA 99352
!c          U.S.A.
!c
!c--------------------------------------------------------------------------
!c modifications for gchm application by eg chapman
!c	1. translated from c language to fortran
!c	2. input date must be in year, month, day.fraction format; other input 
!c	   code eliminated.              
!c	3. added indicator iriseset. when equal to -1, indicates location
!c	   or date is out of range. 
!c	
!c---------------------------------------------------------------------------

!c-------------------------------------------------------------------------
!c explanation of terms taken from c code
!c int    iyear,         Four digit year (Gregorian calendar).
!c                       [1950 through 2049; 0 if using days_1900] 
!c        month;        Month number.
!c                       [1 through 12; 0 if using daynumber for day] 
!c 
!c day,           /* Calendar day.fraction, or daynumber.fraction.
!c                       *   [If month is NOT 0:
!c
!c                       *      0 through 32; 31st @ 18:10:00 UT = 31.75694
!c                       *    If month IS 0:
!c                       *      0 through 367; 366 @ 18:10:00 UT = 366.75694] */
!c       days_1900,     /* Days since 1900 January 0 @ 00:00:00 UT.
!c                       *   [18262.0 (1950/01/00) through 54788.0 (2049/12/32);
!c
!c                       *    1990/01/01 @ 18:10:00 UT = 32873.75694;
!c                       *    0.0 o.k. if using {year, month, day} or
!c                       *    {year, daynumber}] */
!c       rlat       Observation site geographic latitude.
!c                  [degrees.fraction, North positive] 
!c       rlong      Observation site geographic longitude.
!c                  [degrees.fraction, East positive] 
!c       *ap_ra,        /* Apparent solar right ascension.
!c                       *   [hours; 0.0 <= *ap_ra < 24.0] */
!c       *ap_dec,       /* Apparent solar declination.
!c                       *   [degrees; -90.0 <= *ap_dec <= 90.0] */
!c
!c       *sunrise,      /* Time of sunrise.
!c                           [UT hours.fraction; -1.0 if no sunrise or sunset] */
!c       *sunset;       /* Time of sunset.
!c                           [UT hours.fraction; -1.0 if no sunset or sunrise] */
!c  int    daynum();        /* Computes a sequential daynumber during a year. */
!c  int    daynumber,       /* Sequential daynumber during a year. */
!c         delta_days,      /* Whole days since 2000 January 0. */
!c         delta_years;     /* Whole years since 2000. */
!c  double cent_J2000,      /* Julian centuries since epoch J2000.0 at 0h UT. */
!c         days_J2000,      /* Days since epoch J2000.0. */
!c         ecliptic_long,   /* Solar ecliptic longitude. */
!c
!c         gmst0h,          /* Greenwich mean sidereal time at 0 hours UT. */
!c         integral,        /* Integral portion of double precision number. */
!c         mean_anomaly,    /* Earth mean anomaly. */
!c         mean_longitude,  /* Solar mean longitude. */
!c         mean_obliquity,  /* Mean obliquity of the ecliptic. */
!c         tan_dec,         /* Tangent of apparent declination. */
!c         tan_lat,         /* Tangent of latitude. */
!c
!c         tangterm,        /* Tangent term of Sun rise/set equation. */
!c         timeterm;        /* Time term of Sun rise/set equation. */
!c----------------------------------------------------------------------
	iriseset = 0
!c check latitude, longitude, dates for proper range before calculating dates.
	 if (((rlat .lt. -90._r8) .or. (rlat .gt. 90._r8)) .or. &
       ((rlong .lt. -180._r8) .or. (rlong .gt. 180._r8))) then
		iriseset = -1
		return
	end if

! Year assumed to be betweeen 1950 and 2049. As the model is outside these
!  	boundary in many cases. year 2000 is assumed for this version of the
!  	model


!  	if (iyear .lt. 1950 .or. iyear .gt. 2049) then
!		iriseset = -1
!		return
!	end if
!        if (((month .lt. 1) .or. (month .gt. 12)) .or. &
!           ((iday .lt. 0) .or. (iday .gt. 32))) then
!     		iriseset = -1    
!		return
!	end if
!c determine julian day number



!c there is no year 0 in the Gregorian calendar and the leap year cycle
!c changes for earlier years. 
!	if (iyear .lt. 1) then
!		iriseset = -1
!		return
!	end if
!c leap years are divisible by 4, except for centurial years not divisible
!c by 400. 


!	year = real (iyear)
!	if ((amod(year,4.) .eq. 0.0) .and. (amod(year,100.) .ne. 0.0)) &
!     	  leapyr = 1
!	if(amod(year,400.) .eq. 0.0) leapyr = 1 
  	jday = iimonth(month) + iday
!	if ((leapyr .eq. 1) .and. (month .gt. 2)) jday = jday + 1

! 

!The 
!c construct Julian centuries since J2000 at 0 hours UT of date,
!c days.fraction since J2000, and UT hours.
 	   delta_years = iyear - 2000._r8
!c delta_days is days from 2000/01/00 (1900's are negative). 
  	 delta_days = delta_years * 365._r8 + delta_years / 4._r8 + jday
    	if (iyear .gt. 2000) delta_days = delta_days + 1._r8
!c J2000 is 2000/01/01.5 
  	  days_j2000 = delta_days - 1.5_r8
    	cent_j2000 = days_j2000 / 36525._r8
!c compute solar position parameters.
!c    A. A. 1990, C24.
  	f_mean_anomaly = (357.528_r8 + 0.9856003_r8 * days_j2000)
  	f_mean_longitude = (280.460_r8 + 0.9856474_r8 * days_j2000)
!c put mean_anomaly and mean_longitude in the range 0 -> 2 pi. 
	mean_anomaly = (f_mean_anomaly / 360._r8 - int(f_mean_anomaly    &
        /360._r8)) * twopi
	mean_longitude = (f_mean_longitude /360. - int(            &
        f_mean_longitude/360._r8)) * twopi
  	mean_obliquity = (23.439_r8 - 4.0e-7_r8 * days_j2000) * deg_rad   
  	ecliptic_long = ((1.915_r8 * sin(mean_anomaly)) +     &
                   (0.020_r8 * sin(2.0 * mean_anomaly))) * deg_rad +  &
                  mean_longitude
! tangent of ecliptic_long separated into sine and cosine parts for ap_ra. 
  	f_ap_ra = atan2(cos(mean_obliquity) * sin(ecliptic_long), &
                 cos(ecliptic_long))
! change range of ap_ra from -pi -> pi to 0 -> 2 pi. 
  	if (f_ap_ra .lt. 0.0) f_ap_ra = f_ap_ra + twopi
! put ap_ra in the range 0 -> 24 hours. 
	ap_ra = (f_ap_ra / twopi - int(f_ap_ra /twopi)) * 24._r8
  	ap_dec = asin(sin(mean_obliquity) * sin(ecliptic_long))
! calculate local mean sidereal time.
! A. A. 1990, B6-B7.
! horner's method of polynomial exponent expansion used for gmst0h. 
	f_gmst0h = 24110.54841_r8 + cent_j2000 * (8640184.812866_r8 &
          +cent_j2000 * (0.093104_r8 - cent_j2000 * 6.2e-6_r8)) 
! convert gmst0h from seconds to hours and put in the range 0 -> 24. 
! 24 hours = 86400 seconds 
	gmst0h = (f_gmst0h / 86400._r8 - int(f_gmst0h / 86400._r8)) * 24._r8
  	if (gmst0h .lt. 0._r8) gmst0h = gmst0h + 24._r8
!c convert latitude to radians. 
  	rlat_r =  rlat * deg_rad 
!c avoid tangent overflow at +-90 degrees.
!c 1.57079615 radians is equal to 89.99999 degrees.
  	if (abs(rlat_r) .lt. 1.57079615_r8) then
    		tan_lat = tan(rlat_r)
  	else
    		tan_lat = 6.0e6_r8
	end if
  	if (abs(ap_dec) .lt. 1.57079615_r8) then
    		tan_dec = tan(ap_dec)
  	else
    		tan_dec = 6.0e6_r8
	end if
!c compute UTs of sunrise and sunset.
!c A. A. 1990, A12.
  	tangterm = tan_lat * tan_dec
  	if (abs(tangterm) .gt. 1.0_r8) then
    		sunrise = -100._r8
    		sunset = -100._r8
  	else
!c compute angle of tangterm and convert to hours. 
    		tangterm = acos(-tangterm) / twopi * 24._r8
    		timeterm = ap_ra - rlong / 15._r8 - gmst0h
    		sunrise = timeterm - tangterm
    		sunset = timeterm + tangterm
!c put sunrise and sunset in the range 0 to 24 hours. 
!cec inserted following statement since in some latitudes timeterm
!cec minus tangterm is less than -25
		if (sunrise .le. -24._r8) sunrise = sunrise + 48._r8
    		if (sunrise .lt. 0._r8) sunrise = sunrise + 24._r8
    		if (sunrise .ge. 24._r8) sunrise = sunrise - 24._r8
    		if (sunset .lt. 0._r8) sunset = sunset + 24._r8
    		if (sunset .ge. 24._r8) sunset = sunset - 24._r8
!c mean sidereal day is 0.99727 mean solar days. 
    		sunrise = sunrise * 0.99727_r8
		sunset =  sunset * 0.99727_r8
  	end if 
!c convert ap_dec to degrees. 
  	ap_dec = ap_dec * rad_deg 
	return
	end subroutine srisesetxx


end module oxi_diurnal_var




