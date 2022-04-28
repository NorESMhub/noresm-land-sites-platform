module seasalt_model

use constituents,     only: cnst_name
use aerosoldef,       only: l_ss_a1, l_ss_a2, l_ss_a3,l_om_ni   &
                           , MODE_IDX_SS_A1, MODE_IDX_SS_A2, MODE_IDX_SS_A3 &
                           , rhopart
use const,            only: volumeToNumber
use shr_kind_mod,     only: r8 => shr_kind_r8, cl => shr_kind_cl
use spmd_utils,       only: masterproc
use camsrfexch,       only: cam_in_t, cam_out_t    
use ppgrid,           only: pcols, pver,pverp
use constituents,     only: pcnst, cnst_add, cnst_name, cnst_get_ind
use aerodep_flx,      only: aerodep_flx_prescribed
use cam_abortutils,   only: endrun
use cam_logfile,      only: iulog
use oslo_ocean_intr,  only: oslo_opom_emis_intr, oslo_opom_inq

implicit none
private     
save 

   !Add Spracklen OC source related to sea salt, Spracklen says about 5.5 sub-micron Tg(C) per year (page 3) 
   !Total sea salt emissions are about 8000 Tg/year, take into account OM/OC-factor of about 1.4
   !==> scale factor of approx 7.7/8000
   !Note: The emissions are not REALLY related to sea salt, 
   !but this is as close as we get with the current version
   !GRL Volume 35, Issue 12, 28 June 2008, http://onlinelibrary.wiley.com/doi/10.1029/2008GL033359/abstract
   real(r8), parameter :: seasaltToSpracklenOM = 7.7_r8/8000_r8

   !After discussions with Alf K, it is better to scale with only smallest SS-mode since POM is small
   !and assume same production mechanism. Nudged 1 degree simulations give 2.52 Tg/yr of SS_A1, so 
   !to obtain 7.7, we need to scale them by 7.7 / 2.52 ==> 3.03
!cak   real(r8), parameter :: seasaltToSpracklenOM2 = 3.03_r8
   !updated value for Salter et al. sea-salt treatment, which gives global annual SS_A1 emissions of
   !2.663 instead of 0.153 ng m-2 s-1 (i.e. ca 17 times more than the old sea-salt treatment):
   real(r8), parameter :: seasaltToSpracklenOM2 = 3.03_r8*0.153_r8/2.663_r8
!cak

   integer, parameter :: numberOfSaltModes = 3
   character(len=6), public, dimension(10)        :: seasalt_names
   integer, parameter, public :: seasalt_nbin             = numberOfSaltModes              !just because this is needed by mo_photo.F90

   !Numbers in table below are from Kirkevåg et al (2013) http://www.geosci-model-dev.net/6/207/2013/gmd-6-207-2013.html
   !Based on Struthers et al 2011 (http://www.atmos-chem-phys.net/11/3459/2011/acp-11-3459-2011.html)
   !which are again modified from Maartensson , JGR, vol 108. no D9, 4297, 2003
   !
   !Note that using the numbers from the Kirkevag paper will give 20% too small mass emissions of sea salt globally!!
   !The number of significant digits there should have been larger! We are here using the numbers as received from the swedes.
   !
   !THESE ARE THE NUMBERS RECEIVED FROM THE SWEDES, THEY ARE UN-DOCUMENTED (SEE EMISSIONS.F90 of NORESM1)
   !***************************************************************************************************
!   real(r8), dimension(numberOfSaltModes), parameter :: coeffA = (/0.0_r8      ,     0.0_r8  ,  3.0608e3_r8 /)
!   real(r8), dimension(numberOfSaltModes), parameter :: coeffB = (/-3.3551e6_r8, 1.1768e5_r8 , -1.6675e6_r8 /)
!   real(r8), dimension(numberOfSaltModes), parameter :: coeffC = (/1.0554e9_r8 ,  -1.1369e7_r8, 2.2879e8_r8 /)

   !FOR INFO: THESE ARE THE NUMBERS FROM THE PAPER WHICH GIVE TOO LOW EMISSIONS!!
   !*******************************************************************************************************
   !real(r8), dimension(numberOfSaltModes), parameter :: coeffA = (/0.0_r8,     0.0_r8,     3.06e3_r8 /)
   !real(r8), dimension(numberOfSaltModes), parameter :: coeffB = (/-3.36e6_r8, 1.18e5_r8, -1.67e6_r8 /)
   !real(r8), dimension(numberOfSaltModes), parameter :: coeffC = (/1.05e9_r8,  -1.14e7_r8, 2.29e8_r8 /)

   !New numbers are based on Salter et al. (2105): www.atmos-chem-phys-discuss.net/15/13783/2015/doi:10.5194/acpd-15-13783-2015 
   !Values from Table 1 in Salter et al. (2015):
   !*******************************************************************************************************
    real(r8), dimension(numberOfSaltModes), parameter :: coeffA = (/-5.2168e5_r8,  0.0_r8,      0.0_r8      /)
    real(r8), dimension(numberOfSaltModes), parameter :: coeffB = (/ 3.31725e7_r8, 7.374e5_r8,  1.4210e4_r8 /) 
    real(r8), dimension(numberOfSaltModes), parameter :: coeffC = (/-6.95275e8_r8,-2.4803e7_r8, 1.4662e7_r8 /)
    real(r8), dimension(numberOfSaltModes), parameter :: coeffD = (/ 1.0684e10_r8, 7.7373e8_r8, 1.7075e8_r8 /)

   real(r8), parameter               :: z0= 0.0001_r8 ![m] roughness length over ocean

   
   integer, dimension(numberOfSaltModes)     :: modeMap    ! [idx] which modes are we modifying
   integer, dimension(numberOfSaltModes)     :: tracerMap  ! [idx] which tracers are we modifying

   real(r8), dimension(pcols), save, public  :: OMOceanSource ![kg/m2/s] new OM ocean source
   real(r8), dimension(pcols), save, public  :: spracklenOMOceanSource ![kg/m2/s] spracklen ocean source
   !real(r8), dimension(pcols), save, public  :: spracklenOMOceanSource2 ![kg/m2/s] spracklen ocean source
   real(r8), dimension(pcols)                 :: onOMOceanSource ![kg/m2/s] OM source from Nilsson/O'Dowd
   logical, parameter, public :: seasalt_active = .TRUE.

public oslo_salt_emis_intr
public seasalt_init

!===============================================================================
contains
!===============================================================================

   subroutine seasalt_init()

      implicit none

      integer         :: i

      modeMap(1) = MODE_IDX_SS_A1  
      modeMap(2) = MODE_IDX_SS_A2
      modeMap(3) = MODE_IDX_SS_A3

      tracerMap(1) = l_ss_a1
      tracerMap(2) = l_ss_a2
      tracerMap(3) = l_ss_a3

      seasalt_names(:)="      "
      do i=1,numberOfSaltModes
         seasalt_names(i) = cnst_name(tracerMap(i))
      end do

      spracklenOMOceanSource(:) = 0.0_r8
   end subroutine seasalt_init

subroutine oslo_salt_emis_intr(state, cam_in)

    !----------------------------------------------------------------------- 
    ! Purpose: 
    ! Interface to emission of sea salt
    !-----------------------------------------------------------------------
    use cam_history,   only: outfld
    use physics_types, only: physics_state

    ! Arguments:

    type(physics_state),    intent(in)    :: state   ! Physics state variables
    type(cam_in_t), target, intent(inout) :: cam_in  ! import state

    real(r8), dimension(pcols)                   :: whiteCapAreaFraction ![fraction]
    real(r8), dimension(pcols)                   :: open_ocean ![fraction]
    real(r8), dimension(pcols,numberOfSaltModes) :: numberFlux ![#/m2/sec]
    real(r8), dimension(pcols)                   :: u10m       ![m/s]
    real(r8), dimension(pcols)                   :: totalSaltEmis       ![kg/m2/s]
    real(r8), pointer                            :: sst(:)     ![frc] sea surface temperature
    real(r8), pointer                            :: ocnfrc(:)  ![frc] ocean fraction
    real(r8), pointer                            :: icefrc(:)  ![frc] ice fraction
    integer                                      :: n          ![] counter for modes
    integer                                      :: ncol       ![nbr] number of columns in use
    integer                                      :: lchnk      ! chunk index


    !number of columns in use
    ncol = state%ncol
    lchnk = state%lchnk

    !pointers to land model variables
    ocnfrc => cam_in%ocnfrac
    icefrc => cam_in%icefrac
    sst    => cam_in%sst

    !start with midpoint wind speed
    u10m(:ncol)=sqrt(state%u(:ncol,pver)**2+state%v(:ncol,pver)**2)
    
    ! move the winds to 10m high from the midpoint of the gridbox: 
    u10m(:ncol)=u10m(:ncol)*log(10._r8/z0)/log(state%zm(:ncol,pver)/z0)

!    !whitecap area (eqn 1 in Struthers et al., 2011)
!    whitecapAreaFraction(:ncol) = (3.84_r8*10.0_r8**(-6.0_r8))*(u10m(:ncol)**3.41_r8)

    ! New whitecap area fraction / air entrainment flux from eqn. 6 in Salter et al. (2015)
    ! JCA & MS Using Hanson & Phillips 99 air entrainment vs. wind speed 
    ! (Note the uncertainty in the factor 2, written as 2 pluss/minus 1 in Eq. 6 -> possible tuning factor)
!aktst+    whitecapAreaFraction(:ncol) = (2.0_r8*10.0_r8**(-8.0_r8))*(u10m(:ncol)**3.41_r8)
    whitecapAreaFraction(:ncol) = (2.0_r8*10.0_r8**(-8.0_r8))*(u10m(:ncol)**3.74_r8)
!aktst-

    whitecapAreaFraction(:ncol) = ocnfrc(:ncol) * (1._r8-icefrc(:ncol)) * whitecapAreaFraction(:ncol)
    open_ocean(:ncol) = ocnfrc(:ncol) * (1._r8-icefrc(:ncol))

    do n=1,numberOfSaltModes

!       !eqn 1 in Kirkevag et al. (2013)
!       numberFlux(:ncol,n) = whitecapAreaFraction(:ncol)*     &
!                              (                       &
!                              coeffA(n)*sst(:ncol)*sst(:ncol)   &
!                             + coeffB(n)*sst(:ncol)             &
!                             + coeffC(n)                        &
!                             )
       !Eqn. 9 in Salter et al. (2015)
       numberFlux(:ncol,n) = whitecapAreaFraction(:ncol)*                                                     &
                            ( coeffA(n)*(sst(:ncol)-273.15_r8)*(sst(:ncol)-273.15_r8)*(sst(:ncol)-273.15_r8)  &
                            + coeffB(n)*(sst(:ncol)-273.15_r8)*(sst(:ncol)-273.15_r8)                         &
                            + coeffC(n)*(sst(:ncol)-273.15_r8)                                                &
                            + coeffD(n) )
    end do          


    do n=1,numberOfSaltModes
       cam_in%cflx(:ncol, tracerMap(n)) = numberFlux(:ncol,n)         &  !#/m2/sec
                                       / volumeToNumber(modeMap(n))  &  !==> m3/m2/sec 
                                       * rhopart(tracerMap(n))          !==> kg/m2/sec
    end do

    !totalSaltEmis(:ncol)=0.0_r8
    !do n=1,numberOfSaltModes
    !  totalSaltEmis(:ncol) = totalSaltEmis(:ncol) + cam_in%cflx(:ncol,tracerMap(n))
    !end do
    !spracklenOMOceanSource(:ncol) =  seasaltToSpracklenOM * totalSaltEmis(:ncol)
  
    !The above code scales to total seasalt emisisons. This scales to mode 1
    !so assuming that submicron OM is proportional to smallest sea salt mode
    spracklenOMOceanSource(:ncol) = cam_in%cflx(:ncol, tracerMap(1))*seasaltToSpracklenOM2

    !do i=1,ncol
    !   if(ocnfrc(i).gt.0.999_r8 .and. icefrc(i).lt.0.000001_r8 .and. u10m(i).gt.5 .and. sst(i).gt. 284.0_r8)then
    !      print*, "u,sst, s1, s2", u10m(i), sst(i), spracklenOMOceanSource(i), spracklenOMOCeanSource2(i)
    !   end if
    !end do

    if (oslo_opom_inq())then
       call oslo_opom_emis_intr(cam_in%cflx(:ncol, tracerMap(1)), &
                                 cam_in%cflx(:ncol, tracerMap(2)), &
                                 cam_in%cflx(:ncol, tracerMap(3)), &
                                 open_ocean ,ncol,lchnk,  onOMOceanSource )
       OMOceanSource(:ncol) = onOMOceanSource(:ncol)
    else
       OMOceanSource(:ncol) = spracklenOMOceanSource(:ncol)
    endif

    return
  end subroutine oslo_salt_emis_intr

end module seasalt_model
