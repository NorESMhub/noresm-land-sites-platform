module dust_model
  use shr_kind_mod,     only: r8 => shr_kind_r8, cl => shr_kind_cl
  use spmd_utils,       only: masterproc
  use cam_abortutils,   only: endrun

use constituents,     only: cnst_name
use aerosoldef,       only: l_dst_a2, l_dst_a3
use camsrfexch,       only: cam_in_t
use ppgrid,           only: pcols
use cam_logfile,      only: iulog

implicit none
private
save

   integer, parameter :: numberOfDustModes = 2  !define in aerosoldef?

   !This can be refined, but the fractions in coarse/fine mode are approx ok
   real(r8), parameter, dimension(numberOfDustModes) :: emis_fraction_in_mode = (/0.13_r8, 0.87_r8 /)
   integer, dimension(numberOfDustModes)             :: tracerMap = (/-99, -99/) !index of dust tracers in the modes
   character(len=6), public, dimension(10)        :: dust_names
   integer, parameter, public                     :: dust_nbin = numberOfDustModes

   !Related to soil erodibility
   real(r8)          :: dust_emis_fact = -1.e36_r8        ! tuning parameter for dust emissions
   character(len=cl) :: soil_erod_file = 'soil_erod_file' ! full pathname for soil erodibility dataset

   logical, parameter, public :: dust_active = .TRUE.
public oslo_dust_emis_intr
public getNumberOfDustModes
public getDustTracerIndexInMode
public getEmissionFractionInDustMode
public isOsloDustTracer
public dust_init
public dust_readnl


!===============================================================================
contains
!===============================================================================

  subroutine dust_readnl(nlfile)

    use namelist_utils,  only: find_group_name
    use units,           only: getunit, freeunit
    use mpishorthand

    character(len=*), intent(in) :: nlfile  ! filepath for file containing namelist input

    ! Local variables
    integer :: unitn, ierr
    character(len=*), parameter :: subname = 'dust_readnl'

    namelist /dust_nl/ dust_emis_fact, soil_erod_file

    !-----------------------------------------------------------------------------

    ! Read namelist
    if (masterproc) then
       unitn = getunit()
       open( unitn, file=trim(nlfile), status='old' )
       call find_group_name(unitn, 'dust_nl', status=ierr)
       if (ierr == 0) then
          read(unitn, dust_nl, iostat=ierr)
          if (ierr /= 0) then
             call endrun(subname // ':: ERROR reading namelist')
          end if
       end if
       close(unitn)
       call freeunit(unitn)
    end if

#ifdef SPMD
    ! Broadcast namelist variables
    call mpibcast(dust_emis_fact, 1,                   mpir8,   0, mpicom)
    call mpibcast(soil_erod_file, len(soil_erod_file), mpichar, 0, mpicom)
#endif



  end subroutine dust_readnl
   function getEmissionFractionInDustMode(modeIndex) RESULT(fraction)
      integer, intent(in) :: modeIndex
      real(r8)            :: fraction
      fraction = emis_fraction_in_mode(modeIndex)
   end function getEmissionFractionInDustMode

   function getNumberOfDustModes() RESULT(answer)
      integer answer
      answer = numberOfDustModes
   end function getNumberOfDustModes


   subroutine dust_init()

      use soil_erod_mod, only: soil_erod_init
      implicit none
      integer             :: i


      call  soil_erod_init( dust_emis_fact, soil_erod_file )

      call set_oslo_indices()

      dust_names(:)="      "
      do i=1,numberOfDustModes
         dust_names(i) = cnst_name(tracerMap(i))
      end do

   end subroutine dust_init

   subroutine set_oslo_indices()
      implicit none
      tracerMap(1) = l_dst_a2
      tracerMap(2) = l_dst_a3
   end subroutine set_oslo_indices


   !****************************************************
   !This is copied from the MAM aerosols. Should not really
   !be necessary since the land model could calculate emissions
   !based on soil erodibility. 
   
   !However, the following code in dustMod.F90 (land model) makes it 
   !necessary to apply it here!
   !715 Set basin factor to 1 for now
   !716 
   !717     call get_proc_bounds(begg, endg, begl, endl, begc, endc, begp, endp)
   !718     do c = begc, endc
   !719       l = clm3%g%l%c%landunit(c)
   !720       if (.not. clm3%g%l%lakpoi(l)) then
   !721          mbl_bsn_fct(c) = 1.0_r8
   !722       end if
   !723     end do

   !For a general discussion of these factors, see: 
   !Zender et al JGR (vol 108, D16, 2003) 
   !http://onlinelibrary.wiley.com/doi/10.1029/2002JD003039/abstract

   function getDustTracerIndexInMode(modeIndex)RESULT(answer)
      integer, intent(in) :: modeIndex
      integer answer

      answer = tracerMap(modeIndex)
   
   end function getDustTracerIndexInMode

   function isOsloDustTracer(physTracerIndex) RESULT(answer)
      implicit none
      integer, intent(in) :: physTracerIndex
      integer             :: n
      logical             :: answer
      answer = .FALSE.
      do n = 1, numberOfDustModes
         if(tracerMap(n) .eq. physTracerIndex)then
            answer = .TRUE.
         end if
      end do
   end function isOsloDustTracer

   subroutine oslo_dust_emis_intr(state, cam_in)

      !----------------------------------------------------------------------- 
      ! 
      ! Purpose: 
      ! Interface to emission of all dusts.
      ! Notice that the mobilization is calculated in the land model (need #define BGC) and
      ! the soil erodibility factor is applied here.
      ! 
      ! see comments above in subroutine read_soil_erodibility_data 
      !-----------------------------------------------------------------------
      use cam_history,   only: outfld
      use physics_types, only: physics_state
      use soil_erod_mod, only : soil_erod_fact
      use soil_erod_mod, only : soil_erodibility

      implicit none

      ! Arguments:

      type(physics_state),    intent(in)    :: state   ! Physics state variables
      type(cam_in_t), target, intent(inout) :: cam_in  ! import state

      ! Local variables

      integer :: lchnk
      integer :: ncol
      integer :: i,n
      real(r8) :: soil_erod_tmp(pcols)
      real(r8) :: totalEmissionFlux(pcols)
      real(r8), pointer :: cflx(:,:)

      lchnk = state%lchnk
      ncol = state%ncol

      !Filter away unreasonable values for soil erodibility
      !(using low values e.g. gives emissions in greenland..)
      where(soil_erodibility(:,lchnk) .lt. 0.1_r8)
         soil_erod_tmp(:)=0.0_r8
      elsewhere
         soil_erod_tmp(:)=soil_erodibility(:,lchnk)
      end where

      totalEmissionFlux(:)=0.0_r8
      do i=1,ncol
         totalEmissionFlux(i) = totalEmissionFlux(i) + sum(cam_in%dstflx(i,:))
      end do

      cflx => cam_in%cflx
   
      !Note that following CESM use of "dust_emis_fact", the emissions are 
      !scaled by the INVERSE of the factor!!
      !There is another random scale factor of 1.15 there. Adapting the exact
      !same formulation as MAM now and tune later
      !As of NE-380: Oslo dust emissions are 2/3 of CAM emissions
      do n=1, numberOfDustModes
         cflx(:ncol, tracerMap(n)) = -1.0_r8*emis_fraction_in_mode(n) &
            *totalEmissionFlux(:ncol)*soil_erod_tmp(:ncol)/(dust_emis_fact)*1.15_r8  ! gives better AOD close to dust sources
      end do
    

      !call outfld('MBL_BSN_FCT',soil_erod_tmp,pcols,lchnk) 
      !call outfld('OSLO_DUST_EMIS',totalEmissionFlux,pcols,lchnk)

      return
   end subroutine oslo_dust_emis_intr

end module dust_model
