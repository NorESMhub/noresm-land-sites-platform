module oslo_control
!-----------------------------------------------------------------------
! Purpose:
!
! Provides a control interface to CAM-Oslo packages
!-----------------------------------------------------------------------

use spmd_utils,    only: masterproc
use cam_logfile,   only: iulog
use cam_abortutils, only: endrun
use shr_kind_mod,  only: r8 => shr_kind_r8
use cam_cpl_indices, only:index_x2a_Faoo_fdms_ocn

implicit none
private
save

public :: &
   oslo_ctl_readnl,   &! read namelist from file
   oslo_getopts        ! generic query method

! Private module data

integer, parameter,public :: dir_string_length=256
character(len=16), parameter :: unset_str = 'UNSET'
integer,           parameter :: unset_int = huge(1)

! Namelist variables:
real(r8), private                         :: volc_fraction_coarse = 0.0_r8  !Fraction of volcanic aerosols in coarse mode
character(len=dir_string_length), private     :: aerotab_table_dir = unset_str
! DMS/Ocean namelist variables
character(len=20), private                    :: dms_source       = unset_str
character(len=32), private                    :: dms_source_type  = unset_str
character(len=20), private                    :: opom_source      = unset_str
character(len=32), private                    :: opom_source_type = unset_str
character(len=dir_string_length), private     :: ocean_filename   = unset_str
character(len=dir_string_length), private     :: ocean_filepath   = unset_str
integer, private                              :: dms_cycle_year   = 0 ! =unset_int?
integer, private                              :: opom_cycle_year  = 0 ! =unset_int?

!======================================================================= 
contains
!======================================================================= 

subroutine oslo_ctl_readnl(nlfile)

   use namelist_utils,  only: find_group_name
   use units,           only: getunit, freeunit
   use mpishorthand

   character(len=*), intent(in) :: nlfile  ! filepath for file containing namelist input

   ! Local variables
   integer :: unitn, ierr
   character(len=*), parameter :: subname = 'oslo_ctl_readnl'
   logical                     :: dirExists=.FALSE.
!new
   logical                     :: fileExists=.FALSE.

   namelist /oslo_ctl_nl/ volc_fraction_coarse, aerotab_table_dir, dms_source, &
                          dms_source_type, opom_source, opom_source_type, &
                          ocean_filename, ocean_filepath, dms_cycle_year, opom_cycle_year     
   !-----------------------------------------------------------------------------

   if (masterproc) then
      unitn = getunit()
      open( unitn, file=trim(nlfile), status='old' )
      call find_group_name(unitn, 'oslo_ctl_nl', status=ierr)
      if (ierr == 0) then
         read(unitn, oslo_ctl_nl, iostat=ierr)
         if (ierr /= 0) then
            call endrun(subname // ':: ERROR reading namelist')
         end if
      end if
      close(unitn)
      call freeunit(unitn)
   end if

#ifdef SPMD
   ! Broadcast namelist variables
   call mpibcast(volc_fraction_coarse,                        1 , mpir8,  0, mpicom)
   call mpibcast(aerotab_table_dir,      len(aerotab_table_dir) , mpichar,  0, mpicom)
!new dms variables
   call mpibcast(dms_source,             len(dms_source)        , mpichar,  0, mpicom)
   call mpibcast(dms_source_type,        len(dms_source_type)   , mpichar,  0, mpicom)
   call mpibcast(opom_source,            len(opom_source)       , mpichar,  0, mpicom)
   call mpibcast(opom_source_type,       len(opom_source_type)  , mpichar,  0, mpicom)
   call mpibcast(ocean_filename,         len(ocean_filename)    , mpichar,  0, mpicom)
   call mpibcast(ocean_filepath,         len(ocean_filepath)    , mpichar,  0, mpicom)
   call mpibcast(dms_cycle_year,                              1 , mpiint,   0, mpicom)
   call mpibcast(opom_cycle_year,                             1 , mpiint,   0, mpicom)

#endif

   ! Error checking:

   ! Defaults for PBL and microphysics are set in build-namelist.  Check here that
   ! values have been set to guard against problems with hand edited namelists.
   if(volc_fraction_coarse .lt. 0.0_r8 .OR. volc_fraction_coarse .gt. 1.0_r8)then
      write(iulog,*)'cam_oslo: illegal value of volc_fraction_coarse', volc_fraction_coarse
      call endrun('cam_oslo: illegal value of volc_fraction_coarse')
   end if

#if defined CPRGNU || defined __GFORTRAN__
   inquire( file=trim(aerotab_table_dir), exist=dirExists ) 
#elif defined CPRINTEL
   inquire( directory=trim(aerotab_table_dir), exist=dirExists )
#else
   !Don't know how to check this on other compilres.. Assume exists
   !and let crash later..
   dirExists = .TRUE.
#endif
   if(.not. dirExists)then
      call endrun("cam_oslo: can not find aerotab table directory "//trim(aerotab_table_dir))
   else
      write(iulog,*)"Reading aerosol tables from : " // trim(aerotab_table_dir)
   endif

   ! Error check for OCEAN file
   ! can ocean file be found?
   inquire( file=trim(ocean_filepath)//'/'//trim(ocean_filename), exist=fileExists )
   if(.not. fileExists)then
      call endrun("oslo_control: can not find ocean file "//trim(ocean_filepath)//'/'//trim(ocean_filename))
   else
      write(iulog,*)"Reading ocean tracers from : " // trim(ocean_filepath)//'/'//trim(ocean_filename)
   endif

   ! Error check for dms_source from namelist
   if (dms_source=='ocean_flux')then
      if (index_x2a_Faoo_fdms_ocn == 0) then
         call endrun("cam_oslo: dms source set to "//trim(dms_source)//" but bgc is off")
      else
         write(iulog,*)"DMS emission source is : "// trim(dms_source)
      endif
   elseif(dms_source=='kettle' .or.  dms_source=='lana' .or. dms_source=='emission_file')then
      write(iulog,*)"DMS emission source is : "// trim(dms_source)
   else
      call endrun("oslo_control: no valid dms source from namelist: " //trim(dms_source))
   endif

   ! Error check for opom_source from namelist
   if(opom_source=='no_file' .or. opom_source=='nilsson' .or. opom_source=='odowd')then
      write(iulog,*)"Ocean POM emission source is : "// trim(opom_source)
   else
      call endrun("oslo_control: no valid opom source from namelist: " //trim(opom_source))
   endif



! more security checks needed?

! end of test

end subroutine oslo_ctl_readnl

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine oslo_getopts(volc_fraction_coarse_out, &
                        aerotab_table_dir_out,    &
                        dms_source_out,           &
                        dms_source_type_out,      &
                        opom_source_out,          &
                        opom_source_type_out,     &
                        ocean_filename_out,       &
                        ocean_filepath_out,       &
                        opom_cycle_year_out,      &
                        dms_cycle_year_out  )
!-----------------------------------------------------------------------
! Purpose: Return runtime settings
!-----------------------------------------------------------------------

   real(r8),          intent(out), optional :: volc_fraction_coarse_out 
   character(len=dir_string_length), intent(out), optional :: aerotab_table_dir_out

   character(len=dir_string_length), intent(out), optional :: ocean_filename_out
   character(len=dir_string_length), intent(out), optional :: ocean_filepath_out
   character(len=20),                intent(out), optional :: dms_source_out
   character(len=32),                intent(out), optional :: dms_source_type_out
   integer          ,                intent(out), optional :: dms_cycle_year_out
   character(len=20),                intent(out), optional :: opom_source_out
   character(len=32),                intent(out), optional :: opom_source_type_out
   integer          ,                intent(out), optional :: opom_cycle_year_out

   if ( present(volc_fraction_coarse_out) ) volc_fraction_coarse_out = volc_fraction_coarse
   if ( present(aerotab_table_dir_out) ) aerotab_table_dir_out = aerotab_table_dir

   if ( present(ocean_filename_out) ) ocean_filename_out  = ocean_filename 
   if ( present(ocean_filepath_out) ) ocean_filepath_out  = ocean_filepath
   if ( present(dms_source_out) )     dms_source_out      = dms_source
   if ( present(dms_source_type_out) )dms_source_type_out = dms_source_type
   if ( present(dms_cycle_year_out) ) dms_cycle_year_out  = dms_cycle_year
   if ( present(opom_source_out) )    opom_source_out     = opom_source
   if ( present(opom_source_type_out))opom_source_type_out= opom_source_type
   if ( present(opom_cycle_year_out) )opom_cycle_year_out = opom_cycle_year
end subroutine oslo_getopts

!===============================================================================
end module oslo_control
