!-------------------------------------------------------------------
! Marine DMS and POM emissions module
! Documentation: Implementation of interactive DMS and marine organic 
! emission schemes in NorESM2, Lewinschal, 2015
! Manages reading and interpolation of ocean tracer concentrations from file
! and calculates DMS and marine POM emissions.
! Parameterisations available:
! Nightingale et al. Global biogeochemical cycles 2000 (DMS)
! Nilsson, unpublished (POM)
! O'Dowd et al. GRL 2008 (POM)
! Based on prescribed_volcaero created by Francis Vitt and mo_srf_emissions
!-------------------------------------------------------------------
module oslo_ocean_intr

   use shr_kind_mod, only : r8 => shr_kind_r8
   use cam_abortutils,   only : endrun
   use spmd_utils,   only : masterproc
   use tracer_data,  only : trfld, trfile
   use cam_logfile,  only : iulog
   use ppgrid,       only : pcols, pver,pverp
   use camsrfexch,   only : cam_in_t !, cam_out_t  ?

   implicit none



!  new type for ocean species

   type :: oceanspc
!      integer              :: spc_ndx    ! could be added for selective reading
      character(len=16)    :: species(1)  ! Species name
!      character(len=8)     :: units      ! could be added for units check
      type(trfld), pointer :: fields(:)   ! where the data ends up fields%data
      type(trfile)         :: file 
   end type oceanspc


!-------------------------------------------------------------------------------------



!  List of subroutines that can be accesed from outside module

   public :: oslo_ocean_getnl    ! should this be public. Only used locally...
   public :: oslo_ocean_init     ! initializing, reading file
   public :: oslo_ocean_time     ! time interpolation
   public :: oslo_dms_emis_intr  ! calculate dms surface emissions
   public :: oslo_dms_inq        ! logical function which tells mo_srf_emis what to do
   public :: oslo_opom_emis_intr ! calculate opom surface emissions
   public :: oslo_opom_inq       ! logical function which tells oslo_salt what to do



   private
   save

   type(oceanspc), allocatable :: oceanspcs(:)





!  These variables are settable via the namelist (with longer names)
!  For reading concentration file
   character(len=16)  :: dmsl_fld_name  = 'dms'        !not set from namelist, hard coded, name of nc var
   character(len=16)  :: dmsk_fld_name  = 'dms_Kettle' !not set from namelist, hard coded, name of nc var
   character(len=16)  :: opomo_fld_name = 'chlor_a' !not set from namelist, hard coded, name of nc var
   character(len=16)  :: opomn_fld_name = 'poc'     !not set from namelist, hard coded, name of nc var
   character(len=256) :: filename = ''              !will be collected from NAMELIST
   character(len=256) :: filelist = ''              !not needed?
   character(len=256) :: datapath = ''              !will be collected from NAMELIST
   character(len=32)  :: dms_data_type = 'CYCLICAL' !will be collected from NAMELIST 
   character(len=32)  :: opom_data_type= 'CYCLICAL' !will be collected from NAMELIST 
   logical            :: rmv_file      = .false.    !delete file when finished with it
   integer            :: dms_cycle_yr  = 0          !will be collected from NAMELIST
   integer            :: opom_cycle_yr = 0          !will be collected from NAMELIST
   integer            :: fixed_ymd     = 0          !running one date only?
   integer            :: fixed_tod     = 0          !running one time of day only?

   character(len=20)  :: dms_source    = 'emission_file' !will be collected from NAMELIST 
   character(len=20)  :: opom_source   = 'no_file'       !will be collected from NAMELIST 
   integer            :: n_ocean_species                 !Number of variables read from ocean file
   integer            :: pndx_fdms                       !DMS surface flux physics index

contains
!---------------------------------------------------------------------
!---------------------------------------------------------------------
subroutine oslo_ocean_getnl()
! Read namelist variables. For oslo namelist variables this is done through oslo_getopts

   use oslo_control,     only: oslo_getopts

   implicit none

   ! declaration of variables collected from namelist
   character(len=256) ::  in_filename
   character(len=256) ::  in_datapath
   character(len=20)  ::  in_dms_data_source
   character(len=32)  ::  in_dms_data_type
   integer            ::  in_dms_cycle_yr
   character(len=20)  ::  in_opom_data_source
   character(len=32)  ::  in_opom_data_type
   integer            ::  in_opom_cycle_yr


   ! Initialize namelist variables from local module variables.
   in_filename         = filename
   in_datapath         = datapath
   in_dms_data_type    = dms_data_type
   in_dms_cycle_yr     = dms_cycle_yr
   in_dms_data_source  = dms_source
   in_opom_data_type   = opom_data_type
   in_opom_cycle_yr    = opom_cycle_yr
   in_opom_data_source = opom_source

   ! Read namelist.
   call oslo_getopts(dms_source_out      = in_dms_data_source,  &
                     dms_source_type_out = in_dms_data_type,    &
                     dms_cycle_year_out  = in_dms_cycle_yr,     &
                     opom_source_out     = in_opom_data_source, &
                     opom_source_type_out= in_opom_data_type,   &
                     opom_cycle_year_out = in_opom_cycle_yr,    &
                     ocean_filename_out  = in_filename,         &
                     ocean_filepath_out  = in_datapath)


   ! Update module variables with user settings.
   filename      = in_filename
   datapath      = in_datapath
   dms_data_type = in_dms_data_type
   dms_cycle_yr  = in_dms_cycle_yr
   dms_source    = in_dms_data_source
   opom_data_type= in_opom_data_type
   opom_cycle_yr = in_opom_cycle_yr
   opom_source   = in_opom_data_source

   ! Write new value set from namelist to log
!   write(iulog,*)"test pom namelist 2: " // trim(opom_source)

endsubroutine oslo_ocean_getnl
!---------------------------------------------------------------------
!---------------------------------------------------------------------
subroutine oslo_ocean_init()
! no in parameters all information is local

   use tracer_data,      only : trcdata_init
   use constituents,     only : cnst_get_ind
   use cam_history,      only : addfld, add_default, horiz_only

   implicit none

   integer  :: astat
   integer  :: m
   integer            :: cycle_yr(2) 
   character(len=32)  :: data_type(2) 
   character(len=16)  :: emis_species(2)

   ! Collect and save namelist information in module
   call oslo_ocean_getnl()

   !get physics index for dms surface flux. Index for cflx
   call cnst_get_ind('DMS', pndx_fdms, abort=.true.)

!   write(iulog,*)"test dms p index: " ,pndx_fdms

   if (dms_source=='lana')then
      emis_species(1) = dmsl_fld_name
   else
      emis_species(1) = dmsk_fld_name
   endif
   if (opom_source=='odowd')then
      emis_species(2) = opomo_fld_name
   else
      emis_species(2) = opomn_fld_name
   endif
   cycle_yr(1)= dms_cycle_yr
   cycle_yr(2)= opom_cycle_yr
   data_type(1) = dms_data_type
   data_type(2) = opom_data_type
   n_ocean_species = 2

   if (masterproc) write(iulog,*) 'oslo_dms_inti: n_ocean_species = ',n_ocean_species

   allocate( oceanspcs(n_ocean_species), stat=astat )
   if( astat/= 0 ) then
      write(iulog,*) 'oslo_dms_inti: failed to allocate oceanspcs array; error = ',astat
      call endrun
   end if


   !-----------------------------------------------------------------------
   !   ... setup the oceanspcs type array
   !-----------------------------------------------------------------------
!  Add support for selective reading with saved units etc.? 
   do m=1,n_ocean_species ! one for now... start with dms
!      oceanspcs(m)%spc_ndx          = emis_indexes(m) ! physics index
!      oceanspcs(m)%units            = 'nmol/L' 
      oceanspcs(m)%species          = emis_species(m)  ! nc var name

   enddo

   do m=1,n_ocean_species

      ! Ocean concentrations are not stored in pbuf
      allocate(oceanspcs(m)%file%in_pbuf(1))
      oceanspcs(m)%file%in_pbuf(:) = .false.

      call trcdata_init( oceanspcs(m)%species, &
                      filename, filelist, datapath, &
                      oceanspcs(m)%fields,  &
                      oceanspcs(m)%file, &
                      rmv_file, cycle_yr(m), fixed_ymd, fixed_tod, data_type(m) )

   enddo
!   write(iulog,*) 'oslo_ocean_init: read file '

   call addfld( 'odms', horiz_only,  'A',  'nmol/L', 'DMS upper ocean concentration' )

   call add_default('odms', 1, ' ')

endsubroutine oslo_ocean_init
!------------------------------------------------------------------------------------------
!------------------------------------------------------------------------------------------
subroutine oslo_ocean_time(state, pbuf2d)

    use physics_types,  only : physics_state
    use ppgrid,         only : begchunk, endchunk
    use tracer_data,    only : advance_trcdata
    use physics_buffer, only : physics_buffer_desc

    implicit none

    type(physics_state), intent(in)    :: state(begchunk:endchunk)
    type(physics_buffer_desc), pointer :: pbuf2d(:,:)

    !-----------------------------------------------------------------------
    !       ... local variables
    !-----------------------------------------------------------------------
    integer :: m

    do m = 1,n_ocean_species

       call advance_trcdata( oceanspcs(m)%fields, oceanspcs(m)%file, state, pbuf2d  )
    end do


endsubroutine oslo_ocean_time

!------------------------------------------------------------------------------------------
!------------------------------------------------------------------------------------------

subroutine oslo_dms_emis_intr(state, cam_in)

   use physics_types, only: physics_state
   use constituents,  only: cnst_mw   !molecular weight for physics constituents
   use cam_history,   only: outfld

   type(physics_state),    intent(in)    :: state   ! Physics state variables
   type(cam_in_t), target, intent(inout) :: cam_in  ! import state


   real(r8), dimension(pcols)                   :: u10m       ![m/s]
   real(r8), pointer                            :: ocnfrc(:)  ![frc] ocean fraction
   real(r8), pointer                            :: icefrc(:)  ![frc] ice fraction
   integer                                      :: ncol       ![nbr] number of columns in use
   integer                                      :: lchnk   ! chunk index

   real(r8) :: rk600(pcols)               ! ocean/atmos. DMS exchange factor [cm/hr]
   real(r8) :: flux(pcols)                ! Local flux array: DMS emission rate [kg m-2 s-1]
   real(r8) :: odms(pcols)                ! Ocean dms concentration [nmol/L] from file
   real(r8) :: open_ocn(pcols)            ! Open Ocean 

   real(r8), dimension(pcols):: t,scdms,kwdms

   real(r8), parameter  :: z0= 0.0001_r8 ![m] roughness length over ocean
   real(r8), parameter  :: Xconvxa= 6.97e-07   ! Wanninkhof's a=0.251 converted to ms-1/(ms-1)^2 

   logical, parameter :: method_oslo  =.false.
   logical, parameter :: method_hamocc=.true.

      !pointers to land model variables
      ocnfrc => cam_in%ocnfrac
      icefrc => cam_in%icefrac
   ncol  = state%ncol
   lchnk = state%lchnk 

   ! IF CONCENTRATION FILE
   if (dms_source=='lana' .or. dms_source=='kettle') then

      ! collect dms data from file
      flux(:) = 0._r8
      odms(:) = 0._r8
      odms(:ncol) = oceanspcs(1)%fields(1)%data(:ncol,1,lchnk) 

      ! open ocean
      open_ocn(:ncol) = ocnfrc(:ncol) * (1._r8-icefrc(:ncol))
      !start with midpoint wind speed
      u10m(:ncol)=sqrt(state%u(:ncol,pver)**2+state%v(:ncol,pver)**2)

      if (method_oslo) then  
         ! move the winds to 10m high from the midpoint of the gridbox: 
         u10m (:ncol) = u10m(:ncol)*log(10._r8/z0)/log(state%zm(:ncol,pver)/z0)
         rk600(:ncol) = (0.222_r8*(u10m(:ncol)*u10m(:ncol))) + (0.333_r8*u10m(:ncol))        ! [cm/hr]
         flux (:ncol) = 2.778e-15*cnst_mw(pndx_fdms)*rk600(:ncol)*open_ocn(:ncol)*odms(:ncol) ! [kg m-2 s-1]
      else if (method_hamocc) then  
         t(:ncol)=cam_in%sst(:ncol)-273.15_r8
         u10m (:ncol) = u10m(:ncol)*log(10._r8/z0)/log(state%zm(:ncol,pver)/z0)
         scdms(:ncol) = 2855.7+  (-177.63 + (6.0438 + (-0.11645 + 0.00094743*t(:ncol))*t(:ncol))*t(:ncol))*t(:ncol)
         kwdms(:ncol) = open_ocn(:ncol) * Xconvxa *u10m(:ncol)**2*(660./scdms(:ncol))**0.5 
         flux (:ncol) = 62.13*kwdms(:ncol)*1e-9*odms(:ncol)
      endif
      cam_in%cflx(:ncol, pndx_fdms  )  = flux(:ncol) 

      call outfld('odms', odms(:ncol), ncol, lchnk)

   ! IF OCEAN FLUX
   elseif(dms_source=='ocean_flux') then 
      cam_in%cflx(:ncol, pndx_fdms)  =  cam_in%fdms(:ncol)
   endif

   ! IF EMISSION FILE
   ! return without changing cflx
   ! return?

endsubroutine oslo_dms_emis_intr
!------------------------------------------------------------------------------------------
!------------------------------------------------------------------------------------------

subroutine oslo_opom_emis_intr(em_ss1,em_ss2,em_ss3,open_ocn,ncol,lchnk, opomem_out)



   integer , intent(in)                    :: ncol              ![nbr] number of columns in use
   integer , intent(in)                    :: lchnk             !current chunk
   real(r8), intent(in)                    :: em_ss1(pcols)     !sea salt emission mode a1
   real(r8), intent(in)                    :: em_ss2(pcols)     !sea salt emission mode a2
   real(r8), intent(in)                    :: em_ss3(pcols)     !sea salt emission mode a3
   real(r8), intent(in)                    :: open_ocn(pcols)   !open ocean
   real(r8), intent(out)                   :: opomem_out(pcols) !ocean POM emission rate [kg m-2 s-1]
!   integer :: lchnk   ! chunk index

   real(r8) :: flux(ncol)                          ! Local flux array: ocean POM emission rate [kg m-2 s-1]

   ! Variables for Nilsson parameterisation
   real(r8) :: opoc(ncol)                          ! Ocean POC concentration [mg m-3]
!   real(r8), parameter :: c_n = 0.000288657_r8    ! OM tuning constant (NorESM1 value)
   real(r8), parameter :: c_n = 0.000507456_r8    ! OM tuning constant (Tuned for NorESM2)
   real(r8), parameter :: c_a1 = 2.06_r8           ! OM fraction in a1 mode
   real(r8), parameter :: c_a2 = 0.355_r8          ! OM fraction in a2 mode
   real(r8), parameter :: c_a3 = 0.0623_r8         ! OM fraction in a3 mode

   ! Variables for O'Dowd parameterisation
   real(r8) :: omFrac(ncol)                ! OM fraction of total seaspray mass
   real(r8) :: ochlor(ncol)                ! Ocean chlorophyll concentration [nmol/L]
   real(r8),parameter :: c_o = 0.5238_r8   ! Arbritraty scaling factor to make the emissions match Spracklen.
                                           ! Not consistent with the parameterisation of O'Dowd. Set to 1
                                           ! for original parameterisation. 




   ! Nilsson parameterisation
   if (opom_source=='nilsson') then

     ! collect POC data from file
      flux(:) = 0._r8
      opoc(:) = 0._r8
                                           
      opoc(:ncol) =  oceanspcs(2)%fields(1)%data(:ncol,1,lchnk) 

      flux(:ncol) = c_n*open_ocn(:ncol)*opoc(:ncol)* &
                    (c_a1*em_ss1(:ncol)+c_a2*em_ss2(:ncol)+c_a3*em_ss3(:ncol))

      opomem_out(:ncol) = flux(:ncol)


   ! O'Dowd parameterisation
   elseif (opom_source=='odowd') then

     ! collect dms data from file
      flux(:) = 0._r8
      ochlor(:) = 0._r8
 
      ochlor(:ncol) =  oceanspcs(2)%fields(1)%data(:ncol,1,lchnk)
      ! OM fraction saturates at 90% according to O'Dowd 2008
      omFrac(:ncol) = min(0.01_r8*(43.5_r8 * ochlor(:ncol) + 13.805_r8),0.76_r8)
      omFrac(:ncol) = omFrac(:ncol) / (1._r8 - omFrac(:ncol))
      flux(:ncol)   = c_o*omFrac(:ncol) * em_ss1(:ncol)  
      opomem_out(:ncol) = flux(:ncol)
   endif

   ! return?

endsubroutine oslo_opom_emis_intr
!------------------------------------------------------------------------------------------
!------------------------------------------------------------------------------------------

logical function oslo_dms_inq()
   implicit none

   if (dms_source=='emission_file') then
      oslo_dms_inq = .true.
   else
      oslo_dms_inq = .false.
   endif
   return

end function oslo_dms_inq


!------------------------------------------------------------------------------------------
!------------------------------------------------------------------------------------------

logical function oslo_opom_inq()
   implicit none

   if (opom_source=='nilsson' .or. opom_source=='odowd') then
      oslo_opom_inq = .true.
   else
      oslo_opom_inq = .false.
   endif
   return

end function oslo_opom_inq

!------------------------------------------------------------------------------------------
!------------------------------------------------------------------------------------------

end module oslo_ocean_intr
