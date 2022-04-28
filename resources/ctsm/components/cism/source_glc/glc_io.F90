!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

 module glc_io

!BOP
! !MODULE: glc_io

! !DESCRIPTION:
!  Contains routines for specialized glc IO
!
! !REVISION HISTORY:
!
! !USES:

   use glc_time_management, only: iyear, imonth, iday, ihour, iminute, isecond, &
                                  runtype, cesm_date_stamp, elapsed_days, elapsed_days0
   use glc_communicate,     only: my_task, master_task
   use glimmer_ncdf,        only: add_output, delete_output, nc_errorhandle, glimmer_nc_output
   use glc_broadcast,       only: broadcast_scalar
   use glimmer_ncio,        only: glimmer_nc_checkwrite, &
                                  glimmer_nc_createfile
   use glimmer_global,      only: fname_length
   use glc_constants
   use glc_kinds_mod
   use esmf,            only: ESMF_Clock
   use seq_timemgr_mod,     only: seq_timemgr_EClockGetData
   use shr_sys_mod
   use shr_kind_mod,        only: CL=>SHR_KIND_CL, CX=>SHR_KIND_CX, &
                                  IN=>SHR_KIND_IN
   use shr_file_mod,        only: shr_file_getunit, shr_file_freeunit
   use netcdf

   implicit none
   private
   save

! !PUBLIC MEMBER FUNCTIONS:

   public :: glc_io_read_restart_time,         &
             glc_io_write_history,             &
             glc_io_write_history_tavg_helper, &
             glc_io_write_restart

! !PRIVATE MEMBER DATA:

   ! Baseline year to use for time units - i.e., the year to use in the string,
   ! 'common_year since YYYY-01-01'. Note that the baseline year here is tied in with the
   ! specification of external_time in the calls to glimmer_nc_checkwrite - so if we use
   ! a baseline year other than 0, we'd need to change how we specify that external_time.
   integer, parameter :: baseline_year = 0

   ! This output structure is accessible to CISM throughout the run, and is
   ! used to accumulate and average the time-average ("tavg") output fields.
   type(glimmer_nc_output), pointer :: oc_tavg_helper => null()

!EOP
!BOC
!EOC
!***********************************************************************
!***********************************************************************

 contains

!***********************************************************************
!BOP
! !IROUTINE: glc_io_read_restart_time
! !INTERFACE:

   subroutine glc_io_read_restart_time(nhour_glad, av_start_time_restart, filename)

    use glc_files, only : ptr_filename

    implicit none
    integer(IN),             intent(out) :: nhour_glad
    integer(IN),             intent(out) :: av_start_time_restart
    character(fname_length), intent(out) :: filename

    ! local variables
    character(fname_length) :: filename0
    integer(IN)             :: rst_elapsed_days  ! 
    integer(IN)             :: ptr_unit          ! unit for pointer file
    integer(IN)             :: rst_unit          ! unit for restart file
    integer(IN)             :: status            !

!-----------------------------------------------------------------------

    if (my_task == master_task) then
       
       ! get restart filename from rpointer file
       ptr_unit = shr_file_getUnit()
       open(ptr_unit,file=ptr_filename)
       read(ptr_unit,'(a)') filename0
       filename = trim(filename0)
       close(ptr_unit)
       write(stdout,*) &
            'glc_io_read_restart_time: using dumpfile for restart = ', filename
       call shr_sys_flush(stdout)
       call shr_file_freeunit(ptr_unit)

       ! read time from the restart file, since CISM needs this to initialize
       rst_unit = shr_file_getUnit()
       status = nf90_open(filename,0,rst_unit)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_get_att(rst_unit, NF90_GLOBAL, 'elapsed_days', rst_elapsed_days)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_get_att(rst_unit, NF90_GLOBAL, 'av_start_time_restart', av_start_time_restart)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_close(rst_unit)
       call nc_errorhandle(__FILE__,__LINE__,status)
    end if

    call broadcast_scalar (filename        , master_task)
    call broadcast_scalar (rst_elapsed_days, master_task)
    call broadcast_scalar (av_start_time_restart, master_task)

    ! calculate nhour_glad for return
    nhour_glad = rst_elapsed_days * 24

  end subroutine glc_io_read_restart_time

!***********************************************************************
!BOP
! !IROUTINE: glc_io_write_history
! !INTERFACE:

  subroutine glc_io_write_history(instance, EClock, history_vars, &
       initial_history, history_frequency_metadata)

    ! Write a CISM history file
    !
    ! If initial_history is present and true, that means that we're writing a history file
    ! in initialization. This uses a different extension than standard history files.
    !
    ! history_frequency_metadata gives the text to use for the time_period_freq global
    ! attribute. It must be present if initial_history is .false.
    !
    use glad_type
    use glide_io, only : glide_io_create, glide_io_write
    use glad_io, only : glad_io_create, glad_io_write

    use glide_nc_custom, only: glide_nc_filldvars

    implicit none

    type(glad_instance) , intent(inout) :: instance
    type(ESMF_Clock)    , intent(in)    :: EClock
    character(len=*)    , intent(in)    :: history_vars
    logical             , intent(in)    :: initial_history       

    ! If present, history_frequency_metadata gives the text to use for the
    ! time_period_freq global attribute. If absent, there will be no time_period_freq
    ! global attribute.
    character(len=*)    , intent(in), optional :: history_frequency_metadata
    
    ! local variables
    type(glimmer_nc_output),  pointer :: oc => null()

    character(len=32) :: file_type
    character(CL) :: filename
    integer(IN)   :: cesmYMD           ! cesm model date
    integer(IN)   :: cesmTOD           ! cesm model sec
    integer(IN)   :: cesmYR            ! cesm model year
    integer(IN)   :: cesmMON           ! cesm model month
    integer(IN)   :: cesmDAY           ! cesm model day
    integer(IN)   :: glcYMD            ! cism model date
    integer(IN)   :: glcTOD            ! cism model sec
    integer(IN)   :: rst_elapsed_days  ! 
    integer(IN)   :: ptr_unit          ! unit for pointer file
    integer(IN)   :: status            !

!-----------------------------------------------------------------------

    ! Error checking on arguments
    if (.not. initial_history) then
       if (.not. present(history_frequency_metadata)) then
          call shr_sys_abort('glc_io_write_history: history_frequency_metadata must be present if initial_history is .false.')
       end if
    end if

    ! figure out history filename
    call seq_timemgr_EClockGetData(EClock, curr_ymd=cesmYMD, curr_tod=cesmTOD, &
                                   curr_yr=cesmYR, curr_mon=cesmMON, curr_day=cesmDAY)
    if (initial_history) then
       file_type = 'initial_history'
    else
       file_type = 'history'
    end if
    filename = glc_filename(cesmYR, cesmMON, cesmDAY, cesmTOD, file_type)

    if (my_task == master_task) then
       write(stdout,*) &
            'glc_io_write_history: calling dumpfile for history filename= ', filename
       call shr_sys_flush(stdout)
    endif

    allocate(oc)
    oc%freq          = 1
    oc%append        = .false.
    oc%default_xtype = NF90_DOUBLE
    oc%nc%filename   = ''
    oc%nc%filename   = trim(filename)
    oc%nc%vars       = trim(history_vars)
    oc%nc%vars_copy  = oc%nc%vars

!jw TO DO: fill out the rest of the metadata
!jw    oc%metadata%title =
!jw    oc%metadata%institution =
!jw    oc%metadata%source =
!jw    oc%metadata%history =
!jw    oc%metadata%references =
!jw    oc%metadata%comment = 

    ! create the output unit
    call glimmer_nc_createfile(oc, instance%model, baseline_year=baseline_year)
    call glide_io_create(oc, instance%model, instance%model)
    call glad_io_create(oc, instance%model, instance)

    if (my_task == master_task) then
       ! write time to the file
       glcYMD = iyear*10000 + imonth*100 + iday
       glcTOD = ihour*3600 + iminute*60 + isecond
       status = nf90_put_att(oc%nc%id, NF90_GLOBAL, 'cesmYMD', cesmYMD)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(oc%nc%id, NF90_GLOBAL, 'cesmTOD', cesmTOD)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(oc%nc%id, NF90_GLOBAL, 'glcYMD', glcYMD)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(oc%nc%id, NF90_GLOBAL, 'glcTOD', glcTOD)
       call nc_errorhandle(__FILE__,__LINE__,status)
       rst_elapsed_days = elapsed_days - elapsed_days0
       status = nf90_put_att(oc%nc%id, NF90_GLOBAL, 'elapsed_days', rst_elapsed_days)
       call nc_errorhandle(__FILE__,__LINE__,status)

       ! The following piece of metadata is needed to follow a CESM convention
       if (present(history_frequency_metadata)) then
          status = nf90_put_att(oc%nc%id, NF90_GLOBAL, 'time_period_freq', &
               history_frequency_metadata)
          call nc_errorhandle(__FILE__,__LINE__,status)
       end if

       ! Another piece of metadata needed to follow a CESM convention
       status = nf90_put_att(oc%nc%id, NF90_GLOBAL, 'model_doi_url', &
            model_doi_url)
       call nc_errorhandle(__FILE__,__LINE__,status)
    end if
    
    call glide_nc_filldvars(oc, instance%model)

    call glimmer_nc_checkwrite(oc, instance%model, forcewrite=.true., &
         time=instance%glide_time, &
         external_time = real(cesmYR, r8))

    ! Copy oc%total_time from oc_tavg_helper, which has been accumulating the total time.
    ! We need this total_time in glide_io_write to do time averaging correctly.
    if (associated(oc_tavg_helper)) then
       oc%total_time = oc_tavg_helper%total_time
    else
       oc%total_time = 0.0d0
    end if

    call glide_io_write(oc, instance%model)
    call glad_io_write(oc, instance)

    if (my_task == master_task) then
       status = nf90_close(oc%nc%id)
       call nc_errorhandle(__FILE__,__LINE__,status)
    end if

    oc => null()

!jw TO DO: figure out why deallocate statement crashes the code
!jw    deallocate(oc)

  end subroutine glc_io_write_history

!***********************************************************************
!BOP
! !IROUTINE: glc_io_write_history_tavg_helper
! !INTERFACE:

  subroutine glc_io_write_history_tavg_helper(instance, history_vars)

    ! Manage an auxiliary output structure that is used to handle time-average ("tavg") fields
    ! in history files.
    !
    ! The tavg fields are accumulated after every ice dynamic timestep. Accumulation works as follows:
    !    * At initialization, we create an auxiliary glimmer_nc_output structure ("oc_tavg_helper") and
    !      let model%funits%out_first point to it.
    !    * Within glad_i_step_gcm, there is a call to glide_io_writeall, which calls glide_avg_accumulate
    !      provided that model%funits%out_first is associated, and has do_averages = .true.
    !      Thus, all tavg fields contained in the model derived type are accumulated,
    !      and oc_tavg_helper%total_time is incremented.
    !    * When it is time to write a CESM history file (in subroutine glc_io_write_history),
    !      we set oc%total_time = oc_tavg_helper%total_time, so the time average is computed correctly.
    !    * Then we pass oc_tavg_helper to glide_avg_reset, zeroing out oc_tavg_helper%total_time and the tavg variables.
    !
    ! Note: Restart files do not contain tavg fields, so this extra structure is not needed for restarts.

    use glad_type
    use glide_io, only : glide_io_create, glide_avg_reset
    use glad_io, only : glad_io_create
    use glide_nc_custom, only: glide_nc_filldvars

    !WHL - debug
    use glimmer_log

    implicit none

    type(glad_instance) , intent(inout) :: instance
    character(len=*)    , intent(in)    :: history_vars

    character(CL) :: filename

!-----------------------------------------------------------------------

    if (associated(oc_tavg_helper)) then

       call write_log('WHL, oc_tavg_helper is already associated; reset the tavg fields')

       ! If tavg fields are present, then reset them now. 
       if (oc_tavg_helper%do_averages) then
          call glide_avg_reset(oc_tavg_helper, instance%model)
          ! Note: Currently Glad has no tavg files, and subroutine glad_avg_reset is not generated.
          !       If this changes, then uncomment the following line and add 'use glad_io' above.
!!       call glad_avg_reset(oc_tavg_helper, instance%model)
       end if

    else

       call write_log('WHL: oc_tavg_helper is not associated; associate now')
       allocate(oc_tavg_helper)

       ! assign a generic filename
       filename = glc_filename(0, 0, 0, 0, 'tavg_helper')

       ! set up a structure that includes all the history vars but will not be written out
       oc_tavg_helper%freq           = 9999999      ! large number such that output will not be written
       oc_tavg_helper%append         = .false.
       oc_tavg_helper%write_init     = .false.
       oc_tavg_helper%default_xtype  = NF90_DOUBLE  ! WHL - same as oc above.  Wondering why this is not NF90_FLOAT
       oc_tavg_helper%nc%filename    = ''
       oc_tavg_helper%nc%filename    = trim(filename)
       oc_tavg_helper%nc%vars        = trim(history_vars)
       oc_tavg_helper%nc%vars_copy   = oc_tavg_helper%nc%vars

       ! create the output unit
       ! Note: With tavg files present, oc_tavg_helper%do_averages is set to .true. in glide_io_create and/or glad_io_create
       call glimmer_nc_createfile(oc_tavg_helper, instance%model, baseline_year=baseline_year)
       call glide_io_create(oc_tavg_helper, instance%model, instance%model)
       call glad_io_create(oc_tavg_helper, instance%model, instance)  !WHL - not sure this is needed
       call glide_nc_filldvars(oc_tavg_helper, instance%model)

       ! Let model%funits%out_first point to this structure.
       ! Then it can be accessed from within subroutine glide_io_writeall during calls from glad_i_tstep_gcm.
       instance%model%funits%out_first => oc_tavg_helper

    end if

  end subroutine glc_io_write_history_tavg_helper


!***********************************************************************
!BOP
! !IROUTINE: glc_io_write_restart
! !INTERFACE:

   subroutine glc_io_write_restart(instance, EClock)

    use glc_files, only : ptr_filename
    use glad_type
    use glide_io, only : glide_io_create, glide_io_write
    use glad_io, only : glad_io_create, glad_io_write
    use glide_nc_custom, only: glide_nc_filldvars
    use glad_main, only : glad_okay_to_restart
    use glad_input_averages, only : get_av_start_time
    implicit none
    type(glad_instance), intent(inout) :: instance
    type(ESMF_Clock),     intent(in)    :: EClock

    ! local variables
    type(glimmer_nc_output),  pointer :: oc => null()
    character(CL) :: filename
    integer(IN)   :: cesmYMD           ! cesm model date
    integer(IN)   :: cesmTOD           ! cesm model sec
    integer(IN)   :: cesmYR            ! cesm model year
    integer(IN)   :: cesmMON           ! cesm model month
    integer(IN)   :: cesmDAY           ! cesm model day
    integer(IN)   :: glcYMD            ! cism model date
    integer(IN)   :: glcTOD            ! cism model sec
    integer(IN)   :: rst_elapsed_days  ! 
    integer(IN)   :: ptr_unit          ! unit for pointer file
    integer(IN)   :: status            !

!-----------------------------------------------------------------------

    if (.not. glad_okay_to_restart(instance)) then
       if (my_task == master_task) then
          write(stdout,*) 'ERROR: Attempt to write a restart file at an invalid time'
          write(stdout,*) 'This can occur if GLC_AVG_PERIOD is shorter than the mass balance time step,'
          write(stdout,*) 'and if you are trying to write a restart file in the middle of a mass balance time step.'
          write(stdout,*) '(This is because CISM does not save the accumulated input fields when you restart'
          write(stdout,*) 'in the middle of a mass balance time step.)'
          write(stdout,*) 'For example, this problem can occur for GLC_AVG_PERIOD=glc_coupling_period,'
          write(stdout,*) 'when the glc coupling period is 1 day, and the mass balance time step is 1 year,'
          write(stdout,*) 'if you try to write a restart file mid-year.'
          write(stdout,*) 'The solution is generally to set GLC_AVG_PERIOD=yearly if you want'
          write(stdout,*) 'to be able to write mid-year restart files.'
       end if
       call shr_sys_abort('glc_io_write_restart: Attempt to write a restart file at an invalid time')
    end if

    ! figure out restart filename
    call seq_timemgr_EClockGetData(EClock, curr_ymd=cesmYMD, curr_tod=cesmTOD, &
                                   curr_yr=cesmYR, curr_mon=cesmMON, curr_day=cesmDAY)
    filename = glc_filename(cesmYR, cesmMON, cesmDAY, cesmTOD, 'restart')

    if (my_task == master_task) then
       write(stdout,*) &
            'glc_io_write_restart: calling dumpfile for restart filename= ', filename
       call shr_sys_flush(stdout)
    endif

    allocate(oc)
    oc%freq          = 1
    oc%append        = .false.
    oc%default_xtype = NF90_DOUBLE
    oc%nc%filename   = ''
    oc%nc%filename   = trim(filename)
    oc%nc%vars       = ' restart '
    oc%nc%vars_copy  = oc%nc%vars
!jw TO DO: fill out the rest of the metadata
!jw    oc%metadata%title =
!jw    oc%metadata%institution =
!jw    oc%metadata%source =
!jw    oc%metadata%history =
!jw    oc%metadata%references =
!jw    oc%metadata%comment = 

    ! create the output unit
    call glimmer_nc_createfile(oc, instance%model, baseline_year=baseline_year)
    call glide_io_create(oc, instance%model, instance%model)
    call glad_io_create(oc, instance%model, instance)

    if (my_task == master_task) then
       ! write time to the file
       glcYMD = iyear*10000 + imonth*100 + iday
       glcTOD = ihour*3600 + iminute*60 + isecond
       status = nf90_put_att(oc%nc%id, NF90_GLOBAL, 'cesmYMD', cesmYMD)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(oc%nc%id, NF90_GLOBAL, 'cesmTOD', cesmTOD)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(oc%nc%id, NF90_GLOBAL, 'glcYMD', glcYMD)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(oc%nc%id, NF90_GLOBAL, 'glcTOD', glcTOD)
       call nc_errorhandle(__FILE__,__LINE__,status)
       rst_elapsed_days = elapsed_days - elapsed_days0
       status = nf90_put_att(oc%nc%id, NF90_GLOBAL, 'elapsed_days', rst_elapsed_days)
       call nc_errorhandle(__FILE__,__LINE__,status)
       status = nf90_put_att(oc%nc%id, NF90_GLOBAL, 'av_start_time_restart', &
            get_av_start_time(instance%glad_inputs))
       call nc_errorhandle(__FILE__,__LINE__,status)
    end if
    
    call glide_nc_filldvars(oc, instance%model)
    call glimmer_nc_checkwrite(oc, instance%model, forcewrite=.true., &
         time=instance%glide_time, &
         external_time = real(cesmYR, r8))
    call glide_io_write(oc, instance%model)
    call glad_io_write(oc, instance)

    if (my_task == master_task) then
       status = nf90_close(oc%nc%id)
       call nc_errorhandle(__FILE__,__LINE__,status)
    end if

    oc => null()
!jw TO DO: figure out why deallocate statement crashes the code
!jw    deallocate(oc)

    ! write pointer to restart file
    if (my_task == master_task) then
       ptr_unit = shr_file_getUnit()
       open(ptr_unit,file=ptr_filename)
       write(ptr_unit,'(a)') filename
       close(ptr_unit)
       call shr_file_freeunit(ptr_unit)
    endif

  end subroutine glc_io_write_restart

!***********************************************************************
! BOP
!
! !ROUTINE: glc_filename
!
! !INTERFACE:
  character(CL) function glc_filename( yr_spec, mon_spec, day_spec, sec_spec, file_type )
!
! !DESCRIPTION: Create a filename from a filename specifier. Interpret filename specifier
! string with:
! %c for case
! %i for instance suffix
! %y for year
! %m for month
! %d for day
! %s for second
! %% for the "%" character
! If the filename specifier has spaces " ", they will be trimmed out
! of the resulting filename.
!
! !USES:
    use glc_time_management, only: runid
    use glc_ensemble       , only: get_inst_suffix
!
! !INPUT/OUTPUT PARAMETERS:
  integer          ,      intent(in) :: yr_spec   ! Simulation year
  integer          ,      intent(in) :: mon_spec  ! Simulation month
  integer          ,      intent(in) :: day_spec  ! Simulation day
  integer          ,      intent(in) :: sec_spec  ! Seconds into current simulation day
  character(len=*) ,      intent(in) :: file_type ! file type: 'history', 'initial_history' or 'restart'
!
! EOP
!
  integer       :: i, n           ! Loop variables
  integer       :: year           ! Simulation year
  integer       :: month          ! Simulation month
  integer       :: day            ! Simulation day
  integer       :: ncsec          ! Seconds into current simulation day
  character(CX) :: string         ! Temporary character string
  character(CL) :: format         ! Format character string
  character(CL) :: filename_spec  ! cism filename specifier

  !---------------------------------------------------------------------------
  ! Determine what the file tpye is and set the filename specifier accordingly
  !---------------------------------------------------------------------------

  filename_spec = ' '
  if (file_type.eq.'history') then
     filename_spec = '%c.cism%i.h.%y-%m-%d-%s'
  else if (file_type.eq.'tavg_helper') then
     filename_spec = '%c.cism%i.tavg_helper.%y-%m-%d-%s'
  else if (file_type.eq.'initial_history') then
     ! Give the initial history file (i.e., the file generated based on the diagnostic
     ! solve in initialization) a different extension so that it isn't picked up by the
     ! CESM test system. (If the test system picks it up, there will sometimes be
     ! failures - e.g., in ERI tests - because this file can be present in one run but
     ! not in another.)
     filename_spec = '%c.cism%i.initial_hist.%y-%m-%d-%s'
  else if (file_type.eq.'restart') then
     filename_spec = '%c.cism%i.r.%y-%m-%d-%s'
  else
     call shr_sys_abort ('glc_filename: file_type specifier is invalid')
  endif

  !-----------------------------------------------------------------
  ! Determine year, month, day and sec to put in filename
  !-----------------------------------------------------------------

 if ( len_trim(filename_spec) == 0 )then
     call shr_sys_abort ('glc_filename: filename specifier is empty')
  end if
  if ( index(trim(filename_spec)," ") /= 0 )then
     call shr_sys_abort ('glc_filename: filename specifier can not contain a space:'//trim(filename_spec))
  end if

  year  = yr_spec
  month = mon_spec
  day   = day_spec
  ncsec = sec_spec

  ! Go through each character in the filename specifier and interpret if special string

  i = 1
  glc_filename = ''
  string = ''
  do while ( i <= len_trim(filename_spec) )
     if ( filename_spec(i:i) == "%" )then
        i = i + 1
        select case( filename_spec(i:i) )
        case( 'c' )   ! runid
           string = trim(runid)
        case( 'i' )   ! instance suffix
           call get_inst_suffix(string)
        case( 'y' )   ! year
           if ( year > 99999   ) then
              format = '(i6.6)'
           else if ( year > 9999    ) then
              format = '(i5.5)'
           else
              format = '(i4.4)'
           end if
           write(string,format) year
        case( 'm' )   ! month
           write(string,'(i2.2)') month
        case( 'd' )   ! day
           write(string,'(i2.2)') day
        case( 's' )   ! second
           write(string,'(i5.5)') ncsec
        case( '%' )   ! percent character
           string = "%"
        case default
           call shr_sys_abort ('glc_filename: Invalid expansion character: '//filename_spec(i:i))
        end select
     else
       n = index( filename_spec(i:), "%" )
        if ( n == 0 ) n = len_trim( filename_spec(i:) ) + 1
        if ( n == 0 ) exit
        string = filename_spec(i:n+i-2)
        i = n + i - 2
     end if
     if ( len_trim(glc_filename) == 0 )then
        glc_filename = trim(string)
     else
        if ( (len_trim(glc_filename)+len_trim(string)) >= CL )then
           call shr_sys_abort ('glc_filename Resultant filename too long')
        end if
        glc_filename = trim(glc_filename) // trim(string)
     end if
     i = i + 1
  end do
  if ( len_trim(glc_filename) == 0 )then
     call shr_sys_abort ('glc_filename: Resulting filename is empty')
  end if

  ! add ".nc" to tail end
  glc_filename = trim(glc_filename) // '.nc'

end function glc_filename

end module glc_io
