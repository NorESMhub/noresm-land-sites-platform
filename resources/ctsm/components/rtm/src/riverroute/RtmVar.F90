module RtmVar

  use shr_kind_mod , only : r8 => shr_kind_r8, CL => SHR_KIND_CL
  use shr_const_mod, only : SHR_CONST_CDAY,SHR_CONST_REARTH
  use shr_sys_mod  , only : shr_sys_abort
  use RtmSpmd      , only : masterproc

  implicit none

  !TODO - nt_rtm and rtm_tracers need to be removed and set by access to the index array
  integer, parameter, public :: nt_rtm = 2    ! number of tracers
  character(len=3), parameter, public :: rtm_tracers(nt_rtm) =  (/'LIQ','ICE'/)

  ! Constants
  integer, parameter, private ::  iundef = -9999999
  integer, parameter, private ::  rundef = -9999999._r8

  real(r8), public, parameter :: secspday = SHR_CONST_CDAY     ! Seconds per day
  integer,  public, parameter :: isecspday= secspday           ! Integer seconds per day
  real(r8), public, parameter :: spval    = 1.e36_r8           ! special value for real data
  integer , public, parameter :: ispval   = -9999              ! special value for int data
  real(r8) :: re = SHR_CONST_REARTH*0.001_r8                   ! radius of earth (km)

  ! Run control variables
  character(len=CL), public :: caseid  = ' '                   ! case id
  character(len=CL), public :: ctitle  = ' '                   ! case title
  integer, public, parameter :: nsrStartup  = 0                ! Startup from initial conditions
  integer, public, parameter :: nsrContinue = 1                ! Continue from restart files
  integer, public, parameter :: nsrBranch   = 2                ! Branch from restart files
  integer, public :: nsrest = iundef                           ! Type of run
  logical, public :: brnch_retain_casename = .false.           ! true => allow case name to remain the same for branch run
                                                               ! by default this is not allowed
  logical, public :: noland = .false.                          ! true => no valid land points -- do NOT run
  character(len=CL), public :: hostname = ' '                  ! Hostname of machine running on
  character(len=CL), public :: username = ' '                  ! username of user running program
  character(len=CL), public :: version  = " "                  ! version of program
  character(len=CL), public :: conventions = "CF-1.0"          ! dataset conventions
  character(len=CL), public :: source   = "River Transport Model RTM1.0" ! description of this source
  character(len=CL), public :: model_doi_url                   ! Web address of the Digital Object Identifier (DOI) for this model version

  ! Unit Numbers
  integer, public :: iulog = 6        ! "stdout" log file unit number, default is 6

  ! Instance control
  integer, public :: inst_index
  character(len=16), public :: inst_name
  character(len=16), public :: inst_suffix

  ! Rtm control variables
  character(len=CL), public :: nrevsn_rtm   = ' '       ! restart data file name for branch run
  character(len=CL), public :: finidat_rtm  = ' '       ! initial conditions file name
  character(len=CL), public :: frivinp_rtm  = ' '       ! RTM input data file name
  logical,           public :: ice_runoff = .true.      ! true => runoff is split into liquid and ice, 
                                                        ! otherwise just liquid
  logical,           public :: rtm_active    = .true.   ! true => rtm on
  logical,           public :: flood_active  = .false.  ! true => flood on
  logical,           public :: effvel_active = .false.  ! true => calculate eff. velocity from rdirc file
 
  ! Rtm grid size
  integer :: rtmlon = 1 ! number of rtm longitudes (initialize)
  integer :: rtmlat = 1 ! number of rtm latitudes  (initialize)

  character(len=CL), public :: rpntfil = 'rpointer.rof' ! file name for local restart pointer file

  logical, private :: RtmVar_isset = .false.

contains


!================================================================================

  subroutine RtmVarSet( caseid_in, ctitle_in, brnch_retain_casename_in,    &
                        nsrest_in, version_in, hostname_in, username_in,   &
                        model_doi_url_in )

    !-----------------------------------------------------------------------
    !  Set input control variables.
    !
    ! !ARGUMENTS:
    character(len=CL), optional, intent(IN) :: caseid_in                ! case id
    character(len=CL), optional, intent(IN) :: ctitle_in                ! case title
    integer          , optional, intent(IN) :: nsrest_in                ! 0: initial run. 1: restart: 3: branch
    character(len=CL), optional, intent(IN) :: version_in               ! model version
    character(len=CL), optional, intent(IN) :: hostname_in              ! hostname running on
    character(len=CL), optional, intent(IN) :: username_in              ! username running job
    character(len=CL), optional, intent(IN) :: model_doi_url_in         ! web address of Digital Object Identifier (DOI) for model version
    logical          , optional, intent(IN) :: brnch_retain_casename_in ! true => allow case name to
    !-----------------------------------------------------------------------

    if ( RtmVar_isset )then
       call shr_sys_abort( 'RtmVarSet ERROR:: control variables already set -- EXIT' )
    end if

    if (present(caseid_in)) caseid = caseid_in
    if (present(ctitle_in)) ctitle = ctitle_in
    if (present(nsrest_in)) nsrest = nsrest_in
    if (present(version_in)) version = version_in
    if (present(username_in)) username = username_in
    if (present(hostname_in)) hostname = hostname_in
    if (present(model_doi_url_in)) model_doi_url = model_doi_url_in
    if (present(brnch_retain_casename_in)) brnch_retain_casename = brnch_retain_casename_in

  end subroutine RtmVarSet

!================================================================================

  subroutine RtmVarInit( )
    if (masterproc) then
       if (nsrest == iundef) then
          call shr_sys_abort( 'RtmVarInit ERROR:: must set nsrest' )
       end if
       if (nsrest == nsrBranch .and. nrevsn_rtm == ' ') then
          call shr_sys_abort( 'RtmVarInit ERROR: need to set restart data file name' )
       end if
       if (nsrest == nsrStartup ) then
          nrevsn_rtm = ' '
       end if
       if (nsrest == nsrContinue) then
          nrevsn_rtm = 'set by restart pointer file file'
       end if
       if (nsrest /= nsrStartup .and. nsrest /= nsrContinue .and. nsrest /= nsrBranch ) then
          call shr_sys_abort( 'RtmVarInit ERROR: nsrest NOT set to a valid value' )
       end if
    endif   
    RtmVar_isset = .true.
  end subroutine RtmVarInit

end module RtmVar
