!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

 module glc_RunMod

!BOP
! !MODULE: glc_RunMod

! !DESCRIPTION:
!  Contains the routine for stepping the model forward one timestep
!
! !REVISION HISTORY:
!  SVN:$Id: step_mod.F90 2019 2006-09-29 22:00:15Z njn01 $
!  Adapted by William Lipscomb from step_mod.F90 in POP 2.0 and from 
!   glint_example.F90 in CISM
!
! !USES:

   use glc_kinds_mod
   use glc_time_management, only:  thour, time_manager, check_time_flag, init_time_flag
   use shr_sys_mod
   use glc_communicate, only: my_task, master_task
   use glc_constants, only: verbose, stdout, glc_smb, test_coupling
   use glc_exit_mod, only : exit_glc, sigAbort
   
   implicit none
   private
   save

! !PUBLIC MEMBER FUNCTIONS:

   public :: glc_run

! !PRIVATE MEMBER FUNCTIONS:

   private :: test_coupling_adjust_rofi

!----------------------------------------------------------------------
!
!   module variables
!
!----------------------------------------------------------------------

   integer (i4) ::   &
      cpl_stop_now      ,&! flag id for stop_now flag
      tavg_flag           ! flag to access tavg frequencies

!EOP
!BOC
!EOC
!***********************************************************************

 contains

!***********************************************************************
!BOP
! !IROUTINE: glc_run
! !INTERFACE:

 subroutine glc_run(EClock, valid_inputs)

! !DESCRIPTION:
!  This routine advances the simulation one timestep.
!
! !REVISION HISTORY:
!  same as module

! !USES:

   use glad_main
   use glimmer_log
   use glc_fields 
   use glc_history, only : glc_history_write
   use esmf, only : ESMF_Clock

! !ARGUMENTS:
   type(ESMF_Clock), intent(in) :: EClock
   logical         , intent(in) :: valid_inputs      
   
!EOP
!BOC
!-----------------------------------------------------------------------
!
!  local or common variables:
!
!-----------------------------------------------------------------------

   logical, save :: first_call = .true.        ! flag for initializing timers

  character(fname_length) ::  & 
     paramfile     ! Name of the top-level configuration file
 
  ! Scalars which hold information about the global grid
 
  integer (i4) ::  &
     nx,ny          ! Size of global glc_grid 

  ! Scalar model outputs
 
  ! Other variables

  integer (i4) :: instance_index
 
  !TODO - Remove?  Currently not used
  logical ::  &
     ice_tstep    ,&! true if ice timestep was done
     outflag        ! output flag

  integer (i4) ::  & 
     i,j,n          ! indices 

!-----------------------------------------------------------------------
!  things to do on first call
!-----------------------------------------------------------------------

   if (first_call) then
      ! this line should set cpl_stop_now = 1 (flag id index)
      cpl_stop_now  = init_time_flag('stop_now',default=.false.)
      tavg_flag     = init_time_flag('tavg')      
      first_call = .false.
   endif

!-----------------------------------------------------------------------
!
!  Take one GLAD time step 
!  Note: For SMB scheme, tsfc = ground surface temperature (Celsius)
!                        qsmb = flux of new glacier ice (kg/m^2s)
!
!        For PDD scheme, tsfc = 2m reference temperature (Celsius)
!                        qsmb = precipitation (kg/m^2/s)
!-----------------------------------------------------------------------

     if (glc_smb) then

         if (verbose .and. my_task==master_task) then 
            write(stdout,*) ' '
            write(stdout,*) 'Call glad, thour =', thour
            write(stdout,*) ' '
         endif

         ! TODO(wjs, 2015-03-23) We will need a loop over instances, either here or
         ! around the call to glc_run
         instance_index = 1
         
         call glad_gcm (params = ice_sheet, &
                        instance_index = instance_index,               &
                        time = nint(thour),                            &
                        qsmb = qsmb, tsfc = tsfc,                      &
                        ice_covered = ice_covered, topo = topo,        &
                        rofi = rofi, rofl = rofl, hflx = hflx,         &
                        ice_sheet_grid_mask=ice_sheet_grid_mask,       &
                        valid_inputs=valid_inputs,                     &
                        ice_tstep = ice_tstep)

         if (test_coupling) then
            call test_coupling_adjust_rofi(rofi = rofi, &
                 ice_sheet_instance = ice_sheet%instances(instance_index))
         end if

     else    ! use PDD scheme

!TODO - Implement and test PDD option
        call exit_glc(sigAbort, 'ERROR: attempt to use PDD scheme, which has not been implemented')

     endif   ! glc_smb

!-----------------------------------------------------------------------
!
!  update timestep counter, set corresponding model time, set
!  time-dependent logical switches to determine program flow.
!
!-----------------------------------------------------------------------

   call time_manager

   if (verbose .and. my_task==master_task) then
      write(stdout,*) 'Called time manager: new hour =', thour 
   endif

   !-----------------------------------------------------------------------
   ! Write a history file if it's time to do so
   !-----------------------------------------------------------------------

   ! TODO loop over instances
   call glc_history_write(ice_sheet%instances(1), EClock)
   
!-----------------------------------------------------------------------
!EOC

   end subroutine glc_run

!***********************************************************************
!BOP
! !IROUTINE: test_coupling_adjust_rofi
! !INTERFACE:

   subroutine test_coupling_adjust_rofi(rofi, ice_sheet_instance)
     ! !DESCRIPTION:
     !
     ! This routine adjusts the ice runoff field (rofi) if we're running with
     ! test_coupling = .true.
     !
     ! This is a bit of a hack. When we're running with test_coupling, we can get
     ! too-large ice runoff fluxes in the first coupling interval (when CISM sloughs off
     ! excess ice). This can cause CICE to blow up.
     !
     ! In a typical run, the large ice runoff fluxes from the first dynamic time step
     ! would be averaged with near-zero fluxes from the remaining dynamic time steps in
     ! the first coupling interval, leading to approximately an order of magnitude
     ! smaller fluxes. Thus, to avoid crashing test_coupling runs, we adjust the rofi
     ! flux by a factor that accounts for the fact that, with test_coupling, we aren't
     ! averaging in the 0 values from (n-1) dynamic time steps in the first coupling
     ! interval (where n = 1/dt - e.g., if dt = 0.1 yr, then n = 10).
     !
     ! We should only have to do this adjustment in the first coupling interval of a run
     ! that starts from non-spun-up initial conditions. But to avoid the complexity that
     ! would be needed for that logic to restart correctly in various situations (restart,
     ! branch, hybrid runs), we simply always do this adjustment. This means that rofi
     ! will be lower than normal for test_coupling runs, but since this is only meant for
     ! software testing, this should be okay.

     ! !USES:

     use glad_type, only : glad_instance

     ! !ARGUMENTS:
     real(r8), intent(inout) :: rofi(:,:)
     type(glad_instance), intent(in) :: ice_sheet_instance

     !EOP
     !BOC

     ! Local variables:
     real(r8) :: tinc ! ice sheet time step (yrs)
     !-----------------------------------------------------------------------
     
     tinc = ice_sheet_instance%model%numerics%tinc

     ! This multiplication by tinc (ice sheet time step) is basically saying: let's
     ! assume that the given flux appeared in one time step of the year, with the other
     ! time steps of the year having a flux of 0. See subroutine description above for
     ! more rationale.
     rofi(:,:) = rofi(:,:) * tinc

     !EOC
   end subroutine test_coupling_adjust_rofi

!***********************************************************************

 end module glc_RunMod

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
