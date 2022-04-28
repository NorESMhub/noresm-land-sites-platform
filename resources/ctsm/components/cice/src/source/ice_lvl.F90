!  SVN:$Id: ice_lvl.F90 806 2014-07-31 19:00:00Z tcraig $
!=======================================================================

! Ridged ice tracers for sea ice
!
! authors Elizabeth Hunke

      module ice_lvl

      use ice_kinds_mod

      implicit none

      private
      public :: init_lvl, write_restart_lvl, read_restart_lvl

      logical (kind=log_kind), public :: & 
         restart_lvl      ! if .true., read lvl tracer restart file

!=======================================================================

      contains

!=======================================================================

!  Initialize ice lvl tracers (call prior to reading restart data)

      subroutine init_lvl(nx_block, ny_block, ncat, alvl, vlvl) 

      use ice_constants, only: c1

      integer(kind=int_kind), intent(in) :: &
             nx_block , &
             ny_block , &
             ncat

      real(kind=dbl_kind), dimension(nx_block,ny_block,ncat), &
             intent(out) :: &
             alvl , & ! level ice area fraction
             vlvl     ! level ice volume

      alvl(:,:,:) = c1 ! level ice area fraction
      vlvl(:,:,:) = c1 ! level ice volume

      end subroutine init_lvl

!=======================================================================

! Dumps all values needed for restarting
!
! author Elizabeth C. Hunke, LANL

      subroutine write_restart_lvl()

      use ice_communicate, only: my_task, master_task
      use ice_domain_size, only: ncat
      use ice_fileunits, only: nu_diag, nu_dump_lvl
      use ice_state, only: nt_alvl, nt_vlvl, trcrn
      use ice_restart, only: write_restart_field

      ! local variables

      logical (kind=log_kind) :: diag

      diag = .true.

      !-----------------------------------------------------------------

      call write_restart_field(nu_dump_lvl,0,trcrn(:,:,nt_alvl,:,:),'ruf8', &
                               'alvl',ncat,diag)
      call write_restart_field(nu_dump_lvl,0,trcrn(:,:,nt_vlvl,:,:),'ruf8', &
                               'vlvl',ncat,diag)

      end subroutine write_restart_lvl

!=======================================================================
! Reads all values needed for an ice lvl restart
!
! author Elizabeth C. Hunke, LANL

      subroutine read_restart_lvl()

      use ice_communicate, only: my_task, master_task
      use ice_constants, only: field_loc_center, field_type_scalar
      use ice_domain_size, only: ncat
      use ice_fileunits, only: nu_diag, nu_restart_lvl
      use ice_state, only: nt_alvl, nt_vlvl, trcrn
      use ice_restart, only: read_restart_field

      ! local variables

      logical (kind=log_kind) :: &
         diag

      diag = .true.

      if (my_task == master_task) write(nu_diag,*) 'min/max level ice area, volume'

      call read_restart_field(nu_restart_lvl,0,trcrn(:,:,nt_alvl,:,:),'ruf8', &
                       'alvl',ncat,diag,field_loc_center,field_type_scalar)
      call read_restart_field(nu_restart_lvl,0,trcrn(:,:,nt_vlvl,:,:),'ruf8', &
                       'vlvl',ncat,diag,field_loc_center,field_type_scalar)

      end subroutine read_restart_lvl

!=======================================================================

      end module ice_lvl

!=======================================================================
