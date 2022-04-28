!  SVN:$Id: ice_age.F90 806 2014-07-31 19:00:00Z tcraig $
!=======================================================================
!
! authors Elizabeth Hunke

      module ice_age

      use ice_kinds_mod
      use ice_constants

      implicit none

      private
      public :: init_age, increment_age, write_restart_age, read_restart_age

      logical (kind=log_kind), public :: & 
         restart_age      ! if .true., read age tracer restart file

!=======================================================================

      contains

!=======================================================================

!  Initialize ice age tracer (call prior to reading restart data)

      subroutine init_age(nx_block, ny_block, ncat, iage)

      integer(kind=int_kind), intent(in) :: &
             nx_block , &
             ny_block , &
             ncat

      real(kind=dbl_kind), dimension(nx_block,ny_block,ncat), &
             intent(out) :: iage

      iage(:,:,:) = c0

      end subroutine init_age

!=======================================================================

!  Increase ice age tracer by timestep length.

      subroutine increment_age (nx_block, ny_block, &
                                dt,       icells,   &
                                indxi,    indxj,    &
                                iage)

      integer (kind=int_kind), intent(in) :: &
         nx_block, ny_block, & ! block dimensions
         icells                ! number of cells with ice present

      integer (kind=int_kind), dimension (nx_block*ny_block), &
         intent(in) :: &
         indxi, indxj     ! compressed indices for cells with ice

      real (kind=dbl_kind), intent(in) :: &
         dt                    ! time step

      real (kind=dbl_kind), dimension(nx_block,ny_block), &
         intent(inout) :: &
         iage

      !  local variables

      integer (kind=int_kind) :: i, j, ij

      do ij = 1, icells
         i = indxi(ij)
         j = indxj(ij)
         iage(i,j) = iage(i,j) + dt 
      enddo

      end subroutine increment_age

!=======================================================================

! Dumps all values needed for restarting
! author Elizabeth C. Hunke, LANL

      subroutine write_restart_age()

      use ice_communicate, only: my_task, master_task
      use ice_domain_size, only: ncat
      use ice_fileunits, only: nu_diag, nu_dump_age
      use ice_state, only: trcrn, nt_iage
      use ice_restart,only: write_restart_field

      ! local variables

      logical (kind=log_kind) :: diag

      diag = .true.

      !-----------------------------------------------------------------

      call write_restart_field(nu_dump_age,0,trcrn(:,:,nt_iage,:,:),'ruf8', &
                               'iage',ncat,diag)

      end subroutine write_restart_age

!=======================================================================

! Reads all values needed for an ice age restart
! author Elizabeth C. Hunke, LANL

      subroutine read_restart_age()

      use ice_communicate, only: my_task, master_task
      use ice_domain_size, only: ncat
      use ice_fileunits, only: nu_diag, nu_restart_age
      use ice_state, only: trcrn, nt_iage
      use ice_restart,only: read_restart_field

      ! local variables

      logical (kind=log_kind) :: &
         diag

      diag = .true.

      if (my_task == master_task) write(nu_diag,*) 'min/max age (s)'

      call read_restart_field(nu_restart_age,0,trcrn(:,:,nt_iage,:,:),'ruf8', &
                       'iage',ncat,diag,field_loc_center,field_type_scalar)

      end subroutine read_restart_age

!=======================================================================

      end module ice_age

!=======================================================================
