!  SVN:$Id: ice_firstyear.F90 806 2014-07-31 19:00:00Z tcraig $
!=======================================================================
!
! First year concentration tracer for sea ice
!
! see 
! Armour, K. C., C. M. Bitz, L. Thompson and E. C. Hunke (2011). Controls
! on Arctic sea ice from first-year and multi-year ice survivability.
! J. Climate, 24, 23782390. doi: 10.1175/2010JCLI3823.1.
!
! authors C. Bitz, University of Washington, modified from ice_age module
!
! 2012: E. Hunke adopted from CESM into CICE, changed name from ice_FY.F90
!
      module ice_firstyear

      use ice_kinds_mod
      use ice_constants

      implicit none

      private
      public :: init_FY, update_FYarea, write_restart_FY, read_restart_FY

      logical (kind=log_kind), public :: & 
         restart_FY      ! if .true., read FY tracer restart file

!=======================================================================

      contains

!=======================================================================

!  Initialize ice FY tracer (call prior to reading restart data)

      subroutine init_FY(nx_block, ny_block, ncat, firstyear)

        integer(kind=int_kind), intent(in) :: &
             nx_block , &
             ny_block , &
             ncat

        real(kind=dbl_kind), dimension(nx_block,ny_block,ncat), &
             intent(out) :: firstyear

        firstyear(:,:,:) = c0

      end subroutine init_FY

!=======================================================================

!  Zero ice FY tracer on fixed day of year. Zeroing FY ice tracer promotes
!  ice to MY ice. Unfortunately some frazil ice may grow before the 
!  zeroing date and thus get promoted to MY ice too soon.
!  Bummer.

      subroutine update_FYarea (nx_block, ny_block, &
                                dt,       icells,   &
                                indxi,    indxj,    &
                                nhmask,   shmask,   &
                                yday,     FYarea)

      integer (kind=int_kind), intent(in) :: &
         nx_block, ny_block, & ! block dimensions
         icells                ! number of cells with ice present

      integer (kind=int_kind), dimension (nx_block*ny_block), &
         intent(in) :: &
         indxi, indxj          ! compressed indices for cells with ice

      real (kind=dbl_kind), intent(in) :: &
         dt , &                ! time step
         yday                  ! day of the year

      logical (kind=log_kind), dimension(nx_block,ny_block), &
         intent(in) :: &
         nhmask, shmask

      real (kind=dbl_kind), dimension(nx_block,ny_block), &
         intent(inout) :: &
         FYarea

      ! local variables

      integer (kind=int_kind) :: i, j, ij

      if ((yday >= 259._dbl_kind) .and. &
           (yday <  259._dbl_kind+dt/secday)) then
        do ij = 1, icells
           i = indxi(ij)
           j = indxj(ij)
           if (nhmask(i,j)) FYarea(i,j) = c0;
        enddo
      endif

      if ((yday >= 75._dbl_kind) .and. &
           (yday <  75._dbl_kind+dt/secday)) then
        do ij = 1, icells
           i = indxi(ij)
           j = indxj(ij)
           if (shmask(i,j)) FYarea(i,j) = c0;
        enddo
      endif

      end subroutine update_FYarea

!=======================================================================

! Dumps all values needed for restarting
! author Elizabeth C. Hunke, LANL

      subroutine write_restart_FY()

      use ice_communicate, only: my_task, master_task
      use ice_domain_size, only: ncat
      use ice_fileunits, only: nu_diag, nu_dump_FY
      use ice_flux, only: frz_onset
      use ice_state, only: trcrn, nt_FY
      use ice_restart, only: write_restart_field

      ! local variables

      logical (kind=log_kind) :: diag

      diag = .true.

      !-----------------------------------------------------------------

      call write_restart_field(nu_dump_FY,0,trcrn(:,:,nt_FY,:,:),'ruf8', &
                               'FY',ncat,diag)
      call write_restart_field(nu_dump_FY,0,frz_onset,'ruf8', &
                               'frz_onset',1,diag)

      end subroutine write_restart_FY

!=======================================================================

! Reads all values needed for an ice FY restart
! author Elizabeth C. Hunke, LANL

      subroutine read_restart_FY()

      use ice_communicate, only: my_task, master_task
      use ice_domain_size, only: ncat
      use ice_fileunits, only: nu_diag, nu_restart_FY
      use ice_flux, only: frz_onset
      use ice_state, only: trcrn, nt_FY
      use ice_restart, only: read_restart_field

      ! local variables

      logical (kind=log_kind) :: &
         diag

      diag = .true.

      if (my_task == master_task) write(nu_diag,*) 'min/max first-year ice area'

      call read_restart_field(nu_restart_FY,0,trcrn(:,:,nt_FY,:,:),'ruf8', &
                      'FY',ncat,diag,field_loc_center,field_type_scalar)

      if (my_task == master_task) write(nu_diag,*) 'min/max frz_onset'

      call read_restart_field(nu_restart_FY,0,frz_onset,'ruf8', &
                  'frz_onset',1,diag,field_loc_center,field_type_scalar)

      end subroutine read_restart_FY

!=======================================================================

      end module ice_firstyear

!=======================================================================
