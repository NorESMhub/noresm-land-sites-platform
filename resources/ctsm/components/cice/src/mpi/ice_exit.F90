!  SVN:$Id: ice_exit.F90 700 2013-08-15 19:17:39Z eclare $
!=======================================================================
!
! Exit the model. 
! authors William H. Lipscomb (LANL)
!         Elizabeth C. Hunke (LANL)
! 2006 ECH: separated serial and mpi functionality

      module ice_exit

      use ice_kinds_mod

      implicit none
      public

!=======================================================================

      contains

!=======================================================================

      subroutine abort_ice(error_message)

!  This routine aborts the ice model and prints an error message.

#if (defined CESMCOUPLED)
      use ice_fileunits, only: nu_diag, flush_fileunit
      use shr_sys_mod
#else
      use ice_fileunits, only: nu_diag, ice_stderr, flush_fileunit
      include 'mpif.h'   ! MPI Fortran include file
#endif

      character (len=*), intent(in) :: error_message

      ! local variables

#ifndef CESMCOUPLED
      integer (int_kind) :: ierr ! MPI error flag
#endif

#if (defined CESMCOUPLED)
      call flush_fileunit(nu_diag)
      write (nu_diag,*) error_message
      call flush_fileunit(nu_diag)
      call shr_sys_abort(error_message)
#else
      call flush_fileunit(nu_diag)

      write (ice_stderr,*) error_message
      call flush_fileunit(ice_stderr)

      call MPI_ABORT(MPI_COMM_WORLD, ierr)
      stop
#endif

      end subroutine abort_ice

!=======================================================================

      subroutine end_run

! Ends run by calling MPI_FINALIZE.

      integer (int_kind) :: ierr ! MPI error flag

      call MPI_FINALIZE(ierr)

      end subroutine end_run

!=======================================================================

      end module ice_exit

!=======================================================================
