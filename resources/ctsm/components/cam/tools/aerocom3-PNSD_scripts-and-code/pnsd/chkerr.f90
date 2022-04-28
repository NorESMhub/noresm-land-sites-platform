      subroutine chkerr(status)
      use netcdf
      implicit none
      integer status
      if (status.ne.nf90_noerr) then
        write(*,*) trim(nf90_strerror(status))
        stop
      end if
      end subroutine chkerr

