 
       subroutine handle_err(status)
      use netcdf
      integer, intent(in)   :: status  !Error status
      if(status/=nf90_noerr)then
         print *, trim(nf90_strerror(status))
         stop "stopped"
      endif
       end subroutine handle_err
