      subroutine writenc4d_diameter(   &
       fourDfield    &    !I A four dimensional field
       ,fieldname    &    !I The name of the field
       ,fieldlong    &    !I Long name of the field
       ,fieldunits   &    !I Units of the field
       ,filename     &    !I The name of the file to write to
       ,IM           &    !I Number of longitudes
       ,JM           &    !I Number of latitudes
!x       ,LM           &    !I Number of levels
       ,LM           &    !I Number of size bins
!x       ,MM           &    !I Length of fixed time dimensions (usually set to nmonths=12)
       ,ntimestep    &    !I The timestep to read
       ,longitude &
       ,latitude &
       ,diameter &
       ,clongitude &
       ,clatitude &
       ,cdiameter &
       ,ctime &
       ,longname_longitude &
       ,longname_latitude &
       ,longname_diameter &
       ,longname_time &
       ,unit_longitude &
       ,unit_latitude &
       ,unit_diameter &
       ,unit_time &
      )

! --- Purpose: Open a netCDF file and write 4D field to the file
! --- The field written has the name "fieldname", and the file
! --- has the name "filename". 
! --- The timestep in file is set to the input argument ntimestep

      use netcdf

      implicit none
  
! --- INPUT
      character*(*), intent(in)           :: fieldname  !I Name of field
      character*(*), intent(in)           :: fieldlong  !I Long name of field
      character*(*), intent(in)           :: fieldunits !I Units of field
      character*(*), intent(in)           :: filename   !I Name of netCDFfile
      integer,intent(in)                  :: IM         !I Number of longitudes
      integer,intent(in)                  :: JM         !I Number of latitudes
      integer,intent(in)                  :: LM         !I Number of size bins
!x      integer,intent(in)                  :: MM         !I Length of fixed time dimension
      integer,intent(in)                  :: ntimestep  !I The timestep to be read
!x      real, intent(in)         :: fourDfield(IM,JM,LM,MM) !I Four dimensional field
      real, intent(in)         :: fourDfield(IM,JM,LM) !I Four dimensional field

      real, intent(in) :: longitude(IM)
      real, intent(in) :: latitude(JM)
      real, intent(in) :: diameter(LM)
      character*(*), intent(in) :: clongitude
      character*(*), intent(in) :: clatitude
      character*(*), intent(in) :: cdiameter
      character*(*), intent(in) :: ctime

      character*(*), intent(in) :: longname_longitude
      character*(*), intent(in) :: longname_latitude
      character*(*), intent(in) :: longname_diameter
      character*(*), intent(in) :: longname_time 
      character*(*), intent(in) :: unit_longitude 
      character*(*), intent(in) :: unit_latitude 
      character*(*), intent(in) :: unit_diameter 
      character*(*), intent(in) :: unit_time 

! --- LOCAL
! --- LOCAL NETCDF DIMENSION IDs ETCETERA

      integer                  :: lon_dim_id      !Id for longitude dimension
      integer                  :: lat_dim_id      !Id for latitude dimension
!x    integer                  :: lev_dim_id      !Id for level dimension
      integer                  :: bin_dim_id      !Id for level dimension
!x      integer                  :: month_dim_id    !Id for calendar month dimension
      integer                  :: time_dim_id     !Id for time dimension
      integer                  :: field_id        !Variable id for field
      integer                  :: file_id         !NetCDF file identifier
      integer                  :: status          !Error return code

      integer                  :: lon_var_id
      integer                  :: lat_var_id
      integer                  :: bin_var_id 
      integer                  :: time_var_id

      integer                  :: time_array(12)

! --- **********START CODE************************

!x      write(6,*) 'startnc ',im,jm,lm,mm,ntimestep,filename
      write(6,*) 'startnc ',im,jm,lm,ntimestep,filename

! --------------------------------
! --- Define NetCDF grid files ---
! --------------------------------

! --- Create file if ntimestep=1, append to file if ntimestep>1 
      if (ntimestep.eq.1) then          
        call chkerr(nf90_create(trim(filename), nf90_clobber, file_id))
      else 
        call chkerr(nf90_open(trim(filename), nf90_write, file_id)) 
      end if 

! --- Create dimensions
      if (ntimestep.eq.1) then          
        call chkerr(nf90_def_dim(file_id, 'lon', IM, lon_dim_id))   
        call chkerr(nf90_def_dim(file_id, 'lat', JM, lat_dim_id))   
!x        call chkerr(nf90_def_dim(file_id, 'lev', LM, lev_dim_id))   
        call chkerr(nf90_def_dim(file_id, 'particle_diameter',      &
     &  LM, bin_dim_id))   
!x        call chkerr(nf90_def_dim(file_id, 'month', MM, month_dim_id))   
!x        call chkerr(nf90_def_dim(file_id, 'year', NF90_UNLIMITED,     &
!        call chkerr(nf90_def_dim(file_id, 'time', NF90_UNLIMITED,     &
        call chkerr(nf90_def_dim(file_id, 'time', 12,     &
     &    time_dim_id))   
      else 
        call chkerr(nf90_inq_dimid(file_id, 'lon', lon_dim_id))   
        call chkerr(nf90_inq_dimid(file_id, 'lat', lat_dim_id))   
!x        call chkerr(nf90_inq_dimid(file_id, 'lev', lev_dim_id))   
        call chkerr(nf90_inq_dimid(file_id, 'particle_diameter',  & 
     &     bin_dim_id))   
!x        call chkerr(nf90_inq_dimid(file_id, 'month', month_dim_id))   
!x        call chkerr(nf90_inq_dimid(file_id, 'year', time_dim_id))   
        call chkerr(nf90_inq_dimid(file_id, 'time', time_dim_id))   
      endif 

! --- Define variable and attributes 
      if (ntimestep.eq.1) then          
        call chkerr(nf90_def_var(file_id, trim(fieldname), NF90_FLOAT,  &
!x     &    (/lon_dim_id, lat_dim_id, lev_dim_id, month_dim_id,           &
!x     &    time_dim_id/), field_id))  
!x     &    (/lon_dim_id, lat_dim_id, lev_dim_id,                         &
     &    (/lon_dim_id, lat_dim_id, bin_dim_id,                         &
     &    time_dim_id/), field_id))  
        call chkerr(nf90_put_att(file_id,field_id,'long_name',          &
     &    trim(fieldlong)))
        call chkerr(nf90_put_att(file_id,field_id,'units',              &
     &    trim(fieldunits)))
! define longitude
        call chkerr(nf90_def_var(file_id, trim(clongitude), NF90_FLOAT,  &
          (/lon_dim_id/), lon_var_id))
        call chkerr(nf90_put_att(file_id,lon_var_id,'long_name',          &
     &    trim(longname_longitude)))
        call chkerr(nf90_put_att(file_id,lon_var_id,'units',              &
     &    trim(unit_longitude)))
! define latitude
        call chkerr(nf90_def_var(file_id, trim(clatitude), NF90_FLOAT,  &
          (/lat_dim_id/), lat_var_id))
        call chkerr(nf90_put_att(file_id,lat_var_id,'long_name',          &
     &    trim(longname_latitude)))
        call chkerr(nf90_put_att(file_id,lat_var_id,'units',              &
     &    trim(unit_latitude)))
! define diameter
        call chkerr(nf90_def_var(file_id, trim(cdiameter), NF90_FLOAT,  &
          (/bin_dim_id/), bin_var_id))
        call chkerr(nf90_put_att(file_id,bin_var_id,'long_name',          &
     &    trim(longname_diameter)))
        call chkerr(nf90_put_att(file_id,bin_var_id,'units',              &
     &    trim(unit_diameter)))
! define time
        call chkerr(nf90_def_var(file_id, trim(ctime), NF90_INT,  &
          (/time_dim_id/), time_var_id))
        call chkerr(nf90_put_att(file_id,time_var_id,'long_name',          &
     &    trim(longname_time)))
        call chkerr(nf90_put_att(file_id,time_var_id,'units',              &
     &    trim(unit_time)))

        call chkerr(nf90_enddef(file_id))
! write longitude
      call chkerr(nf90_put_var(file_id, lon_var_id, longitude,           &
     &  (/1/),(/IM/)))
! write latitude
      call chkerr(nf90_put_var(file_id, lat_var_id, latitude,           &
     &  (/1/),(/JM/)))
! write diameter
      call chkerr(nf90_put_var(file_id, bin_var_id, diameter,           &
     &  (/1/),(/LM/)))
! write time
      time_array(:)=(/1,2,3,4,5,6,7,8,9,10,11,12/)
      call chkerr(nf90_put_var(file_id, time_var_id, time_array,        &
     &  (/1/),(/12/)))

      else 
        call chkerr(nf90_inq_varid(file_id, trim(fieldname),field_id))
      end if
   
! --- Write data 
!x      call chkerr(nf90_put_var(file_id, field_id, fourDfield,           &
!x     &  (/1, 1, 1, 1, ntimestep/),(/IM, JM, LM, MM, 1/)))       
      call chkerr(nf90_put_var(file_id, field_id, fourDfield,           &
     &  (/1, 1, 1, ntimestep/),(/IM, JM, LM, 1/)))       

!       if ( ntimestep .eq. 1 ) then
!       time_array(1) = ntimestep
!       call chkerr(nf90_put_var(file_id, time_var_id, time_array,   &
!     &     (/ntimestep/),(/0/)))
!       endif


! --- Close file 
      call chkerr(nf90_close(file_id))


      return
      end subroutine writenc4d_diameter
