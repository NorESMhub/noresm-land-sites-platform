subroutine readnclatlon(   &
     oneDfield       &  !O A three dimensional field
     ,fieldname      &  !I The name of the field
     ,filename       &  !I The name of the file to read from
     ,IM             &  !I Number of latutudes/longitudes
     )

  !Purpose: Open a netCDF file and get 2D field from the file
  !The field you want to get has the name "fieldname", and the file
  !has the name "filename". 
  !The timestep you want to read is the input parameter ntimestep

  !Remember to include -I${NETCDF_INC} -I${NETCDF_LIB} for compiling
  !And for linking -L${NETCDF_LIB} and -lnetcdf in your makefile
  !The values of these two should be set in your .bashrc file like this: 
  !export NETCDF_INC = /mn/hox/u8/jsundet/include/  
  !export NETCDF_LIB = /mn/hox/u8/jsundet/lib/
  !Or if you are using C-shell put the following in your .cshrc file
  !setenv NETCDF_INC /mn/hox/u8/jsundet/include/
  !setenv NETCDF_LIB /mn/hox/u8/jsundet/lib/
  !If this doesn't work, the explicitly link $NETCDF_INC/netcdf.mod to your run-dir

  !Author: Alf Grini, alf.grini@geofysikk.uio.no

  use netcdf

  implicit none
  include 'netcdf.inc'  
  !INPUT
  character*(*), intent(in)           :: fieldname  !I Name of field
  character*(*), intent(in)           :: filename   !I Name of netCDFfile
  integer,intent(in)                  :: IM         !I Number of latitudes/longitudes

  !OUTPUT
  real, intent(out)        :: oneDfield(IM)  !Three dimensional field

  !LOCAL
  !LOCAL NETCDF DIMENSION IDs ETCETERA
  integer                  :: lon_dim_id      !Id for longitude dimension
  integer                  :: lon_id          !Id for variable longitude
  real                     :: lon(IM)         !variable lon (in file)
  integer                  :: lat_dim_id      !Id for latitude dimension
  integer                  :: lat_id          !Id for latitude
  real                     :: lat(IM)         !Variable for latitude
!  integer                  :: time_dim_id     !Id for time dimension
!  integer                  :: time_id         !Id for time
  integer                  :: field_dim_id(1) !Dimension id for field
  integer                  :: field_id        !Variable id for field
  integer                  :: srt_lat(1) !Start point 
  integer                  :: cnt_lat(1) !Count indexes
  integer                  :: srt_lon(1) !Start point 
  integer                  :: cnt_lon(1) !Count indexes
  integer                  :: nlons                   !Longitudes in file
  integer                  :: nlats                   !Latitudes in file
!  integer                  :: nsteps                  !Timesteps avaiable in file
  integer                  :: status                  !status of process (0=OK)
  integer                  :: ncid                    !file id 

  !Array which tells you where to start picking your 3D field
  srt_lon= (/ 1 /)    !Start array
  !Array which tells you how far to count when picking it
  cnt_lon= (/ IM /)   !Count array

  !**********START CODE************************''

  status=nf_noerr  !Status is 0 and should be kept that way !!

!  write(6,*) filename,fieldname,IM,JM,ntimestep

  !Open the existing file
  status=nf_open(filename, nf_nowrite, ncid)
!  if(status/=nf_noerr)call handle_err(status)

  !Inquire dimension ids
!  status = nf_inq_dimid(ncid,"time",time_dim_id)

  if(fieldname.eq.'lat') then
   status = nf_inq_dimid(ncid,"lat",lat_dim_id)
!  if(status/=nf_noerr)call handle_err(status)
   field_dim_id(1)=lat_dim_id
  else
   status = nf_inq_dimid(ncid,"lon",lon_dim_id)
!  if(status/=nf_noerr)call handle_err(status)
   field_dim_id(1)=lon_dim_id
  endif

!  if(status/=nf_noerr)call handle_err(status)

  !Dimension id for 3D field /lon/lat/lev/time
!  field_dim_id(3)=time_dim_id
!  field_dim_id(1)=lat_dim_id

  !Inquire dimensions

  if(fieldname.eq.'lat') then
   status = nf_inq_dimlen(ncid,lat_dim_id,nlats)
!  if(status/=nf_noerr)call handle_err(status)
   if(nlats/=IM)then
     write(6,*)'file'//filename//' reports JM = ',nlats
     write(6,*)'your array has dimension ',IM
     stop
   endif
  else
   status = nf_inq_dimlen(ncid,lon_dim_id,nlons)
!  if(status/=nf_noerr)call handle_err(status)
   if(nlons/=IM)then
     write(6,*)'file'//filename//'file reports IM = ',nlons
     write(6,*)'your array has dimension',IM
     stop
   endif
  endif
!  status = nf_inq_dimlen(ncid,time_dim_id,nsteps)
!  if(status/=nf_noerr)call handle_err(status)
!  if(ntimestep.gt.nsteps.or.nsteps.le.0)then
!     write(6,*)'file'//filename//'file reports nsteps = ',nsteps
!     write(6,*)'you try to read timestep',ntimestep
!     stop
!  endif
  
  !Get variable ID
  status=nf_inq_varid(ncid,fieldname,field_id)
!  if(status/=nf_noerr)call handle_err(status)
!  write(6,*) 'Before reading'
  !Finally after all this, you can get the variable you want !!
  !and put it in the threeDfield array
!  status=nf_get_vara_real(ncid,field_id,twoDfield &
!       ,srt_lon_lat_time   &
!       ,cnt_lon_lat_time )

  status=nf_get_vara_double(ncid,field_id &
       ,srt_lon   &
       ,cnt_lon,oneDfield )

  if(status/=nf_noerr) stop
call handle_err(status)

!  write(6,*)'got variable ',fieldname,srt_lon_lat_time,cnt_lon_lat_time

!Closing file
  status=nf_close(ncid)
!  if(status/=nf_noerr)call handle_err(status)
  
  
  return
  end subroutine readnclatlon

  
