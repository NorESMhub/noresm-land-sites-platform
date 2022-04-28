!**********************************************************************
!     This program reads model output data of aerosol mode number, 
!     median radius and width (for log-normal size distriobutions) 
!     from a netCDF file produced with the script pnsd.sh (giving 12 
!     months of climatological data), and calculates and writes to a 
!     new file the number size distribution as function of month, 
!     latitude, longitude and particle diameter. 
!**********************************************************************
!     Main propgram (pnsd.f) is created by Alf Kirkevag and Dirk Olivie, 
!     April 2016. Subroutines (nc read, write, check) are based on code 
!     from Dirk Olivie, Oyvind Seland, Alf Grini, and Jostein Sundet... 
!**********************************************************************

      program pnsd

      implicit none

      integer i,j,ibin
      integer nt, inr, inrmin, ivtype

      real readfield(288,192)
      real nmr(288,192,0:14)
      real sig(288,192,0:14)
      real num(288,192,0:14)
      
      real time(12)
      real longitude(288)
      real latitude(192)
      real diameter(60)

      real rgm(60)
      real dndlogr(288,192,60,0:14), dndlogrsum(288,192,60)
      real dmdlogr_ss(288,192,60), dmdlogr_du(288,192,60)
      real dndlogrsummode(288,192,0:14)
      real writfield(288,192), writfield3d(288,192,60)

      character*128 filename
      character*10 wname,wname1,wname2
      character*10 woutname,longname,unit
      character*120 fwname,fp,wn

      real pi, fourpibythree, pibysix, const

      pi=4.*atan(1.)
      fourpibythree=4.*pi/3.
      pibysix=pi/6.

      fwname='/work/kirkevag/pnsd/PD_MG15MegVadSOA.cam.'// 
     &  'h0.climyr3-12surface_monthly-nolev.nc'

!     First read lat-lon info from the imput file
      call readnclatlon(longitude,'lon',fwname,288)
!      write(*,*) longitude
      call readnclatlon(latitude,'lat',fwname,192)
!      write(*,*) latitude


!     Calculate geometric median radius for 60 bins along log(r) axis,
!     where log is log_10 and r = 0.01 - 10 um  
      do ibin=1,60
        rgm(ibin)=1.e-6*10**(0.05*(ibin-0.5)-2)   ! unit m
!        write(100,*) ibin, rgm(ibin)
        diameter(ibin) = 2.*rgm(ibin)*1.e6        ! unit um
      end do

!     Calculate const in "dN/dlogD = const * NNAT/log(SIGMA)*exp(-0,5*(...)**2)
      const=1./sqrt(2.*pi) 
!      write(*,*) 'const =', const


!     Then start reading the data from postprocess model output netcdf file

         do nt=1,12   ! time loop moved outside to reduce # of dimensions


      do ivtype=1,3   ! loop over variable type

       if(ivtype.eq.1) then
         wname1='NMR'
         inrmin=1
       elseif(ivtype.eq.2) then
         wname1='SIGMA'
         inrmin=1
       else
         wname1='NNAT_'
         inrmin=0
       endif

       do inr=inrmin,14

        if(inr.ne.3.and.inr.ne.11.and.inr.ne.13) then
         wname2="  "
         write(wname2,"(I2)"),inr
         if(inr.lt.10.and.ivtype.ne.3) then
           wname2="0"//adjustl(wname2)
         else
           wname2=adjustl(wname2)
         endif
         wname = trim(wname1)//trim(wname2)

         if(nt.eq.1) write(*,*) 'wname=', wname

            call readnc(
     &              readfield !O Output field
     &              ,wname    !I Name of field to pick
     &              ,fwname   !I Name of file to pick from
     &              ,288      !I Number of longitudes
     &              ,192      !I Number of latitudes
     &              ,nt       !I Timestep to pick
     &              )
          do j=1,192
            do i=1,288
              if(wname1.eq.'NMR') then
                nmr(i,j,inr)=readfield(i,j)   ! unit m
              elseif(wname1.eq.'SIGMA') then
                sig(i,j,inr)=readfield(i,j)   ! no unit
              elseif(wname1.eq.'NNAT_') then
                num(i,j,inr)=readfield(i,j)   ! unit cm-3
              else
                write(*,*) 'Requested variabel non-existent'
                stop
              endif
            end do   
          end do
 
        endif  ! inr.ne.3,11,13

       end do  ! inr

      end do  ! ivtype
    

!     ------ reading for this nt is done: now use the read in fields -------

!     Define fields which have not been read in (because they are constant),
!     and initialize outfields

          do j=1,192
            do i=1,288
              sig(i,j,0)=1.6
              nmr(i,j,0)=0.1e-6
              do ibin=1,60
                dndlogrsum(i,j,ibin)=0.0
                dmdlogr_ss(i,j,ibin)=0.0
                dmdlogr_du(i,j,ibin)=0.0
              end do 
              do inr=0,14
                dndlogrsummode(i,j,inr)=0.0
              end do
           end do
          end do         

!     Calculate dry number size distributions dN/dlogr (unit cm-3) for each mode inr

       do inr=0,14 
        if(inr.ne.3.and.inr.ne.11.and.inr.ne.13) then
         do ibin=1,60
          do j=1,192
            do i=1,288
              dndlogr(i,j,ibin,inr)=const*(num(i,j,inr)
     &          /log10(sig(i,j,inr)))
     &          *exp(-0.5*(log10(rgm(ibin)/nmr(i,j,inr))
     &          /log10(sig(i,j,inr)))**2)                  
!                 write(*,*) 'inr, ibin, dndlogr =', inr, ibin, 
!     &           dndlogr(i,j,ibin,inr)         
            end do
          end do         
         end do 
        endif
       end do

!     Then sum over all modes for the number size distribitions (dndlogrsum has unit cm-3)

        do inr=0,14
          do j=1,192
            do i=1,288
              do ibin=1,60
               dndlogrsum(i,j,ibin)=dndlogrsum(i,j,ibin)
     &          + dndlogr(i,j,ibin,inr)
              end do
            end do         
          end do
        end do

!     And similarly, approximate dry mass size distributions dMdlogr for SS and DU
!     (with mass densities of 2.6 and 2.2 g cm-3). Unit: 1.e12 g cm-3 = ug m-3
     
       do inr=6,7 
         do ibin=1,60
          do j=1,192
            do i=1,288
              dmdlogr_du(i,j,ibin)=dmdlogr_du(i,j,ibin)
     &       +2.6*pibysix*dndlogr(i,j,ibin,inr)*diameter(ibin)**3
            end do
          end do         
         end do 
       end do
       do inr=9,10 
         do ibin=1,60
          do j=1,192
            do i=1,288
              dmdlogr_ss(i,j,ibin)=dmdlogr_ss(i,j,ibin)
     &       +2.2*pibysix*dndlogr(i,j,ibin,inr)*diameter(ibin)**3
            end do
          end do         
         end do 
       end do

!      Some test output------------------------- 
       do ibin=1,60
!      Ca. Birkenes station in southern Norway
        do j=157,157  
          do i=7,7
            write(101,*) diameter(ibin), dndlogr(i,j,ibin,1), nt         
            write(102,*) diameter(ibin), dndlogrsum(i,j,ibin), nt         
          end do
        end do         
       end do 
       write(103,*) nt, nmr(7,157,1)         
       write(104,*) nt, num(7,157,1)         
!     Checking that the sum of particles from each bin actually sums up to the total:
        do inr=0,14
         if(inr.ne.3.and.inr.ne.11.and.inr.ne.13) then
          do j=1,192
            do i=1,288
              do ibin=1,60
               dndlogrsummode(i,j,inr)=dndlogrsummode(i,j,inr)
     &          + 0.05*dndlogr(i,j,ibin,inr)
              end do
            end do         
          end do
         endif
        end do
        do inr=0,14
         if(inr.ne.3.and.inr.ne.11.and.inr.ne.13) then
          do j=157,157
            do i=7,7
               write(100,*) nt, inr, num(i,j,inr),
     &         dndlogrsummode(i,j,inr)/num(i,j,inr)         
            end do         
          end do
         endif
        end do
!      for a dusty region (Arabic peninsula)
!        do j=120,120  
!          do i=38,38
!            write(200,*) diameter(ibin), dmdlogr_du(i,j,ibin)         
!            write(206,*) diameter(ibin), dndlogr(i,j,ibin,6)         
!            write(207,*) diameter(ibin), dndlogr(i,j,ibin,7)         
!          end do
!        end do         
!      and for a marine region (Southern ocean)
!        do j=40,40
!          do i=17,17
!            write(300,*) diameter(ibin), dmdlogr_ss(i,j,ibin)         
!            write(309,*) diameter(ibin), dndlogr(i,j,ibin,9)         
!            write(310,*) diameter(ibin), dndlogr(i,j,ibin,10)         
!          end do
!        end do         
!       end do 
!      Some test output------------------------- 
       

!     Finally write the final data to a single netcdf file (for each PNSD variable) 
!      filename='/work/kirkevag/pnsd/dndlogd_global.nc'
      fp='/work/kirkevag/pnsd/'

      wn='aerocom03_CAM5.3-Oslo_global-PNSD_dndlogdaer_2000_monthly.nc'
      filename=trim(fp)//trim(wn)
      woutname='dndlogdaer'  ! note that dndlogdaer=dndlograer
      longname='dndlogdaer'
      unit='cm-3'
         do ibin=1,60
          do j=1,192
            do i=1,288
              writfield3d(i,j,ibin)=dndlogrsum(i,j,ibin)
            end do
          end do         
         end do
         call writenc4d_diameter(writfield3d, woutname, longname, unit,
     & filename, 288, 192, 60, nt,
     & longitude, latitude, diameter,
     & 'lon','lat','particle_diameter','time',
     & 'longitude EAST to WEST','latitude SOUTH to NORTH',
     & 'particle diameter','time in months',
     & 'deg','deg','um','month')

      wn='aerocom03_CAM5.3-Oslo_global-PNSD_dmdlogdss_2000_monthly.nc'
      filename=trim(fp)//trim(wn)
      woutname='dmdlogdss'
      longname='dmdlogdss'
      unit='ug m-3'
         do ibin=1,60
          do j=1,192
            do i=1,288
              writfield3d(i,j,ibin)=dmdlogr_ss(i,j,ibin)
            end do
          end do         
         end do
         call writenc4d_diameter(writfield3d, woutname, longname, unit,
     & filename, 288, 192, 60, nt,
     & longitude, latitude, diameter,
     & 'lon','lat','particle_diameter','time',
     & 'longitude EAST to WEST','latitude SOUTH to NORTH',
     & 'particle diameter','time in months',
     & 'deg','deg','um','month')

      wn='aerocom03_CAM5.3-Oslo_global-PNSD_dmdlogddust_2000_monthly.nc'
      filename=trim(fp)//trim(wn)
      woutname='dmdlogddust'
      longname='dmdlogddust'
      unit='ug m-3'
         do ibin=1,60
          do j=1,192
            do i=1,288
              writfield3d(i,j,ibin)=dmdlogr_du(i,j,ibin)
            end do
          end do         
         end do
         call writenc4d_diameter(writfield3d, woutname, longname, unit,
     & filename, 288, 192, 60, nt,
     & longitude, latitude, diameter,
     & 'lon','lat','particle_diameter','time',
     & 'longitude EAST to WEST','latitude SOUTH to NORTH',
     & 'particle diameter','time in months',
     & 'deg','deg','um','month')

         end do  ! nt


      return
      end








