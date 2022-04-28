module calcaersize

contains

!     � Seland Calculates mean volume size and hygroscopic growth for use in 
!     dry deposition
      subroutine calcaersize_sub( ncol, &
                 t, h2ommr, pmid, pdel,wetnumberMedianDiameter,wetrho   &
                 , wetNumberMedianDiameter_processmode, wetrho_processmode)


      use constituents, only : pcnst
      use shr_kind_mod,only: r8 => shr_kind_r8
      use ppgrid
      use wv_saturation, only: qsat_water
      use commondefinitions, only: nmodes
      use aerosoldef
      use physconst,     only: rhoh2o

      implicit none

      integer,  intent(in) :: ncol               ! number of columns
      real(r8), intent(in) :: t(pcols,pver)      ! layer temperatures (K)
      real(r8), intent(in) :: h2ommr(pcols,pver) ! layer specific humidity
      real(r8), intent(in) :: pmid(pcols,pver)   ! layer pressure (Pa)
      real(r8), intent(in) :: pdel(pcols,pver)  ! layer pressure thickness (Pa)

      real(r8), intent(out):: wetNumberMedianDiameter(pcols,pver,0:nmodes)  
      real(r8), intent(out):: wetrho(pcols,pver,0:nmodes) ! wet aerosol density
      real(r8), intent(out) :: wetNumberMedianDiameter_processmode(pcols,pver,numberOfProcessModeTracers)
      real(r8), intent(out) :: wetrho_processmode(pcols,pver,numberOfProcessModeTracers)

!     local variables
      real(r8) :: relhum(pcols,pver) ! Relative humidity  
      integer  :: i,k,m,irelh,mm, tracerCounter
      integer  ::l ! species index
      real(r8) :: xrh(pcols,pver)
      real(r8) :: qs(pcols,pver)        ! saturation specific humidity
      real(r8) :: rmeanvol              ! Mean radius with respect to volume 
      integer  :: irh1(pcols,pver),irh2(pcols,pver)
      integer  :: t_irh1,t_irh2
      real(r8) :: t_rh1,t_rh2,t_xrh,rr1,rr2
      real(r8) :: volumeFractionAerosol   !with respect to total (aerosol + water)
      real(r8) :: tmp1, tmp2
      real(r8) :: wetrad_tmp(max_tracers_per_mode)
      real(r8) :: dry_rhopart_tmp(max_tracers_per_mode)
      real(r8) :: mixed_dry_rho
       

      !Get the tabulated rh in all grid cells
      do k=1,pver
        do i=1,ncol
          call qsat_water(t(i,k),pmid(i,k), tmp1, qs(i,k), tmp2)
          xrh(i,k) = h2ommr(i,k)/qs(i,k)
!cak
!          if(xrh(i,k).lt.0.0_r8.or.xrh(i,k).gt.1.0_r8) then
!             write(*,*) 'i,k,rh calcaer=',i,k,xrh(i,k)
!          endif
!cak
          xrh(i,k) = max(xrh(i,k),0.0_r8)
          xrh(i,k) = min(xrh(i,k),1.0_r8)
          relhum(i,k)=xrh(i,k)
          xrh(i,k)=min(xrh(i,k),rhtab(10))                
      end do
     end do
 
      !Find the relh-index in all grid-points
      do irelh=1,SIZE(rhtab) - 1 
         do k=1,pver
            do i=1,ncol
               if(xrh(i,k).ge.rhtab(irelh).and. &
                  xrh(i,k).le.rhtab(irelh+1)) then
                  irh1(i,k)=irelh                !lower index
                  irh2(i,k)=irelh+1              !higher index
                end if
               end do
            end do
      end do

      do k=1,pver
         do i=1,ncol

            !Get the indexes out as floating point single numbers
            t_irh1 = irh1(i,k)
            t_irh2 = irh2(i,k)
            t_rh1  = rhtab(t_irh1)
            t_rh2  = rhtab(t_irh2)
            t_xrh  = xrh(i,k)

            do m = 0, nmodes
               !Do some weighting to mass mean property
               !weighting by 1.5 is number median ==> volumetric mean
               !http://dust.ess.uci.edu/facts/psd/psd.pdf
               rmeanvol = lifeCycleNumberMedianRadius(m)*DEXP(1.5_r8*(log(lifeCycleSigma(m)))**2)
               wetNumberMedianDiameter(i,k,m ) =  0.1e-6_r8 !Initialize to something..
               mixed_dry_rho = 1.e3_r8

               tracerCounter = 0  
               do l = 1,getNumberOfBackgroundTracersInMode(m)

                  tracerCounter = tracerCounter + 1

                  !which tracer is this?
                  mm = getTracerIndex(m,l,.false.)

                  !radius of lower rh-bin for this tracer
                  rr1=rdivr0(t_irh1,mm)

                  !radius of upper rh-bin for this tracer
                  rr2=rdivr0(t_irh2,mm)

                  !linear interpolate dry ==> wet radius for this tracer
                  wetrad_tmp(tracerCounter) = (((t_rh2-t_xrh)*rr1+(t_xrh-t_rh1)*rr2)/ &
                     (t_rh2-t_rh1))*rmeanvol

                  !mixed density of dry particle                  
                  dry_rhopart_tmp(tracerCounter) = getDryDensity(m,l)

               end do

               !Find the average growth of this mode 
               !(still not taking into account how much we have!!)
               if(TracerCounter .gt. 0)then

                  !Convert to diameter and take average (note: This is MASS median diameter)
                  wetNumberMedianDiameter(i,k,m) = 2.0_r8 * SUM(wetrad_tmp(1:tracerCounter))/dble(tracerCounter)

                  !Take average density
                  mixed_dry_rho = SUM(dry_rhopart_tmp(1:tracerCounter))/dble(tracerCounter)

                  !At this point the radius is in "mass mean" space
                  volumeFractionAerosol = MIN(1.0_r8, ( 2.0_r8*rmeanVol / wetNumberMedianDiameter(i,k,m) )**3)

                  !wet density
                  wetrho(i,k,m) = mixed_dry_rho * volumeFractionAerosol   &
                        + (1._r8-volumeFractionAerosol)*rhoh2o 

                  !convert back to number median diameter (wet)
                  wetNumberMedianDiameter(i,k,m) = wetNumberMedianDiameter(i,k,m)*DEXP(-1.5_r8*(log(lifeCycleSigma(m)))**2)
               endif


               end do     !modes
      
               !Same thing for the process modes
               do l=1,numberOfProcessModeTracers

                  mm = tracerInProcessMode(l)   !process mode tracer (physics space)
                  
                  !weighting by 1.5 is number median ==> volumetric mean
                  !http://dust.ess.uci.edu/facts/psd/psd.pdf
                  rmeanvol = processModeNumberMedianRadius(l)*DEXP(1.5_r8*(log(processModeSigma(l)))**2)

                  !radius of lower rh-bin for this tracer
                  rr1=rdivr0(t_irh1,mm)

                  !radius of upper rh-bin for this tracer
                  rr2=rdivr0(t_irh2,mm)
                  
                  !Note this is MASS median diameter
                  wetNumberMedianDiameter_processmode(i,k,l) = (((t_rh2-t_xrh)*rr1+(t_xrh-t_rh1)*rr2)/ &
                                        (t_rh2-t_rh1))*rmeanvol*2.0_r8

                  volumeFractionAerosol = MIN(1.0, (2.0_r8*rmeanVol/wetnumberMedianDiameter_processmode(i,k,l))**3)

                  wetrho_processmode(i,k,l) = volumeFractionAerosol*rhopart(mm) &
                                             + (1.0_r8 - volumeFractionAerosol)*rhoh2o

                  !convert back to number median diameter (wet)
                  wetNumberMedianDiameter_processMode(i,k,l) = wetNumberMedianDiameter_processMode(i,k,l)*DEXP(-1.5_r8*(log(processModeSigma(l)))**2)
               end do     !process modes
            end do        !horizontal points
         end do           !layers

         return
      end subroutine calcaersize_sub
end module


