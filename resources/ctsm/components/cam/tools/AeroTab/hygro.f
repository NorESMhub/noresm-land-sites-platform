      subroutine hygro (rh, xbc, xdst, xoc, xs, xa, xss,
     $                  rhda, rhca, rhdss, rhcss)
     
c **********************************************************************************
c     Created by Alf Kirkevåg and Alf Grini.
c **********************************************************************************

c     Hygro calculates hygroscopic properties (given by x*) for each pure aerosol 
c     component, for use in mixsub. For the hygroscopicities which are assumed to
c     depend on RH, these have been found by solving the Kohler equation w.r.t. x
c     for a given (measured/calculated) growth factor as function of RH. Linear 
c     fits (x=a*RH+b) have here been made for the available number of RH intervals. 
c     The hygroscopicity is defined as (using input from commondefinitions.F90):
c     x = soluble_mass_fraction * number_of_ions * osmotic_coefficient 
c       * (mwh2o/molecular_weight) * (density/rhoh2o).

      use commondefinitions

      implicit none

      REAL rh, rhda, rhca, rhdss, rhcss
      REAL x, xbc, xoc, xs, xa, xss, xdst, xcam
      REAL get_hygroscopicity

c     Soot/BC (aerosoltype 2 in CAM5-Oslo), 
c     a practically hydrophobic component.
      xbc=5.0e-7 ! This is the value previously used
      xcam=get_hygroscopicity(AEROSOL_TYPE_BC)
      if(abs((xbc-xcam)/xcam).gt.0.005) then
        write(*,*) 'xbc differs (more than 0.5%) from CAM5-Oslo value.' 
        write(*,*) 'Edit xbc here or in CAM5-Oslo before continuing.'
        stop
      endif

c     Mineral (aerosoltype 4 in CAM5-Oslo)
      xdst=0.0693 ! This is the value previously used
      xcam=get_hygroscopicity(AEROSOL_TYPE_DUST)
      if(abs((xdst-xcam)/xcam).gt.0.005) then
        write(*,*) 'xdst differs (more than 0.5%) from CAM5-Oslo value.' 
        write(*,*) 'Edit xdst here or in CAM5-Oslo before continuing.'
        stop
      endif

c     Organic carbon (OC) (aerosoltype 3 in CAM5-Oslo) 
c     (MIRAGE-based (Ghan et al., 2001) for large RH, ~0.25*ammonium-sulfate)
      xoc=0.14 ! This is the value previously used
      xcam=get_hygroscopicity(AEROSOL_TYPE_OM)
      if(abs((xoc-xcam)/xcam).gt.0.005) then
        write(*,*) 'xoc differs (more than 0.5%) from CAM5-Oslo value.' 
        write(*,*) 'Edit xoc here or in CAM5-Oslo before continuing.'
        stop
      endif

c     Sulphuric acid (not an aerosoltype in CAM5-Oslo), hygroscopicity 
c     as inferred from Table 2 (for SUSO) in Kopke ert al. (1997):  
c     These values are not used in CAM-Oslo, where sulfate is assumed 
c     to exist only as ammonium sulfate.
      if(rh.le.0.5) then
        xs=1.23
      elseif(rh.gt.0.5.and.rh.le.0.7) then
        xs=-1.05*rh+1.755
      elseif(rh.gt.0.7.and.rh.le.0.8) then
        xs=-1.35*rh+1.965
      elseif(rh.gt.0.8.and.rh.le.0.9) then
        xs=-1.79*rh+2.317
      elseif(rh.gt.0.9.and.rh.le.0.95) then
        xs=-1.74*rh+2.272
      elseif(rh.gt.0.95.and.rh.le.0.98) then
        xs=-2.5*rh+2.994
      elseif(rh.gt.0.98.and.rh.le.0.99) then
        xs=-1.0*rh+1.524
      else  
        xs=0.534
      endif

c     Ammonium sulphate (aerosoltype 1 in CAM5-Oslo)
c     (Ghan et al., 2001; RHC and RHD from Tang & Munkelwitz, 1994)
      xa=0.507 ! This is the value previously used
      xcam=get_hygroscopicity(AEROSOL_TYPE_SULFATE)
      if(abs((xa-xcam)/xcam).gt.0.005) then
        write(*,*) 'xa differs (more than 0.5%) from CAM5-Oslo value.' 
        write(*,*) 'Edit xa here or in CAM5-Oslo before continuing.'
        stop
      endif
      rhca=0.37   ! point of crystallisation (Tang & Munkelwitz, 1994)
      rhda=0.80   ! point of deliquescence (Tang & Munkelwitz, 1994)
      if(rh.ge.rhda) then
        xa=xa    ! value used for cloud activation calculations
      elseif(rh.lt.rhca) then ! below point of crystallization, assume a small but non-zero value
        xa=xbc
      else       ! assume half hygroscopicity in the hysteresis regime
        xa=0.5*xa
      endif

c     Sea-salt (aerosoltype 5 in CAM5-Oslo), hygroscopicity 
c     as inferred from Table 2 in Kopke et al. (1997): 
      if(rh.le.0.5) then
        xss=2.19
      elseif(rh.gt.0.5.and.rh.le.0.7) then
        xss=-2.15*rh+3.265
      elseif(rh.gt.0.7.and.rh.le.0.9) then
        xss=-2.2*rh+3.3
      elseif(rh.gt.0.9.and.rh.le.0.95) then
        xss=-1.0*rh+2.22
      elseif(rh.gt.0.95.and.rh.le.0.98) then
        xss=-2.333*rh+3.486
      else  
        xss=1.20 ! This is the value previously used
        xcam=get_hygroscopicity(AEROSOL_TYPE_SALT)
        if(abs((xss-xcam)/xcam).gt.0.005) then
         write(*,*) 'xss differs (more than 0.5%) from CAM5-Oslo value.' 
         write(*,*) 'Edit xss here or in CAM5-Oslo before continuing.'
         stop
        endif
      endif
      rhcss=0.46   ! point of crystallisation (Tang, 1966) 
      rhdss=0.75   ! point of deliquescence (Tang, 1966) 
c     Assuming half hygroscopicity in the hysteresis regime
      if(rh.ge.rhcss.and.rh.le.rhdss) then
        xss=0.5*xss
      elseif(rh.lt.rhcss) then
        xss=xbc
      endif

      return
      end  


!******************************************************************

      function get_hygroscopicity (typeindex) result(hygroscopicity)

      use commondefinitions

      implicit none
      integer, intent(in) :: typeindex 
      real hygroscopicity
      real Mw, rhow

      data Mw, rhow / 1.8016e1, 1.0e3 /

      hygroscopicity = 
     $         aerosol_type_soluble_mass_fraction(typeindex) 
     $       * aerosol_type_number_of_ions(typeindex)
     $       * aerosol_type_osmotic_coefficient(typeindex)
     $       * Mw/aerosol_type_molecular_weight(typeindex)
     $       * aerosol_type_density(typeindex)/rhow

      end function

!******************************************************************
