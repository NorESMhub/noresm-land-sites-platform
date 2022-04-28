ccccc6ccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5ccccccccc6ccccccccc7cc
      subroutine modepar(kcomp, ksol, imini, Nnat, rk, logsk, 
     $ rhosv, rhob, frombg, frbcbg, catot, catote, relh, 
     $ frac, frabc, fraq, alpha)

c **********************************************************************************
c     Created by Alf Kirkevåg.
c **********************************************************************************

c     Prescribed dry lognormal number size distributions, accomodation coefficients
c     alpha, and hygroscopic swelling index (ksol=1 implies call of subroutine rhsub)  
c     for the clean background and for externally mixed SO4, BC and OC.
c     The index imini determines smallest radius used in the Mie calculations. 
c     We here also define the gridded concentrations of SO4+BC+OC for the tables, 
c     catot/catote, since this is different for each background mode kcomp, as well
c     as the gridded values for relh, frac, frabc and fraq which are independent
c     on background mode number. Unit for catot and catote is ug/m^3 (per particle/cm^3)  

c     May 2016:
c     Recalibrated cate and cate (in modepar.f) to allow for more/less maximum added
c     mass on each background mode, due to large changes since the first AeroTab
c     version, since there is now in general more added mass per background particle.  
c     The cat and cate arrays have also been changed so that their values (for varying
c     icat and icate) can be calculated based on the min and max array values...

c     October 2016: recalibrated cate for kcomp=2 due to ca. doubling in background size
c     (old values are commented as "co").
      
      use commondefinitions

      implicit none

      INTEGER  imini, kcomp, ksol, icat
      REAL Nnat, rk, r0, rbcn, logsk, logs0, alpha
      REAL rhobc, rhooc, rhosv, rhob
      REAL catot(6), frac(6), frabc(6), fraq(6), relh(10)
      REAL frombg(6), frbcbg(6)
      REAL catote(16)

      Nnat=1.0    ! cm^(-3) normalized size distribution

      if(kcomp.ge.5) then  ! dummy array (defined but not used)
        catote=(/ 1e-10, 1e-10, 1e-10, 1e-10, 1e-10, 1e-10, 1e-10,
     $ 1e-10, 1e-10, 1e-10, 1e-10, 1e-10, 1e-10, 1e-10, 1e-10, 1e-10 /)
      endif
      if(kcomp.ge.1.and.kcomp.le.4) then  ! dummy array (defined but not used)
        catot=(/ 1e-10, 1e-10, 1e-10, 1e-10, 1e-10, 1e-10 /)
        catot(1)=1.e-10
      endif
      catote(1)=1.e-10
      catot(1)=1.e-10

c     Median radius and log10(standard deviation) for the modes that are defined in CAM5-Oslo
c     (for those that do not exist there, the values given below counts)
      rk=originalNumberMedianRadius(kcomp)*1.e6
      logsk=log10(originalSigma(kcomp))
      if(kcomp.eq.1) then 
c        write(*,*) 'SO4(A/n), H2SO4 background for cond. of H2SO4 and SOA'
        alpha=1.0
        ksol=1
        rhob=rhosv
        imini=1
co        catote=(/ 1e-10, 1e-5, 2e-5, 4e-5, 8e-5, 1.5e-4, 3e-4,
co     $  6e-4, 1.2e-3, 2.5e-3, 5e-3, 1e-2, 2e-2, 4e-2, 8e-2, 0.15 /)
        do icat=2,16
           catote(icat)=10**((icat-1)/3.0-6.222)
        enddo
      elseif(kcomp.eq.2) then 
c        write(*,*) 'BC(A/n), BC background for cond. of H2SO4'
        alpha=0.3
        ksol=1
        rhob=aerosol_type_density(2)
        imini=1   
co        catote=(/ 1e-10, 1e-5, 2e-5, 4e-5, 8e-5, 1.5e-4, 3e-4,
co     $  6e-4, 1.2e-3, 2.5e-3, 5e-3, 1e-2, 2e-2, 4e-2, 8e-2, 0.15 /)
        do icat=2,16
co           catote(icat)=10**((icat-1)/3.0-6.523)
           catote(icat)=10**((icat-1)/3.0-6.222)
        enddo
      elseif(kcomp.eq.3) then     ! this mode is not defined/used in CAM-Oslo
c        write(*,*) 'OC(A/n), OC background for cond. of H2SO4' 
        alpha=0.7
        ksol=1
        rk=originalNumberMedianRadius(14)*1.e6
        logsk=log10(originalSigma(14))
        rhob=aerosol_type_density(3)
        imini=1   
co        catote=(/ 1e-10, 1e-4, 2e-4, 4e-4, 8e-4, 1.5e-3, 3e-3,
co     $  6e-3, 1.2e-2, 2.5e-2, 5e-2, 0.1, 2e-1, 0.4, 0.8, 1.5 /)
        do icat=2,16
c           catote(icat)=10**((icat-1)/3.0-4.301)
           catote(icat)=1.e-10   ! not used anyway
        enddo
      elseif(kcomp.eq.4) then    
c        write(*,*) 'OC&BC(A/n), OC&BC background for cond. of H2SO4,' 
c        write(*,*) 'assuming OC is the basis for added BC and SO4' 
        alpha=0.5 ! between 0.3 for BC and 0.7 for OC
        ksol=1
        rhob=aerosol_type_density(3)
        imini=1   
co        catote=(/ 1e-10, 0.01, 0.05, 0.1, 0.2, 0.4, 0.7, 1.0,
co     $   1.5, 2.5, 5., 10., 25., 50., 100., 500. /)*1.904e-3 
        do icat=2,16
           catote(icat)=10**((icat-1)/3.0-4.301)
        enddo
       elseif(kcomp.eq.5) then    
c        write(*,*) 'SO4("Ait75"), H2SO4 background for cond/coag/Aq.'
        alpha=1.0
        ksol=1                            
        rhob=rhosv
        imini=13   
co        catot=(/ 1.e-10, 5.e-4, 2.e-3, 0.01, 0.04, 0.15 /) 
        do icat=2,6
           catot(icat)=10**((icat-1)/1.0-3.824)
        enddo
      elseif(kcomp.eq.6) then    
c        write(*,*) '  MINACC, from AEROCOM'
        alpha=0.3
        ksol=1                            
        rhob=aerosol_type_density(4)
        imini=18      
co        catot=(/ 1.e-10, 0.01, 0.05, 0.2, 0.8, 4.0 /)
        do icat=2,6
           catot(icat)=10**((icat-1)/1.0-3.523)
        enddo
      elseif(kcomp.eq.7) then    
c        write(*,*) '  MINCOA, from AEROCOM'
        alpha=0.3
        ksol=1                            
        rhob=aerosol_type_density(4)
        imini=20   
co        catot=(/ 1.e-10, 0.02, 0.1, 0.5, 2.0, 8.0 /)
        do icat=2,6
           catot(icat)=10**((icat-1)/1.0-3.699)
        enddo
      elseif(kcomp.eq.8) then    
c        write(*,*) '  SSAIT, from AEROCOM'
        alpha=1.0
        ksol=1                            
        rhob=aerosol_type_density(5)
        imini=1   
co        catot=(/ 1.e-10, 5.e-4, 2.e-3, 0.01, 0.04, 0.15 /)  ! as for kcomp=5
        catot(1)=1.e-10
        do icat=2,6
           catot(icat)=10**((icat-1)/1.0-4.921)
        enddo
      elseif(kcomp.eq.9) then    
c        write(*,*) '  SSACC, from AEROCOM'
        alpha=1.0
        ksol=1                            
        rhob=aerosol_type_density(5)
        imini=15     
co        catot=(/ 1.e-10, 0.01, 0.05, 0.2, 0.8, 4.0 /) ! as for kcomp=6
        do icat=2,6
           catot(icat)=10**((icat-1)/1.0-3.301)
        enddo
      elseif(kcomp.eq.10) then    
c        write(*,*) '  SSCOA, from AEROCOM'
        alpha=1.0
        ksol=1                            
        rhob=aerosol_type_density(5)
        imini=20
co        catot=(/ 1.e-10, 0.02, 0.1, 0.5, 2.0, 8.0 /)
        do icat=2,6
           catot(icat)=10**((icat-1)/1.0-3.699)
        enddo
      elseif(kcomp.eq.0) then
c        write(*,*) '  soot (BC), fractal a-mode'
        alpha=0.3
        ksol=0
        rk=originalNumberMedianRadius(0)*1.e6
        logsk=log10(originalSigma(0))
        rhob=aerosol_type_density(2)    ! dummy, rhobcax used instead
        imini=1   
      else         
        write(*,*) 'modes 0 through 10 only'
        stop
      endif

c     define input arrays for tabulated optical parameters:
c     relative humidity, RH
      relh =(/ 0.0, 0.37, 0.47, 0.65, 0.75, 0.80, 
     $                    0.85, 0.90, 0.95, 0.995 /)
c     the mass fraction OC/(OC + H2SO4) for the background aerosol of kcomp=1
co      frombg=(/ 0.0, 0.1, 0.3, 0.5, 0.7, 0.999 /)
      frombg=(/ 0.0, 0.2, 0.4, 0.6, 0.8, 1.0 /)
c     the mass fraction BC/(BC + OC) for the background aerosol of kcomp=4
co      frbcbg=(/ 0.0, 0.1, 0.3, 0.5, 0.7, 0.999 /)
      frbcbg(1)=1.e-10 
      do icat=2,6
         frbcbg(icat)=10**((icat-1)/4.0-1.25)
      enddo
c     the fraction (internally mixed BC+OC)/(internally mixed BC+OC+SO4)  (from coag. and cond.)
co      frac=(/ 0.0, 0.1, 0.3, 0.5, 0.7, 0.999 /)
      frac=(/ 0.0, 0.2, 0.4, 0.6, 0.8, 1.0 /)
c     the fraction (internally mixed BC)/(internally mixed BC+OC)  (from coag.)
co      frabc=(/ 0.0, 0.01, 0.1, 0.3, 0.7, 0.999 /)
      frabc(1)=1.e-10 
      do icat=2,6
         frabc(icat)=10**((icat-1)/4.0-1.25)
      enddo
c     the fraction (internally mixed (NH4)2SO4 from cloud processing)/(all internally mixed 
c     H2SO4 and (NH4)2SO4 from cloud processing, coag. and cond.)
co      fraq=(/ 0.0, 0.25, 0.50, 0.75, 0.85, 1.0 /)
      fraq=(/ 0.0, 0.2, 0.4, 0.6, 0.8, 1.0 /)

      return
      end 
