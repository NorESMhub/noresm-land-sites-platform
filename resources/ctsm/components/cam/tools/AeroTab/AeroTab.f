
      program AeroTab   ! Program for making Aerosol look-up tables for CAM5-Oslo

c **********************************************************************************
c     Created by Alf Kirkevåg. The code is originally based on the method developed 
c     and described by Kirkevåg, A., Iversen, T., and Dahlback, A. (1999): On radiative 
c     effects of black carbon and sulphate aerosols. Atmos. Environ. 33, 2621-2635, and
c     Kirkevåg, A., and Iversen, T. (2002): Global direct radiative forcing by process-
c     parameterized aerosol optical properties. J. Geophys. Res. 107, 10.1029/2001JD000886     
c **********************************************************************************
c
c     This program defines initial size distributions (at the point of emission) 
c     and microphysical properties, such as hygroscopicity and wevelength dependent 
c     refractive indices, then calculates the modified aerosol size distributions 
c     after aerosol processing (aging and hygroscopic growth), and finally calculates
c     the gross aerosol optical properties or dry size parameters for lognormal fits
c     to the generally non-lognormal modified size distributions, all as functions
c     added concentrations of internally mixed BC, OC and sulfate from different
c     microphysical processes, and anbient relative humidity (for the optics). The
c     output is a range of look-up tables (*.out) for use in CAM6-Nor (with only some
c     CAM-Oslo/CAM4-Oslo and most CAM5.3-Oslo functionality retained). 
c
c     References for CAM-Oslo (based on CAM3):
c     Kirkevåg, A., Iversen, T., Seland, Ø., Debernard, J.B., Storelvmo, T., and 
c      Kristjánsson, J.E. (2008) Aerosol-cloud-climate interactions in the climate 
c      model CAM-Oslo. Tellus, 60A, 492-512.
c     Seland, Ø., T. Iversen, A. Kirkevåg, and T. Storelvmo (2008) Aerosol-climate 
c      interactions in the CAM-Oslo atmospheric GCM and investigations of associated 
c      shortcomings. Tellus, 60A, 459-491.
c     Reference for CAM4-Oslo (based on CAM4):
c     Kirkevåg, A., T. Iversen, Ø. Seland, C. Hoose, J. E. Kristjánsson, H. Struthers, 
c      A. Ekman, S. Ghan, J. Griesfeller, D. Nilsson, and M. Schulz: Aerosol-climate 
c      interactions in the Norwegian Earth System Model - NorESM1-M, Geosci. Model Dev., 
c      6, 207-244, doi:10.5194/gmd-6-207-2013, 2013. 
c     References for CAM5.3-Oslo (based on CAM5.3):
c     Kirkevåg, A., A. Grini, D. Olivié, Ø. Seland, K. Alterskjær, M. Hummel,
c      I. H. H. Karset, A. Lewinschal, X. Liu, R. Makkonen, I. Bethke, J. Griesfeller,
c      M. Schulz, T. Iversen: A production-tagged aerosol module for Earth system models,
c      OsloAero5.3 – extensions and updates for CAM5.3-Oslo, Geosci. Model Dev., 11,
c      3945-3982, https://doi.org/10.5194/gmd-11-3945-2018, 2018.      
c     References for CAM6-Nor (based on CAM6):
c     Seland et al. (submitted to GMD) and Olivie et al (in prep.), and
c     Kirkevåg (2020): AeroTab user guide (a techinical report).     
C ===================================================================================
C
C     Notes on the most recent code development from CAM5.3-Oslo to CAM6-Nor, see
C     Olivie et al (in prep.) or Kirkevåg (2020) for full references:
c     In the Mie calculations for mineral dust (DU) optical properties in CAM5.3-Oslo,
c     the assumed complex refractive index was that of Hess et al. (1998) (OPAC) for
c     all wavelengths, λ. For wavelengths between 3 and 15μm we now use the multi-site
c     mean from Di Biagio et al. (2017). In lack of other (spectrally complete) updated
c     observation data, we keep the OPAC values at λ > 15μm as in CAM5.3-Oslo. To be able
c     to cover the whole SW wavelength range we make use of a composite of updated
c     observation data for the real (nr) and imaginary part (ni) of the refractive index:
c     Haapanala et al (2012) for λ ≤ 0.29μm and in the range 1.05μm ≤ λ ≤ 2.64μm; Shettle
c     and Fenn (1979) for nr and Colarco et al.(2002) (Dakar values) for ni in the range
c     0.331μm ≤ λ ≤ 0.36μm; Duboviket al. (2002) (Bahrain, Solar Village, Cape Verde)
c     for λ= 0.44μm and in therange 0.67μm ≤ λ ≤ 1.02μm; Shettle and Fenn (1979) for nr
c     and Kim et al.(2011) for ni for λ = 0.55. Between bands and wavelengths mentioned
c     but not covered by the above, refractive indices are found by use of linear
c     interpolation.
c
c     A new feature as of February 2020 is the optional simplified treatment of how size
c     distributions change as they age and grow by hygroscopic swelling. This AeroTab
c     light  version (light=.true.) is the same as the standard version except for the 3
c     subroutines conteqlight.f, rhsublight.f and modetilp-light.f, which replace the
c     standard code files conteq.f, rhsub.f and modetilp.f. The simplifying assumptions
c     are that all internal mixing is here homogeneous with respect to size (within a mixture/
c     mode, kcomp), regardless of production mechanism, and that the modified number size
c     distributions including growth remain log-normal. As in the aerosol microphysics schemes
c     in CESM (e.g. MAM3), we let the median modal radius change while keeping the standard
c     deviation constant. This is described in more detail in the "AeroTab user guide" found
c     at the NorESM github documentions repository.      
C ===================================================================================

      
      implicit none

ccccc6ccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5ccccccccc6ccccccccc7cc
      INTEGER  kcom, iband, ifile
      INTEGER  i, imax, imini, imaxi, is, icl, ib, ibcam, ictot, kcomp,
     $ kc, ksol, itot, ifbc, ifac, ifaq, irelh, iopt, ismolar, 
     $ ismolarh, irh, itilp, ifombg, ifbcbg
      INTEGER irelh1, irelh2, ictot1, ictot2, ifac1, 
     $ ifac2, ifbc1, ifbc2, ifaq1, ifaq2, ictote, ictote1, ictote2,
     $ ifombg1, ifombg2, ifbcbg1, ifbcbg2
      REAL r(0:100), rp(0:100), fki(-1:100), fracdim(0:100),  
     $ dndlrk0(0:100), dndlrk(0:100), dndlrkny(0:100), 
     $ vbci(0:100), voci(0:100), vsi(0:100), vai(0:100), vssol(0:100), 
     $ vbcsol(0:100), vocsol(0:100), vasol(0:100), vw(0:100) 
      REAL rk, rkny, rknywet, r0, rbcn, rcoag, d, ntot, Nnatk, Nnat,
     $ fcondk, fcoagk, faqk, logsk, logs0, 
     $ rhos, rhosv, rhoc2, rhobc, rhooc, rhob, rhow, th, mfv, diff,  
     $ Cac, Cabc, Caoc, Cas1, Cas2, Cas3, Caso4, Ctot, Cdry, dCtot, cat, 
     $ fac, fabc, faq, rh, numb, bcint, cintbg, 
     $ cintsu, cintsc, cintsa, cintbc, cintoc, cintbg05, cintsu05, 
     $ cintsc05, cintsa05, cintbc05, cintoc05, cintbg125, cintsu125, 
     $ cintsc125, cintsa125, cintbc125, cintoc125, aaero, aaeros, 
     $ aaerol, vaero, vaeros, vaerol, bclt05, bcgt125, lambda, alpha, 
     $ fombg, vombg, fbcbg, vbcbg, eps
      REAL frombg(6), frbcbg(6)
      REAL catote(16), catot(6), frac(6), frabc(6), fraq(6), relh(10)
      REAL omega(31), gass(31), bext(31), babs(31), kext(31)
      REAL xlam(31), xlami(32), xlamb(31), xlame(31),
     $ fband(31), fb(16)
      REAL Ctot0, Dm(0:100), Dmp(0:100), K12(0:101), Kp12(0:101),
     $ K12oc(0:101), Kp12oc(0:101), K12so4(0:101), Kp12so4(0:101), 
     $ Ctotnull                               
      REAL rcoag_so4n, rcoag_bcn, rcoag_ocn
      REAL xbc, xdst, xoc, xs, xa, xss, rhda, rhca, rhdss, rhcss
      REAL diffsoa, thsoa, mfvsoa, Dmsoa(0:100), Dmpsoa(0:100)
      REAL pi, e, testnumb 
      LOGICAL light, lightconteq, lightrhsub, extradiag
      COMPLEX cref(5,31)
      PARAMETER  (pi=3.141592654, e=2.718281828)
      PARAMETER (eps=1.e-50)

c     Assumed radius for coagulating (fine mode) particles in um  
co      PARAMETER (rcoag_so4n = 0.0118)  ! rk for kcomp=1
co      PARAMETER (rcoag_bcn  = 0.0118)  ! rk for kcomp=2
co      PARAMETER (rcoag_ocn  = 0.04)    ! rk for kcomp=3
      PARAMETER (rcoag_so4n = 0.025)   ! rk for kcomp=1
      PARAMETER (rcoag_bcn  = 0.025)   ! rk for kcomp=2
      PARAMETER (rcoag_ocn  = 0.06)    ! rk for kcomp=3

c     Do not modify the following input:
c     number of iterations in the Smolarkiewicz advection scheme, ismolar, and
c     a different ismolarh for hygroscopic growth than for dry distributions:
      PARAMETER  (ismolar=2, ismolarh=3)
c     No aportioning between modes (i.e., all material is added onto the same mode): 
c     (this is relevant to modify only when this code is part of a larger multimodal
c     scheme, i.e. for distribution of internally mixed mass onto more than one mode
c     at the time. The offline code for a multimodal scheme does has not been updated,
c     and is not part of this AeroTab package).      
      PARAMETER  (Nnatk=1.0, fcondk=1.0, fcoagk=1.0, faqk=1.0)

c     Modify the following input to create different sets of look-up tables:      
c     **************************************************************************
c     Let iopt=1 for optics tables, or iopt=0 for size distribution calculations 
c     (used in CCN activation in CAM4/5-Oslo and CAM6-Nor with prognostic CDNC):
      iopt=1
c     Lognormal mode fitting (itilp=1, iopt=0) --> logntilp*.out, or not:
      itilp=1-iopt  
c     Outout for iopt=1 --> lwkcomp*.out or kcomp*.out, aerodryk*.out, 
c     aerocomk*.out, and nkcomp*.out (for size distributions for all RH).
c     SW: ib=29 (ave.=>12) (CAMRT), or
c     SW: ib=31 (ave.=>14) (RRTMG) (Added November 2013), or
c     LW: ib=19 (ave.=>16) (RRTMG) (Added November 2013):
      ib=31
c     Use AeroTab light version (light=.true.) or the standard CMIP6 version?
      light=.false.
c     Take out extra diagnistics for inspection of number and volume size
c     distributions and optical parameters?
c      extradiag=.true.
      extradiag=.false.
c     **************************************************************************
      
C     Initialization and calculations of look-up tables starts here...       

      if(ib.eq.29) then
          write(*,*) 
     $ 'Note: for aerocomk*.out, aerodryk*.out or SW RRTMG, use ib=31'    
      endif

c     Define spectral bands and spectral solar fluxes (at TOA) to be used 
c     in Chandrasekhar averaging of the optical parameters (in sizemie):
      call specbands(ib, xlami, xlam, xlamb, xlame, fband, fb, ibcam)

c     Define constants and parameters for calculations of size distributions:
      call constsize(d, imax, imaxi, r, rp, r0, rbcn, logs0,
     $ rhobc, rhooc, rhos, rhosv, rhoc2, rhow, 
     $ bcint, fracdim, diff, th, mfv, diffsoa, thsoa, mfvsoa)

c     The main loop over aerosol mode number for background modes, kcomp=1,10, 
c     plus kcomp=0, for the fractal BC(ac) mode (use itot=0). For this mode no 
c     lognormal mode fitting is needed: it is assumed to be hydrophobic and 
c     therefore not giving any CCN or CDNC contribution.  

      do kcomp=0,10   ! for look-up tables, kcomp=0,10 (only kcomp=1-10 needed for logntilp*.out)

       if(kcomp.eq.0) then
          itot = 0     ! not subject to added mass by condensation etc.
       else
          itot = 1     ! subject to added mass by condensation etc.
       endif

c     Set parameters for prescribed initial dry lognormal size distributions,
c     and define the arrays for the input parameters to the look-up tables:
ccccc6ccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5ccccccccc6ccccccccc7cc
      call modepar(kcomp, ksol, imini, Nnat, rk, logsk, rhosv, rhob, 
     $ frombg,frbcbg,catot, catote, relh, frac, frabc, fraq, alpha)

c     Calculate dry background mode size distribution, dndlrk0, and 
c     dry aerosol contribution to mass concentration, Ctot0 (ug/m^3):
      call drydist(kcomp, Nnat, imini, imax, d, r, rk,  
     $ logsk, logs0, rhob, bcint, pi, dndlrk0, ntot, Ctot0)

      Ctotnull=Ctot0

c     Diffusion and coagulation coeffecients for the respective background
c     mode are then calculated. We here assume that the n/Aitken-modes which 
c     coagulate on a background mode are all monodisperse.
c
c     Diffusion coefficients Dm for H2SO4:
      call condsub (r, imax, diff, mfv, th, alpha, Dm)     
      call condsub (rp, imax, diff, mfv, th, alpha, Dmp)     
c     Diffusion coefficients Dm for SOA:
      call condsub (r, imax, diffsoa, mfvsoa, thsoa, alpha, Dmsoa)     
      call condsub (rp, imax, diffsoa, mfvsoa, thsoa, alpha, Dmpsoa)     
c     Coagulation coefficients K12 for H2SO4:
      call coagsub (r, imax, rcoag_so4n, rhob, rhosv, K12so4)   
      call coagsub (rp, imax, rcoag_so4n, rhob, rhosv, Kp12so4)   
c     Coagulation coefficients K12 for BC:
      call coagsub (r, imax, rcoag_bcn, rhob, rhobc, K12)   
      call coagsub (rp, imax, rcoag_bcn, rhob, rhobc, Kp12)   
c     Coagulation coefficients K12 for OC:
      call coagsub (r, imax, rcoag_ocn, rhob, rhooc, K12oc)   
      call coagsub (rp, imax, rcoag_ocn, rhob, rhooc, Kp12oc)   

c     Wavelength dependent complex rafractive indices (cref) 
c     are found from tabulated values for each aerosol component:
      call tabrefind (kcomp, ib, xlam, cref)

c     Aerosol hygroscopicities for max RH in the look-up tables (LUT),
c     for use as info in the header of each LUT:
      rh=relh(10)
      call hygro (rh, xbc, xdst, xoc, xs, xa, xss,
     $                      rhda, rhca, rhdss, rhcss)

c     Open output files for use in CAM-Oslo: 
      call openfiles(kcomp,iopt,ib,extradiag)

c     Add header info for all look-up tables for each kcomp:
      call tableinfo (kcomp, xbc, xdst, xoc, xs, xa, xss, relh, 
     $ frombg, frbcbg, catote, catot, frac, frabc, fraq, ib, ibcam, 
     $ itilp)


c          Editable input values    ! Full range for look-up tables
c               relh is RH given as a fraction
                irelh1 = 1          ! 1 - 10   Note:
                irelh2 = 10         ! 1 - 10   no loop if iopt=0

c               fombg is mass fractions of OM in the background 
                ifombg1 = 1         ! 1 - 6    Note:
                ifombg2 = 6         ! 1 - 6    loop only for kcomp=1

c               fbcbg is mass fractions of BC in the background 
                ifbcbg1 = 1         ! 1 - 6    Note:
                ifbcbg2 = 6         ! 1 - 6    loop only for kcomp=4

c               ctot is added mass to be internally mixed with the
c               background modes for kcomp=5-10
                ictot1 = 1          ! 1 - 6    Note: loop over       
                ictot2 = 6          ! 1 - 6    ictot OR ictote

c               ctote is added mass to be internally mixed with the
c               background modes for kcomp=1-4
                ictote1 = 1         ! 1 - 16   Note: loop over ictot      
                ictote2 = 16        ! 1 - 16   OR ictote, not both

c               fac is added OM+BC to total mass fractiom to be internally mixed
c               with the background modes for kcomp=1-10                
                ifac1 = 1           ! 1 - 6    Note:
                ifac2 = 6           ! 1 - 6    no loop if kcomp=0

c               fbc is added BC to OM&BC mass fractiom to be internally mixed
c               with the background modes for kcomp=5-10                
                ifbc1 = 1           ! 1 - 6    Note:
                ifbc2 = 6           ! 1 - 6    no loop if kcomp=0-4

c               faq is added wet-phase to total SO4 mass fractiom to be internally mixed
c               with the background modes for kcomp=1-10                
                ifaq1 = 1           ! 1 - 6    Note:
                ifaq2 = 6           ! 1 - 6    no loop if kcomp=0-3

c               Do not edit the following (default) input values! (it's safer to edit the do loops)
                if(iopt.eq.0) then  ! no RH loop, w e only need the new dry aerosol sizes
                 irelh1 = 1
                 irelh2 = 1
                endif
                if(kcomp.ne.1.or.itilp.eq.1) then  ! no OM (as SOA) internally mixed in the background
                 ifombg1 = 1
                 ifombg2 = 1
                endif
                if(kcomp.ne.4) then  ! no BC internally mixed in the background
                 ifbcbg1 = 1
                 ifbcbg2 = 1
                endif
                if(kcomp.eq.0) then  ! no internal mixing here
                 ifac1  = 1
                 ifac2  = 1
                 ifbc1  = 1
                 ifbc2  = 1
                 ifaq1  = 1
                 ifaq2  = 1
                 ictote1= 1
                 ictote2= 1
                 ictot1 = 1   
                 ictot2 = 1
                elseif(kcomp.ge.1.and.kcomp.le.3) then  ! no BC coagulation or cloud processing here 
                 ifbc1  = 1
                 ifbc2  = 1
                 ifaq1  = 1
                 ifaq2  = 1
                 ictot1 = 1   
                 ictot2 = 1
                elseif(kcomp.eq.4) then  ! background is OC and BC, and all added carbonaceous 
                 ifbc1  = 1              ! comes as SOA (fac=SOA/(SOA+Sulfate) added).
                 ifbc2  = 1              ! BC and OC is homogeneously mixed (wrt. r) in the 
                 ictot1 = 1              ! background. Added SO4 and SOA are distributed     
                 ictot2 = 1              ! according to D'(r) or r>rc (for sulfate), however. 
                else
                 ictote1= 1   
                 ictote2= 1 
                endif  ! kcomp
                if(itilp.eq.1) then      ! there is no fombg or fbcbg (size) dependency for itilp=1
                 ifombg1 = 1
                 ifombg2 = 1
                 ifbcbg1 = 1
                 ifbcbg2 = 1
                endif

c             Loop over RH and all process-specific indices. For manual changes below,
c             note that not all combinations are allowed (or taken into account, see above)                 
                
                
               do 540 irelh = irelh1, irelh2
c              do 540 irelh = 1,1

c              relative humidity:
               rh=relh(irelh)
               
cX              extra test loop for hygroscopic growth plots (with e.g. irelh=1,1 in the loop above)
c                do 540 irh=1,99,2
c                rh=0.01*real(irh)
c                or 
c                do 540 irh=1,199
c                rh=0.005*real(irh)
cX

c               calculate/define aerosol hygroscopicities (RH dependent) 
c               and points of deliquescence & crystallisation: 
                call hygro (rh, xbc, xdst, xoc, xs, xa, xss,
     $                      rhda, rhca, rhdss, rhcss)

              do 540 ifombg = ifombg1, ifombg2
c              do 540 ifombg = 1,1
               if(ifombg.gt.1.and.kcomp.ne.1) then
                 write(*,*) "Error: ifombg > 1 only for kcomp=1"  
                 stop   
               endif

              do 540 ifbcbg = ifbcbg1, ifbcbg2
c              do 540 ifbcbg = 1,1
               if(ifbcbg.gt.1.and.kcomp.ne.4) then
                 write(*,*) "Error: ifbcbg > 1 only for kcomp=4"  
                 stop   
               endif

                 
            do 540 ictot  = ictot1, ictot2
c            do 540 ictot = 1,1
            do 540 ictote = ictote1, ictote2
c           do 540 ictote = 1,1
             if(ictote.gt.1.and.(kcomp.eq.0.or.kcomp.gt.4)) then
               write(*,*) "Error: ictote > 1 only for kcomp=1-4"  
               stop   
             endif
             if(ictot.gt.1.and.(kcomp.le.4)) then
               write(*,*) "Error: ictot > 1 only for kcomp>4"  
               stop   
             endif
               
          do 540 ifac = ifac1, ifac2
c          do 540 ifac = 1,1
           if(ictot.gt.1.and.(kcomp.le.4)) then
             write(*,*) "Error: ifac > 1 only for kcomp>4"  
             stop   
           endif

             
        do 540 ifbc = ifbc1, ifbc2
c        do 540 ifbc = 1,1
         if(ictot.gt.1.and.(kcomp.le.4)) then
           write(*,*) "Error: ifbc > 1 only for kcomp>4"  
           stop   
         endif

      do 540 ifaq = ifaq1, ifaq2
c      do 540 ifaq = 1,1
        if(ictot.gt.1.and.(kcomp.le.3)) then
          write(*,*) "Error: ifaq > 1 only for kcomp>3"  
          stop   
        endif

c     Run-log output to screen and log-file        
      if(kcomp.eq.1) then
       write(*,*) 'kcomp,irelh,ifombg,ictote,ifac=',
     $ kcomp,irelh,ifombg,ictote,ifac
       write(999,*) 'kcomp,irelh,ifombg,ictote,ifac=',
     $ kcomp,irelh,ifombg,ictote,ifac
      elseif(kcomp.ge.1.and.kcomp.le.3) then
       write(*,*) 'kcomp,irelh,ictote,ifac=',kcomp,irelh,ictote,ifac
       write(999,*) 'kcomp,irelh,ictote,ifac=',kcomp,irelh,ictote,ifac
      elseif(kcomp.eq.4) then
       write(*,*) 'kcomp,irelh,ifbcbg,ictote,ifac,ifaq=',
     $ kcomp,irelh,ifbcbg,ictote,ifac,ifaq
       write(999,*) 'kcomp,irelh,ifbcbg,ictote,ifac,ifaq=',
     $ kcomp,irelh,ifbcbg,ictote,ifac,ifaq
      else 
       write(*,*) 'kcomp,irelh,ictot,ifac,ifbc,ifaq=',
     $ kcomp,irelh,ictot,ifac,ifbc,ifaq 
       write(999,*) 'kcomp,irelh,ictot,ifac,ifbc,ifaq=',
     $ kcomp,irelh,ictot,ifac,ifbc,ifaq 
      endif

cX     extra test loop for hygroscopic growth (with e.g., irelh=1,1 in the loop above)
c      do 540 irh=1,99
c      rh=0.01*real(irh)
c      do 540 irh=1,199
c      rh=0.005*real(irh)
cX

c     Basic input parameters to the table calculations:

c     Concentrations (ug/m^3, per background particle/cm^3) of internally mixed 
c     SO4, BC and OC. Cas1, Cas2, Cas3 and Caso4 is internally mixed SO4 from 
c     condensation (H2SO4), coagulation (H2SO4), cloud processing ((NH4)2SO4) 
c     and all of the above, respectively. Cabc and Caoc is all internally mixed 
c     BC and OC (from coagulation), respectively.
        Cas1=1.e-40 
        Cas2=1.e-40 
        Cas3=1.e-40
        Cabc=1.e-40
        Caoc=1.e-40
        if(kcomp.ge.1.and.kcomp.le.4) then
          Cac=frac(ifac)*catote(ictote)         !  added Carbonaceous from condensation (SOA)
        else
          Cac=frac(ifac)*catot(ictot)           !  added Carbonaceous from condensation (SOA) and coagulation 
        endif
        Cabc=frabc(ifbc)*Cac                    !  added BC from coagulation 
        if(Cabc.lt.1.e-40) Cabc=1.e-40  
        Caoc=(1.0-frabc(ifbc))*Cac              !  added OC from condensation and coagulation
        if(Caoc.lt.1.e-40) Caoc=1.e-40
        if(kcomp.ge.1.and.kcomp.le.4) then
          Caso4=(1.0-frac(ifac))*catote(ictote) !  added Sulfate from condensation (H2SO4)
        else
          Caso4=(1.0-frac(ifac))*catot(ictot)   !  added Sulfate from condensation and coagulation (H2SO4) and wet phase ((NH4)2SO4)
        endif
        if(Caso4.lt.1.e-40) Caso4=1.e-40        
        if(kcomp.ge.1.and.kcomp.le.4) then
          Cas1=(1.0-fraq(ifaq))*Caso4           !  added Sulfate from condensation (H2SO4)
          Cas2=1.e-40                           !  lump coagulation with condensation for these modes
        else
          Cas1=1.e-40                           !  lump condensation with coagulation for these modes
          Cas2=(1.0-fraq(ifaq))*Caso4           !  added Sulfate from coagulation (H2SO4)
        endif
        Cas3=fraq(ifaq)*Caso4                   !  added Sulfate from wet phase production ((NH4)2SO4)
        if(Cas1.lt.1.e-40) Cas1=1.e-40
        if(Cas2.lt.1.e-40) Cas2=1.e-40
        if(Cas3.lt.1.e-40) Cas3=1.e-40

      Caso4=Cas1+Cas2+Cas3                      !  Total added sulfate mass (H2SO4 and (NH4)2SO4)
      faq=fraq(ifaq)                            !  Wet-phase mass fraction (as (NH4)2SO4) of added sulfate 
      fac=frac(ifac)                            !  Carbonaceous mass fraction of total added mass
      fabc=frabc(ifbc)                          !  BC mass fraction of added carbonaceous mass
      fombg=frombg(ifombg)                      !  OM mass fraction in the background (SOA and H2SO4) for kcomp=1
      vombg=1.0/(1.0+(1.0-fombg)/(fombg*rhosv/rhooc+eps)) !  OM volume fraction in background for kcomp=1
      fbcbg=frbcbg(ifbcbg)                      !  BC mass fraction in the background (OC and BC) for kcomp=4  
      vbcbg=1.0/(1.0+(1.0-fbcbg)/(fbcbg*rhooc/rhobc+eps)) ! BC volume fraction in the background for kcomp=4 

      if(kcomp.ge.1.and.kcomp.le.10) then
c       contribution to total mass conentration (Ctot) from the background mode:
       if(kcomp.eq.1) then
        Ctot0=Ctotnull*(1.0+vombg*(rhooc/rhob-1.0)) ! -> Ctotnull*0.815 for ren OM (vombg=fombg=1).
       elseif(kcomp.eq.4) then
        Ctot0=Ctotnull*(1.0+vbcbg*(rhobc/rhob-1.0)) ! -> Ctotnull*1.2 for ren BC (vbcbg=fbcbg=1).
       endif
c      Output to the run-log file
        write(999,*) 'background contribution:'
        write(999,*) Ctot0
c       contribution to Ctot from internally mixed (non-background) H2SO4 and (NH4)2SO4
c       (note: only H2SO4 for kcomp=1 since ifaq=1 there)
        dCtot=Caso4 
        Ctot=Ctot0+dCtot
        write(999,*) 'sulfate contribution and acc. total (a, tot):'
        write(999,*) dCtot, Ctot
c       contribution to Ctot from internally mixed (non-background) BC
        dCtot=Cabc
        Ctot=Ctot+dCtot
        write(999,*) 'BC contribution and accumulated total (a, tot):'
        write(999,*) dCtot, Ctot
c       contribution to Ctot from internally mixed (non-background) OC
        dCtot=Caoc
        Ctot=Ctot+dCtot
        write(999,*) 'OC contribution and accumulated total (a, tot):'
        write(999,*) dCtot, Ctot
      else
        Ctot=Ctot0
      endif
      Cdry=Ctot
      write(*,*) 'dry Ctot =', Cdry
c
      
      if(kcomp.ge.1.and.kcomp.le.4) then
        cat=catote(ictote)
      else
        cat=catot(ictot)
      endif

c     Calculate modified dry size distributions for process specific 
c     SO4 and BC (and OC) internally mixed with the background aerosol:
       if(light) then
      call conteqlight (r, rbcn, d, imax, rhos, rhobc, rhooc,
     $ rhob, rhosv, Nnatk, fcondk, fcoagk, faqk,
     $ Cas1, Cas2, Cas3, Cabc, Caoc, Ctot0, dndlrk0, dndlrkny, ntot, 
     $ vbci, voci, vsi, vai, cintbg, cintsc, cintsa, cintbc, 
     $ cintoc, cintbg05, cintsc05, cintsa05, cintbc05, cintoc05, 
     $ cintbg125, cintsc125, cintsa125, cintbc125, cintoc125,
     $ aaero, aaeros, vaero, vaeros, fracdim, kcomp, vombg, vbcbg,
     $ rk, rkny, logsk, extradiag) 
       else
      call conteq (r, rp, rbcn, d, itot, imax, ictot, ictote, ifaq, 
     $ imini, rhos, rhobc, rhooc, rhob, rhosv, rhoc2,
     $ Nnatk, fcondk, fcoagk, faqk, Cas1, Cas2, Cas3, Cabc, Caoc, Ctot0,
     $ dndlrk0, dndlrkny, ntot, Dmsoa, Dmpsoa, Dm, Dmp, K12, Kp12, 
     $ K12oc, Kp12oc, K12so4, Kp12so4, ismolar, vbci, voci, vsi, vai, 
     $ cintbg, cintsc, cintsa, cintbc, cintoc, cintbg05, cintsc05, 
     $ cintsa05, cintbc05, cintoc05, cintbg125, cintsc125, cintsa125, 
     $ cintbc125, cintoc125, aaero, aaeros, vaero, vaeros, fracdim, 
     $ kcomp, vombg, vbcbg, fac, extradiag)
       endif
      
c     Hygroscopic growth is taken into account in subroutine rhsub,
c     for the given relative humidity (if itilp=0, i.e. iopt=1):
      if(ksol.eq.1.and.itilp.eq.0) then
       if(light) then
          call rhsublight(imax, rh, d, r, dndlrkny, vsi, vbci, voci, 
     $   vombg, vbcbg, vssol, vbcsol, vocsol, vasol, vw, fki, itot,
     $   rhow, Ctot, kcomp, faq, iopt, xbc, xdst, xoc, xs, xa, xss,
     $   rhda, rhca, rhdss, rhcss, rkny, rknywet, logsk, Nnatk,
     $   extradiag)
       else
          call rhsub (imax, rh, d, r, rp, dndlrkny, vsi, vbci, voci, 
     $   fombg, fbcbg, vombg, vbcbg, vssol, vbcsol, vocsol, vasol, 
     $   vw, fki, itot, rhos, rhosv, rhobc, rhooc, rhob, rhow, Ctot, 
     $   kcomp, ismolarh, cat, fac, fabc, faq, iopt, xbc, xdst, xoc,
     $   xs, xa, xss, rhda, rhca, rhdss, rhcss, extradiag)
        endif  
       endif

c     Tabulate aerosol size distributions after ageing and hygroscopic
c     growth, and check how well the total aerosol number is conserved,
c     i.e. how close numb is to 1:
      numb=0.0
      if(itot.eq.0) then
        do i=1,imax
          dndlrk(i)=dndlrkny(i)
          numb=numb+dndlrk(i)*d
          if(extradiag)
     $      write(9001,500) r(i), dndlrkny(i),  
     $      cat, fac, fabc, faq, rh, kcomp
        enddo
      else
        do i=1,imax
          if(dndlrkny(i).lt.0.0) then
            write(*,*) 'dndlrkny(i) < 0 !'
            stop
          endif
          numb=numb+dndlrkny(i)*d
          if(extradiag) then
c          Additional output (for testing purposes only)
           write(12,100) r(i), dndlrk0(i) 
           write(13,100) r(i), dndlrkny(i)
           write(14,100) r(i), dndlrkny(i)*(4.0*pi/3.0)*r(i)**3
          endif
           if(extradiag)
     $      write(9001,500) r(i), dndlrkny(i),  
     $      cat, fac, fabc, faq, rh, kcomp
        enddo
        if(itilp.eq.1) then
c        Make look-up tables for (fitted) lognormal mode parameters 
         if(light) then
          call modetilplight(cat, fac, fabc, faq, kcomp, rkny, logsk)
         else
          call modetilp(pi, imax, d, r, dndlrkny, dndlrk0,
     $     cat, fac, fabc, faq, kcomp)
        endif
       endif
       write(*,*) 'Wet ntot integrated =', numb
       write(999,*) 'Wet ntot integrated =', numb
      endif

c     Sizemie determines the spectral aerosol gross (size integrated) 
c     optical parameters (by calling the Mie code for each particle size), 
c     and writes the result to the look-up table files:
      if(iopt.eq.1) then 
      call sizemie(imini, imaxi, r, rbcn, d, vsi, vbci, voci, vai,
     $  vombg, fombg, vbcbg, fbcbg,                                          
     $  dndlrk, dndlrkny, kcomp, itot, ib, vssol, vbcsol, vocsol, vasol, 
     $  vw, fki, rh, Ctot, Nnat, cat, fac, fabc, faq, fracdim, xlam, 
     $  xlami, xlamb, xlame, fband, fb, cref, omega, gass, bext, kext,
     $  Cdry, extradiag)
      endif

      if(iopt.eq.1) then
c      Here comes the aerodryk*.out look-up tables:
      if(ib.eq.31.and.irelh.eq.1) then
       aaerol=aaero-aaeros                              
       vaerol=vaero-vaeros
       if(itot.eq.1) then                              
         if(kcomp.eq.1) then
           write(9600,2100) kcomp, fombg, cat, fac,
     $     cintbg, cintbg05, cintbg125, cintbc, cintbc05, cintbc125,  
     $     cintoc, cintoc05, cintoc125, cintsc, cintsc05, cintsc125,        
     $     cintsa, cintsa05, cintsa125, aaeros, aaerol, vaeros, vaerol          
         elseif(kcomp.eq.2.or.kcomp.eq.3) then         
           write(9600,2000) kcomp, cat, fac,
     $     cintbg, cintbg05, cintbg125, cintbc, cintbc05, cintbc125,  
     $     cintoc, cintoc05, cintoc125, cintsc, cintsc05, cintsc125,        
     $     cintsa, cintsa05, cintsa125, aaeros, aaerol, vaeros, vaerol          
         elseif(kcomp.eq.4) then
           write(9600,3000) kcomp, fbcbg, cat, fac, faq,
     $     cintbg, cintbg05, cintbg125, cintbc, cintbc05, cintbc125,  
     $     cintoc, cintoc05, cintoc125, cintsc, cintsc05, cintsc125,        
     $     cintsa, cintsa05, cintsa125, aaeros, aaerol, vaeros, vaerol          
         else  ! (kcomp=5-10))
           write(9600,3000) kcomp, cat, fac, fabc, faq, 
     $     cintbg, cintbg05, cintbg125, cintbc, cintbc05, cintbc125,  
     $     cintoc, cintoc05, cintoc125, cintsc, cintsc05, cintsc125,        
     $     cintsa, cintsa05, cintsa125, aaeros, aaerol, vaeros, vaerol          
         endif 
       else  ! itot (kcomp=0)
         write(9600,4000) kcomp, cintbg, cintbg05, cintbg125, 
     $     aaeros, aaerol, vaeros, vaerol          
       endif ! itot
      endif ! ib & relh

!     A rough check:       
      if(cintbg.ge.1.e100.or.cintoc.ge.1.e100.or.cintsa.ge.1.e100) then
        write(*,*) 'cintbg or cintoc or cintsa too large for format,' 
        stop
      endif

      endif  ! iopt

 540  continue  ! ifaq, ifbc, ifac, ictot/ictote, ifombg, ifbcbg, irelh

      enddo  ! kcomp
     
      do ifile=12,14
        close(ifile) 
      enddo   
      do ifile=40,44
        close(ifile) 
      enddo
      do ifile=60,64
        close(ifile) 
      enddo
      do ifile=132,136
        close(ifile) 
      enddo
      close(999) 
      do ifile=9000,9003
        close(ifile) 
      enddo
      close(9600) 

      call system('mkdir LUT-output')
      call system('mv *.out LUT-output/')
      call system('mkdir Extra-output')
      call system('mv *.dat Extra-output/')


 100  format(2(x,e10.4))
 500  format(6(x,e12.5),f7.2,I3)
 2000 format(I2,21e10.3)
 2100 format(I2,22e10.3)
 3000 format(I2,23e10.3)
 4000 format(I2,7e11.4)
       

      end
