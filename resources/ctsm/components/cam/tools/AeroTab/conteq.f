      subroutine conteq (r, rp, rbcn, d, itot, imax, ictot, 
     $ ictote, ifaq, imini, rhos, rhobc, rhooc, rhob, 
     $ rhosv, rhoc2, Nnatk, fcondk, fcoagk, faqk, Cas1, Cas2, Cas3, 
     $ Cabc, Caoc, Ctot0, dndlrk, dndlrkny, ntot, Dmsoa, Dmpsoa, Dm,
     $ Dmp, K12in, Kp12in, K12ocin, Kp12ocin, K12so4in, Kp12so4in, 
     $ ismolar, vbci, voci, vsi, vai, cintbg, cintsc, cintsa, cintbc, 
     $ cintoc, cintbg05, cintsc05, cintsa05, cintbc05, cintoc05, 
     $ cintbg125, cintsc125, cintsa125, cintbc125, cintoc125, aaero, 
     $ aaeros, vaero, vaeros, fracdim, kcomp, vombg, vbcbg, fac,
     $ extradiag) 

c **********************************************************************************
c     Created by Alf Kirkevåg.
c **********************************************************************************

c     Here the modified dry size distributions for process specific SO4, BC and OC
c     internally mixed with the background aerosol are calculated. 
ccccc6ccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5ccccccccc6ccccccccc7cc

      implicit none

      INTEGER  i, ic, imax, ix, imini, itot, ictot, ictote, 
     $ ismolar, ifaq, j, jmax, jmaxx, k, kcomp, itest, incjmax
      REAL K12in(0:101), Kp12in(0:101), K12ocin(0:101), 
     $ Kp12ocin(0:101),  K12so4in(0:101), Kp12so4in(0:101),
     $ K12(0:101), Kp12(0:101), K12oc(0:101), Kp12oc(0:101), 
     $ K12so4(0:101), Kp12so4(0:101)
       REAL r(0:100), rp(0:100), dip(0:100), Dm(0:100), 
     $ Dmp(0:100), dninc(0:100), dndlrk(0:100), dndlrkny(0:100), 
     $ dncny(0:100), cbg(0:100), csu12(0:100), csu3(0:100), 
     $ csu(0:100), cbc(0:100), coc(0:100), vbci(0:100), voci(0:100), 
     $ vsi(0:100), vai(0:100), fracdim(0:100), 
     $ rhorbc(0:100), Dmsoa(0:100), Dmpsoa(0:100)  
      REAL vcbg(100), vcbc(100), vcoc(100), vcsu12(100), vcsu3(100), 
     $ vcsu(100), dqsu12(100), dqsu3(100), dqbc(100), dqoc(100), 
     $ dcincbg(100), dcincs12(100), dcincs3(100), dcincbc(100), 
     $ dcincoc(100)
      REAL rbcn, rc, rcmin, rcmax,  rjm, rjmg, rjmax, rjmaxx, d,
     $ Nnatk, ntot, nt, Nag, NrD, NK12, NK12oc, NK12so4, fcondk, fcoagk, 
     $ faqk, fr, frcoag, radikand, dv, dvcon, dvcos, dvcoa, dvcoaoc, 
     $ dvaq, dvbc, dvoc, dvaq0, dvs1, dvs2, dvs3, rhos, rhooc, rhobc, 
     $ rhob, rhosv, rhoc2, Cas1, Cas2, Cas3, Caso4, Cabc, Caoc, Ctot0, 
     $ cintbg, cintsc, cintsa, cintbc, cintoc, cintbg05, cintsc05, 
     $ cintsa05, cintbc05, cintoc05, cintbg125, cintsc125, cintsa125, 
     $ cintbc125, cintoc125, dcintbg, vtot, aaero, aaeros, vaero, 
     $ vaeros, pi, e, p1, p2, fac
      REAL NrDsoa, dvsoa, dvconsoa, vombg, vbcbg, rhobg
      LOGICAL extradiag
      
c     Critical radius for cloud processing, rc, ranging between rcmin and 
c     rcmax (and smoothed over this range), from Chuang and Penner (1995). 
c     We have assumed that this range is independent of background aerosol 
      PARAMETER (rcmin=0.05, rcmax=0.2)

      PARAMETER (pi=3.141592654, e=2.718281828 )

      Caso4=Cas1+Cas2+Cas3        ! total mass conc. of H2SO4 and (NH4)2SO4
      frcoag=Cas2/Caso4           ! (H2SO4 coagulate)/Caso4 
      fr=Cas3/Caso4               ! (wet-phase (NH4)2SO4)/Caso4

c     Some array initializations:      
      do i=0,imax
         K12(i)    = K12in(i)
         Kp12(i)   = Kp12in(i)
         K12oc(i)  = K12ocin(i)
         Kp12oc(i) = Kp12ocin(i)
         K12so4(i) = K12so4in(i)
         Kp12so4(i)= Kp12so4in(i)
      enddo

c     Initial guess of jmax, the maximum required iterations to satisfy
c     the stability criterion for the continuity equation:
      jmaxx=10000 

c     Initially, modified size distribution = background size distribution 
      itest=0
      incjmax=0   
 11   do i=0,imax                     
        dndlrkny(i)=dndlrk(i)
      enddo 
      if(itot.eq.0) then
        ix=0                  
      else
        if(itest.eq.0) ix=1  
      endif
 12   do i=0,imax
       if(i.le.ix) then
         K12(i)=0.0
         Kp12(i)=0.0
         K12oc(i)=0.0
         Kp12oc(i)=0.0
         K12so4(i)=0.0
         Kp12so4(i)=0.0
         dndlrkny(i)=1e-50
       endif
      enddo

c     Below, condensation of H2SO4 or SOA and coagulation of BC, OC and SO4 aerosol
c     onto the background distribution for the first time step is calculated   
c     For SOA, add condensation --> NrDsoa (NrD is for so4 only)!
      NrD=0.0             ! H2SO4
      NrDsoa=0.0          ! SOA
      NK12=0.0            ! BC
      NK12oc=0.0          ! OC (OM)
      NK12so4=0.0         ! H2SO4
      do i=0,imax
        NrD=NrD+dndlrkny(i)*Dmp(i)*rp(i)*d           ! unit 1.e-12 s^-1 
        NrDsoa=NrDsoa+dndlrkny(i)*Dmpsoa(i)*rp(i)*d  ! unit 1.e-12 s^-1
        NK12=NK12+dndlrkny(i)*Kp12(i)*d              ! unit 1.e-12 s^-1 
        NK12oc=NK12oc+dndlrkny(i)*Kp12oc(i)*d        ! unit 1.e-12 s^-1
        NK12so4=NK12so4+dndlrkny(i)*Kp12so4(i)*d     ! unit 1.e-12 s^-1 
      enddo

c     Process specific volumes per volume of dry air to be added (unit: 
c     (ug/m^3/cm^-3)/(ug/m^3)=cm^3): cloud processed sulfate (s3) is 
c     assumed to exist as (NH4)2SO4, while sulfate from diffusional growth 
c     (s1) and coagulation (s2) exists (like in the nucleation mode) as H2SO4
      dvs1=(fcondk/Nnatk)*1e-9*(1.0-frcoag-fr)*Caso4/rhosv  ! volume of H2SO4 condensate
      dvs2=(fcoagk/Nnatk)*1e-9*frcoag*Caso4/rhosv           ! volume of H2SO4 coagulate
      dvbc=(fcoagk/Nnatk)*1e-9*Cabc/rhobc                   ! volume of BC coagulate 
      if(kcomp.ge.1.and.kcomp.le.4) then        ! OC only comes as SOA 
        dvsoa=1.e-9*Caoc/rhooc
        dvoc=1.e-50
      else                                      ! SOA is lumped together with and treated as OC coagulate 
        dvsoa=1.e-50
        dvoc=(fcoagk/Nnatk)*1e-9*Caoc/rhooc
      endif

        if(incjmax.eq.0) then

c     Searching for sufficiently large total number of iterations, 
c     jmax (>10000), to satisfy the stability criterium for the 
c     continuity equation. If incjmax=1 (last guess was too small), 
c     jmax is doubled. Also estimate the necessary amount of 
c     "moves to the right" (ix), to facilitate a solution.
      rjmg=0.0
      do i=ix+1,imax

          dvcon=dvs1*rp(i)*Dmp(i)/NrD        ! as H2SO4 
          dvcos=dvs2*Kp12so4(i)/NK12so4      ! as H2SO4
          dvcoa=dvbc*Kp12(i)/NK12  
          dvcoaoc=dvoc*Kp12oc(i)/NK12oc

c       dv* unit: ug/m^3/(ug/m^3/cm^-3)=cm^3
        dvcon=dvs1*rp(i)*Dmp(i)/NrD
        dvcos=dvs2*Kp12so4(i)/NK12so4
        dvcoa=dvbc*Kp12(i)/NK12
        dvcoaoc=dvoc*Kp12oc(i)/NK12oc
        dvconsoa=dvsoa*rp(i)*Dmpsoa(i)/NrDsoa
        dv=dvcon+dvcoa+dvcos+dvcoaoc+dvconsoa
c
        rjm=3e12*dv/(4.0*pi*r(i)**3.0*((1.0+d/log10(e))**3.0-1.0))
        rjm=rjm/(1.01-fr)
        if(rjm.gt.rjmg) then
          rjmax=rjm
c          write(*,*) i, r(i), rjmax 
        endif
        rjmg=rjm
      enddo
      rjmaxx=1.0*jmaxx
      if(rjmax.gt.rjmaxx) then
        ix=ix+1
        if(ix.gt.imini) then
          itest=1
          jmaxx=jmaxx+jmaxx
c          write(*,*) jmaxx
          goto 11
        endif
        goto 12
      endif
      jmax=int(rjmax)+1
      if(jmax.lt.10000.and.(ictot.gt.1.or.ictote.gt.1)) then
        jmax=10000
        if(ifaq.eq.6) jmax=20000
      endif
      write(*,*) 'jmax, ix =', jmax, ix 

        endif  ! incjmax


c     Process specific volumes of SO4, BC and OC per volume of dry air 
c     to be added PER ITERATION is then determined:     
      dvs1=(fcondk/Nnatk)*1e-9*(1.0-frcoag-fr)*Caso4
     $     /(rhosv*jmax)                                        
      dvs2=(fcoagk/Nnatk)*1e-9*frcoag*Caso4/(rhosv*jmax)
      dvs3=(faqk/Nnatk)*1e-9*fr*Caso4/(rhos*jmax)         
      dvbc=(fcoagk/Nnatk)*1e-9*Cabc/(rhobc*jmax)                           
      if(kcomp.ge.1.and.kcomp.le.4) then
        dvsoa=1e-9*Caoc/(rhooc*jmax)
        dvoc=1.e-50
      else
        dvsoa=1.e-50
        dvoc=(fcoagk/Nnatk)*1e-9*Caoc/(rhooc*jmax)
      endif

c     Initialize arrays for mass concentrations of the background aerosol 
c     (after correcting for internal mixtures in the background aerosol)...
      if(kcomp.eq.1) then
        rhobg=rhob*(1.0+vombg*(rhooc/rhob-1.0))
      elseif(kcomp.eq.4) then
        rhobg=rhob*(1.0+vbcbg*(rhobc/rhob-1.0))
      else
        rhobg=rhob
      endif
      do i=1,imax
        cbg(i)=1.0e-3*(4.0*pi/3.0)*r(i)**3.0*(rhobg*dndlrkny(i))  ! ug/m^3
      enddo

c     ... and initialize arrays for internally mixed 
c     BC, H2SO4 and OC (POM or SOA) from condensation and coagulation 
c     and (NH4)2SO4 from cloud processing
      do i=0,imax
        cbc(i)=1.0e-100
        coc(i)=1.0e-100
        csu12(i)=1.0e-100
        csu3(i)=1.0e-100                          
      enddo

c     Then solve continuity equation using jmax time steps/iterations 
      do 20 j=1,jmax
c
        rc=rcmin+j*(rcmax-rcmin)/jmax

c       Initialization of key variables for each time step
        NrD=0.0             ! H2SO4
        NrDsoa=0.0          ! SOA
        NK12=0.0            ! BC
        NK12oc=0.0          ! OC (OM)
        NK12so4=0.0         ! H2SO4
        Nag=0.0             ! (NH4)2SO4
        k=0
c       Variables for growth by condensation and coagulation
        do i=1,imax
          if(i.le.ix.or.dndlrkny(i).lt.1.e-50) dndlrkny(i)=1.0e-50  ! fix
          NrD=NrD+dndlrkny(i)*Dmp(i)*rp(i)*d           ! unit 1.e-12 s^-1   
          NrDsoa=NrDsoa+dndlrkny(i)*Dmpsoa(i)*rp(i)*d  ! unit 1.e-12 s^-1
          NK12=NK12+dndlrkny(i)*Kp12(i)*d              ! unit 1.e-12 s^-1 
          NK12oc=NK12oc+dndlrkny(i)*Kp12oc(i)*d        ! unit 1.e-12 s^-1
          NK12so4=NK12so4+dndlrkny(i)*Kp12so4(i)*d     ! unit 1.e-12 s^-1 
          if(rp(i).ge.rc) k=k+1        
          if(k.eq.1) ic=i
        enddo
c       Variables for growth by cloud processing (wetphase chemistry) 
        Nag=dndlrkny(ic)*log10(rp(ic)/rc)
          if(Nag.lt.0.0) write(*,*) 'ic, dndlrkny =', ic, dndlrkny(ic)
        do i=ic+1,imax
          Nag=Nag+dndlrkny(i)*d
        enddo
        dvaq0=dvs3/Nag      ! as (NH4)2SO4

c       Calculate process specific volumes of SO4 aerosol, BC and OC 
c       per volume of dry air to be added (per particle) in each size bin
        do i=1,imax       
          if(i.lt.ic) then
            dvaq=0.0
          elseif(i.eq.ic) then
            dvaq=dvaq0*(log10(rp(ic)/rc))/d
          elseif(i.ge.ic+1) then
            dvaq=dvaq0
          endif
          dvcon=dvs1*rp(i)*Dmp(i)/NrD        ! as H2SO4 
          dvcos=dvs2*Kp12so4(i)/NK12so4      ! as H2SO4
          dvcoa=dvbc*Kp12(i)/NK12  
          dvcoaoc=dvoc*Kp12oc(i)/NK12oc
          dvconsoa=dvsoa*rp(i)*Dmpsoa(i)/NrDsoa
          dv=dvcon+dvaq+dvcoa+dvcos+dvcoaoc+dvconsoa
c         Find the increment of log(r/um) at r=rp, i.e. in the center 
c         of the size bin, dip
          radikand=1.0+3.0e12*dv/(4.0*pi*rp(i)**3.0)
          dip(i)=log10(e)*(radikand**(1/3.0)-1.0)
          if(dip(i).lt.0.0) write(*,*) 'r, dip =', r(i), dip(i)
c         Process specific mass concentration increments (ug/m^3)
          dqbc(i)=1e9*rhobc*dvcoa*dndlrkny(i)
          dqoc(i)=1e9*rhooc*(dvcoaoc+dvconsoa)*dndlrkny(i)
          dqsu12(i)=1e9*rhosv*(dvcon+dvcos)*dndlrkny(i)  ! as H2SO4
          dqsu3(i)=1e9*rhos*dvaq*dndlrkny(i)             ! as (NH4)2SO4
        enddo

c       Finally solve the continuity equations (using a simple upwind  
c       advection scheme) for the size distribution, dndlrkny, and for 
c       the process specific mass concentrations, dcinc*
        dip(0)=0.0
        do i=1,imax       
          dninc(i)=-(dndlrkny(i)*dip(i)-dndlrkny(i-1)*dip(i-1))/d
          dcincbg(i)=-(cbg(i)*dip(i)-cbg(i-1)*dip(i-1))/d
         if(ismolar.eq.0) then
          dcincbc(i)=-(cbc(i)*dip(i)-cbc(i-1)*dip(i-1))/d+dqbc(i)
          dcincoc(i)=-(coc(i)*dip(i)-coc(i-1)*dip(i-1))/d+dqoc(i)
          dcincs12(i)=-(csu12(i)*dip(i)-csu12(i-1)*dip(i-1))/d
     $                +dqsu12(i)
          dcincs3(i)=-(csu3(i)*dip(i)-csu3(i-1)*dip(i-1))/d+dqsu3(i)
         else
          dcincbc(i)=-(cbc(i)*dip(i)-cbc(i-1)*dip(i-1))/d
          dcincoc(i)=-(coc(i)*dip(i)-coc(i-1)*dip(i-1))/d
          dcincs12(i)=-(csu12(i)*dip(i)-csu12(i-1)*dip(i-1))/d
          dcincs3(i)=-(csu3(i)*dip(i)-csu3(i-1)*dip(i-1))/d
         endif
        enddo
        do i=1,imax       
          dndlrkny(i)=dndlrkny(i)+dninc(i)
          if(dndlrkny(i).lt.1.e-99) dndlrkny(i)=1.e-99 
          cbg(i)=cbg(i)+dcincbg(i)          
          coc(i)=coc(i)+dcincoc(i)          
          cbc(i)=cbc(i)+dcincbc(i)          
          csu12(i)=csu12(i)+dcincs12(i)          
          csu3(i)=csu3(i)+dcincs3(i)          
          csu(i)=csu12(i)+csu3(i)          
c
          cbg(i)=max(cbg(i),0.0)
          coc(i)=max(coc(i),0.0)
          cbc(i)=max(cbc(i),0.0)
          csu12(i)=max(csu12(i),0.0)
          csu3(i)=max(csu3(i),0.0)
          csu(i)=max(csu12(i),0.0)
c
        enddo

c       Here the anti-diffusive part of the upwind scheme by 
c       Smolarkiewicz (1983) kicks in, providing that the  
c       number of corrective steps is chosen larger than 0 
        if(ismolar.gt.0) then
c         size distribution (number concentration)
          call smolar (ismolar, imax, d, dndlrkny, dip)  
c         background mass concentration
          call smolar (ismolar, imax, d, cbg, dip)  
c         non-backgrond OC mass concentration
          do i=1,imax
            dncny(i)=coc(i)
          enddo
          call smolar (ismolar, imax, d, dncny, dip)  
          do i=1,imax
            coc(i)=dncny(i)+dqoc(i)
          enddo
c         non-backgrond BC mass concentration
          do i=1,imax
            dncny(i)=cbc(i)
          enddo
          call smolar (ismolar, imax, d, dncny, dip)  
          do i=1,imax
            cbc(i)=dncny(i)+dqbc(i)
          enddo
c         process specific and total (non-backgrond) H2SO4 or/and (NH4)2SO4 mass concentrations
          do i=1,imax
            dncny(i)=csu12(i)    ! as H2SO4
          enddo
          call smolar (ismolar, imax, d, dncny, dip)  
          do i=1,imax
            csu12(i)=dncny(i)+dqsu12(i)
          enddo
          do i=1,imax
            dncny(i)=csu3(i)     ! as (NH4)2SO4
          enddo
          call smolar (ismolar, imax, d, dncny, dip)  
          do i=1,imax
            csu3(i)=dncny(i)+dqsu3(i) 
            csu(i)=csu12(i)+csu3(i)   ! as H2SO4 + (NH4)2SO4 mass       
          enddo
        endif  ! ismolar

 20   continue    ! j=1,jmax 

c     Check if total dry aerosol number is conserved
c     and calculate aerosol area and volume, total and below 0.5um
c     (thereby implicitely also above 0.5um), for AEROCOM diagnostics
      nt=0.0
      aaero=0.0
      vaero=0.0
      p1=4.0*pi
      p2=p1/3.0
      aaeros=0.99*p1*r(28)**2*dndlrkny(28)*d
      vaeros=0.99*p2*r(28)**3*dndlrkny(28)*d
      do i=1,imax       
        nt=nt+dndlrkny(i)*d
        aaero=aaero+p1*r(i)**2*dndlrkny(i)*d
        vaero=vaero+p2*r(i)**3*dndlrkny(i)*d
        if(i.le.27) then
          aaeros=aaeros+p1*r(i)**2*dndlrkny(i)*d
          vaeros=vaeros+p2*r(i)**3*dndlrkny(i)*d
        endif
      enddo  
      write(*,*) 'Nt / Ntot = :', nt / ntot
c     Accept bigger number conservation error for extreme cases
      if(((nt/ntot.lt.0.95.or.nt/ntot.gt.1.05).and.jmax.le.300000)
     $.or.((nt/ntot.lt.0.1.or.nt/ntot.gt.1.05).and.jmax.gt.300000)) then
        jmax=jmax*2
        incjmax=1
        write(*,*) 'jmax_ny, ix =', jmax, ix 
        goto 11
      endif

c     Size-integrated dry mass concentrations, integrated over all r,
c     and r<0.5um and r>1.25um (for AEROCOM).         
      cintbg=0.0
      cintbc=0.0
      cintoc=0.0
      cintsc=0.0
      cintsa=0.0
      cintbg05=0.99*cbg(28)*d
      cintbc05=0.99*cbc(28)*d
      cintoc05=0.99*coc(28)*d
      cintsc05=0.99*csu12(28)*d   ! as H2SO4
      cintsa05=0.99*csu3(28)*d    ! as (NH4)2SO4
      cintbg125=0.03*cbg(31)*d
      cintbc125=0.03*cbc(31)*d
      cintoc125=0.03*coc(31)*d
      cintsc125=0.03*csu12(31)*d  ! as H2SO4
      cintsa125=0.03*csu3(31)*d   ! as (NH4)2SO4
      do i=1,imax
        if(cbg(i).lt.1.e-100)   cbg(i)=1.e-100
        if(cbc(i).lt.1.e-100)   cbc(i)=1.e-100
        if(coc(i).lt.1.e-100)   coc(i)=1.e-100
        if(csu12(i).lt.1.e-100) csu12(i)=1.e-100
        if(csu3(i).lt.1.e-100)  csu3(i)=1.e-100
        csu(i)=csu12(i)+csu3(i)   ! as H2SO4 + (NH4)2SO4 mass        
        cintbg=cintbg+cbg(i)*d
        cintbc=cintbc+cbc(i)*d
        cintoc=cintoc+coc(i)*d
        cintsc=cintsc+csu12(i)*d  ! as H2SO4
        cintsa=cintsa+csu3(i)*d   ! as (NH4)2SO4
        if(i.le.27) then
          cintbg05=cintbg05+cbg(i)*d
          cintbc05=cintbc05+cbc(i)*d
          cintoc05=cintoc05+coc(i)*d
          cintsc05=cintsc05+csu12(i)*d  ! as H2SO4
          cintsa05=cintsa05+csu3(i)*d   ! as (NH4)2SO4
        endif
        if(i.ge.32) then
          cintbg125=cintbg125+cbg(i)*d
          cintbc125=cintbc125+cbc(i)*d
          cintoc125=cintoc125+coc(i)*d
          cintsc125=cintsc125+csu12(i)*d  ! as H2SO4
          cintsa125=cintsa125+csu3(i)*d   ! as (NH4)2SO4
        endif
      enddo
c***************** special treatment for kcomp=0 *************
      if(kcomp.eq.0) then
        do i=0,imax
          if(r(i).le.rbcn) then
            rhorbc(i)=rhobc
          else
            rhorbc(i)=rhobc*(rbcn/r(i))**(3.0-fracdim(i))
          endif
c          write(30,*) r(i), rhorbc(i), fracdim(i)
        enddo
        cintbg   =0.0
        cintbg05 =0.99*1.0e-3*(4.0*pi/3.0)*r(28)**3.0
     $                        *(rhorbc(28)*dndlrk(28))*d 
        cintbg125=0.03*1.0e-3*(4.0*pi/3.0)*r(31)**3.0
     $                        *(rhorbc(31)*dndlrk(31))*d 
        do i=0,imax
          dcintbg=1.0e-3*p2*r(i)**3.0*(rhorbc(i)*dndlrk(i))*d  
          cintbg=cintbg+dcintbg
          if(i.le.27) cintbg05 =cintbg05 +dcintbg
          if(i.ge.32) cintbg125=cintbg125+dcintbg
        enddo
      endif
c*************************************************************
      write(*,*) 'Cbc, Coc, Csu12, Csu3 og Cbg ='
      write(*,*) cintbc, cintoc, cintsc, cintsa, cintbg       
      write(*,*) 'Ctot integrated / Ctot in =', 
     $ (cintbc + cintoc + cintsc + cintsa + cintbg) /      
     $ (Ctot0 + Caso4 + Cabc + Caoc) 

      write(999,*) 'Ntot integrated / Ntot in =', nt / ntot
      write(999,*) 'Ctot integrated / Ctot in =', 
     $ (cintbc + cintoc + cintsc + cintsa + cintbg) /      
     $ (Ctot0 + Caso4 + Cabc + Caoc) 

c      write(1001,*) 'cintoc/Caoc = ', cintoc/Caoc 
c      write(1002,*) 'cintoc, Caoc = ', cintoc, Caoc 
 
c     Dry volume fractions for H2SO4+(NH4)2SO4, vsi, soot, vbci, oc, voci,
c     and background aerosol, vai. Note that vsi+vbci+voci+vai=1.  
      do i=1,imax 
       vtot=cbc(i)/rhobc+coc(i)/rhooc+csu12(i)/rhosv+csu3(i)/rhos
     $      +cbg(i)/rhobg 
       vcbg(i)=(cbg(i)/rhobg)/vtot
       vcbc(i)=(cbc(i)/rhobc)/vtot
       vcoc(i)=(coc(i)/rhooc)/vtot
       vcsu12(i)=(csu12(i)/rhosv)/vtot
       vcsu3(i)=(csu3(i)/rhos)/vtot
       vcsu(i)=vcsu12(i)+vcsu3(i) 
       vai(i)=vcbg(i)                     ! background (sulfate, OC, BC, SS or DU, or a mixture of two both for kcomp=1&4)
       vbci(i)=vcbc(i)                    ! non-background BC
       voci(i)=vcoc(i)                    ! non-background OC
       vsi(i)=vcsu(i)                     ! non-background sulfate
      enddo
      if(extradiag) then
       do i=1,imax 
        write(60,*) r(i), vsi(i)
        write(61,*) r(i), vbci(i)
        write(62,*) r(i), voci(i)
        write(63,*) r(i), vai(i) 
       enddo
      endif
       
      return
      end  
