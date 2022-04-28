      subroutine conteqlight (r, rbcn, d, imax, rhos, rhobc, rhooc,
     $ rhob, rhosv, Nnatk, fcondk, fcoagk, faqk,
     $ Cas1, Cas2, Cas3, Cabc, Caoc, Ctot0, dndlrk, dndlrkny, ntot, 
     $ vbci, voci, vsi, vai, cintbg, cintsc, cintsa, cintbc, 
     $ cintoc, cintbg05, cintsc05, cintsa05, cintbc05, cintoc05, 
     $ cintbg125, cintsc125, cintsa125, cintbc125, cintoc125,
     $ aaero, aaeros, vaero, vaeros, fracdim, kcomp, vombg, vbcbg,
     $ rk, rkny, logsk, extradiag) 

c **********************************************************************************
c     Created by Alf Kirkevåg.
c **********************************************************************************
c     New treatment for simplified code version, AeroTab light: We here
c     assume homogenous internal mixing (no r dependency) and that size
c     distributions remain log-normal (new R, but unhanged SIGMA)  
ccccc6ccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5ccccccccc6ccccccccc7cc

      implicit none

      INTEGER  i, imax, kcomp
       REAL r(0:100), dndlrk(0:100), dndlrkny(0:100), 
     $ cbg(0:100), csu12(0:100), csu3(0:100), 
     $ csu(0:100), cbc(0:100), coc(0:100), vbci(0:100), voci(0:100), 
     $ vsi(0:100), vai(0:100), fracdim(0:100), 
     $ rhorbc(0:100)
      REAL vcbg(100), vcbc(100), vcoc(100), vcsu12(100), vcsu3(100), 
     $ vcsu(100)
      REAL rbcn, d, Nnatk, ntot, nt, fcondk, fcoagk, faqk, fr, frcoag,
     $ dv, dvcon, dvcos, dvcoa, dvcoaoc, dvaq, dvbc, dvoc, dvaq0,
     $ dvs1, dvs2, dvs3, rhos, rhooc, rhobc, rhob, rhosv,
     $ Cas1, Cas2, Cas3, Caso4, Cabc, Caoc, Ctot0, 
     $ cintbg, cintsc, cintsa, cintbc, cintoc, cintbg05, cintsc05, 
     $ cintsa05, cintbc05, cintoc05, cintbg125, cintsc125, cintsa125, 
     $ cintbc125, cintoc125, dcintbg, vtot, aaero, aaeros, vaero, 
     $ vaeros, pi, e, p1, p2
      REAL dvsoa, dvconsoa, vombg, vbcbg, rhobg
      REAL rk, rkny, logsk, nk, dCtot, Ctot, lnsk, fv, v0, rhotot
      REAL vtotal(0:100)
      LOGICAL extradiag
      
      PARAMETER (pi=3.141592654, e=2.718281828 )

      Caso4=Cas1+Cas2+Cas3        ! total mass conc. of H2SO4 and (NH4)2SO4
      frcoag=Cas2/Caso4           ! (H2SO4 coagulate)/Caso4 
      fr=Cas3/Caso4               ! (wet-phase (NH4)2SO4)/Caso4

      
      write(*,*) 'Cas1,2,3=', Cas1, Cas2, Cas3
      write(*,*) 'fr, frcoag=', fr, frcoag

c     Process specific volumes per volume of dry air to be added (unit: 
c     (ug/m^3/cm^-3)/(ug/m^3)=cm^3): cloud processed sulfate (s3) is 
c     assumed to exist as (NH4)2SO4, while sulfate from diffusional growth 
c     (s1) and coagulation (s2) exists (like in the nucleation mode) as H2SO4
      dvs1=(fcondk/Nnatk)*1e-9*(1.0-frcoag-fr)*Caso4/rhosv  ! volume of H2SO4 condensate
      dvs2=(fcoagk/Nnatk)*1e-9*frcoag*Caso4/rhosv           ! volume of H2SO4 coagulate
      dvbc=(fcoagk/Nnatk)*1e-9*Cabc/rhobc                   ! volume of BC coagulate 
      if(kcomp.ge.1.and.kcomp.le.4) then        ! OC only comes as SOA 
        dvsoa=1.e-9*Caoc/rhooc
        dvoc=0.0
      else                                      ! SOA is lumped together with and treated as OC coagulate 
        dvsoa=0.0
        dvoc=(fcoagk/Nnatk)*1e-9*Caoc/rhooc
      endif
c      write(*,*) "dvbc, dvoc+dvsoa, dvs1, dvs2, dvs3 = ",
c     $ dvbc, dvoc+dvsoa, dvs1, dvs2, dvs3

c     We here calculate the simplified new number (log-normal) and mass concentration
c     distributions without loosing the process specific info, in order to get look-up
c     tables on the same form as with the detailed method in conteq.f       
c      
c     First, calculating added volume per normalized lognormal mode/mixture
      dvs3=(faqk/Nnatk)*1e-9*fr*Caso4/rhos         
      dv=dvs1+dvs2+dvs3+dvbc+dvoc+dvsoa
c     Then the new modal radius rkny from the old rk and the increase in total volume 
      lnsk=log(10**logsk)
c     write(*,*) "logsk, lnsk = ", logsk, lnsk
c     We need a conversion factor of 1.e-12 (um**3 to cm**3, as for dv above)       
      v0=1.0e-12*(4.0*pi/3.0)*rk**3.0*exp(4.5*(lnsk)**2)
c      write(*,*) "v0, dv = ", v0, dv
      fv=(v0+dv)/v0
c      write(*,*) "fv = ", fv
      rkny=rk*(fv)**(1.0/3.0)
      write(*,*) "rk, rkny = ", rk, rkny

c     The new log-normal number size distribution dndlrkny is calculated
c     and tested for conservation of particle number      
      ntot=0.0
      do i=0,imax
        nk=(1.0/logsk)*exp(-0.5*(log10(r(i)/rkny)/logsk)**2.0)
        dndlrkny(i)=Nnatk*nk/sqrt(2.0*pi)
        ntot=ntot+dndlrkny(i)*d
      enddo
c      write(*,*) 'ntot =', ntot

c     Then calculate total (mean) particle mass density (for kcomp.gt.0)
      rhotot = (v0*rhob+(dvs1+dvs2)*rhosv+dvs3*rhos+dvbc*rhobc
     $          +(dvoc+dvsoa)*rhooc)/(v0+dv)
      write(*,*) 'rhotot =', rhotot
     
c     The mass integrated dndlrkny is calculated for the purpose of
c     testing for conservation of particle mass (kcomp=0 is OK already)     
      Ctot=0.0
      if(kcomp.ne.0) then
        do i=0,imax  
          dCtot=1.0e-3*(4.0*pi/3.0)*r(i)**3.0*(rhotot*dndlrkny(i))*d  
          Ctot=Ctot+dCtot
        enddo
        write(*,*) 'Total dry mass conc.  = ', Ctot
      endif
            
c     Correct mass densities for internally mixed background aerosols
      if(kcomp.eq.1) then
        rhobg=rhob*(1.0+vombg*(rhooc/rhob-1.0))
      elseif(kcomp.eq.4) then
        rhobg=rhob*(1.0+vbcbg*(rhobc/rhob-1.0))
      else
        rhobg=rhob
      endif

c     Initialize arrays for mass concentrations of the background aerosol 
      do i=1,imax
        vtotal(i)=1.0e-3*(4.0*pi/3.0)*r(i)**3.0*dndlrkny(i)
c        mass concentrations (ug/m^3) for the normalized mode
        cbg(i)=rhobg*v0/(v0+dv)*vtotal(i)                                
        cbc(i)=rhobc*dvbc/(v0+dv)*vtotal(i)
        coc(i)=rhooc*(dvoc+dvsoa)/(v0+dv)*vtotal(i)
        csu12(i)=rhosv*(dvs1+dvs2)/(v0+dv)*vtotal(i)
        csu3(i)=rhos*dvs3/(v0+dv)*vtotal(i)   
      enddo

c     Check if total dry aerosol number is conserved and calculate
c     aerosol area and volume, total and below 0.5um (thereby implicitely
c     also above 0.5um), for AEROCOM diagnostics
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
c     Additional output (for testing purposes only)
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
