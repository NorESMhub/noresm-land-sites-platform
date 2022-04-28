      subroutine sizemie (imin, imax, r, rbcn, d, vsi, vbci, voci, vai, 
     $ vombg, fombg, vbcbg, fbcbg, dndlrk, dndlrkny, kcomp, itot, ib, 
     $ vssol, vbcsol, vocsol, vasol, vw, fki, rh, Ctot, Nnat, catot, 
     $ fac, fabc, faq, fracdim, xlam, xlami, xlamb, xlame, 
     $ fband, fb, cref, omega, gass, bext, kext, Cdry, extradiag) 

c **********************************************************************************
c     Created by Alf Kirkevåg.
c **********************************************************************************

c     Sizemie: determines the spectral aerosol gross optical parameters by 
c     calling the Mie code for each particle size (with subsequent integration 
c     over the size distribution), and writes the result to file.

      implicit none

      LOGICAL  ANYANG, PERFCT, PRNT(2)
      INTEGER  IPOLZN(4,2), NUMMOM, i, imax, j, k, ntype, kcomp,
     $         itot, iband, ib, imin, numang, nmom, MAXANG, MOMDIM
      real     pi, e, lam 
      real     xlam(31), xlami(32), xlamb(31), xlame(31) 
      PARAMETER  (MAXANG=40, MOMDIM=10, pi=3.141592654, e=2.718281828)
      REAL     MIMCUT, PMOM(0:MOMDIM,4), XMU(MAXANG), XX(2), Cdry,
     $         SPIKE, rmax, lambda1, lmax, betas, betae, dba, 
     $         dbs, dbe, qsca, qext, qabs, gqsc, ssaltot, gtot, gbetas, 
     $         dgbs, d, rbcn, ga, catot, rh, faq, fabc, fac, Ctot, Nnat,
     $         dbebg, bebg(31),dbabg, babg(31), dbebc, bebc(31), dbabc, 
     $         babc(31), dbeoc, beoc(31), dbaoc, baoc(31), dbesu, 
     $         besu(31), dbasu, basu(31), dbewa, bewa(31), dbawa, 
     $         bawa(31), bebggt1, bebglt1, be, bcgt1, bebcgt1, bebclt1,
     $         beocgt1, beoclt1, besugt1, besult1, bewagt1, bewalt1
      real     omega(31), gass(31), bext(31), babs(31), kext(31), 
     $         fband(31), omch(16), gch(16), bch(16), fb(16)
      real     p_phase, p_phaseint, lamrat, dbacksc, backsc
      real     angext, angabs
      real     r(0:100), dndlrk(0:100), dndlrkny(0:100), vsi(0:100),
     $         vbci(0:100), voci(0:100), fki(-1:100), fracdim(0:100), 
     $         vai(0:100), vssol(0:100), vbcsol(0:100), vocsol(0:100), 
     $         vasol(0:100), vw(0:100)
      COMPLEX  CREFIN(2), SFORW, SBACK, S1(MAXANG), S2(MAXANG),
     $         TFORW(2), TBACK(2), crin, cref(5,31)
      LOGICAL  iband11, iband16, iband440, iband500, iband670, iband870
      LOGICAL  iband550, extradiag
      REAL fombg, vombg, vbcbg, fbcbg

	perfct =.false.
	mimcut=1.0e-6
	anyang =.false.
	numang=0
	nmom=0
	ipolzn(1,1)=0
	prnt(1)=.true.
	prnt(2)=.true.
ctest
c	anyang =.true.
c	numang=21
c        do j=1,21
c          xmu(j)=real(j-1)/10.0-1.0
c        enddo
ctest        

c       initializing optical parameters
        do iband = 1, 31
          omega(iband)= 0.0
          gass(iband) = 0.0
          bext(iband) = 0.0
          kext(iband) = 0.0
          babs(iband) = 0.0
          bebg(iband) = 0.0
          babg(iband) = 0.0
          bebc(iband) = 0.0
          babc(iband) = 0.0
          beoc(iband) = 0.0
          baoc(iband) = 0.0
          besu(iband) = 0.0
          basu(iband) = 0.0
        enddo

      do iband = 1, ib
        iband11=.false.
        iband16=.false.
        if (ib.eq.29.and.iband.eq.11) then
          iband11=.true.
        elseif (ib.eq.29.and.iband.eq.16) then
          iband16=.true.
        endif 

c       logical variables for specific AeroCom calculations (if .true.)
        iband440=.false.
        iband500=.false.
        iband550=.false.  ! 20nov08
        iband670=.false.
        iband870=.false.
        if (ib.eq.31.and.iband.eq.9) then
          iband440=.true.                         ! 440 nm band
        elseif (ib.eq.31.and.iband.eq.11) then
          iband500=.true.                         ! 500 nm band
        elseif (ib.eq.31.and.iband.eq.12) then                    ! 20nov08
          iband550=.true.                         ! 550 nm band   ! 20nov08
        elseif (ib.eq.31.and.iband.eq.15) then
          iband670=.true.                         ! 670 nm band
        elseif (ib.eq.31.and.iband.eq.18) then
          iband870=.true.                         ! 870 nm band
        endif 

        betas=0.0
        betae=0.0
        gbetas=0.0
        if(iband.eq.1) then
          bebglt1=0.0
          bebclt1=0.0
          beoclt1=0.0
          besult1=0.0
        endif 
        if(iband.eq.12) then
          backsc=0.0
        endif

        lamrat=(xlam(iband)/(2*pi))**2

c       the gross optical parameters for an aerosol mode is found by
c       integrating over all particle radii (for each wavelength)  
c        write(*,*) 'imin, imax =', imin, imax
        do 1000 i=imin,imax   ! intergration over size

          xx(1)=2*pi*r(i)/xlam(iband)

c         wavelength dependent complex rafractive indices (crin) are
c         found from linear interpolation of tabulated values for each
c         aerosol component in subroutine refind
          call refind (xlam(iband), i, ib, iband, cref, crin, kcomp, 
     $      vbcsol, vocsol, vssol, vasol, vw, vombg, vbcbg, fki, r, 
     $      rbcn, fracdim)

          crefin(1)=crin
c          write(*,*) 'xx=', xx(1)
c          write(*,*) 'n=', crefin(1)

c         Mie calculations with size parameter xx as input 
          call MIEV0 ( XX, CREFIN, PERFCT, MIMCUT, ANYANG,
     $              NUMANG, XMU, NMOM, IPOLZN, MOMDIM, PRNT,
     $              QEXT, QSCA, GQSC, PMOM, SFORW, SBACK, S1,
     $              S2, TFORW, TBACK, SPIKE ) 
          ga=gqsc/qsca

ctest
c         if(iband.eq.8) then
c          p_phaseint=0.0
c          do j=1,20
c         normalisert fasefunksjon (P/2)       
c           p_phase=2.0*((cabs(S1(j)))**2+(cabs(S2(j)))**2)
c     $                  /(XX(1)*XX(1)*QSCA)
c           write(500,*) XMU(j), p_phase
c           p_phaseint=p_phaseint+p_phase*0.1
c          enddo
c          write(502,*) 'S1(21), S2(1), SBACK =', S1(21), S2(1), SBACK
c         endif
c         write(*,*) 'p_phaseint =', p_phaseint
c         write(*,*) 'CABS(SBACK)**2 =', CABS(SBACK)**2
c          dbe=pi*r(i)**2*(qext*dndlrkny(i))*d
c          backsc=lamrat*CABS(SBACK)**2*dndlrkny(i)*d                  
c          write(*,*) 'backsc =', backsc 
c          write(*,*) 'S =', dbe/backsc
ctest

          if(itot.eq.0) then  
            dbe=pi*r(i)**2*(qext*dndlrk(i))*d
            betae=betae+dbe    
            dbs=pi*r(i)**2*(qsca*dndlrk(i))*d
            betas=betas+dbs
            dgbs=pi*r(i)**2*(qsca*ga*dndlrk(i))*d
            gbetas=gbetas+dgbs       
           if(iband11.or.iband16.or.iband440.or.iband500
     $        .or.iband550.or.iband670.or.iband870) then
c           AeroCom-calculations.
            dbebg=dbe*1.e-3 
            bebg(iband)=bebg(iband)+dbebg    
            dba=dbe-dbs
            dbabg=dba*1.e-3 
            babg(iband)=babg(iband)+dbabg    
            if(i.lt.28.and.iband11)  bebglt1=bebglt1+dbebg
            if(i.lt.28.and.iband550) bebglt1=bebglt1+dbebg
           endif
c          Size integrated backscatter at 180 deg. (s. 54 i permXXIII) (CABS = complex ABS)
           if(iband550) then
            dbacksc=1.e-3*lamrat*CABS(SBACK)**2*dndlrk(i)*d
            backsc=backsc+dbacksc
           endif
          else   ! itot = 1
            dbe=pi*r(i)**2*(qext*dndlrkny(i))*d
            betae=betae+dbe    
            dbs=pi*r(i)**2*(qsca*dndlrkny(i))*d
            betas=betas+dbs
            dgbs=pi*r(i)**2*(qsca*ga*dndlrkny(i))*d
            gbetas=gbetas+dgbs       
            if(iband11.or.iband16.or.iband440.or.iband500
     $        .or.iband550.or.iband670.or.iband870) then
c          AeroCom-calculations: use v*dry but dndlrkny wet, i.e. r wet, so that
c          the sum of extinctions for each species, except water, equals the total
c          extiction, including water. Note that this method yield SSA for each 
c          component which doesn't take into account the specie's refractive index.    
             dba=dbe-dbs

             dbebg=dbe*vai(i)*1.e-3 
             bebg(iband)=bebg(iband)+dbebg    
             dbabg=dba*vai(i)*1.e-3 
             babg(iband)=babg(iband)+dbabg    
             if(i.lt.28.and.iband11)  bebglt1=bebglt1+dbebg
             if(i.lt.28.and.iband550) bebglt1=bebglt1+dbebg

             dbebc=dbe*vbci(i)*1.e-3 
             bebc(iband)=bebc(iband)+dbebc    
             dbabc=dba*vbci(i)*1.e-3 
             babc(iband)=babc(iband)+dbabc    
             if(i.lt.28.and.iband11)  bebclt1=bebclt1+dbebc
             if(i.lt.28.and.iband550) bebclt1=bebclt1+dbebc

             dbeoc=dbe*voci(i)*1.e-3 
             beoc(iband)=beoc(iband)+dbeoc    
             dbaoc=dba*voci(i)*1.e-3 
             baoc(iband)=baoc(iband)+dbaoc    
             if(i.lt.28.and.iband11)  beoclt1=beoclt1+dbeoc
             if(i.lt.28.and.iband550) beoclt1=beoclt1+dbeoc

             dbesu=dbe*vsi(i)*1.e-3 
             besu(iband)=besu(iband)+dbesu    
             dbasu=dba*vsi(i)*1.e-3 
             basu(iband)=basu(iband)+dbasu    
             if(i.lt.28.and.iband11)  besult1=besult1+dbesu
             if(i.lt.28.and.iband550) besult1=besult1+dbesu

            endif  ! AeroCom specific bands
            if(iband550) then 
             dbacksc=1.e-3*lamrat*CABS(SBACK)**2*dndlrkny(i)*d
             backsc=backsc+dbacksc
            endif
          endif    ! itot (0 or 1)

 1000   continue   ! intergration over size

c        if(iband550) then 
c          write(*,*) 'betae, backsc =', betae, backsc 
c          write(*,*) 'S =', betae*1.e-3/backsc
c          write(*,*) 'SBACK, CABS(SBACK) =', SBACK, CABS(SBACK)
c        endif

c       size integrated values of the single scattering albedo,
c       ssaltot, and asymmetry factor, gtot:
        ssaltot=betas/betae
        gtot=gbetas/betas
        omega(iband)=ssaltot
        gass(iband) =gtot
        bext(iband) =betae*1.e-3
        kext(iband) =betae/Ctot
        babs(iband) =(1.0-omega(iband))*bext(iband)
      if(extradiag) then
        write(40,3000) xlam(iband), omega(iband)
        write(41,3000) xlam(iband), gass(iband)
        write(42,3000) xlam(iband), bext(iband)
        write(43,3000) xlam(iband), kext(iband)
        write(44,3000) xlam(iband), kext(iband)*Ctot/Cdry
      endif
c        if(iband.eq.12) then
c          write(*,*) iband, xlam(iband), omega(iband)
c          write(*,*) iband, kext(iband), kext(iband)*(1.0-omega(iband)) 
c          write(*,*) 'Cdry, Ctot = ', Cdry, Ctot
c          write(*,*) 'lam, kext = ', xlam(iband), kext(iband)
c          write(*,*) 'lam, kextny = ',xlam(iband), kext(iband)*Ctot/Cdry
c         endif

      enddo     ! iband


c    The AEROCOM specific optics look-up tables are made below, by 
c    writing the results to file (unit=9500) 
ccccc6ccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5ccccccccc6ccccccccc7cc

      if(ib.eq.31) then  !   xlam(9)=0.44, xlam(11)=0.50, xlam(12)=0.55, xlam(15)=0.67 and xlam(18)=0.87 um 

       bebggt1=bebg(12)-bebglt1   
       bebcgt1=bebc(12)-bebclt1   
       beocgt1=beoc(12)-beoclt1   
       besugt1=besu(12)-besult1   
c
c      Here comes the aerocomk*.out look-up tables:
c
        if(kcomp.eq.0) then
         write(9500,7100) kcomp, rh,  
     $     bebg(9), babg(9), bebg(11), babg(11), babg(12), 
     $     bebg(15), babg(15), bebg(18), babg(18), 
     $     bebglt1, bebggt1, backsc
c          write(*,*) 'bext(12) =', bext(12)
c          write(*,*) 'bext12sum=', bebglt1+bebggt1
c          write(*,*) 'backsc=', backsc
c          write(*,*) 'S* =', bext(12)/backsc
        elseif(kcomp.eq.1) then
         write(9500,8600) kcomp, rh, fombg, catot, fac, 
     $     bext(9),  bext(11), bext(15), bext(18), 
     $     bebg(9),  bebg(11), bebg(15), bebg(18), 
     $     bebc(9),  bebc(11), bebc(15), bebc(18),
     $     beoc(9),  beoc(11), beoc(15), beoc(18),
     $     besu(9),  besu(11), besu(15), besu(18),
     $     babs(9),  babs(11), babs(12), babs(15), babs(18),
     $     bebglt1, bebggt1, bebclt1, bebcgt1, 
     $     beoclt1, beocgt1, besult1, besugt1, backsc,
     $     babg(12), babc(12), baoc(12), basu(12) 
        elseif(kcomp.eq.2.or.kcomp.eq.3) then
         write(9500,8200) kcomp, rh, catot, fac, 
     $     bext(9),  bext(11), bext(15), bext(18), 
     $     bebg(9),  bebg(11), bebg(15), bebg(18), 
     $     bebc(9),  bebc(11), bebc(15), bebc(18),
     $     beoc(9),  beoc(11), beoc(15), beoc(18),
     $     besu(9),  besu(11), besu(15), besu(18),
     $     babs(9),  babs(11), babs(12), babs(15), babs(18),
     $     bebglt1, bebggt1, bebclt1, bebcgt1, 
     $     beoclt1, beocgt1, besult1, besugt1, backsc,
     $     babg(12), babc(12), baoc(12), basu(12) 
c          write(*,*) 'bext(12) =', bext(12)
c          write(*,*) 'bext12sum=', bebglt1+bebggt1+bebclt1+bebcgt1
c     $                            +beoclt1+beocgt1+besult1+besugt1      
c          write(*,*) 'backsc=', backsc
c          write(*,*) 'S* =', bext(12)/backsc
        elseif(kcomp.eq.4) then
         write(9500,6100) kcomp, rh, fbcbg, catot, fac, faq, 
     $     bext(9),  bext(11), bext(15), bext(18), 
     $     bebg(9),  bebg(11), bebg(15), bebg(18), 
     $     bebc(9),  bebc(11), bebc(15), bebc(18),
     $     beoc(9),  beoc(11), beoc(15), beoc(18),
     $     besu(9),  besu(11), besu(15), besu(18),
     $     babs(9),  babs(11), babs(12), babs(15), babs(18),
     $     bebglt1, bebggt1, bebclt1, bebcgt1, 
     $     beoclt1, beocgt1, besult1, besugt1, backsc,  
     $     babg(12), babc(12), baoc(12), basu(12)  
c          write(*,*) 'bext(12) =', bext(12)
c          write(*,*) 'bext12sum=', bebglt1+bebggt1+bebclt1+bebcgt1
c     $                            +beoclt1+beocgt1+besult1+besugt1      
c          write(*,*) 'backsc=', backsc
c          write(*,*) 'S* =', bext(12)/backsc
        else  ! kcomp = 5, 6, 7, 8, 9 or 10
         write(9500,6100) kcomp, rh, catot, fac, fabc, faq, 
     $     bext(9),  bext(11), bext(15), bext(18), 
     $     bebg(9),  bebg(11), bebg(15), bebg(18), 
     $     bebc(9),  bebc(11), bebc(15), bebc(18),
     $     beoc(9),  beoc(11), beoc(15), beoc(18),
     $     besu(9),  besu(11), besu(15), besu(18),
     $     babs(9),  babs(11), babs(12), babs(15), babs(18),
     $     bebglt1, bebggt1, bebclt1, bebcgt1, 
     $     beoclt1, beocgt1, besult1, besugt1, backsc,  
     $     babg(12), babc(12), baoc(12), basu(12)  
c          write(*,*) 'bext(12) =', bext(12)
c          write(*,*) 'bext12sum=', bebglt1+bebggt1+bebclt1+bebcgt1
c     $                            +beoclt1+beocgt1+besult1+besugt1      
c          write(*,*) 'k, besu(12) =', kcomp, besu(12)
c          write(*,*) 'k, besu12sum=', kcomp, besult1+besugt1 
c          write(*,*) 'backsc=', backsc
c          write(*,*) 'S* =', bext(12)/backsc
        endif ! kcomp
cANG+   some extra angstrom diagnostics
c       xlam(9)=0.44, xlam(11)=0.50, xlam(12)=0.55, xlam(15)=0.67 and xlam(18)=0.87 um 
c        angext = -log(bext(18)/bext(9))/log(xlam(18)/xlam(9))
c        angabs = -log((babs(18)+1.e-50)/(babs(9)+1.e-50))
c     $          /log(xlam(18)/xlam(9))
c        write(111,*) rh, angext, kcomp, catot, fac, fabc, faq 
c        write(112,*) rh, angabs, kcomp, catot, fac, fabc, faq 
cANG-

c       find Chandrasekhar-averaged optical parameters for the wide bands
ccccc6ccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5ccccccccc6ccccccccc7cc
        call chandrav (ib, xlam, xlamb, xlame, fband, fb,
     $                   omega, gass, bext, omch, gch, bch) 

c       convert from ib=31 to the usual ib=14 for input to CAM5 
        do iband=1,9
          omega(iband) = omch(iband)
          gass(iband)  = gch(iband)
          bext(iband)  = bch(iband)
          kext(iband)  = 1.e3*bch(iband)/Ctot
        enddo
        do iband = 10,14
          omega(iband) = omega(iband+17)
          gass(iband)  = gass(iband+17)
          bext(iband)  = bext(iband+17)
          kext(iband)  = kext(iband+17)
        enddo
copt+   optional calculations and output for test and plotting purposes
c        do iband = 1,14
c          if(iband.eq.1) then
c            lam = 0.2315 
c          elseif(iband.eq.2) then
c            lam = 0.304 
c          elseif(iband.eq.3) then
c            lam = 0.3935 
c          elseif(iband.eq.4) then
c            lam = 0.5335  
c          elseif(iband.eq.5) then
c            lam = 0.7015
c          elseif(iband.eq.6) then
c            lam = 1.01 
c          elseif(iband.eq.7) then 
c            lam = 1.2705 
c          elseif(iband.eq.8) then
c            lam = 1.4625 
c          elseif(iband.eq.9) then
c            lam = 1.784
c          elseif(iband.gt.9) then
c            lam = xlam(iband+17)
c          endif           
c          write(50,3000) lam, omega(iband)
c          write(51,3000) lam, gass(iband)
c          write(52,3000) lam, bext(iband)
c          write(53,3000) lam, kext(iband)
c          write(54,3000) lam, kext(iband)*Ctot/Cdry
c          write(55,3000) lam, kext(iband)*(1.0-omega(iband))
c          write(56,3000) lam, bext(iband)*(1.0-omega(iband))
c            write(*,*) iband, lam
c        enddo
copt-
        
c*****************************************************************

      elseif(ib.eq.19) then   ! xlam(16)=24.2855, xlam(17)=45, xlam(18)=60, xlam(19)=85 

c       no AEROCOM look-up tables needed here in LW
c       find Chandrasekhar-averaged optical parameters for the wide bands
ccccc6ccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5ccccccccc6ccccccccc7cc
        call chandrav (ib, xlam, xlamb, xlame, fband, fb,
     $                   omega, gass, bext, omch, gch, bch) 

c       convert from ib=19 to the usual ib=16 for input to CAM5 
        do iband=1,15
          omega(iband) = omega(iband)
          gass(iband)  = gass(iband)
          bext(iband)  = bext(iband)
          kext(iband)  = kext(iband)
        enddo
        do iband=16,16
          omega(iband) = omch(iband)
          gass(iband)  = gch(iband)
          bext(iband)  = bch(iband)
          kext(iband)  = 1.e3*bch(iband)/Ctot
        enddo

c        do iband = 1,16
c          if(iband.lt.15) then
c            lam = xlam(iband)
c          elseif(iband.eq.16) then
c            lam = 64.3  ! midband of 28.571-100
c          endif           
c          write(50,3000) lam, omega(iband)
c          write(51,3000) lam, gass(iband)
c          write(52,3000) lam, bext(iband)
c          write(53,3000) lam, kext(iband)
c          write(54,3000) lam, kext(iband)*Ctot/Cdry
c            write(*,*) iband, lam
c        enddo

c*****************************************************************

      elseif(ib.eq.29) then   ! xlam(11)=0.55 and xlam(16)=0.865 

       bebggt1=bebg(11)-bebglt1   
       bebcgt1=bebc(11)-bebclt1   
       beocgt1=beoc(11)-beoclt1   
       besugt1=besu(11)-besult1   


c       find Chandrasekhar-averaged optical parameters for the wide bands
ccccc6ccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5ccccccccc6ccccccccc7cc
        call chandrav (ib, xlam, xlamb, xlame, fband, fb,
     $                   omega, gass, bext, omch, gch, bch) 

c       convert from ib=29 to the usual ib=12 for input to CAM 
        omega(8) = omch(8)
        gass(8)  = gch(8)
        bext(8)  = bch(8)
        kext(8)  = 1.e3*bch(8)/Ctot
        omega(9) = omega(13)
        gass(9)  = gass(13)
        bext(9)  = bext(13)
        kext(9)  = kext(13)
        do iband = 10,12
          omega(iband) = omch(iband)
          gass(iband)  = gch(iband)
          bext(iband)  = bch(iband)
          kext(iband)  = 1.e3*bch(iband)/Ctot
        enddo
        do iband = 13,29
          omega(iband) = 0.0
          gass(iband)  = 0.0
          bext(iband)  = 0.0
          kext(iband)  = 0.0
        enddo
        do iband = 1,12
          if(iband.lt.8) then
            lam = xlam(iband)
          elseif(iband.eq.8) then
            lam = 0.495  
          elseif(iband.eq.9) then
            lam = 0.665  
          elseif(iband.eq.10) then
            lam = 0.94  
          elseif(iband.eq.11) then
            lam = 1.785  
          else
            lam = 3.19  
          endif           
c          write(50,3000) lam, omega(iband)
c          write(51,3000) lam, gass(iband)
c          write(52,3000) lam, bext(iband)
c          write(53,3000) lam, kext(iband)
c          write(54,3000) lam, kext(iband)*Ctot/Cdry
c            write(*,*) iband, lam
        enddo

      endif  ! ib=29 or ib=31


c    The CAM-RT / RRTMG optics look-up tables are made below, by writing 
c    the results to file (unit=9000 for SW, unit=9001 for LW) 
ccccc6ccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5ccccccccc6ccccccccc7cc

      if(ib.eq.31) then  ! RRTMG SW
c
c      Here comes the kcomp*.out look-up tables:
c
        if(kcomp.eq.0) then
         do iband = 1, 14
           write(9000,4000) kcomp, iband, rh,  
     $     omega(iband), gass(iband), bext(iband), kext(iband)
         enddo
        elseif(kcomp.eq.1) then
         do iband = 1, 14
           write(9000,9500) kcomp, iband, rh, fombg, catot, fac, 
     $     omega(iband), gass(iband), bext(iband), kext(iband)
         enddo
        elseif(kcomp.eq.2.or.kcomp.eq.3) then
         do iband = 1, 14
           write(9000,9100) kcomp, iband, rh, catot, fac, 
     $     omega(iband), gass(iband), bext(iband), kext(iband)
         enddo
        elseif(kcomp.eq.4) then
         do iband = 1, 14
           write(9000,5000) kcomp, iband, rh, fbcbg, catot, fac, faq,  
     $     omega(iband), gass(iband), bext(iband), kext(iband)
         enddo
        else ! (kcomp = 5, 6, 7, 8, 9 or 10)
         do iband = 1, 14
           write(9000,5000) kcomp, iband, rh, catot, fac, fabc, faq,
     $     omega(iband), gass(iband), bext(iband), kext(iband)
         enddo
        endif
c
c19/11-2013:
      elseif(ib.eq.19) then  ! RRTMG LW  
c     We only need (1-omega)*kext, since mass is abtained via the SW files
       if(kcomp.eq.0) then
         do iband = 1, 16
           write(9009,4010) kcomp, iband, rh,  
     $     (1.0-omega(iband))*kext(iband)
         enddo
        elseif(kcomp.eq.1) then
         do iband = 1, 16
           write(9009,9510) kcomp, iband, rh, fombg, catot, fac, 
     $     (1.0-omega(iband))*kext(iband)
         enddo
        elseif(kcomp.eq.2.or.kcomp.eq.3) then
         do iband = 1, 16
           write(9009,9110) kcomp, iband, rh, catot, fac, 
     $     (1.0-omega(iband))*kext(iband)
         enddo
        elseif(kcomp.eq.4) then
         do iband = 1, 16
           write(9009,5010) kcomp, iband, rh, fbcbg, catot, fac, faq,  
     $     (1.0-omega(iband))*kext(iband)
         enddo
        else  ! kcomp=5-10
         do iband = 1, 16
           write(9009,5010) kcomp, iband, rh, catot, fac, fabc, faq,
     $     (1.0-omega(iband))*kext(iband)
         enddo
        endif
c8/11-2013:
c
      elseif(ib.eq.29) then  ! CAM-RT SW

       if(itot.eq.0.and.ib.ge.12) then
         do iband = 1, 12
           write(9000,4000) kcomp, iband, rh,  
     $     omega(iband), gass(iband), bext(iband), kext(iband)
         enddo
       elseif(itot.eq.1.and.ib.ge.12) then
        if(kcomp.eq.1) then
         do iband = 1, 12
           write(9000,9500) kcomp, iband, rh, fombg, catot, fac,  
     $     omega(iband), gass(iband), bext(iband), kext(iband)
         enddo
        elseif(kcomp.eq.2.or.kcomp.eq.3) then
         do iband = 1, 12
           write(9000,9100) kcomp, iband, rh, catot, fac,  
     $     omega(iband), gass(iband), bext(iband), kext(iband)
         enddo
        elseif(kcomp.eq.4) then
         do iband = 1, 12
           write(9000,5000) kcomp, iband, rh, fbcbg, catot, fac, faq,  
     $     omega(iband), gass(iband), bext(iband), kext(iband)
         enddo
        else  ! kcomp=6-10
         do iband = 1, 12
           write(9000,5000) kcomp, iband, rh, catot, fac, fabc, faq,
     $     omega(iband), gass(iband), bext(iband), kext(iband)
         enddo
        endif
       endif

      endif

 3000 format(2(x,e12.5))
 4000 format(2I3,f8.3,4(x,e12.5))
 4010 format(2I3,f8.3,x,e12.5)
 5000 format(2I3,f8.3,3(x,e10.3),f7.2,4(x,e12.5))
 5010 format(2I3,f8.3,3(x,e10.3),f7.2,x,e12.5)
 6000 format(I2,f6.3,3e10.3,f5.2,28e10.3)
 6100 format(I2,f6.3,3e10.3,f5.2,38e10.3)
 7000 format(I2,f5.2,6e11.4)
 7100 format(I2,f6.3,12e11.4)
 8000 format(I2,f6.3,e10.3,28e10.3)
 8100 format(I2,f6.3,e10.3,38e10.3)
 8200 format(I2,f6.3,2e10.3,38e10.3)
 8500 format(I2,f6.3,3e10.3,28e10.3)
 8600 format(I2,f6.3,3e10.3,38e10.3)
 9000 format(2I3,f8.3,x,e10.3,4(x,e12.5))
 9100 format(2I3,f8.3,2(x,e10.3),4(x,e12.5))
 9110 format(2I3,f8.3,2(x,e10.3),x,e12.5)
 9500 format(2I3,f8.3,3(x,e10.3),4(x,e12.5))
 9510 format(2I3,f8.3,3(x,e10.3),x,e12.5)

	end






