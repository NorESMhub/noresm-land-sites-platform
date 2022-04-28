
      subroutine rhsublight (imax, rh, d, r, dndlrkny, vsi, vbci,
     $ voci, vombg, vbcbg, vssol, vbcsol, vocsol, vasol, vw, 
     $ fki, itot, rhow, Ctot, kcomp, faq, iopt, xbc, xdst, xoc,
     $ xs, xa, xss, rhda, rhca, rhdss, rhcss, rkny, rknywet, logsk,
     $ Nnatk, extradiag)

c **********************************************************************************
c     Created by Alf Kirkevåg.
c **********************************************************************************

c     Hygroscopic growth is here taken into account for the given 
c     relative humidity (if iopt=1). New number and mass concentrations 
c     and volume fractions are calculated in a simplified way like in
c     conteqlight.f for dry aerosol growth: we here assume homogenous
c     internal mixing (no r dependency) and that the size distributions
c     remain log-normal (new R, but unhanged SIGMA).

      implicit none

      INTEGER i, imax, j, jmax, itot, kcomp, ismolarh, iopt, irk, ivk
      REAL Ctot, dCtot, rhow, fmax, fmlight, rny, rh, d, faq, pi
      REAL rk, rkny, rknywet, logsk, lnsk, ntot, nk, Nnatk, vkny
      REAL xbc, xdst, xoc, xs, xa, xss, rhda, rhca, rhdss, rhcss
      REAL vombg, vbcbg
      REAL dndlrkny(0:100), r(0:100), vbci(0:100), voci(0:100),
     $ vsi(0:100), vssol(0:100), vbcsol(0:100), vocsol(0:100),
     $ vasol(0:100), vw(0:100)
      REAL vssolub(100), vbcsolub(100), vocsolub(100), vasolub(100) 
      REAL fki(-1:100), f(-1:100), fm(-1:100) 
      LOGICAL extradiag
      
      PARAMETER  (pi=3.141592654)

c     Initializing local arrays
      do i=-1,100
        f(i)=0.0
        fm(i)=0.0
        fki(i)=0.0
      enddo
      do i=0,100
        vssol(i)=0.0
        vbcsol(i)=0.0
        vocsol(i)=0.0
        vasol(i)=0.0
        vw(i)=0.0
      enddo
      do i=1,100
        vssolub(i)=0.0
        vbcsolub(i)=0.0
        vocsolub(i)=0.0
        vasolub(i)=0.0
      enddo

c     Initialize wet volume fractions for sulfate, vssol, soot, vbcsol,
c     and background aerosol, vasol. Note that the background aerosol 
c     consists of an internal mixture of two constituents for kcomp=1 
c     (Sulfate and OM) and kcomp=4 (OM and BC)
      do i=1,imax
        vssol(i)=vsi(i)                      ! non-background sulfate
        vbcsol(i)=vbci(i)                    ! non-background BC
        vocsol(i)=voci(i)                    ! non-background OC
        vasol(i)=max(1.0-vsi(i)-vbci(i)-voci(i),0.0)  ! background (sulfate, OC, BC, SS or DU, or a mixture of two if kcomp = 1 or 4)
      enddo

c     subroutine koehler solves the koehler equation to find wet particle 
c     radii for a given relative humidity, rh.
      call koehler (d, imax, r, vsi, vbci, voci, vombg, vbcbg, 
     $ rh, f, fm, itot, faq, kcomp, iopt, xbc, xdst, xoc, xs, xa, xss, 
     $ rhda, rhca, rhdss, rhcss)

      do i=-1,imax
        if(i.le.0) then
          f(i)=1.0
          fm(i)=1.0
        endif
c       fki is for use in sub-routine refind
        fki(i)=f(i)
c        write(*,*) 'i, fki(i) = ', i, fki(i)
      enddo
      fmax=1.0
      do i=1,imax
        if(fm(i).gt.fmax) then
          fmax=fm(i)
        endif
      enddo

c     Find i-value with r(i) clostest to rkny          
      irk=NINT(1.0+(3.0+log10(rkny))/d)
      write(*,*) 'irk = ', irk
      write(*,*) 'fm(irk) = ', fm(irk)
c     Find i-value with r(i) clostest to vkny, which is the
c     corresponding median r for the volume size distribution
c     From Seinfeld & Pandis "ACP" 1998 (Eq. 7.52):
      lnsk=log(10**logsk)
      vkny=0.5*exp(log(2.0*rkny)+3.0*lnsk**2)
      ivk=min(NINT(1.0+(3.0+log10(vkny))/d),imax)
      write(*,*) 'ivk = ', ivk
c
      
c     New modal radius including hygroscopic growth based on
c     modal radius for the volume size distribution      
      fmlight=fm(ivk)
      rknywet=rkny*fmlight
ctest++
c      rknywet=rkny*fmax
ctest--
      write(*,*) 'fmlight = ', fmlight
      write(*,*) 'fmax = ', fmax
      write(*,*) "rkny, rknywet = ", rkny, rknywet

c     The new log-normal number size distribution dndlrkny is calculated
c     and tested for conservation of particle number      
      ntot=0.0
      do i=0,imax
        nk=(1.0/logsk)*exp(-0.5*(log10(r(i)/rknywet)/logsk)**2.0)
        dndlrkny(i)=Nnatk*nk/sqrt(2.0*pi)
        ntot=ntot+dndlrkny(i)*d
      enddo
      write(*,*) 'ntot =', ntot

c     New volume fractions for sulfate, vssol, soot, vbcsol, oc, vocsol, 
c     background aerosol, vasol, and water, vw, after hygroscopic growth. 
c     Here vssol+vbcsol+vocsol+vasol+vw=1.
      do i=1,imax
         vssol(i)=vsi(i)*fmlight**(-3.0)
         vbcsol(i)=vbci(i)*fmlight**(-3.0)
         vocsol(i)=voci(i)*fmlight**(-3.0)
         vasol(i)=max(1.0-vsi(i)-vbci(i)-voci(i),0.0)
     $    * fmlight**(-3.0)
         vw(i)=1.0-min(vssol(i)+vbcsol(i)+vocsol(i)+vasol(i),1.0)
       enddo       
        
c     Additional output (for testing purposes only)
      if(extradiag) then
       do i=1,imax      
        write(132,100) r(i), vssol(i)
        write(133,100) r(i), vbcsol(i)
        write(134,100) r(i), vocsol(i)
        write(135,100) r(i), vasol(i)
        write(136,100) r(i), vw(i)
       enddo
      endif
      
c     condensed water contribution, dCtot, to the total aerosol 
c     concentration, Ctot (ug/m**-3) 
      do i=1,imax 
        dCtot=1.0e-3*(4.0*pi/3.0)*r(i)**3.0
     $        *(rhow*vw(i)*dndlrkny(i))*d
        Ctot=Ctot+dCtot                  
      enddo
      write(*,*) 'Wetted Ctot =', Ctot
      write(999,*) 'Wetted Ctot =', Ctot
 

 100  format(2(x,e10.4))

      return
      end

