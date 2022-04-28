
      subroutine rhsub (imax, rh, d, r, rp, dndlrkny, vsi, vbci, voci,
     $ fombg, fbcbg, vombg, vbcbg, vssol, vbcsol, vocsol, vasol, vw, 
     $ fki, itot, rhos, rhosv, rhobc, rhooc, rhob, rhow, Ctot, kcomp, 
     $ ismolarh, cat, fac, fabc, faq, iopt, xbc, xdst, xoc, xs, xa,
     $ xss, rhda, rhca, rhdss, rhcss, extradiag)

c **********************************************************************************
c     Created by Alf Kirkevåg.
c **********************************************************************************

c     Hygroscopic growth is here taken into account for the given 
c     relative humidity (if iopt=1). New number and mass concentrations 
c     and volume fractions are calculated.

      implicit none

      INTEGER i, imax, j, jmax, itot, kcomp, ismolarh, iopt
      REAL Ctot, dCtot, rhos, rhosv, rhobc, rhooc, rhob, rhow, 
     $ fmax, rny, rh, d, cat, fac, fabc, faq, pi
      REAL xbc, xdst, xoc, xs, xa, xss, rhda, rhca, rhdss, rhcss
      REAL fombg, fbcbg, vombg, vbcbg
      REAL dninc(0:100), dip(0:100), dndlrkny(0:100), dncny(0:100), 
     $ r(0:100), rp(0:100), vbci(0:100), voci(0:100), vsi(0:100), 
     $ vssol(0:100), vbcsol(0:100), vocsol(0:100), vasol(0:100), 
     $ vw(0:100)
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
        dncny(i)=0.0
        dninc(i)=0.0
        dip(i)=0.0
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
c        if(i.eq.12) write(90,*) rh, fki(i) ! r=0.0118 (ca)  mode 1 & 2
c        if(i.eq.14) write(90,*) rh, fki(i) ! r=0.022 (ca)   mode 8
c        if(i.eq.17) write(90,*) rh, fki(i) ! r=0.04 (ca)    mode 4
c        if(i.eq.20) write(90,*) rh, fki(i) ! r=0.075 (ca)   mode 5
c        if(i.eq.21) write(90,*) rh, fki(i) ! r=0.1 (ca) 
c        if(i.eq.22) write(90,*) rh, fki(i) ! r=0.13 (ca)    mode 9
c        if(i.eq.24) write(90,*) rh, fki(i) ! r=0.22 (ca)    mode 6
c        if(i.eq.29) write(90,*) rh, fki(i) ! r=0.63 (ca)    mode 7
c        if(i.eq.30) write(90,*) rh, fki(i) ! r=0.74 (ca)    mode 10
c        if(i.eq.43) write(90,*) rh, fki(i)
c        if(i.eq.30) write(91,*) rh, fm(i)  ! test 4nov2014 (ikke bruk denne)
      enddo
      fmax=1.0
      do i=1,imax
        if(fm(i).gt.fmax) then
          fmax=fm(i)
        endif
      enddo

c     the total iteration number jmax must be sufficiently large to 
c     satisfy the stability criterium for the continuity equation.
      jmax=int(log10(fmax)/d)+1
      write(*,*) 'fmax, jmax =', fmax, jmax
      
c     determine the increment of log(r/um) at r=rp, i.e. in the center of 
c     the size bin, dip (chosen to be the same for every time step). 
      dip(0)=0.0
      do i=1,imax
        if(i.eq.imax) then
          dip(i)=log10(fm(i))/jmax
        else
          dip(i)=log10(0.5*(fm(i)+fm(i+1)))/jmax
        endif
      enddo

c     solve the continuity equations (with a simple upwind advection scheme,
c     or with corrective anti-diffusive steps from the Smolarkiewicz scheme) 
c     using jmax time steps/iterations for the size distribution, dndlrkny. 
c     Process specific wet volume fractions and mass concentrations are 
c     determined directly from the growth factor and the dry values

         do j=1,jmax

       do i=1,imax
         dninc(i)=-(dndlrkny(i)*dip(i)-dndlrkny(i-1)*dip(i-1))/d
       enddo
       do i=1,imax       
         dndlrkny(i)=dndlrkny(i)+dninc(i)
       enddo

       do i=1,imax       
          rny=r(i)*fm(i)**(-1.0/real(jmax))
         if(i.eq.1) then
           vssolub(i)=vssol(i)
           vbcsolub(i)=vbcsol(i)
           vocsolub(i)=vocsol(i)
           vasolub(i)=vasol(i)
         else
           vssolub(i)=(vssol(i-1)*log10(r(i)/rny)       
     $     +vssol(i)*log10(rny/r(i-1)))/d
           vbcsolub(i)=(vbcsol(i-1)*log10(r(i)/rny)       
     $     +vbcsol(i)*log10(rny/r(i-1)))/d
           vocsolub(i)=(vocsol(i-1)*log10(r(i)/rny)       
     $     +vocsol(i)*log10(rny/r(i-1)))/d
           vasolub(i)=(vasol(i-1)*log10(r(i)/rny)       
     $     +vasol(i)*log10(rny/r(i-1)))/d
         endif
       enddo
       do i=1,imax       
         vssol(i)=vssolub(i)*fm(i)**(-3.0/real(jmax))
         vbcsol(i)=vbcsolub(i)*fm(i)**(-3.0/real(jmax))
         vocsol(i)=vocsolub(i)*fm(i)**(-3.0/real(jmax))
         vasol(i)=vasolub(i)*fm(i)**(-3.0/real(jmax))
         vw(i)=1.0-min(vssol(i)+vbcsol(i)+vocsol(i)+vasol(i),1.0)
       enddo

      if(ismolarh.gt.0) then
c       Smolarkiewicz-scheme with ismolar corrective steps
        do i=1,imax
          dncny(i)=dndlrkny(i)
        enddo
        call smolar (ismolarh, imax, d, dncny, dip)  
        do i=1,imax
          dndlrkny(i)=max(1.e-50,dncny(i))
        enddo
      endif

         enddo                     ! j-loop

c     volume fractions for sulfate, vssol, soot, vbcsol, oc, vocsol, 
c     background aerosol, vasol, and water, vw, after hygroscopic growth. 
c     Here vssol+vbcsol+vocsol+vasol+vw=1.
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

