      subroutine coagsub (r, imax, rcoag, rhob, rhoc2, Kg12)
       
c **********************************************************************************
c     Created by Alf Kirkevåg.
c **********************************************************************************

c     Brownian coagulation coefficient on Fuchs form, Kg12, for monodisperse 
c     nucleation mode particles (with radius rcoag) of sulfate, BC or OC (OM). 

      implicit none

      INTEGER i, imax
      REAL rhob, rhoc2, r(0:100), Kg12(0:101), 
     $     diff1(0:101), diff2(0:101), g12(0:101), 
     $     g1(0:101), g2(0:101), c12(0:101), c1(0:101), c2(0:101), 
     $     mfv1(0:101), mfv2(0:101), rcoag, pi
      PARAMETER  (pi=3.141592654)

      do i=0,imax
        c1(i)=4.786e4/(rhob*r(i)**3)**0.5                         ! unit um/s
        c2(i)=4.786e4/(rhoc2*rcoag**3)**0.5                       ! unit um/s
        c12(i)=(c1(i)**2+c2(i)**2)**0.5                           ! unit um/s
        diff1(i)=(11.64/r(i))*(5.0+0.253/r(i)+0.024/r(i)**2
     $   +0.00457/r(i)**3)/(5.0-0.0633/r(i)+0.0446/r(i)**2)       ! unit um^2/s
        diff2(i)=(11.64/rcoag)*(5.0+0.253/rcoag+0.024/rcoag**2
     $   +0.00457/rcoag**3)/(5.0-0.0633/rcoag+0.0446/rcoag**2)    ! unit um^2/s
        mfv1(i)=8.0*diff1(i)/(pi*c1(i))                           ! unit um
        mfv2(i)=8.0*diff2(i)/(pi*c2(i))                           ! unit um
        g1(i)=((2*r(i)+mfv1(i))**3                               
     $   -(4.0*r(i)**2+mfv1(i)**2)**1.5)
     $   /(6.0*r(i)*mfv1(i))-2*r(i)                               ! unit um
        g2(i)=((2*rcoag+mfv2(i))**3
     $   -(4.0*rcoag**2+mfv2(i)**2)**1.5)
     $   /(6.0*rcoag*mfv2(i))-2*rcoag
        g12(i)=(g1(i)**2+g2(i)**2)**0.5                           ! unit um
        Kg12(i)=4*pi*(r(i)+rcoag)*(diff1(i)+diff2(i))             
     $   /((r(i)+rcoag)/(r(i)+rcoag+g12(i))
     $    +(4.0/c12(i))*(diff1(i)+diff2(i))/(rcoag+r(i)))         ! unit um^3/s
c       write(*,*) r(i), Kg12(i)
      enddo

      return
      end
