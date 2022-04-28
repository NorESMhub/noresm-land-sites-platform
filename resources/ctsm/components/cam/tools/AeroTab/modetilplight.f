      subroutine modetilplight (cat,fac,fabc,faq,kcomp,rkny,logsk)

c **********************************************************************************
c     Created by Alf Kirkevåg, January 2020.
c **********************************************************************************

c     This subroutine produces look-up tables of modal parameters for
c     the dry lognormal size distrubutions with simplified growth from
c     conteqlight.f and rhsublight.f. Since sigma is conserved with
c     these simplified model assumptions, only the dry modal radius 
c     different from the initial value.

      implicit none

      INTEGER  kcomp
      REAL     logsk, rkny, cat, fac, fabc, faq
      
c
c      Here comes the logntilp*.out look-up tables:
c
        if(kcomp.ge.1.and.kcomp.le.3) then
          write(9003,150) kcomp, cat, fac, rkny, logsk
        elseif(kcomp.eq.4) then
          write(9003,200) kcomp, cat, fac, faq, rkny, logsk
        elseif(kcomp.ge.5.and.kcomp.le.10) then
          write(9003,300) kcomp, cat, fac, fabc, faq, rkny, logsk
          write(*,300) kcomp, cat, fac, fabc, faq, rkny, logsk
        else
          write(*,*) 'Only calculations for modes 1-10 are necessary'
          stop
        endif

 100  format(I3,3(x,e12.5))
 150  format(I3,4(x,e12.5))
 200  format(I3,5(x,e12.5))
 300  format(I3,6(x,e12.5))

      return
      end

