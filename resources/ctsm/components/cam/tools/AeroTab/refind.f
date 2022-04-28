
       subroutine refind (lambda, i, ib, iband, cref, crin, kcomp, 
csoa     $   vbcsol, vocsol, vssol, vasol, vw, fki, r, rbcn, fracdim)
     $   vbcsol, vocsol, vssol, vasol, vw, vombg, vbcbg, 
     $   fki, r, rbcn, fracdim)

c **********************************************************************************
c     Created by Alf Kirkevåg.
c **********************************************************************************

c      Wavelength dependent complex rafractive indices (crin) for 
c      internal mixing is computed according to the volume mixing 
c      approximation for relatively nonabsorbing components (sulfate
c      oc, sea-salt, dust and water), and Maxwell Garnetts mixing rule 
c      between the soot/BC and other components.

      implicit none

      integer  i, ib, iband, j, kcomp
      real     lambda, vbcsol(0:100), vocsol(0:100), vssol(0:100), 
     $         vasol(0:100), vw(0:100), vbc12(0:100), vbcmix(0:100), 
     $         fki(-1:100), r(0:101), fracdim(0:100), rbcn
      complex  cref(5,31), crin, crin0, crin2, crina
csoa
      real    vombg, vbcbg 
      complex crefbg
c
c     Take into account that the bacground aerosol (1) can be an internal mixture of two constituents:    
      if(kcomp.eq.1) then
        crefbg=cref(1,iband)*(1-vombg)+cref(5,iband)*vombg    ! H2SO4 and OM (as SOA) internally mixed
csoa      elseif(kcomp.eq.4) then
csoa        crefbg=cref(1,iband)                      ! NB! hadde glemt å ta hensyn til BC-delen her!!!
c        crefbg=cref(1,iband)*(1-vbcbg)+cref(3,iband)*vbcbg   ! må dele opp annerledes når BC er i bakgr., se under
      else
        crefbg=cref(1,iband)
      endif
csoa
      if(kcomp.ge.1.and.kcomp.le.10) then     ! BC in background should also be treated with MG!
c       Internal mixture of sulfate, soot/bc, oc and water with the background 
c       component (SO4, OC, BC, mineral or seasalt), where (tabrefind.f):
c       1=background, 2=sulfate, 3=BC, 4=water, 5=OC
       if(kcomp.eq.2) then  ! Aitken mode BC background
        crin0=(vssol(i)*cref(2,iband)+vocsol(i)*cref(5,iband)    ! All internally mixed constituents except BC:
     $       +vw(i)*cref(4,iband))                               ! using the 
     $       /(vssol(i)+vocsol(i)+vw(i))                         ! volume mixing rule.
csoa        crina=cref(1,iband)                                      ! BC in the background.
        crina=crefbg                                             ! (only) BC in the background
        crin2=crin0*crin0*(crina*crina+2*crin0*crin0             ! All internally mixed constituents:
     $       +2*vasol(i)*(crina*crina-crin0*crin0))              ! using the 
     $       /(crina*crina+2*crin0*crin0-vasol(i)                ! Maxwell Garnett 
     $       *(crina*crina-crin0*crin0))                         ! mixing rule.
csoa
       elseif(kcomp.eq.4) then  ! Aitken mode OM&BC background
        crin0=(vssol(i)*cref(2,iband)+vocsol(i)*cref(5,iband)    ! All internally mixed constituents except BC:
     $       +vasol(i)*(1.0-vbcbg)*cref(5,iband)                 ! (non-BC background contribution)
     $       +vw(i)*cref(4,iband))                               ! using the 
     $       /(vssol(i)+vocsol(i)+vasol(i)*(1.0-vbcbg)+vw(i))    ! volume mixing rule.
        crina=cref(3,iband)                                      ! BC (only in the background)
        crin2=crin0*crin0*(crina*crina+2*crin0*crin0             ! All internally mixed constituents:
     $       +2*vasol(i)*vbcbg*(crina*crina-crin0*crin0))        ! using the 
     $       /(crina*crina+2*crin0*crin0-vasol(i)*vbcbg          ! Maxwell Garnett 
     $       *(crina*crina-crin0*crin0))                         ! mixing rule.
csoa
       else  ! non-BC background constituents
        crin0=(vssol(i)*cref(2,iband)+vocsol(i)*cref(5,iband)    ! All internally mixed constituents except BC:
csoa     $       +vasol(i)*cref(1,iband)+vw(i)*cref(4,iband))        ! using the 
     $       +vasol(i)*crefbg+vw(i)*cref(4,iband))               ! using the 
     $       /(vssol(i)+vocsol(i)+vasol(i)+vw(i))                ! volume mixing rule.
        crina=cref(3,iband)                                      ! Added BC (not BC in the background).
        crin2=crin0*crin0*(crina*crina+2*crin0*crin0             ! All internally mixed constituents:
     $       +2*vbcsol(i)*(crina*crina-crin0*crin0))             ! using the 
     $       /(crina*crina+2*crin0*crin0-vbcsol(i)               ! Maxwell Garnett 
     $       *(crina*crina-crin0*crin0))                         ! mixing rule.
       endif
        crin=csqrt(crin2)
ctest   testing volume mixing for kcomp=4:
c       if(kcomp.eq.4) then
c        crin = (vssol(i)*cref(2,iband)+vocsol(i)*cref(5,iband)
c     $       + vasol(i)*(1.0-vbcbg)*cref(5,iband)
c     $       + vasol(i)*vbcbg*cref(3,iband)
c     $       + vw(i)*cref(4,iband))                            
c     $       /(vssol(i)+vocsol(i)+vasol(i)+vw(i))
c       or alternatively (giving the same answer):
c        crin=vasol(i)*vbcbg*crina+(1.0-vasol(i)*vbcbg)*crin0
c       endif
ctest
c        if(lambda.eq.0.5) then
c         write(36,*) r(i), real(crin0)
c         write(37,*) r(i), aimag(crin0)
c         write(38,*) r(i), real(crin)
c         write(39,*) r(i), aimag(crin)
c        endif
      elseif(kcomp.eq.0) then
c       Internal mixture of soot and air within the fractal a-mode soot/BC 
        if(r(i).le.rbcn) then
          vbcmix(i)=1.0
        else
          vbcmix(i)=(rbcn/r(i))**(3.0-fracdim(i))
        endif
        crina=cref(3,iband)
        crin0=(1,0)
        crin2=crin0*crin0*(crina*crina+2*crin0*crin0
     $       +2*vbcmix(i)*(crina*crina-crin0*crin0))
     $       /(crina*crina+2*crin0*crin0-vbcmix(i)
     $       *(crina*crina-crin0*crin0))
        crin=csqrt(crin2)
cvol        crin=vbcmix(i)*crina+(1.0-vbcmix(i))*crin0  ! volume mixing 
      endif


      return
      end 

