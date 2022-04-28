subroutine intdrypar0 (lchnk, ncol, Nnatk,                             &   
           cintbg, cintbg05, cintbg125, cintbc, cintbc05, cintbc125,   & 
           cintoc, cintoc05, cintoc125, cintsc, cintsc05, cintsc125,   &
           cintsa, cintsa05, cintsa125, aaeros, aaerol, vaeros, vaerol,&
           cknorm,cknlt05,ckngt125)

   use ppgrid
   use shr_kind_mod, only: r8 => shr_kind_r8
   use opttab,   only: cate, cat, fac, faq, fbc, rh 
   use commondefinitions, only: nmodes, nbmodes

   implicit none

#include <aerodry.h>
!
! Input arguments
!
   integer, intent(in) :: lchnk                     ! chunk identifier
   integer, intent(in) :: ncol                      ! number of atmospheric columns
   real(r8), intent(in) :: Nnatk(pcols,pver,0:nmodes) ! modal aerosol number concentration  
!
! Input-Output arguments
!
   real(r8), intent(inout) :: &
     cknorm(pcols,pver,0:nmodes), cknlt05(pcols,pver,0:nmodes), ckngt125(pcols,pver,0:nmodes)
!
! Output arguments: Modal mass concentrations (cint), area (aaero) and volume (vaero)
! (for AeroCom determination of particle effective radii) of each constituent. cint*05 
! and cint*125 are  for r<0.5um and r>1.25um, respectively. aaeros and vaeros are
! integrated over r<0.5um, and aaerol and vaerol over r>0.5um.  
!
   real(r8), intent(out) :: &
     cintbg(pcols,pver,0:nbmodes), cintbg05(pcols,pver,0:nbmodes), cintbg125(pcols,pver,0:nbmodes), & 
     cintbc(pcols,pver,0:nbmodes), cintbc05(pcols,pver,0:nbmodes), cintbc125(pcols,pver,0:nbmodes), & 
     cintoc(pcols,pver,0:nbmodes), cintoc05(pcols,pver,0:nbmodes), cintoc125(pcols,pver,0:nbmodes), &
     cintsc(pcols,pver,0:nbmodes), cintsc05(pcols,pver,0:nbmodes), cintsc125(pcols,pver,0:nbmodes), &       
     cintsa(pcols,pver,0:nbmodes), cintsa05(pcols,pver,0:nbmodes), cintsa125(pcols,pver,0:nbmodes), &
     aaeros(pcols,pver,0:nbmodes), aaerol(pcols,pver,0:nbmodes),                                    &
     vaeros(pcols,pver,0:nbmodes), vaerol(pcols,pver,0:nbmodes)
!
!---------------------------Local variables-----------------------------
!
      real(r8) a, b, e, eps

      integer i, ierr, kcomp, k, icol

      parameter (eps=1.0e-60_r8)

!       Mode 0, BC(ax):

        kcomp=0

!      initialize output fields
      do k=1,pver
         do icol=1,ncol
        cintbg(icol,k,kcomp)=0.0_r8
        cintbg05(icol,k,kcomp)=0.0_r8
        cintbg125(icol,k,kcomp)=0.0_r8
        cintbc(icol,k,kcomp)=0.0_r8
        cintbc05(icol,k,kcomp)=0.0_r8
        cintbc125(icol,k,kcomp)=0.0_r8
        cintoc(icol,k,kcomp)=0.0_r8
        cintoc05(icol,k,kcomp)=0.0_r8
        cintoc125(icol,k,kcomp)=0.0_r8
        cintsc(icol,k,kcomp)=0.0_r8
        cintsc05(icol,k,kcomp)=0.0_r8
        cintsc125(icol,k,kcomp)=0.0_r8
        cintsa(icol,k,kcomp)=0.0_r8
        cintsa05(icol,k,kcomp)=0.0_r8
        cintsa125(icol,k,kcomp)=0.0_r8
        aaeros(icol,k,kcomp)=0.0_r8
        aaerol(icol,k,kcomp)=0.0_r8
        vaeros(icol,k,kcomp)=0.0_r8
        vaerol(icol,k,kcomp)=0.0_r8
         end do
       end do

        do k=1,pver 
          do icol=1,ncol
         
           if(Nnatk(icol,k,kcomp)>0.0_r8) then

!ccccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5ccccccccc6ccccccccc7cc

         do i=1,19  ! variable number

       if(i==1) then
         cintbg(icol,k,kcomp)=a0cintbg
       elseif(i==2) then
         cintbg05(icol,k,kcomp)=a0cintbg05
       elseif(i==3) then
         cintbg125(icol,k,kcomp)=a0cintbg125
       elseif(i==4) then
        cintbc(icol,k,kcomp)=eps
       elseif(i==5) then
        cintbc05(icol,k,kcomp)=eps
       elseif(i==6) then
        cintbc125(icol,k,kcomp)=eps
       elseif(i==7) then
        cintoc(icol,k,kcomp)=eps
       elseif(i==8) then
        cintoc05(icol,k,kcomp)=eps
       elseif(i==9) then
        cintoc125(icol,k,kcomp)=eps
       elseif(i==10) then
        cintsc(icol,k,kcomp)=eps
       elseif(i==11) then
        cintsc05(icol,k,kcomp)=eps
       elseif(i==12) then
        cintsc125(icol,k,kcomp)=eps
       elseif(i==13) then
        cintsa(icol,k,kcomp)=eps
       elseif(i==14) then
        cintsa05(icol,k,kcomp)=eps
       elseif(i==15) then
        cintsa125(icol,k,kcomp)=eps
       elseif(i==16) then
        aaeros(icol,k,kcomp)=a0aaeros
       elseif(i==17) then
        aaerol(icol,k,kcomp)=a0aaerol
       elseif(i==18) then
        vaeros(icol,k,kcomp)=a0vaeros
       elseif(i==19) then
        vaerol(icol,k,kcomp)=a0vaerol
       endif

         end do ! i=1,19 

           endif
         
       cknorm(icol,k,kcomp)  = a0cintbg
       cknlt05(icol,k,kcomp) = a0cintbg05
       ckngt125(icol,k,kcomp)= a0cintbg125

!       if(k.eq.1.or.k.eq.pver) write(*,*) 'cknorm =', cknorm(icol,k,kcomp)
!       if(k.eq.1.or.k.eq.pver) write(*,*) 'cknlt05 =', cknlt05(icol,k,kcomp)
!       if(k.eq.1.or.k.eq.pver) write(*,*) 'ckngt125 =', ckngt125(icol,k,kcomp)

       end do ! icol
      end do ! k


      return
end subroutine intdrypar0




