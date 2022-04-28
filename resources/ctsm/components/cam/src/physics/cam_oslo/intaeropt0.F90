subroutine intaeropt0 (lchnk, ncol, Nnatk, &
           bext440, bext500, bext550, bext670, bext870,                &
           bebg440, bebg500, bebg550, bebg670, bebg870,                &
           bebc440, bebc500, bebc550, bebc670, bebc870,                &
           beoc440, beoc500, beoc550, beoc670, beoc870,                &
           besu440, besu500, besu550, besu670, besu870,                &
           babs440, babs500, babs550, babs670, babs870,                &
           bebg550lt1, bebg550gt1, bebc550lt1, bebc550gt1,             &
           beoc550lt1, beoc550gt1, besu550lt1, besu550gt1,             &
           backsc550, babg550, babc550, baoc550, basu550) 


   use ppgrid
   use shr_kind_mod, only: r8 => shr_kind_r8
   use opttab,   only: cate, cat, fac, faq, fbc 
   use commondefinitions, only: nmodes, nbmodes

   implicit none

#include <aerocopt2.h>
!
! Input arguments
!
   integer, intent(in) :: lchnk                     ! chunk identifier
   integer, intent(in) :: ncol                      ! number of atmospheric columns
   real(r8), intent(in) :: Nnatk(pcols,pver,0:nmodes) ! modal aerosol number concentration  
!
! Output arguments: Modal total and absorption extiction coefficients (for AeroCom)
! for 440nm, 500nm, 550nm, 670nm and 870nm, and for d<1um (lt1) and d>1um (gt1).
! March 2009: + backscatter coefficient, backsc550 (km-1 sr-1).

   real(r8), intent(out) :: &
     bext440(pcols,pver,0:nbmodes), babs440(pcols,pver,0:nbmodes), &
     bext500(pcols,pver,0:nbmodes), babs500(pcols,pver,0:nbmodes), &
     bext550(pcols,pver,0:nbmodes), babs550(pcols,pver,0:nbmodes), &
     bext670(pcols,pver,0:nbmodes), babs670(pcols,pver,0:nbmodes), &
     bext870(pcols,pver,0:nbmodes), babs870(pcols,pver,0:nbmodes), &
     bebg440(pcols,pver,0:nbmodes), & ! babg440(pcols,pver,0:nbmodes), &
     bebg500(pcols,pver,0:nbmodes), & ! babg500(pcols,pver,0:nbmodes), &
     bebg550(pcols,pver,0:nbmodes), babg550(pcols,pver,0:nbmodes), &
     bebg670(pcols,pver,0:nbmodes), & ! babg670(pcols,pver,0:nbmodes), &
     bebg870(pcols,pver,0:nbmodes), & ! babg870(pcols,pver,0:nbmodes), &
     bebc440(pcols,pver,0:nbmodes), & ! babc440(pcols,pver,0:nbmodes), &
     bebc500(pcols,pver,0:nbmodes), & ! babc500(pcols,pver,0:nbmodes), &
     bebc550(pcols,pver,0:nbmodes), babc550(pcols,pver,0:nbmodes),     &
     bebc670(pcols,pver,0:nbmodes), & ! babc670(pcols,pver,0:nbmodes), &
     bebc870(pcols,pver,0:nbmodes), & ! babc870(pcols,pver,0:nbmodes), &
     beoc440(pcols,pver,0:nbmodes), & ! baoc440(pcols,pver,0:nbmodes), &
     beoc500(pcols,pver,0:nbmodes), & ! baoc500(pcols,pver,0:nbmodes), &
     beoc550(pcols,pver,0:nbmodes), baoc550(pcols,pver,0:nbmodes),     &
     beoc670(pcols,pver,0:nbmodes), & ! baoc670(pcols,pver,0:nbmodes), &
     beoc870(pcols,pver,0:nbmodes), & ! baoc870(pcols,pver,0:nbmodes), &
     besu440(pcols,pver,0:nbmodes), & ! basu440(pcols,pver,0:nbmodes), &
     besu500(pcols,pver,0:nbmodes), & ! basu500(pcols,pver,0:nbmodes), &
     besu550(pcols,pver,0:nbmodes), basu550(pcols,pver,0:nbmodes),     &
     besu670(pcols,pver,0:nbmodes), & ! basu670(pcols,pver,0:nbmodes), &
     besu870(pcols,pver,0:nbmodes), & ! basu870(pcols,pver,0:nbmodes), &
     bebg550lt1(pcols,pver,0:nbmodes), bebg550gt1(pcols,pver,0:nbmodes), &
     bebc550lt1(pcols,pver,0:nbmodes), bebc550gt1(pcols,pver,0:nbmodes), &
     beoc550lt1(pcols,pver,0:nbmodes), beoc550gt1(pcols,pver,0:nbmodes), &
     besu550lt1(pcols,pver,0:nbmodes), besu550gt1(pcols,pver,0:nbmodes), &  
     backsc550(pcols,pver,0:nbmodes)
!
!---------------------------Local variables-----------------------------
!

      integer i, iv, ierr, k, kcomp, icol

      kcomp=0

!       BC(ax) mode: 

!     initialize all output fields
      do k=1,pver
        do icol=1,ncol
         bext440(icol,k,kcomp)=0.0_r8 
         babs440(icol,k,kcomp)=0.0_r8 
         bext500(icol,k,kcomp)=0.0_r8 
         babs500(icol,k,kcomp)=0.0_r8 
         bext550(icol,k,kcomp)=0.0_r8 
         babs550(icol,k,kcomp)=0.0_r8 
         bext670(icol,k,kcomp)=0.0_r8 
         babs670(icol,k,kcomp)=0.0_r8 
         bext870(icol,k,kcomp)=0.0_r8 
         babs870(icol,k,kcomp)=0.0_r8 
         bebg440(icol,k,kcomp)=0.0_r8 
!         babg440(icol,k,kcomp)=0.0_r8 
         bebg500(icol,k,kcomp)=0.0_r8 
!         babg500(icol,k,kcomp)=0.0_r8 
         bebg550(icol,k,kcomp)=0.0_r8 
         babg550(icol,k,kcomp)=0.0_r8 
         bebg670(icol,k,kcomp)=0.0_r8 
!         babg670(icol,k,kcomp)=0.0_r8 
         bebg870(icol,k,kcomp)=0.0_r8 
!         babg870(icol,k,kcomp)=0.0_r8 
         bebc440(icol,k,kcomp)=0.0_r8 
!         babc440(icol,k,kcomp)=0.0_r8 
         bebc500(icol,k,kcomp)=0.0_r8 
!         babc500(icol,k,kcomp)=0.0_r8 
         bebc550(icol,k,kcomp)=0.0_r8 
         babc550(icol,k,kcomp)=0.0_r8 
         bebc670(icol,k,kcomp)=0.0_r8 
!         babc670(icol,k,kcomp)=0.0_r8 
         bebc870(icol,k,kcomp)=0.0_r8 
!         babc870(icol,k,kcomp)=0.0_r8 
         beoc440(icol,k,kcomp)=0.0_r8 
!         baoc440(icol,k,kcomp)=0.0_r8 
         beoc500(icol,k,kcomp)=0.0_r8 
!         baoc500(icol,k,kcomp)=0.0_r8 
         beoc550(icol,k,kcomp)=0.0_r8 
         baoc550(icol,k,kcomp)=0.0_r8 
         beoc670(icol,k,kcomp)=0.0_r8 
!         baoc670(icol,k,kcomp)=0.0_r8 
         beoc870(icol,k,kcomp)=0.0_r8 
!         baoc870(icol,k,kcomp)=0.0_r8 
         besu440(icol,k,kcomp)=0.0_r8 
!         basu440(icol,k,kcomp)=0.0_r8 
         besu500(icol,k,kcomp)=0.0_r8 
!         basu500(icol,k,kcomp)=0.0_r8 
         besu550(icol,k,kcomp)=0.0_r8 
         basu550(icol,k,kcomp)=0.0_r8 
         besu670(icol,k,kcomp)=0.0_r8 
!         basu670(icol,k,kcomp)=0.0_r8 
         besu870(icol,k,kcomp)=0.0_r8 
!         basu870(icol,k,kcomp)=0.0_r8 
         bebg550lt1(icol,k,kcomp)=0.0_r8 
         bebg550gt1(icol,k,kcomp)=0.0_r8 
         bebc550lt1(icol,k,kcomp)=0.0_r8 
         bebc550gt1(icol,k,kcomp)=0.0_r8 
         beoc550lt1(icol,k,kcomp)=0.0_r8 
         beoc550gt1(icol,k,kcomp)=0.0_r8 
         besu550lt1(icol,k,kcomp)=0.0_r8 
         besu550gt1(icol,k,kcomp)=0.0_r8 
         backsc550(icol,k,kcomp)=0.0_r8 
        end do
      end do

        do k=1,pver 
          do icol=1,ncol
         
           if(Nnatk(icol,k,kcomp).gt.0) then

         bext440(icol,k,kcomp)=bex440
         babs440(icol,k,kcomp)=bax440
         bext500(icol,k,kcomp)=bex500
         babs500(icol,k,kcomp)=bax500
         bext550(icol,k,kcomp)=bex550lt1+bex550gt1
         babs550(icol,k,kcomp)=bax550
         bext670(icol,k,kcomp)=bex670
         babs670(icol,k,kcomp)=bax670
         bext870(icol,k,kcomp)=bex870
         babs870(icol,k,kcomp)=bax870
         bebg440(icol,k,kcomp)=bex440
!         babg440(icol,k,kcomp)=bax440
         bebg500(icol,k,kcomp)=bex500
!         babg500(icol,k,kcomp)=bax500
         bebg550(icol,k,kcomp)=bex550lt1+bex550gt1
         babg550(icol,k,kcomp)=bax550
         bebg670(icol,k,kcomp)=bex670
!         babg670(icol,k,kcomp)=bax670
         bebg870(icol,k,kcomp)=bex870
!         babg870(icol,k,kcomp)=bax870
         bebc440(icol,k,kcomp)=0.0_r8
!         babc440(icol,k,kcomp)=0.0_r8
         bebc500(icol,k,kcomp)=0.0_r8
!         babc500(icol,k,kcomp)=0.0_r8
         bebc670(icol,k,kcomp)=0.0_r8
!         babc670(icol,k,kcomp)=0.0_r8
         bebc870(icol,k,kcomp)=0.0_r8
!         babc870(icol,k,kcomp)=0.0_r8
         beoc440(icol,k,kcomp)=0.0_r8
!         baoc440(icol,k,kcomp)=0.0_r8
         beoc500(icol,k,kcomp)=0.0_r8
!         baoc500(icol,k,kcomp)=0.0_r8
         beoc670(icol,k,kcomp)=0.0_r8
!         baoc670(icol,k,kcomp)=0.0_r8
         beoc870(icol,k,kcomp)=0.0_r8
!         baoc870(icol,k,kcomp)=0.0_r8
         besu440(icol,k,kcomp)=0.0_r8
!         basu440(icol,k,kcomp)=0.0_r8
         besu500(icol,k,kcomp)=0.0_r8
!         basu500(icol,k,kcomp)=0.0_r8
         besu670(icol,k,kcomp)=0.0_r8
!         basu670(icol,k,kcomp)=0.0_r8
         besu870(icol,k,kcomp)=0.0_r8
!         basu870(icol,k,kcomp)=0.0_r8
         bebg550lt1(icol,k,kcomp)=bex550lt1
         bebg550gt1(icol,k,kcomp)=bex550gt1
         bebc550lt1(icol,k,kcomp)=0.0_r8
         bebc550gt1(icol,k,kcomp)=0.0_r8
         beoc550lt1(icol,k,kcomp)=0.0_r8
         beoc550gt1(icol,k,kcomp)=0.0_r8
         besu550lt1(icol,k,kcomp)=0.0_r8
         besu550gt1(icol,k,kcomp)=0.0_r8
         backsc550(icol,k,kcomp)=backscx550

           endif
         
       end do ! icol
      end do ! k

      return
end subroutine intaeropt0




