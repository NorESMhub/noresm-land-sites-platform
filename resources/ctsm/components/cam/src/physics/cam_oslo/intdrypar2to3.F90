subroutine intdrypar2to3 (lchnk, ncol, Nnatk, xct, ict1, xfac, ifac1,   & 
           cintbg, cintbg05, cintbg125, cintbc, cintbc05, cintbc125,    & 
           cintoc, cintoc05, cintoc125, cintsc, cintsc05, cintsc125,    &
           cintsa, cintsa05, cintsa125, aaeros, aaerol, vaeros, vaerol, &
           aaerosn,aaeroln,vaerosn,vaeroln,cknorm,cknlt05,ckngt125)

   use ppgrid
   use shr_kind_mod, only: r8 => shr_kind_r8
   use opttab,   only: cate, cat, fac, nbmp1
   use commondefinitions, only: nmodes, nbmodes

   implicit none

#include <aerodry.h>
!
! Input arguments
!
   integer, intent(in) :: lchnk                       ! chunk identifier
   integer, intent(in) :: ncol                        ! number of atmospheric columns
   real(r8), intent(in) :: Nnatk(pcols,pver,0:nmodes) ! modal aerosol number concentration  
   real(r8), intent(in) :: xct(pcols,pver,nmodes)     ! modal internally mixed SO4+BC+OC conc.
   integer,  intent(in) :: ict1(pcols,pver,nmodes)        
   real(r8), intent(in) :: xfac(pcols,pver,nbmodes)   ! condensed SOA/(SOA+H2SO4) (1-4) or added carbonaceous fraction (5-10)
   integer,  intent(in) :: ifac1(pcols,pver,nbmodes)        
!
! Input-Output arguments
!
   real(r8), intent(inout) :: &
     aaerosn(pcols,pver,nbmp1:nmodes), aaeroln(pcols,pver,nbmp1:nmodes), &
     vaerosn(pcols,pver,nbmp1:nmodes), vaeroln(pcols,pver,nbmp1:nmodes), &
     cknorm(pcols,pver,0:nmodes), cknlt05(pcols,pver,0:nmodes), ckngt125(pcols,pver,0:nmodes)
!
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

      integer iv, kcomp, k, icol

!      Temporary storage of often used array elements
      integer t_ict1, t_ict2
      real(r8) t_xct,  t_cat1, t_cat2
      real(r8) t_fac1, t_fac2, t_xfac
      integer  t_ifc1, t_ifc2
      real(r8) d2mx(2), dxm1(2), invd(2)
      real(r8) opt2d(2,2)
      real(r8) opt1, opt2, opt

      parameter (e=2.718281828_r8, eps=1.0e-60_r8)


!      write(*,*) 'Before kcomp-loop'

!       Modes 1-3,  SO4(Ait), BC(Ait) and OC(Ait):

      do kcomp=2,3

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
 
        end do ! kcomp

        do kcomp=2,2

        do k=1,pver 
          do icol=1,ncol
         
           if(Nnatk(icol,k,kcomp)>0.0_r8) then

!      Collect all the vector elements into temporary storage
!      to avoid cache conflicts and excessive cross-referencing
      t_ict1 = ict1(icol,k,kcomp)
      t_ict2 = t_ict1+1
      t_cat1 = cate(kcomp,t_ict1)
      t_cat2 = cate(kcomp,t_ict2)
      t_xct  = xct(icol,k,kcomp)
      t_ifc1 = ifac1(icol,k,kcomp)
      t_ifc2 = t_ifc1+1
      t_fac1 = fac(t_ifc1)
      t_fac2 = fac(t_ifc2)
      t_xfac = xfac(icol,k,kcomp)

!     partial lengths along each dimension (1-2) for interpolation 
      d2mx(1) = (t_cat2-t_xct)
      dxm1(1) = (t_xct-t_cat1)
      invd(1) = 1.0_r8/(t_cat2-t_cat1)
      d2mx(2) = (t_fac2-t_xfac)
      dxm1(2) = (t_xfac-t_fac1)
      invd(2) = 1.0_r8/(t_fac2-t_fac1)


!ccccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5ccccccccc6ccccccccc7cc

         do iv=1,19  ! variable number

!     end points as basis for multidimentional linear interpolation  
      opt2d(1,1)=a2to3var(iv,t_ict1,t_ifc1,kcomp)
      opt2d(1,2)=a2to3var(iv,t_ict1,t_ifc2,kcomp)
      opt2d(2,1)=a2to3var(iv,t_ict2,t_ifc1,kcomp)
      opt2d(2,2)=a2to3var(iv,t_ict2,t_ifc2,kcomp)

!     interpolation in the fac dimension
      opt1=(d2mx(2)*opt2d(1,1)+dxm1(2)*opt2d(1,2))*invd(2)
      opt2=(d2mx(2)*opt2d(2,1)+dxm1(2)*opt2d(2,2))*invd(2)

!     finally, interpolation in the cat dimension 
      opt = (d2mx(1)*opt1+dxm1(1)*opt2)*invd(1)

!      if(k.eq.1) write(*,*) 'opt2to3 =', opt

!      write(*,*) 'Before array'

       if(iv==1) then
         cintbg(icol,k,kcomp)=opt
       elseif(iv==2) then
         cintbg05(icol,k,kcomp)=opt
       elseif(iv==3) then
         cintbg125(icol,k,kcomp)=opt
       elseif(iv==4) then
        cintbc(icol,k,kcomp)=opt
       elseif(iv==5) then
        cintbc05(icol,k,kcomp)=opt
       elseif(iv==6) then
        cintbc125(icol,k,kcomp)=opt
       elseif(iv==7) then
        cintoc(icol,k,kcomp)=opt
       elseif(iv==8) then
        cintoc05(icol,k,kcomp)=opt
       elseif(iv==9) then
        cintoc125(icol,k,kcomp)=opt
       elseif(iv==10) then
        cintsc(icol,k,kcomp)=opt
       elseif(iv==11) then
        cintsc05(icol,k,kcomp)=opt
       elseif(iv==12) then
        cintsc125(icol,k,kcomp)=opt
       elseif(iv==13) then
        cintsa(icol,k,kcomp)=opt
       elseif(iv==14) then
        cintsa05(icol,k,kcomp)=opt
       elseif(iv==15) then
        cintsa125(icol,k,kcomp)=opt
       elseif(iv==16) then
        aaeros(icol,k,kcomp)=opt
       elseif(iv==17) then
        aaerol(icol,k,kcomp)=opt
       elseif(iv==18) then
        vaeros(icol,k,kcomp)=opt
       elseif(iv==19) then
        vaerol(icol,k,kcomp)=opt
       endif
         end do ! iv=1,19 

           endif
     
       end do ! icol
      end do ! k

        end do  ! kcomp

!      Dry parameters for externally mixed modes modes 12-13,  
!      BC(n) and OC(n):

        do kcomp=12,13    ! using dummy initialization for kcomp=3
!        do kcomp=12,12

        do k=1,pver 
          do icol=1,ncol

           cknorm(icol,k,kcomp)  = a2to3var(1,1,1,kcomp-10)
           cknlt05(icol,k,kcomp) = a2to3var(2,1,1,kcomp-10)
           ckngt125(icol,k,kcomp)= a2to3var(3,1,1,kcomp-10)
           aaerosn(icol,k,kcomp) = a2to3var(16,1,1,kcomp-10)
           aaeroln(icol,k,kcomp) = a2to3var(17,1,1,kcomp-10)
           vaerosn(icol,k,kcomp) = a2to3var(18,1,1,kcomp-10)
           vaeroln(icol,k,kcomp) = a2to3var(19,1,1,kcomp-10)

         end do ! icol
        end do ! k

       end do  ! kcomp


      return
end subroutine intdrypar2to3
