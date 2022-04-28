    subroutine inputForInterpol (lchnk, ncol, rhum, xrh, irh1,    &
      f_soana, xfombg, ifombg1, faitbc, xfbcbg, ifbcbg1,          &
      fnbc, xfbcbgn, ifbcbgn1, Nnatk, Cam, xct, ict1,             &
      focm, fcm, xfac, ifac1, fbcm, xfbc, ifbc1, faqm, xfaq, ifaq1)

   use ppgrid
   use shr_kind_mod, only: r8 => shr_kind_r8
   use opttab, only: fombg, fbcbg, cate, cat, fac, faq, fbc, rh, eps
   use commondefinitions, only: nbmodes, nmodes

   implicit none

!
! Input arguments
!
      integer, intent(in)  :: lchnk                      ! chunk identifier
      integer, intent(in)  :: ncol                       ! number of atmospheric columns
      real(r8), intent(in) :: rhum(pcols,pver)           ! level relative humidity (fraction)
      real(r8), intent(in) :: f_soana(pcols,pver)        ! SOA/(SOA+H2SO4) mass fraction for the background in mode 1
      real(r8), intent(in) :: faitbc(pcols,pver)         ! BC/(BC + OC) mass fraction for the background in mode 4
      real(r8), intent(in) :: fnbc(pcols,pver)           ! BC/(BC + OC) mass fraction for the background in mode 14
      real(r8), intent(in) :: focm(pcols,pver,4)         ! fraction of added mass which is either SOA condensate or OC coagulate
      real(r8), intent(in) :: Cam(pcols,pver,nbmodes)    ! added internally mixed SO4+BC+OC concentration for a normalized mode
      real(r8), intent(in) :: Nnatk(pcols,pver,0:nmodes) ! aerosol mode number concentration  
      real(r8), intent(in) :: fcm(pcols,pver,nbmodes)    ! fraction of added mass which is either BC or OC/SOA (carbonaceous)
      real(r8), intent(in) :: fbcm(pcols,pver,nbmodes)   ! fraction of added mass as BC/(BC+OC) 
      real(r8), intent(in) :: faqm(pcols,pver,nbmodes)   ! fraction of added sulfate which is from aqueous phase (ammonium sulfate)
      real(r8) :: eps10 = 1.e-10_r8
!
! Output arguments
!
      real(r8), intent(out) :: xrh(pcols,pver)           ! rhum for use in the interpolations
      integer,  intent(out) :: irh1(pcols,pver)
      real(r8), intent(out) :: xfombg(pcols,pver)        ! f_soana for use in the interpolations (mode 1)
      integer,  intent(out) :: ifombg1(pcols,pver)
      real(r8), intent(out) :: xfbcbg(pcols,pver)        ! faitbc for use in the interpolations (mode 4)
      integer,  intent(out) :: ifbcbg1(pcols,pver)
      real(r8), intent(out) :: xfbcbgn(pcols,pver)       ! fnbc for use in the interpolations (mode 14)
      integer,  intent(out) :: ifbcbgn1(pcols,pver)
      real(r8), intent(out) :: xct(pcols,pver,nmodes)    ! Cam/Nnatk for use in the interpolations 
      integer,  intent(out) :: ict1(pcols,pver,nmodes)
      real(r8), intent(out) :: xfac(pcols,pver,nbmodes)  ! focm (1-4) or fcm (5-10) for use in the interpolations 
      integer,  intent(out) :: ifac1(pcols,pver,nbmodes)
      real(r8), intent(out) :: xfbc(pcols,pver,nbmodes)  ! fbcm for use in the interpolations 
      integer,  intent(out) :: ifbc1(pcols,pver,nbmodes)
      real(r8), intent(out) :: xfaq(pcols,pver,nbmodes)  ! faqm for use in the interpolations 
      integer,  intent(out) :: ifaq1(pcols,pver,nbmodes)
!
!---------------------------Local variables-----------------------------
!
      integer k, icol, i, irelh 
!
!------------------------------------------------------------------------
!

!      write(*,*) 'Before xrh-loop'
      do k=1,pver
        do icol=1,ncol
          xrh(icol,k)  = min(max(rhum(icol,k),rh(1)),rh(10))
        end do 
      end do

!      write(*,*) 'Before rh-loop'
      do irelh=1,9
       do k=1,pver
        do icol=1,ncol
           if(xrh(icol,k) >= rh(irelh).and. &
             xrh(icol,k)<=rh(irelh+1)) then
             irh1(icol,k)=irelh
           endif
         end do
       end do
      end do
!      write(*,*) 'xrh, irh1, irh2 =', xrh(1,26), irh1(1,26), irh2(1,26)

      do k=1,pver
        do icol=1,ncol
!       find common xfombg, ifombg1 and ifombg2 for use in the interpolation routines
          xfombg(icol,k) =min(max(f_soana(icol,k),fombg(1)),fombg(6))
          ifombg1(icol,k)=int(5.0_r8*xfombg(icol,k)-eps10)+1                       ! Boer linkes til def. i opttab.F90
        end do
      enddo  

      do k=1,pver
       do icol=1,ncol
!       find common xfbcbg, ifbcbg1 and ifbcbg2 for use in the interpolation routines
          xfbcbg(icol,k) =min(max(faitbc(icol,k),fbcbg(1)),fbcbg(6))               ! Boer linkes til def. i opttab.F90
          ifbcbg1(icol,k)=min(max(int(4*log10(xfbcbg(icol,k))+6),1),5)
!       find common xfbcbgn, ifbcbgn1 and ifbcbgn2 for use in the interpolation routines
          xfbcbgn(icol,k) =min(max(fnbc(icol,k),fbcbg(1)),fbcbg(6))                ! Boer linkes til def. i opttab.F90
          ifbcbgn1(icol,k)=min(max(int(4*log10(xfbcbgn(icol,k))+6),1),5)
       end do
      enddo  

      do i=1,4
       do k=1,pver
        do icol=1,ncol
!        find common xfac, ifac1 and ifac2 for use in the interpolation routines
          xfac(icol,k,i) =min(max(focm(icol,k,i),fac(1)),fac(6))
          ifac1(icol,k,i)=int(5.0_r8*xfac(icol,k,i)-eps10)+1                       ! Boer linkes til def. i opttab.F90
        end do
       enddo  
      enddo  
      do i=5,nbmodes
       do k=1,pver
        do icol=1,ncol
!        find common xfac, ifac1 and ifac2 for use in the interpolation routines
          xfac(icol,k,i) =min(max(fcm(icol,k,i),fac(1)),fac(6))
          ifac1(icol,k,i)=int(5.0_r8*xfac(icol,k,i)-eps10)+1                       ! Boer linkes til def. i opttab.F90
        end do
       enddo  
      enddo  

      do i=1,nbmodes
       do k=1,pver
        do icol=1,ncol
!        find common xfbc, ifbc1 and ifbc2 for use in the interpolation routines
          xfbc(icol,k,i) =min(max(fbcm(icol,k,i),fbc(1)),fbc(6))                  ! Boer linkes til def. i opttab.F90
          ifbc1(icol,k,i)=min(max(int(4*log10(xfbc(icol,k,i))+6),1),5)
        end do
       enddo  
      enddo  

      do i=1,nbmodes
       do k=1,pver
        do icol=1,ncol
!        find common xfaq, ifaq1 and ifaq2 for use in the interpolation routines
          xfaq(icol,k,i) =min(max(faqm(icol,k,i),faq(1)),faq(6))
          ifaq1(icol,k,i)=int(5.0_r8*xfaq(icol,k,i)-eps10)+1                       ! Boer linkes til def. i opttab.F90
        end do
       enddo  
      enddo  

!     find common xct, ict1 and ict2 for use in the interpolation routines         ! Boer linkes til def. i opttab.F90
      do i=1,4
       do k=1,pver
        do icol=1,ncol
          xct(icol,k,i)=min(max(Cam(icol,k,i)/(Nnatk(icol,k,i)+eps),cate(i,1)),cate(i,16))     
          if(i.le.2) then
            ict1(icol,k,i)=min(max(int(3*log10(xct(icol,k,i))+19.666_r8),1),15)
          elseif(i.eq.3) then   ! mode not used
            xct(icol,k,i)=cate(i,1)
            ict1(icol,k,i)=1
          else
            ict1(icol,k,i)=min(max(int(3*log10(xct(icol,k,i))+13.903_r8),1),15)
          endif
        end do
       end do  
      end do

      do i=5,10
       do k=1,pver
        do icol=1,ncol
          xct(icol,k,i)=min(max(Cam(icol,k,i)/(Nnatk(icol,k,i)+eps),cat(i,1)),cat(i,6))
          if(i.eq.5) then
            ict1(icol,k,i)=min(max(int(log10(xct(icol,k,i))+4.824_r8),1),5)
          elseif(i.eq.6) then
            ict1(icol,k,i)=min(max(int(log10(xct(icol,k,i))+4.523_r8),1),5)
          elseif(i.eq.7) then
            ict1(icol,k,i)=min(max(int(log10(xct(icol,k,i))+4.699_r8),1),5)
          elseif(i.eq.8) then
            ict1(icol,k,i)=min(max(int(log10(xct(icol,k,i))+5.921_r8),1),5)
          elseif(i.eq.9) then
            ict1(icol,k,i)=min(max(int(log10(xct(icol,k,i))+4.301_r8),1),5)
          else
            ict1(icol,k,i)=min(max(int(log10(xct(icol,k,i))+4.699_r8),1),5)
          endif
        end do
       end do  
      end do

      do i=11,nmodes      ! for the externally mixed modes 11-14 (now only 12 and 14)
       do k=1,pver
        do icol=1,ncol
          xct(icol,k,i)=cate(i-10,1)
          ict1(icol,k,i)=1
        end do
       end do  
      end do

      return

end subroutine inputForInterpol
