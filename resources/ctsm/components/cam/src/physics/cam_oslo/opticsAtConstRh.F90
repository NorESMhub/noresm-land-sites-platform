
 subroutine opticsAtConstRh (lchnk, ncol, pint, rhoda, Nnatk, xrh, irh1, irf, &
           xct, ict1, xfaq, ifaq1, xfbcbg, ifbcbg1,           &
           xfbcbgn, ifbcbgn1, xfac, ifac1, xfbc, ifbc1,       &
           xfombg, ifombg1, vnbc, vaitbc, v_soana,            &
           bext440, bext500, bext550, bext670, bext870,       &
           bebg440, bebg500, bebg550, bebg670, bebg870,       &
           bebc440, bebc500, bebc550, bebc670, bebc870,       &
           beoc440, beoc500, beoc550, beoc670, beoc870,       &
           besu440, besu500, besu550, besu670, besu870,       &
           babs440, babs500, babs550, babs670, babs870,       &
           bebglt1, bebggt1, bebclt1, bebcgt1,                &
           beoclt1, beocgt1, bes4lt1, bes4gt1,                &
           backsc550, babg550, babc550, baoc550, basu550,     & 
           bext440n, bext500n, bext550n, bext670n, bext870n,  &
           bebg440n, bebg500n, bebg550n, bebg670n, bebg870n,  &
           bebc440n, bebc500n, bebc550n, bebc670n, bebc870n,  &
           beoc440n, beoc500n, beoc550n, beoc670n, beoc870n,  &
           besu440n, besu500n, besu550n, besu670n, besu870n,  &
           babs440n, babs500n, babs550n, babs670n, babs870n,  &
           bebglt1n, bebggt1n, bebclt1n, bebcgt1n,            &
           beoclt1n, beocgt1n, bes4lt1n, bes4gt1n,            &
           backsc550n, babg550n, babc550n, baoc550n, basu550n)

!     Extra AeroCom diagnostics requiring table look-ups with constant/fixed RH,
!     i.e. for RH = (/"00","40","55","65","75","85" /) (see opttab.F90)

   use ppgrid
   use shr_kind_mod, only: r8 => shr_kind_r8
   use cam_history,  only: outfld
   use constituents, only: pcnst
   use opttab
   use const
   use aerosoldef
   use commondefinitions
   use physics_types,   only: physics_state

   implicit none

!
! Input arguments
!
   integer,  intent(in) :: lchnk                      ! chunk identifier
   integer,  intent(in) :: ncol                       ! number of atmospheric columns
   real(r8), intent(in) :: pint(pcols,pverp)          ! Model interface pressures (10*Pa)
   real(r8), intent(in) :: rhoda(pcols,pver)          ! Density of dry air (kg/m^3)
   real(r8), intent(in) :: xrh(pcols,pver)            ! level relative humidity (fraction)
   integer,  intent(in) :: irh1(pcols,pver)
   integer,  intent(in) :: irf
   real(r8), intent(in) :: Nnatk(pcols,pver,0:nmodes) ! aerosol mode number concentration  
   real(r8), intent(in) :: vnbc(pcols,pver)
   real(r8), intent(in) :: vaitbc(pcols,pver)
   real(r8), intent(in) :: v_soana(pcols,pver)
   real(r8), intent(in) :: xfombg(pcols,pver)
   integer,  intent(in) :: ifombg1(pcols,pver)
   real(r8), intent(in) :: xfbcbg(pcols,pver)
   integer,  intent(in) :: ifbcbg1(pcols,pver)
   real(r8), intent(in) :: xfbcbgn(pcols,pver)
   integer,  intent(in) :: ifbcbgn1(pcols,pver)
   real(r8), intent(in) :: xct(pcols,pver,nmodes)     ! modal internally mixed SO4+BC+OC conc.
   integer,  intent(in) :: ict1(pcols,pver,nmodes)        
   real(r8), intent(in) :: xfac(pcols,pver,nbmodes)   ! facm for use in the interpolations 
   integer,  intent(in) :: ifac1(pcols,pver,nbmodes)
   real(r8), intent(in) :: xfbc(pcols,pver,nbmodes)   ! fbcm for use in the interpolations 
   integer,  intent(in) :: ifbc1(pcols,pver,nbmodes)
   real(r8), intent(in) :: xfaq(pcols,pver,nbmodes)   ! faqm for use in the interpolations 
   integer,  intent(in) :: ifaq1(pcols,pver,nbmodes)

!
! Output arguments
!
   real(r8), intent(out) :: &
     bext440(pcols,pver,0:nbmodes), babs440(pcols,pver,0:nbmodes), &
     bext500(pcols,pver,0:nbmodes), babs500(pcols,pver,0:nbmodes), &
     bext550(pcols,pver,0:nbmodes), babs550(pcols,pver,0:nbmodes), &
     bext670(pcols,pver,0:nbmodes), babs670(pcols,pver,0:nbmodes), &
     bext870(pcols,pver,0:nbmodes), babs870(pcols,pver,0:nbmodes), &
     bebg440(pcols,pver,0:nbmodes), & 
     bebg500(pcols,pver,0:nbmodes), & 
     bebg550(pcols,pver,0:nbmodes), babg550(pcols,pver,0:nbmodes), &
     bebg670(pcols,pver,0:nbmodes), & 
     bebg870(pcols,pver,0:nbmodes), & 
     bebc440(pcols,pver,0:nbmodes), & 
     bebc500(pcols,pver,0:nbmodes), & 
     bebc550(pcols,pver,0:nbmodes), babc550(pcols,pver,0:nbmodes), &
     bebc670(pcols,pver,0:nbmodes), & 
     bebc870(pcols,pver,0:nbmodes), & 
     beoc440(pcols,pver,0:nbmodes), & 
     beoc500(pcols,pver,0:nbmodes), & 
     beoc550(pcols,pver,0:nbmodes), baoc550(pcols,pver,0:nbmodes), &
     beoc670(pcols,pver,0:nbmodes), & 
     beoc870(pcols,pver,0:nbmodes), & 
     besu440(pcols,pver,0:nbmodes), & 
     besu500(pcols,pver,0:nbmodes), & 
     besu550(pcols,pver,0:nbmodes), basu550(pcols,pver,0:nbmodes), &
     besu670(pcols,pver,0:nbmodes), & 
     besu870(pcols,pver,0:nbmodes), & 
     bebglt1(pcols,pver,0:nbmodes), bebggt1(pcols,pver,0:nbmodes), &
     bebclt1(pcols,pver,0:nbmodes), bebcgt1(pcols,pver,0:nbmodes), &
     beoclt1(pcols,pver,0:nbmodes), beocgt1(pcols,pver,0:nbmodes), &
     bes4lt1(pcols,pver,0:nbmodes), bes4gt1(pcols,pver,0:nbmodes), &  
     backsc550(pcols,pver,0:nbmodes)

   real(r8), intent(out) :: &
     bext440n(pcols,pver,0:nbmodes), babs440n(pcols,pver,0:nbmodes), &
     bext500n(pcols,pver,0:nbmodes), babs500n(pcols,pver,0:nbmodes), &
     bext550n(pcols,pver,0:nbmodes), babs550n(pcols,pver,0:nbmodes), &
     bext670n(pcols,pver,0:nbmodes), babs670n(pcols,pver,0:nbmodes), &
     bext870n(pcols,pver,0:nbmodes), babs870n(pcols,pver,0:nbmodes), &
     bebg440n(pcols,pver,0:nbmodes), & 
     bebg500n(pcols,pver,0:nbmodes), & 
     bebg550n(pcols,pver,0:nbmodes), babg550n(pcols,pver,0:nbmodes), &
     bebg670n(pcols,pver,0:nbmodes), & 
     bebg870n(pcols,pver,0:nbmodes), & 
     bebc440n(pcols,pver,0:nbmodes), & 
     bebc500n(pcols,pver,0:nbmodes), & 
     bebc550n(pcols,pver,0:nbmodes), babc550n(pcols,pver,0:nbmodes), &
     bebc670n(pcols,pver,0:nbmodes), & 
     bebc870n(pcols,pver,0:nbmodes), & 
     beoc440n(pcols,pver,0:nbmodes), & 
     beoc500n(pcols,pver,0:nbmodes), & 
     beoc550n(pcols,pver,0:nbmodes), baoc550n(pcols,pver,0:nbmodes), &
     beoc670n(pcols,pver,0:nbmodes), & 
     beoc870n(pcols,pver,0:nbmodes), & 
     besu440n(pcols,pver,0:nbmodes), & 
     besu500n(pcols,pver,0:nbmodes), & 
     besu550n(pcols,pver,0:nbmodes), basu550n(pcols,pver,0:nbmodes), &
     besu670n(pcols,pver,0:nbmodes), & 
     besu870n(pcols,pver,0:nbmodes), & 
     bebglt1n(pcols,pver,0:nbmodes), bebggt1n(pcols,pver,0:nbmodes), &
     bebclt1n(pcols,pver,0:nbmodes), bebcgt1n(pcols,pver,0:nbmodes), &
     beoclt1n(pcols,pver,0:nbmodes), beocgt1n(pcols,pver,0:nbmodes), &
     bes4lt1n(pcols,pver,0:nbmodes), bes4gt1n(pcols,pver,0:nbmodes), &  
     backsc550n(pcols,pver,0:nbmodes)

!
!---------------------------Local variables-----------------------------
!
   integer  i, k, icol, mplus10, irh
   integer  iloop

   real(r8) deltah
   real(r8) dod550rh(pcols), abs550rh(pcols)
!
   real(r8) babg440(pcols,pver,0:nbmodes), &
            babg500(pcols,pver,0:nbmodes), &
            babg670(pcols,pver,0:nbmodes), &
            babg870(pcols,pver,0:nbmodes), &
            babc440(pcols,pver,0:nbmodes), &
            babc500(pcols,pver,0:nbmodes), &
            babc670(pcols,pver,0:nbmodes), &
            babc870(pcols,pver,0:nbmodes), &
            baoc440(pcols,pver,0:nbmodes), &
            baoc500(pcols,pver,0:nbmodes), &
            baoc670(pcols,pver,0:nbmodes), &
            baoc870(pcols,pver,0:nbmodes), &
            basu440(pcols,pver,0:nbmodes), &
            basu500(pcols,pver,0:nbmodes), &
            basu670(pcols,pver,0:nbmodes), &
            basu870(pcols,pver,0:nbmodes)
   real(r8) ec550rh_aer(pcols,pver), abs550rh_aer(pcols,pver)
   real(r8) bebglt1t(pcols,pver), bebclt1t(pcols,pver), &
            beoclt1t(pcols,pver), bes4lt1t(pcols,pver)
   real(r8) basu550tot(pcols,pver), babc550tot(pcols,pver), baoc550tot(pcols,pver), &
            babc550xt(pcols,pver), baoc550xt(pcols,pver), &
            ba550x(pcols,pver,nbmp1:nmodes), belt1x(pcols,pver,nbmp1:nmodes)
!           Additional AeroCom Phase III output:   
   real(r8) ec440rh_aer(pcols,pver), abs440rh_aer(pcols,pver), &
            ec870rh_aer(pcols,pver), abs870rh_aer(pcols,pver), &
            be550lt1_aer(pcols,pver,0:nbmodes), ec550rhlt1_aer(pcols,pver), &
            abs550rh_bc(pcols,pver), abs550rh_oc(pcols,pver), &
            abs550rh_su(pcols,pver), abs550rh_ss(pcols,pver), &
            abs550rh_du(pcols,pver), ec550rhlt1_bc(pcols,pver), &
            ec550rhlt1_oc(pcols,pver), ec550rhlt1_su(pcols,pver), &
            ec550rhlt1_ss(pcols,pver), ec550rhlt1_du(pcols,pver) 
!  
   real(r8) babg440n(pcols,pver,0:nbmodes), &
            babg500n(pcols,pver,0:nbmodes), &
            babg670n(pcols,pver,0:nbmodes), &
            babg870n(pcols,pver,0:nbmodes), &
            babc440n(pcols,pver,0:nbmodes), &
            babc500n(pcols,pver,0:nbmodes), &
            babc670n(pcols,pver,0:nbmodes), &
            babc870n(pcols,pver,0:nbmodes), &
            baoc440n(pcols,pver,0:nbmodes), &
            baoc500n(pcols,pver,0:nbmodes), &
            baoc670n(pcols,pver,0:nbmodes), &
            baoc870n(pcols,pver,0:nbmodes), &
            basu440n(pcols,pver,0:nbmodes), &
            basu500n(pcols,pver,0:nbmodes), &
            basu670n(pcols,pver,0:nbmodes), &
            basu870n(pcols,pver,0:nbmodes)

   real(r8) bedustlt1(pcols,pver), bedustgt1(pcols,pver), &
            besslt1(pcols,pver), bessgt1(pcols,pver)
   real(r8) bbclt1xt(pcols,pver), &
            boclt1xt(pcols,pver), bocgt1xt(pcols,pver)

   character(len=10) :: modeString
   character(len=20) :: varname
   

!000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000

     belt1x(:,:,:) = 0._r8

      do iloop=1,1

!     BC(ax) mode (hydrophobic, so no rhum needed here):
        call intaeropt0(lchnk, ncol, Nnatk,               &
           bext440, bext500, bext550, bext670, bext870,   &
           bebg440, bebg500, bebg550, bebg670, bebg870,   &
           bebc440, bebc500, bebc550, bebc670, bebc870,   &
           beoc440, beoc500, beoc550, beoc670, beoc870,   &
           besu440, besu500, besu550, besu670, besu870,   &
           babs440, babs500, babs550, babs670, babs870,   &
           bebglt1, bebggt1, bebclt1, bebcgt1,            &
           beoclt1, beocgt1, bes4lt1, bes4gt1,            &
           backsc550, babg550, babc550, baoc550, basu550)

!     SO4(Ait), BC(Ait) and OC(Ait) modes:
      mplus10=0
        call intaeropt1(lchnk, ncol, xrh, irh1, mplus10,  &
           Nnatk, xfombg, ifombg1, xct, ict1, xfac, ifac1,&
           bext440, bext500, bext550, bext670, bext870,   &
           bebg440, bebg500, bebg550, bebg670, bebg870,   &
           bebc440, bebc500, bebc550, bebc670, bebc870,   &
           beoc440, beoc500, beoc550, beoc670, beoc870,   &
           besu440, besu500, besu550, besu670, besu870,   &
           babs440, babs500, babs550, babs670, babs870,   &
           bebglt1, bebggt1, bebclt1, bebcgt1,            &
           beoclt1, beocgt1, bes4lt1, bes4gt1,            &
           backsc550, babg550, babc550, baoc550, basu550)
      mplus10=0
        call intaeropt2to3(lchnk, ncol, xrh, irh1, mplus10, &
           Nnatk, xct, ict1, xfac, ifac1,                   &
           bext440, bext500, bext550, bext670, bext870,     &
           bebg440, bebg500, bebg550, bebg670, bebg870,     &
           bebc440, bebc500, bebc550, bebc670, bebc870,     &
           beoc440, beoc500, beoc550, beoc670, beoc870,     &
           besu440, besu500, besu550, besu670, besu870,     &
           babs440, babs500, babs550, babs670, babs870,     &
           bebglt1, bebggt1, bebclt1, bebcgt1,              &
           beoclt1, beocgt1, bes4lt1, bes4gt1,              &
           backsc550, babg550, babc550, baoc550, basu550)

!     BC&OC(Ait) (4), OC&BC(Ait) mode
      mplus10=0
        call intaeropt4(lchnk, ncol, xrh, irh1, mplus10, Nnatk,  &
           xfbcbg, ifbcbg1, xct, ict1, xfac, ifac1, xfaq, ifaq1, &
           bext440, bext500, bext550, bext670, bext870,          &
           bebg440, bebg500, bebg550, bebg670, bebg870,          &
           bebc440, bebc500, bebc550, bebc670, bebc870,          &
           beoc440, beoc500, beoc550, beoc670, beoc870,          &
           besu440, besu500, besu550, besu670, besu870,          &
           babs440, babs500, babs550, babs670, babs870,          &
           bebglt1, bebggt1, bebclt1, bebcgt1,                   &
           beoclt1, beocgt1, bes4lt1, bes4gt1,                   &
           backsc550, babg550, babc550, baoc550, basu550)
  
!     SO4(Ait75) (5), Mineral (6-7) and Sea-salt (8-10) modes:
        call intaeropt5to10(lchnk, ncol, xrh, irh1, Nnatk,   &
           xct, ict1, xfac, ifac1, xfbc, ifbc1, xfaq, ifaq1, &
           bext440, bext500, bext550, bext670, bext870,      &
           bebg440, bebg500, bebg550, bebg670, bebg870,      &
           bebc440, bebc500, bebc550, bebc670, bebc870,      &
           beoc440, beoc500, beoc550, beoc670, beoc870,      &
           besu440, besu500, besu550, besu670, besu870,      &
           babs440, babs500, babs550, babs670, babs870,      &
           bebglt1, bebggt1, bebclt1, bebcgt1,               &
           beoclt1, beocgt1, bes4lt1, bes4gt1,               &
           backsc550, babg550, babc550, baoc550, basu550)

!     then to the externally mixed SO4(n), BC(n) and OC(n) modes:
      mplus10=1
        call intaeropt2to3(lchnk, ncol, xrh, irh1, mplus10,  &
           Nnatk, xct, ict1, xfac, ifac1,                    &
           bext440n, bext500n, bext550n, bext670n, bext870n, &
           bebg440n, bebg500n, bebg550n, bebg670n, bebg870n, &
           bebc440n, bebc500n, bebc550n, bebc670n, bebc870n, &
           beoc440n, beoc500n, beoc550n, beoc670n, beoc870n, &
           besu440n, besu500n, besu550n, besu670n, besu870n, &
           babs440n, babs500n, babs550n, babs670n, babs870n, &
           bebglt1n, bebggt1n, bebclt1n, bebcgt1n,           &
           beoclt1n, beocgt1n, bes4lt1n, bes4gt1n,           &
           backsc550n, babg550n, babc550n, baoc550n, basu550n)

!     and finally the BC&OC(n) mode:
      mplus10=1
        call intaeropt4(lchnk, ncol, xrh, irh1, mplus10, Nnatk,    &
           xfbcbgn, ifbcbgn1, xct, ict1, xfac, ifac1, xfaq, ifaq1, &
           bext440n, bext500n, bext550n, bext670n, bext870n,       &
           bebg440n, bebg500n, bebg550n, bebg670n, bebg870n,       &
           bebc440n, bebc500n, bebc550n, bebc670n, bebc870n,       &
           beoc440n, beoc500n, beoc550n, beoc670n, beoc870n,       &
           besu440n, besu500n, besu550n, besu670n, besu870n,       &
           babs440n, babs500n, babs550n, babs670n, babs870n,       &
           bebglt1n, bebggt1n, bebclt1n, bebcgt1n,                 &
           beoclt1n, beocgt1n, bes4lt1n, bes4gt1n,                 &
           backsc550n, babg550n, babc550n, baoc550n, basu550n)

      end do ! iloop


!     Initialization
      do k=1,pver  
        do icol=1,ncol
          ec550rh_aer(icol,k)=0.0_r8 
          abs550rh_aer(icol,k)=0.0_r8 
          ec550rhlt1_aer(icol,k)=0.0_r8 
          abs550rh_bc(icol,k)=0.0_r8
          abs550rh_oc(icol,k)=0.0_r8
          abs550rh_su(icol,k)=0.0_r8
          abs550rh_ss(icol,k)=0.0_r8
          abs550rh_du(icol,k)=0.0_r8
          ec440rh_aer(icol,k)=0.0_r8 
          abs440rh_aer(icol,k)=0.0_r8 
          ec870rh_aer(icol,k)=0.0_r8 
          abs870rh_aer(icol,k)=0.0_r8 
          basu550tot(icol,k)=0.0_r8 
          babc550tot(icol,k)=0.0_r8 
          baoc550tot(icol,k)=0.0_r8 
          bebglt1t(icol,k)=0.0_r8
          bebclt1t(icol,k)=0.0_r8
          beoclt1t(icol,k)=0.0_r8
          bes4lt1t(icol,k)=0.0_r8
          bedustlt1(icol,k)=0.0_r8
          besslt1(icol,k)=0.0_r8
        end do
      end do
      do icol=1,ncol
        dod550rh(icol)=0.0_r8 
        abs550rh(icol)=0.0_r8 
      end do

!       Calculation of extinction at given RH and absorption for all r and for r<0.5um
        do k=1,pver
          do icol=1,ncol

            do i=0,10
              ec550rh_aer(icol,k)  = ec550rh_aer(icol,k)+Nnatk(icol,k,i)*bext550(icol,k,i)
              abs550rh_aer(icol,k) = abs550rh_aer(icol,k)+Nnatk(icol,k,i)*babs550(icol,k,i)
              ec440rh_aer(icol,k)  = ec440rh_aer(icol,k)+Nnatk(icol,k,i)*bext440(icol,k,i)
              abs440rh_aer(icol,k) = abs440rh_aer(icol,k)+Nnatk(icol,k,i)*babs440(icol,k,i)
              ec870rh_aer(icol,k)  = ec870rh_aer(icol,k)+Nnatk(icol,k,i)*bext870(icol,k,i)
              abs870rh_aer(icol,k) = abs870rh_aer(icol,k)+Nnatk(icol,k,i)*babs870(icol,k,i)
              basu550tot(icol,k) = basu550tot(icol,k)+Nnatk(icol,k,i)*basu550(icol,k,i)
              babc550tot(icol,k) = babc550tot(icol,k)+Nnatk(icol,k,i)*babc550(icol,k,i)
              baoc550tot(icol,k) = baoc550tot(icol,k)+Nnatk(icol,k,i)*baoc550(icol,k,i)
              bes4lt1t(icol,k) = bes4lt1t(icol,k)+Nnatk(icol,k,i)*bes4lt1(icol,k,i)
              bebclt1t(icol,k) = bebclt1t(icol,k)+Nnatk(icol,k,i)*bebclt1(icol,k,i)
              beoclt1t(icol,k) = beoclt1t(icol,k)+Nnatk(icol,k,i)*beoclt1(icol,k,i)
            enddo
            do i=11,14
              ec550rh_aer(icol,k)  = ec550rh_aer(icol,k)+Nnatk(icol,k,i)*bext550n(icol,k,i-10)
              abs550rh_aer(icol,k) = abs550rh_aer(icol,k)+Nnatk(icol,k,i)*babs550n(icol,k,i-10)
              ec440rh_aer(icol,k)  = ec440rh_aer(icol,k)+Nnatk(icol,k,i)*bext440n(icol,k,i-10)
              abs440rh_aer(icol,k) = abs440rh_aer(icol,k)+Nnatk(icol,k,i)*babs440n(icol,k,i-10)
              ec870rh_aer(icol,k)  = ec870rh_aer(icol,k)+Nnatk(icol,k,i)*bext870n(icol,k,i-10)
              abs870rh_aer(icol,k) = abs870rh_aer(icol,k)+Nnatk(icol,k,i)*babs870n(icol,k,i-10)
              ba550x(icol,k,i)=babs550n(icol,k,i-10)
              belt1x(icol,k,i)=bebglt1n(icol,k,i-10)
            enddo

            do i=6,7
              bedustlt1(icol,k) = bedustlt1(icol,k) + Nnatk(icol,k,i)*bebglt1(icol,k,i)
            enddo
            do i=8,10
              besslt1(icol,k) = besslt1(icol,k) + Nnatk(icol,k,i)*bebglt1(icol,k,i)
            enddo             
            ec550rhlt1_du(icol,k) = bedustlt1(icol,k)
            ec550rhlt1_ss(icol,k) = besslt1(icol,k)

!soa: *(1-v_soan) for the sulfate volume fraction of mode 11
            bbclt1xt(icol,k) = Nnatk(icol,k,12)*belt1x(icol,k,12) &
                             + Nnatk(icol,k,14)*belt1x(icol,k,14)*vnbc(icol,k)
!soa + v_soan part of mode 11 for the OC volume fraction of that mode
            boclt1xt(icol,k) = Nnatk(icol,k,13)*belt1x(icol,k,13) &
                             + Nnatk(icol,k,14)*belt1x(icol,k,14)*(1.0_r8-vnbc(icol,k)) 

!soa: *(1-v_soana) for the sulfate volume fraction of mode 1
            ec550rhlt1_su(icol,k) = bes4lt1t(icol,k)                         &  ! condensate
                  + Nnatk(icol,k,1)*bebglt1(icol,k,1)*(1.0_r8-v_soana(icol,k))&  ! background, SO4(Ait) mode (1)
                  + Nnatk(icol,k,5)*bebglt1(icol,k,5)                            ! background, SO4(Ait75) mode (5)
            ec550rhlt1_bc(icol,k) = bebclt1t(icol,k)+bbclt1xt(icol,k)        &  ! coagulated + n-mode BC (12)
                   + Nnatk(icol,k,2)*bebglt1(icol,k,2)                        &  ! background, BC(Ait) mode (2)
                   + Nnatk(icol,k,4)*bebglt1(icol,k,4)*vaitbc(icol,k)         &  ! background in OC&BC(Ait) mode (4)
                   + Nnatk(icol,k,0)*bebglt1(icol,k,0)                           ! background, BC(ax) mode (0)
!soa + v_soan part of mode 11 for the OC volume fraction of that mode
            ec550rhlt1_oc(icol,k) = beoclt1t(icol,k)+boclt1xt(icol,k)        &  ! coagulated + n-mode OC (13)
                   + Nnatk(icol,k,3)*bebglt1(icol,k,3)                        &  ! background, OC(Ait) mode (3)
                   + Nnatk(icol,k,4)*bebglt1(icol,k,4)*(1.0_r8-vaitbc(icol,k))&  ! background in OC&BC(Ait) mode (4)
                   + Nnatk(icol,k,1)*bebglt1(icol,k,1)*v_soana(icol,k)

            ec550rhlt1_aer(icol,k) = ec550rhlt1_su(icol,k)+ec550rhlt1_bc(icol,k) &
              + ec550rhlt1_oc(icol,k) + ec550rhlt1_ss(icol,k)+ec550rhlt1_du(icol,k)
            ec550rhlt1_aer(icol,k) = 1.e-3_r8*ec550rhlt1_aer(icol,k)

            abs550rh_du(icol,k) = Nnatk(icol,k,6)*babg550(icol,k,6) &
                                 + Nnatk(icol,k,7)*babg550(icol,k,7)
            abs550rh_ss(icol,k) = Nnatk(icol,k,8)*babg550(icol,k,8) &
                                 + Nnatk(icol,k,9)*babg550(icol,k,9) &
                                 + Nnatk(icol,k,10)*babg550(icol,k,10)
!soa: *(1-v_soana) for the sulfate volume fraction of mode 1
            abs550rh_su(icol,k) = basu550tot(icol,k)                   &  ! condensate:w

           + (1.0_r8-v_soana(icol,k))*Nnatk(icol,k,1)*babg550(icol,k,1) &  ! background, SO4(Ait) mode (1)
                                    + Nnatk(icol,k,5)*babg550(icol,k,5)    ! background, SO4(Ait75) mode (5)

!soa: *(1-v_soan) for the sulfate volume fraction
            babc550xt(icol,k) = Nnatk(icol,k,12)*ba550x(icol,k,12)  &
                              + Nnatk(icol,k,14)*ba550x(icol,k,14)*vnbc(icol,k)

            baoc550xt(icol,k) = Nnatk(icol,k,13)*ba550x(icol,k,13) &
                              + Nnatk(icol,k,14)*ba550x(icol,k,14)*(1.0_r8-vnbc(icol,k)) 

            abs550rh_bc(icol,k) = babc550tot(icol,k)+babc550xt(icol,k) &     ! coagulated + n-mode BC (12)
                                 + Nnatk(icol,k,2)*babg550(icol,k,2) &        ! background, BC(Ait) mode (2)
                  + vaitbc(icol,k)*Nnatk(icol,k,4)*babg550(icol,k,4) &        ! background in OC&BC(Ait) mode (4)
                                 + Nnatk(icol,k,0)*babg550(icol,k,0)          ! background, BC(ax) mode (0)

            abs550rh_oc(icol,k) = baoc550tot(icol,k)+baoc550xt(icol,k) &     ! coagulated + n-mode OC (13)
                  + v_soana(icol,k)*Nnatk(icol,k,1)*babg550(icol,k,1) &       ! SOA fraction of mode 1
                                  + Nnatk(icol,k,3)*babg550(icol,k,3) &       ! background, OC(Ait) mode (3)
          + (1.0_r8-vaitbc(icol,k))*Nnatk(icol,k,4)*babg550(icol,k,4)         ! background in OC&BC(Ait) mode (4)

            deltah=1.e-4_r8*(pint(icol,k+1)-pint(icol,k))/(rhoda(icol,k)*9.8_r8)
            dod550rh(icol) = dod550rh(icol)+ec550rh_aer(icol,k)*deltah
            abs550rh(icol) = abs550rh(icol)+abs550rh_aer(icol,k)*deltah

            ec550rh_aer(icol,k)  = 1.e-3_r8*ec550rh_aer(icol,k)
            abs550rh_aer(icol,k) = 1.e-3_r8*abs550rh_aer(icol,k)
            ec440rh_aer(icol,k)  = 1.e-3_r8*ec440rh_aer(icol,k)
            abs440rh_aer(icol,k) = 1.e-3_r8*abs440rh_aer(icol,k)
            ec870rh_aer(icol,k)  = 1.e-3_r8*ec870rh_aer(icol,k)
            abs870rh_aer(icol,k) = 1.e-3_r8*abs870rh_aer(icol,k)

            abs550rh_bc(icol,k)  = 1.e-3_r8*abs550rh_bc(icol,k)
            abs550rh_oc(icol,k)  = 1.e-3_r8*abs550rh_oc(icol,k)
            abs550rh_su(icol,k)  = 1.e-3_r8*abs550rh_su(icol,k)
            abs550rh_ss(icol,k)  = 1.e-3_r8*abs550rh_ss(icol,k)
            abs550rh_du(icol,k)  = 1.e-3_r8*abs550rh_du(icol,k)

          enddo
        enddo 

        if(irf.eq.1) then

          call outfld('ECDRYAER',ec550rh_aer,pcols,lchnk)
          call outfld('ABSDRYAE',abs550rh_aer,pcols,lchnk)
          call outfld('OD550DRY',dod550rh,pcols,lchnk)       ! 2D variable
          call outfld('AB550DRY',abs550rh,pcols,lchnk)       ! 2D variable
          call outfld('ECDRY440',ec440rh_aer,pcols,lchnk)
          call outfld('ABSDR440',abs440rh_aer,pcols,lchnk)
          call outfld('ECDRY870',ec870rh_aer,pcols,lchnk)
          call outfld('ABSDR870',abs870rh_aer,pcols,lchnk)
          call outfld('ECDRYLT1',ec550rhlt1_aer,pcols,lchnk)
!         Since we do not have enough look-up table info to take abs550rhlt1_aer,
!         instead take out abs550rh for each constituent:
          call outfld('ABSDRYBC',abs550rh_bc,pcols,lchnk)
          call outfld('ABSDRYOC',abs550rh_oc,pcols,lchnk)
          call outfld('ABSDRYSU',abs550rh_su,pcols,lchnk)
          call outfld('ABSDRYSS',abs550rh_ss,pcols,lchnk)
          call outfld('ABSDRYDU',abs550rh_du,pcols,lchnk)

        elseif(irf.ge.2) then   ! only happens for AEROCOM_INSITU

          irh=RF(irf)

          modeString="  "
          write(modeString,"(I2)"),irh
          if(RF(irf).eq.0) modeString="00"

!-          varName = "EC44RH"//trim(modeString)
!-          call outfld(varName,ec440rh_aer(:,:),pcols,lchnk)
          varName = "EC55RH"//trim(modeString)
          call outfld(varName,ec550rh_aer(:,:),pcols,lchnk)
!-          varName = "EC87RH"//trim(modeString)
!-          call outfld(varName,ec870rh_aer(:,:),pcols,lchnk)
 
!-          varName = "AB44RH"//trim(modeString)
!-          call outfld(varName,abs440rh_aer(:,:),pcols,lchnk)
          varName = "AB55RH"//trim(modeString)
          call outfld(varName,abs550rh_aer(:,:),pcols,lchnk)
!-          varName = "AB87RH"//trim(modeString)
!-          call outfld(varName,abs870rh_aer(:,:),pcols,lchnk)

        end if ! irf

!000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000


      return
end subroutine opticsAtConstRh

