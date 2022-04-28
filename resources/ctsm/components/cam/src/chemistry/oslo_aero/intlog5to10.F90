module intlog5to10

contains

	subroutine intlog5to10_sub (ncol, ind, kcomp, xctin, Nnat, &
                         xfacin, xfbcin, xfaqin, cxs, xstdv, xrk)

!Created by Trude Storelvmo, fall 2007, based on method of A. Kirkevag. 
!This subroutine gives as output the "new" modal radius and standard deviation 
!for a given aerosol mode, kcomp 1-5. These parameters are calculated for a 
!best lognormal fit approximation of the aerosol size distribution. 
!This because the aerosol activation routine (developed by Abdul-Razzak & Ghan,
!2000) requires the size distribution to be described by lognormal modes.
!Rewritten by Alf Kirkevaag September 2015 to a more generalized for for 
!interpolations using common subroutines interpol*dim.

      use shr_kind_mod, only: r8 => shr_kind_r8
      use ppgrid, only :  pcols
      use commondefinitions, only: nmodes, nbmodes
      use const, only : sss, rrr
      use opttab,   only: cat, fbc, fac, faq

      implicit none

      integer, intent(in) :: ncol
      integer, intent(in) :: ind(pcols)
      integer, intent(in) :: kcomp
      real(r8), intent(in) :: Nnat(pcols)          ! Modal number concentration
      real(r8), intent(in) :: xctin(pcols)        ! total internally mixed conc. (ug/m3)	
      real(r8), intent(in) :: xfacin(pcols)        ! = (Cbc+Coc)/(Cbc+Coc+Cso4)
      real(r8), intent(in) :: xfbcin(pcols)        ! = Cbc/(Cbc+Coc)
      real(r8), intent(in) :: xfaqin(pcols)        ! = Cso4a2/(Cso4a1+Cso4a2)
      real(r8), intent(out) :: xstdv(pcols)        ! log10 of standard deviation of lognormal fit
      real(r8), intent(out) :: xrk(pcols)          ! Modal radius of lognormal fit
      real(r8), intent(out) :: cxs(pcols)        ! excess (modal) internally mixed conc.

      real(r8) xctsave, camdiff
      real(r8), dimension(pcols) :: xct, xfac,  xfbc, xfaq  

      integer lon, long

      integer i, ictot, ifac, ifbc, ifaq, &
        ict1, ict2, ifac1, ifac2, &
        ifbc1, ifbc2, ifaq1, ifaq2

      real(r8) t_fac1, t_fac2, t_xfac, t_xct, t_cat1, t_cat2, &
        t_faq1, t_faq2, t_xfaq, t_fbc1, t_fbc2, t_xfbc 
      real(r8) r1, r2, s1, s2, tmp, e
      real(r8) d2mx(4), dxm1(4), invd(4)
      real(r8) sizepar4d(2,2,2,2)

       real(r8), parameter :: eps=1.0e-10_r8

!     Initialize excess mass cxs, wrt. maximum allowed internal mixing
      do lon=1,ncol
        cxs(lon) = 0.0_r8
        xct(lon)  = 0.0_r8
        xfac(lon) = 0.0_r8
        xfbc(lon) = 0.0_r8
        xfaq(lon) = 0.0_r8
      enddo

!ces: All loops "do long=1,nlons" combined to one loop:

!      do lon=1,ncol
      do long=1,ncol
       lon=ind(long)
       xstdv(lon) = 0._r8
       xrk(lon) = 0._r8

	xct(lon)  = min(max(xctin(lon)/(Nnat(lon)+eps),cat(kcomp,1)),cat(kcomp,6))
	xfac(lon) = min(max(xfacin(lon),fac(1)),fac(6))
	xfbc(lon) = min(max(xfbcin(lon),fbc(1)),fbc(6))
	xfaq(lon) = min(max(xfaqin(lon),faq(1)),faq(6))

        camdiff   = xctin(lon)-xct(lon)*(Nnat(lon)+eps)

        cxs(lon)  = max(0.0_r8,camdiff)

        ictot=1
	tmp = xct(lon)
	do while (ictot.lt.5.and.(tmp.lt.cat(kcomp,ictot).or. &
                tmp.gt.cat(kcomp,ictot+1)))
	 ictot=ictot+1
	enddo
	ict1=ictot
	ict2=ictot+1

	ifac=1
	tmp = xfac(lon)
	do while (ifac.lt.5.and.(tmp.lt.fac(ifac).or. &
                 tmp.gt.fac(ifac+1)))
	 ifac=ifac+1
	enddo
	ifac1=ifac
	ifac2=ifac+1

	ifbc=1
	tmp = xfbc(lon)
	do while (ifbc.lt.5.and.(tmp.lt.fbc(ifbc).or. &
                 tmp.gt.fbc(ifbc+1)))
	 ifbc=ifbc+1
	enddo
	ifbc1=ifbc
	ifbc2=ifbc+1

	ifaq=1
	tmp = xfaq(lon)
	do while (ifaq.lt.5.and.(tmp.lt.faq(ifaq) &
                 .or.tmp.gt.faq(ifaq+1)))
	 ifaq=ifaq+1
        enddo
	ifaq1=ifaq
	ifaq2=ifaq+1

!      Collect all the vector elements into temporary storage
!      to avoid cache conflicts and excessive cross-referencing
      t_cat1 = cat(kcomp,ict1)
      t_cat2 = cat(kcomp,ict2)
      t_fac1 = fac(ifac1)
      t_fac2 = fac(ifac2)
      t_fbc1 = fbc(ifbc1)
      t_fbc2 = fbc(ifbc2)
      t_faq1 = faq(ifaq1)
      t_faq2 = faq(ifaq2)

      t_xct  = xct(lon)
      t_xfac = xfac(lon)
      t_xfbc = xfbc(lon)
      t_xfaq = xfaq(lon)

!     partial lengths along each dimension (1-4) for interpolation 
      d2mx(1) = (t_cat2-t_xct)
      dxm1(1) = (t_xct-t_cat1)
      invd(1) = 1.0_r8/(t_cat2-t_cat1)
      d2mx(2) = (t_fac2-t_xfac)
      dxm1(2) = (t_xfac-t_fac1)
      invd(2) = 1.0_r8/(t_fac2-t_fac1)
      d2mx(3) = (t_fbc2-t_xfbc)
      dxm1(3) = (t_xfbc-t_fbc1)
      invd(3) = 1.0_r8/(t_fbc2-t_fbc1)
      d2mx(4) = (t_faq2-t_xfaq)
      dxm1(4) = (t_xfaq-t_faq1)
      invd(4) = 1.0_r8/(t_faq2-t_faq1)

!     Table points as basis for multidimentional linear interpolation,
!     modal median radius:  

      sizepar4d(1,1,1,1)=rrr(kcomp,ict1,ifac1,ifbc1,ifaq1)
      sizepar4d(1,1,1,2)=rrr(kcomp,ict1,ifac1,ifbc1,ifaq2)
      sizepar4d(1,1,2,1)=rrr(kcomp,ict1,ifac1,ifbc2,ifaq1)
      sizepar4d(1,1,2,2)=rrr(kcomp,ict1,ifac1,ifbc2,ifaq2)
      sizepar4d(1,2,1,1)=rrr(kcomp,ict1,ifac2,ifbc1,ifaq1)
      sizepar4d(1,2,1,2)=rrr(kcomp,ict1,ifac2,ifbc1,ifaq2)
      sizepar4d(1,2,2,1)=rrr(kcomp,ict1,ifac2,ifbc2,ifaq1)
      sizepar4d(1,2,2,2)=rrr(kcomp,ict1,ifac2,ifbc2,ifaq2)
      sizepar4d(2,1,1,1)=rrr(kcomp,ict2,ifac1,ifbc1,ifaq1)
      sizepar4d(2,1,1,2)=rrr(kcomp,ict2,ifac1,ifbc1,ifaq2)
      sizepar4d(2,1,2,1)=rrr(kcomp,ict2,ifac1,ifbc2,ifaq1)
      sizepar4d(2,1,2,2)=rrr(kcomp,ict2,ifac1,ifbc2,ifaq2)
      sizepar4d(2,2,1,1)=rrr(kcomp,ict2,ifac2,ifbc1,ifaq1)
      sizepar4d(2,2,1,2)=rrr(kcomp,ict2,ifac2,ifbc1,ifaq2)
      sizepar4d(2,2,2,1)=rrr(kcomp,ict2,ifac2,ifbc2,ifaq1)
      sizepar4d(2,2,2,2)=rrr(kcomp,ict2,ifac2,ifbc2,ifaq2)

!     interpolation in the faq, fbc, fac and cat dimensions
      call lininterpol4dim (d2mx, dxm1, invd, sizepar4d, r1, r2)

!     finally, interpolation in the cat dimension
      xrk(lon)=(d2mx(1)*r1+dxm1(1)*r2)*invd(1)*1.e-6_r8  ! look-up table radii in um

!     Table points as basis for multidimentional linear interpolation,
!     modal standard deviation:  

      sizepar4d(1,1,1,1)=sss(kcomp,ict1,ifac1,ifbc1,ifaq1)
      sizepar4d(1,1,1,2)=sss(kcomp,ict1,ifac1,ifbc1,ifaq2)
      sizepar4d(1,1,2,1)=sss(kcomp,ict1,ifac1,ifbc2,ifaq1)
      sizepar4d(1,1,2,2)=sss(kcomp,ict1,ifac1,ifbc2,ifaq2)
      sizepar4d(1,2,1,1)=sss(kcomp,ict1,ifac2,ifbc1,ifaq1)
      sizepar4d(1,2,1,2)=sss(kcomp,ict1,ifac2,ifbc1,ifaq2)
      sizepar4d(1,2,2,1)=sss(kcomp,ict1,ifac2,ifbc2,ifaq1)
      sizepar4d(1,2,2,2)=sss(kcomp,ict1,ifac2,ifbc2,ifaq2)
      sizepar4d(2,1,1,1)=sss(kcomp,ict2,ifac1,ifbc1,ifaq1)
      sizepar4d(2,1,1,2)=sss(kcomp,ict2,ifac1,ifbc1,ifaq2)
      sizepar4d(2,1,2,1)=sss(kcomp,ict2,ifac1,ifbc2,ifaq1)
      sizepar4d(2,1,2,2)=sss(kcomp,ict2,ifac1,ifbc2,ifaq2)
      sizepar4d(2,2,1,1)=sss(kcomp,ict2,ifac2,ifbc1,ifaq1)
      sizepar4d(2,2,1,2)=sss(kcomp,ict2,ifac2,ifbc1,ifaq2)
      sizepar4d(2,2,2,1)=sss(kcomp,ict2,ifac2,ifbc2,ifaq1)
      sizepar4d(2,2,2,2)=sss(kcomp,ict2,ifac2,ifbc2,ifaq2)

!     interpolation in the faq, fbc, fac and cat dimensions
      call lininterpol4dim (d2mx, dxm1, invd, sizepar4d, s1, s2)

!     finally, interpolation in the cat dimension
      xstdv(lon)=(d2mx(1)*s1+dxm1(1)*s2)*invd(1)

      end do   ! lon
      return
end subroutine intlog5to10_sub

end module intlog5to10

