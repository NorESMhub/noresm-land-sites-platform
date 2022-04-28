module intlog1to3

contains

	subroutine intlog1to3_sub (ncol, ind, kcomp, xctin, &
                                 Nnat, xfacin, cxs, xstdv, xrk)

!	Created by Trude Storelvmo, fall 2007. This subroutine gives as output   
!	the "new" modal radius and standard deviation for a given aerosol mode, kcomp 
!       1-3. These parameters are calculated for a best lognormal fit approximation of
!       the aerosol size distribution. This because the aerosol activation routine
!       (developed by Abdul-Razzak & Ghan, 2000) requiers the size distribution to be 
!       described by lognormal modes.
!       Changed by Alf Kirkevåg to take into account condensation of SOA, September 2015, 
       

      use shr_kind_mod, only: r8 => shr_kind_r8
      use ppgrid, only: pcols
      use const, only: sss1to3, rrr1to3
      use opttab, only: cate, fac

      implicit none

      integer, intent(in) :: ncol
      integer, intent(in) :: ind(pcols)
      integer, intent(in) :: kcomp
      real(r8), intent(in) :: Nnat(pcols)        ! Modal number concentration
      real(r8), intent(in) :: xctin(pcols)	 ! total internally mixed conc. (ug/m3)	
      real(r8), intent(in) :: xfacin(pcols)         ! SOA/(SOA+H2SO4) for condensated mass 
      real(r8), intent(out) :: xstdv(pcols)      ! log10 of standard deviation for lognormal fit
      real(r8), intent(out) :: xrk(pcols)        ! Modal radius for lognormal fit
      real(r8), intent(out) :: cxs(pcols)   ! excess (modal) internally mixed conc.

      real(r8) camdiff
      real(r8), dimension(pcols) ::  xct
      real(r8) xfac(ncol) 
      integer lon, long

      integer i, ictot, ict1, ict2
      real(r8) r1, r2, s1, s2
      integer ifac, ifac1, ifac2
      real(r8) t_fac1, t_fac2, t_xfac, t_xct, t_cat1, t_cat2
      real(r8) r11, r12, r21, r22, s11, s12, s21, s22
      real(r8) d2mx(2), dxm1(2), invd(2)

      real(r8) esssf10, ess

      real(r8), parameter :: eps= 1.0e-10_r8

!     Initialize excess mass cxs, wrt. maximum allowed internal mixing
      do lon=1,ncol
        cxs(lon) = 0.0_r8
        xct(lon) = 0.0_r8
        xfac(lon) = 0.0_r8
      enddo

      do long=1,ncol
       lon=ind(long)
       xstdv(lon) = 0._r8
       xrk(lon) = 0._r8

	xct(lon)  = min(max(xctin(lon)/(Nnat(lon)+eps),cate(kcomp,1)),cate(kcomp,16))
	xfac(lon) = min(max(xfacin(lon),fac(1)),fac(6))
        camdiff   = xctin(lon)-xct(lon)*(Nnat(lon)+eps)

        cxs(lon)  = max(0.0_r8,camdiff)

        ictot=1
	ess = xct(lon)
	do while (ictot.lt.15.and.(ess.lt.cate(kcomp,ictot).or. &
                ess.gt.cate(kcomp,ictot+1)))
	 ictot=ictot+1
	enddo
	ict1=ictot
	ict2=ictot+1

	ifac=1
	ess = xfac(lon)
	do while (ifac.lt.5.and.(ess.lt.fac(ifac).or. &
                 ess.gt.fac(ifac+1)))
	 ifac=ifac+1
	enddo
	ifac1=ifac
	ifac2=ifac+1

!      Collect all the vector elements into temporary storage
!      to avoid cache conflicts and excessive cross-referencing

      t_cat1 = cate(kcomp,ict1)
      t_cat2 = cate(kcomp,ict2)
      t_fac1 = fac(ifac1)
      t_fac2 = fac(ifac2)

      t_xct  = xct(lon)
      t_xfac = xfac(lon)

!     partial lengths along each dimension (1-2) for interpolation 

      d2mx(1) = (t_cat2-t_xct)
      dxm1(1) = (t_xct-t_cat1)
      invd(1) = 1.0_r8/(t_cat2-t_cat1)
      d2mx(2) = (t_fac2-t_xfac)
      dxm1(2) = (t_xfac-t_fac1)
      invd(2) = 1.0_r8/(t_fac2-t_fac1)

!  interpolated (in 2 dimensions) modal median radius:

      r11=rrr1to3(kcomp,ict1,ifac1)
      r12=rrr1to3(kcomp,ict1,ifac2)
      r21=rrr1to3(kcomp,ict2,ifac1)
      r22=rrr1to3(kcomp,ict2,ifac2)

      r1 =d2mx(2)*r11+dxm1(2)*r12
      r2 =d2mx(2)*r21+dxm1(2)*r22

      xrk(lon) = (d2mx(1)*r1+dxm1(1)*r2)*invd(2)*invd(1)*1.e-6_r8  !Look-up table radii in um

!  interpolated (in 2 dimensions) modal standard deviation:

      s11=sss1to3(kcomp,ict1,ifac1)
      s12=sss1to3(kcomp,ict1,ifac2)
      s21=sss1to3(kcomp,ict2,ifac1)
      s22=sss1to3(kcomp,ict2,ifac2)

      s1 =d2mx(2)*s11+dxm1(2)*s12
      s2 =d2mx(2)*s21+dxm1(2)*s22

      xstdv(lon) = (d2mx(1)*s1+dxm1(1)*s2)*invd(2)*invd(1)


      end do   ! lon

      return
      end subroutine intlog1to3_sub

end module intlog1to3

