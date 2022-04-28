module intlog4

contains
	subroutine intlog4_sub (ncol, ind, kcomp, xctin, Nnat, &
                          xfacin, xfaqin, cxs, xstdv, xrk)

!   Created by Trude Storelvmo, fall 2007. This subroutine gives as output   
!   the "new" modal radius and standard deviation for aerosol mode kcomp=4.
!   These parameters are calculated for a best lognormal fit approximation of
!   the aerosol size distribution. This because the aerosol activation routine
!   (developed by Abdul-Razzak & Ghan, 2000) requires the size distribution 
!   to be described by lognormal modes.
!   Changed by Alf Kirkevåg to take into account condensation of SOA, September 
!   2015, and also rewritten to a more generalized for for interpolations using 
!   common subroutines interpol*dim.

      use shr_kind_mod, only: r8 => shr_kind_r8
      use ppgrid, only: pcols
      use const, only: sss4, rrr4
      use opttab,   only: nbmp1, cate, fac, faq
      implicit none

      integer, intent(in) :: ncol
      integer, intent(in) :: ind(pcols)
      integer, intent(in) :: kcomp
      real(r8), intent(in) :: Nnat(pcols)           ! Modal number concentration
      real(r8), intent(in) :: xctin(pcols)          ! total internally mixed conc. (ug/m3)	
      real(r8), intent(in) :: xfacin(pcols)         ! SOA/(SOA+H2SO4) for condensated mass
      real(r8), intent(in) :: xfaqin(pcols)         ! = Cso4a2/(Cso4a1+Cso4a2)
      real(r8), intent(out) :: xstdv(pcols)         ! log10 of standard deviation for lognormal fit
      real(r8), intent(out) :: xrk(pcols)           ! Modal radius for lognormal fit
      real(r8), intent(out) :: cxs(pcols)           ! excess (modal) internally mixed conc.

      real(r8) camdiff
      real(r8), dimension(pcols) ::  xct, xfac, xfaq
!ces: integer arrays ict1, ict2, ifaq1 and ifaq2
!     substituted with scalar variables with the same name.

      integer lon, long

      integer i, ictot, ifac, ifaq, &
        ict1, ict2, ifac1, ifac2, ifaq1, ifaq2

      real(r8) t_fac1, t_fac2, t_xfac, t_xct, t_cat1, t_cat2, &
        t_faq1, t_faq2, t_xfaq 
      real(r8) r1, r2, s1, s2, tmp, e
      real(r8) d2mx(3), dxm1(3), invd(3)
      real(r8) sizepar3d(2,2,2)

!ces: New local variables introduced by (or inspired by) Egil Stoeren:

       real(r8), parameter :: eps=1.0e-60_r8

!     Initialize excess mass cxs, wrt. maximum allowed internal mixing
      do lon=1,ncol
        cxs(lon) = 0.0_r8
        xct(lon)  = 0.0_r8
        xfac(lon) = 0.0_r8
        xfaq(lon) = 0.0_r8
      enddo

!ces: All loops "do long=1,nlons" combined to one loop:

!      do lon=1,ncol
      do long=1,ncol
       lon=ind(long)
       xstdv(lon) = 0._r8
       xrk(lon) = 0._r8

	xct(lon)  = min(max(xctin(lon)/(Nnat(lon)+eps),cate(kcomp,1)),cate(kcomp,16))
	xfac(lon) = min(max(xfacin(lon),fac(1)),fac(6))
	xfaq(lon) = min(max(xfaqin(lon),faq(1)),faq(6))

        camdiff   = xctin(lon)-xct(lon)*(Nnat(lon)+eps)

        cxs(lon)  = max(0.0_r8,camdiff)

        ictot=1
	tmp = xct(lon)
	do while (ictot.lt.15.and.(tmp.lt.cate(kcomp,ictot).or. &
                tmp.gt.cate(kcomp,ictot+1)))
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
      t_cat1 = cate(kcomp,ict1)
      t_cat2 = cate(kcomp,ict2)
      t_fac1 = fac(ifac1)
      t_fac2 = fac(ifac2)
      t_faq1 = faq(ifaq1)
      t_faq2 = faq(ifaq2)

      t_xct  = xct(lon)
      t_xfac = xfac(lon)
      t_xfaq = xfaq(lon)

!     partial lengths along each dimension (1-4) for interpolation 
      d2mx(1) = (t_cat2-t_xct)
      dxm1(1) = (t_xct-t_cat1)
      invd(1) = 1.0_r8/(t_cat2-t_cat1)
      d2mx(2) = (t_fac2-t_xfac)
      dxm1(2) = (t_xfac-t_fac1)
      invd(2) = 1.0_r8/(t_fac2-t_fac1)
      d2mx(3) = (t_faq2-t_xfaq)
      dxm1(3) = (t_xfaq-t_faq1)
      invd(3) = 1.0_r8/(t_faq2-t_faq1)

!     Table points as basis for multidimentional linear interpolation,
!     modal median radius:  

      sizepar3d(1,1,1)=rrr4(ict1,ifac1,ifaq1)
      sizepar3d(1,1,2)=rrr4(ict1,ifac1,ifaq2)
      sizepar3d(1,2,1)=rrr4(ict1,ifac2,ifaq1)
      sizepar3d(1,2,2)=rrr4(ict1,ifac2,ifaq2)
      sizepar3d(2,1,1)=rrr4(ict2,ifac1,ifaq1)
      sizepar3d(2,1,2)=rrr4(ict2,ifac1,ifaq2)
      sizepar3d(2,2,1)=rrr4(ict2,ifac2,ifaq1)
      sizepar3d(2,2,2)=rrr4(ict2,ifac2,ifaq2)

!     interpolation in the faq and fac dimension
      call lininterpol3dim (d2mx, dxm1, invd, sizepar3d, r1, r2)

!     finally, interpolation in the cate dimension
      xrk(lon)=(d2mx(1)*r1+dxm1(1)*r2)*invd(1)*1.e-6_r8  ! look up table radii in um


!     Table points as basis for multidimentional linear interpolation,
!     modal standard deviation:  
      sizepar3d(1,1,1)=sss4(ict1,ifac1,ifaq1)
      sizepar3d(1,1,2)=sss4(ict1,ifac1,ifaq2)
      sizepar3d(1,2,1)=sss4(ict1,ifac2,ifaq1)
      sizepar3d(1,2,2)=sss4(ict1,ifac2,ifaq2)
      sizepar3d(2,1,1)=sss4(ict2,ifac1,ifaq1)
      sizepar3d(2,1,2)=sss4(ict2,ifac1,ifaq2)
      sizepar3d(2,2,1)=sss4(ict2,ifac2,ifaq1)
      sizepar3d(2,2,2)=sss4(ict2,ifac2,ifaq2)

!     interpolation in the faq and fac dimension
      call lininterpol3dim (d2mx, dxm1, invd, sizepar3d, s1, s2)

!     finally, interpolation in the cate dimension
      xstdv(lon)=(d2mx(1)*s1+dxm1(1)*s2)*invd(1)

      end do   ! lon

      return
end subroutine intlog4_sub

end module intlog4

