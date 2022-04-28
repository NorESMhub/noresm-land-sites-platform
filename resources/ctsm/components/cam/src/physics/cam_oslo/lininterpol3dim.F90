
   subroutine lininterpol3dim (d2mx, dxm1, invd, opt3d, optout1, optout2)


   use shr_kind_mod, only: r8 => shr_kind_r8

   implicit none

!
! Input arguments
!
      real(r8), intent(in) :: opt3d(2,2,2)
      real(r8), intent(in) :: d2mx(3)
      real(r8), intent(in) :: dxm1(3)
      real(r8), intent(in) :: invd(3)
!
! Output arguments
!
      real(r8), intent(out) :: optout1
      real(r8), intent(out) :: optout2
!
!---------------------------Local variables-----------------------------
!
      real(r8) opt2d(2,2)
!
!------------------------------------------------------------------------
!
!     interpolation in the third dimension (except invd(3) factor)
      opt2d(1,1)=d2mx(3)*opt3d(1,1,1)+dxm1(3)*opt3d(1,1,2)
      opt2d(1,2)=d2mx(3)*opt3d(1,2,1)+dxm1(3)*opt3d(1,2,2)
      opt2d(2,1)=d2mx(3)*opt3d(2,1,1)+dxm1(3)*opt3d(2,1,2)
      opt2d(2,2)=d2mx(3)*opt3d(2,2,1)+dxm1(3)*opt3d(2,2,2)

!     interpolation in the (third and) second dimension
      optout1=(d2mx(2)*opt2d(1,1)+dxm1(2)*opt2d(1,2))*invd(3)*invd(2)
      optout2=(d2mx(2)*opt2d(2,1)+dxm1(2)*opt2d(2,2))*invd(3)*invd(2)


      return

end subroutine lininterpol3dim
