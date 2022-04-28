      module mo_adjrxt
      private
      public :: adjrxt
      contains
      subroutine adjrxt( rate, inv, m, ncol, nlev )
      use shr_kind_mod, only : r8 => shr_kind_r8
      use chem_mods, only : nfs, rxntot
      implicit none
!--------------------------------------------------------------------
! ... dummy arguments
!--------------------------------------------------------------------
      integer, intent(in) :: ncol, nlev
      real(r8), intent(in) :: inv(ncol,nlev,nfs)
      real(r8), intent(in) :: m(ncol,nlev)
      real(r8), intent(inout) :: rate(ncol,nlev,rxntot)
!--------------------------------------------------------------------
! ... local variables
!--------------------------------------------------------------------
      real(r8) :: im(ncol,nlev)
      im(:,:) = 1._r8 / m(:,:)
      rate(:,:, 3) = rate(:,:, 3) * inv(:,:, 5)
      rate(:,:, 4) = rate(:,:, 4) * inv(:,:, 5)
      rate(:,:, 5) = rate(:,:, 5) * inv(:,:, 6)
      rate(:,:, 7) = rate(:,:, 7) * inv(:,:, 5)
      rate(:,:, 8) = rate(:,:, 8) * inv(:,:, 4)
      rate(:,:, 9) = rate(:,:, 9) * inv(:,:, 5)
      rate(:,:, 10) = rate(:,:, 10) * inv(:,:, 6)
      rate(:,:, 11) = rate(:,:, 11) * inv(:,:, 4)
      rate(:,:, 12) = rate(:,:, 12) * inv(:,:, 5)
      rate(:,:, 13) = rate(:,:, 13) * inv(:,:, 6)
      rate(:,:, 2) = rate(:,:, 2) * inv(:,:, 7) * inv(:,:, 7) * im(:,:)
      rate(:,:, 6) = rate(:,:, 6) * inv(:,:, 5) * inv(:,:, 1)
      end subroutine adjrxt
      end module mo_adjrxt
