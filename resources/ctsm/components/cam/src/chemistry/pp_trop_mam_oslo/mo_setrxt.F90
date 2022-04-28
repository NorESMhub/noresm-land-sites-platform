
      module mo_setrxt

      use shr_kind_mod, only : r8 => shr_kind_r8

      private
      public :: setrxt
      public :: setrxt_hrates

      contains

      subroutine setrxt( rate, temp, m, ncol )

      use ppgrid,       only : pver, pcols
      use shr_kind_mod, only : r8 => shr_kind_r8
      use chem_mods, only : rxntot
      use mo_jpl,    only : jpl

      implicit none

!-------------------------------------------------------
!       ... dummy arguments
!-------------------------------------------------------
      integer, intent(in) :: ncol
      real(r8), intent(in)    :: temp(pcols,pver)
      real(r8), intent(in)    :: m(ncol,pver)
      real(r8), intent(inout) :: rate(ncol,pver,rxntot)

!-------------------------------------------------------
!       ... local variables
!-------------------------------------------------------
      integer   ::  n
      real(r8)  ::  itemp(ncol,pver)
      real(r8)  ::  exp_fac(ncol,pver)
      real(r8)  :: ko(ncol,pver)
      real(r8)  :: kinf(ncol,pver)

      itemp(:ncol,:) = 1._r8 / temp(:ncol,:)
      n = ncol*pver
      rate(:,:,3) = 2.9e-12_r8 * exp( -160._r8 * itemp(:,:) )
      rate(:,:,4) = 9.6e-12_r8 * exp( -234._r8 * itemp(:,:) )
      rate(:,:,5) = 1.9e-13_r8 * exp( 520._r8 * itemp(:,:) )
      rate(:,:,8) = 8.05e-16_r8 * exp( -640._r8 * itemp(:,:) )
      rate(:,:,9) = 1.2e-11_r8 * exp( 440._r8 * itemp(:,:) )
      rate(:,:,10) = 1.2e-12_r8 * exp( 490._r8 * itemp(:,:) )
      rate(:,:,11) = 1.03e-14_r8 * exp( -1995._r8 * itemp(:,:) )
      rate(:,:,12) = 2.7e-11_r8 * exp( 390._r8 * itemp(:,:) )
      rate(:,:,13) = 3.15e-12_r8 * exp( -450._r8 * itemp(:,:) )

      itemp(:,:) = 300._r8 * itemp(:,:)

      ko(:,:) = 3.0e-31_r8 * itemp(:,:)**3.3_r8
      kinf(:,:) = 1.5e-12_r8
      call jpl( rate(1,1,6), m, 0.6_r8, ko, kinf, n )

      end subroutine setrxt


      subroutine setrxt_hrates( rate, temp, m, ncol, kbot )

      use ppgrid,       only : pver, pcols
      use shr_kind_mod, only : r8 => shr_kind_r8
      use chem_mods, only : rxntot
      use mo_jpl,    only : jpl

      implicit none

!-------------------------------------------------------
!       ... dummy arguments
!-------------------------------------------------------
      integer, intent(in) :: ncol
      integer, intent(in) :: kbot
      real(r8), intent(in)    :: temp(pcols,pver)
      real(r8), intent(in)    :: m(ncol,pver)
      real(r8), intent(inout) :: rate(ncol,pver,rxntot)

!-------------------------------------------------------
!       ... local variables
!-------------------------------------------------------
      integer   ::  n
      real(r8)  ::  itemp(ncol,kbot)
      real(r8)  ::  exp_fac(ncol,kbot)
      real(r8)  :: ko(ncol,kbot)
      real(r8)  :: kinf(ncol,kbot)
      real(r8)  :: wrk(ncol,kbot)


      end subroutine setrxt_hrates

      end module mo_setrxt
