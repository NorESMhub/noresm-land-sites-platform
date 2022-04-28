module aero_to_srf

!------------------------------------------------------------------------------------------------
! Purpose:
!
! Partition the contributions from modal components of wet and dry 
! deposition at the surface into the fields passed to the coupler.
!
! *** N.B. *** Currently only a simple scheme for the 3-mode version
!              of MAM has been implemented.
!
! Revision history:
! Feb 2009  M. Flanner, B. Eaton   Original version for trop_mam3.
! Sept 2009 � Seland Modified to CAM-Oslo aerosol physics. 
! The initialisation part is not used at present time.
!------------------------------------------------------------------------------------------------

#include <preprocessorDefinitions.h>

use shr_kind_mod,     only: r8 => shr_kind_r8
use camsrfexch,       only: cam_out_t
use constituents,     only: pcnst, cnst_get_ind
use ppgrid,           only: pcols
use aerosoldef
implicit none
private
save

public :: &
   modal_aero_deposition_init, &
   set_srf_drydep,             &
   set_srf_wetdep

! Private module data
integer :: idx_bc1  = -1
integer :: idx_pom1 = -1
integer :: idx_soa1 = -1
integer :: idx_soa2 = -1
integer :: idx_dst1 = -1
integer :: idx_dst3 = -1
integer :: idx_ncl3 = -1
integer :: idx_so43 = -1
integer :: idx_num3 = -1

!==============================================================================
contains
!==============================================================================

subroutine modal_aero_deposition_init()

! set aerosol indices for re-mapping surface deposition fluxes:
! *_a1 = accumulation mode
! *_a2 = aitken mode
! *_a3 = coarse mode

   ! Currently only trop_mam3 scheme is implemented.
#ifndef MODAL_AERO_3MODE
   return               
#endif                  

   call cnst_get_ind('bc_a1',  idx_bc1)
   call cnst_get_ind('pom_a1', idx_pom1)
   call cnst_get_ind('soa_a1', idx_soa1)
   call cnst_get_ind('soa_a2', idx_soa2)
   call cnst_get_ind('dst_a1', idx_dst1)
   call cnst_get_ind('dst_a3', idx_dst3)
   call cnst_get_ind('ncl_a3', idx_ncl3)
   call cnst_get_ind('so4_a3', idx_so43)
   call cnst_get_ind('num_a3', idx_num3)

end subroutine modal_aero_deposition_init

!==============================================================================
subroutine set_srf_wetdep(wetdepflx, cam_out)

! Set surface wet deposition fluxes passed to coupler.

   ! Arguments:
!Does not differentiate between different wet scavenging processes
!   real(r8), intent(in) :: aerdepwetis(pcols,pcnst)  
! aerosol wet deposition (interstitial)
! aerosol wet deposition (cloud water)
   real(r8), intent(in) :: wetdepflx(pcols,pcnst)  

   type(cam_out_t), intent(inout) :: cam_out     ! cam export state

   ! Local variables:
   integer :: i
   integer :: ncol                      ! number of columns
!cak 
! Mass fractions of deposited sea-salt modes a2 and a3 which belong to size bins 1-4.
! Particle diameters < 0.1 um and > 20 um are not included (size bins are defined w.r.t. 
! particle diameters, confirmed by Mark Flanner)
real(r8), parameter :: fdst1a2 = 5.55e-1_r8
real(r8), parameter :: fdst2a2 = 4.29e-1_r8
real(r8), parameter :: fdst3a2 = 1.59e-2_r8
real(r8), parameter :: fdst4a2 = 1.32e-4_r8
real(r8), parameter :: fdst1a3 = 4.84e-3_r8 
real(r8), parameter :: fdst2a3 = 1.01e-1_r8 
real(r8), parameter :: fdst3a3 = 2.96e-1_r8 
real(r8), parameter :: fdst4a3 = 5.99e-1_r8 
!with cut-off at 10 um (not recommended by Mark Flanner) as for the optics calculations:
!real(r8), parameter :: fdst4a3 = 3.73e-1_r8 
!cak
   !----------------------------------------------------------------------------

   ! Currently only trop_mam3 scheme is implemented.
   ! CAM_OSLO added
#ifdef AEROFFL
   return
#endif

   ncol = cam_out%ncol

   ! derive cam_out variables from deposition fluxes
   !  note: wet deposition fluxes are negative into surface, 
   !        dry deposition fluxes are positive into surface.
   !        CLM wants positive definite fluxes.
! OS Only bcphi and dst1 is used 
   do i = 1, ncol
      ! black carbon fluxes
      cam_out%bcphiwet(i) = - (wetdepflx(i,l_bc_n)+wetdepflx(i,l_bc_ax) &
                          +wetdepflx(i,l_bc_ni)+wetdepflx(i,l_bc_a)     &
                          +wetdepflx(i,l_bc_ai)+wetdepflx(i,l_bc_ac))
!(aerdepwetis(i,idx_bc1)+aerdepwetcw(i,idx_bc1))

      ! organic carbon fluxes
!      cam_out%ocphiwet(i) = -(aerdepwetis(i,idx_pom1)+aerdepwetis(i,idx_soa1)+aerdepwetcw(i,idx_pom1)+aerdepwetcw(i,idx_soa1))
!cak_temp
      cam_out%ocphiwet(i) = 0._r8
!      cam_out%ocphiwet(i) = 1.e-20_r8
!cak_temp

      ! dust fluxes
      !
      ! bulk bin1 (fine) dust deposition equals accumulation mode deposition:
! os       All dust aerosols
!cak      cam_out%dstwet1(i) = -(wetdepflx(i,l_dst_a2)+wetdepflx(i,l_dst_a3)) 
      cam_out%dstwet1(i) = -(fdst1a2*wetdepflx(i,l_dst_a2)+fdst1a3*wetdepflx(i,l_dst_a3))    
!cak

!(aerdepwetis(i,idx_dst1)+aerdepwetcw(i,idx_dst1))
      
!      !  A. Simple: Assign all coarse-mode dust to bulk size bin 3:
!      cam_out%dstwet2(i) = 0._r8
!      cam_out%dstwet3(i) = -(aerdepwetis(i,idx_dst3)+aerdepwetcw(i,idx_dst3))
!      cam_out%dstwet4(i) = 0._r8

      ! in rare cases, integrated deposition tendency is upward
      if (cam_out%bcphiwet(i) .lt. 0._r8) cam_out%bcphiwet(i) = 0._r8
!t2      if (cam_out%bcphiwet(i) .lt. 0._r8) cam_out%bcphiwet(i) = 1.e-20_r8
!      if (cam_out%bcphiwet(i) .le. 0._r8) cam_out%bcphiwet(i) = 1.e-20_r8
!feil      if (cam_out%dstwet3(i)  .lt. 0._r8) cam_out%dstwet3(i)  = 0._r8
      if (cam_out%dstwet1(i)  .lt. 0._r8) cam_out%dstwet1(i)  = 0._r8
!t2      if (cam_out%dstwet1(i)  .lt. 0._r8) cam_out%dstwet1(i)  = 1.e-20_r8
!      if (cam_out%dstwet1(i)  .le. 0._r8) cam_out%dstwet1(i)  = 1.e-20_r8
!cak_temp
      cam_out%dstwet2(i) = -(fdst2a2*wetdepflx(i,l_dst_a2)+fdst2a3*wetdepflx(i,l_dst_a3))    
      cam_out%dstwet3(i) = -(fdst3a2*wetdepflx(i,l_dst_a2)+fdst3a3*wetdepflx(i,l_dst_a3))    
      cam_out%dstwet4(i) = -(fdst4a2*wetdepflx(i,l_dst_a2)+fdst4a3*wetdepflx(i,l_dst_a3))    
      if (cam_out%dstwet2(i).lt.0._r8) cam_out%dstwet2(i) = 0._r8
      if (cam_out%dstwet3(i).lt.0._r8) cam_out%dstwet3(i) = 0._r8
      if (cam_out%dstwet4(i).lt.0._r8) cam_out%dstwet4(i) = 0._r8
!t2      if (cam_out%dstwet2(i).lt.0._r8) cam_out%dstwet2(i) = 1.e-20_r8
!t2      if (cam_out%dstwet3(i).lt.0._r8) cam_out%dstwet3(i) = 1.e-20_r8
!t2      if (cam_out%dstwet4(i).lt.0._r8) cam_out%dstwet4(i) = 1.e-20_r8
!      if (cam_out%dstwet2(i).le.0._r8) cam_out%dstwet2(i) = 1.e-20_r8
!      if (cam_out%dstwet3(i).le.0._r8) cam_out%dstwet3(i) = 1.e-20_r8
!      if (cam_out%dstwet4(i).le.0._r8) cam_out%dstwet4(i) = 1.e-20_r8
!cak_temp

!cak_0
!      cam_out%bcphiwet(i) = 1.e-20_r8
!      cam_out%ocphiwet(i) = 1.e-20_r8
!      cam_out%dstwet1(i) = 1.e-20_r8
!      cam_out%dstwet2(i) = 1.e-20_r8
!      cam_out%dstwet3(i) = 1.e-20_r8
!      cam_out%dstwet4(i) = 1.e-20_r8
!      cam_out%bcphiwet(i) = 1.e-7_r8      ! TEST !!!
!      cam_out%dstwet1(i) = 1.e-11_r8
!      cam_out%dstwet2(i) = 1.e-10_r8
!      cam_out%dstwet3(i) = 1.e-09_r8
!      cam_out%dstwet4(i) = 1.e-08_r8
!    if(i==ncol) then
!       write(*,*) 'bcphiwet = ', cam_out%bcphiwet(i)
!       write(*,*) 'dstwet1 = ', cam_out%dstwet1(i)  
!       write(*,*) 'dstwet2 = ', cam_out%dstwet2(i)
!       write(*,*) 'dstwet3 = ', cam_out%dstwet3(i)
!       write(*,*) 'dstwet4 = ', cam_out%dstwet4(i)
!    endif
!cak_0

   enddo

end subroutine set_srf_wetdep

!==============================================================================

subroutine set_srf_drydep(sflx, cam_out)

! Set surface dry deposition fluxes passed to coupler.
   
   ! Arguments:
   real(r8), intent(in) :: sflx(pcols,pcnst)  ! aerosol dry deposition (interstitial)
   type(cam_out_t), intent(inout) :: cam_out     ! cam export state

   ! Local variables:
   integer :: i
   integer :: ncol                      ! number of columns
!cak 
! Mass fractions of deposited sea-salt modes a2 and a3 which belong to size bins 1-4.
! Particle diameters < 0.1 um and > 20 um are not included (size bins are defined w.r.t. 
! particle diameters, confirmed by Mark Flanner)
real(r8), parameter :: fdst1a2 = 5.55e-1_r8
real(r8), parameter :: fdst2a2 = 4.29e-1_r8
real(r8), parameter :: fdst3a2 = 1.59e-2_r8
real(r8), parameter :: fdst4a2 = 1.32e-4_r8
real(r8), parameter :: fdst1a3 = 4.84e-3_r8 
real(r8), parameter :: fdst2a3 = 1.01e-1_r8 
real(r8), parameter :: fdst3a3 = 2.96e-1_r8 
real(r8), parameter :: fdst4a3 = 5.99e-1_r8 
!with cut-off at 10 um (not recommended by Mark Flanner) as for the optics calculations:
!real(r8), parameter :: fdst4a3 = 3.73e-1_r8 
!cak
   !----------------------------------------------------------------------------

!cak   write(*,*) 'test dry 1'

   ! Currently only trop_mam3 scheme is implemented.
#ifdef AEROFFL  
   return
#endif

!cak   write(*,*) 'test dry 2'

   ncol = cam_out%ncol

   ! derive cam_out variables from deposition fluxes
   !  note: wet deposition fluxes are negative into surface, 
   !        dry deposition fluxes are positive into surface.
   !        CLM wants positive definite fluxes.
!cak: all cam_out fluxes are positive definite here...
   do i = 1, ncol
      ! black carbon fluxes
!cak_old      cam_out%bcphidry(i) = -(sflx(i,l_bc_n)+sflx(i,l_bc_ax) & 
!cak_old + sflx(i,l_bc_ni)+sflx(i,l_bc_a)+sflx(i,l_bc_ai)+sflx(i,l_bc_ac)) 
      cam_out%bcphidry(i) = -(sflx(i,l_bc_ni)+sflx(i,l_bc_a)+sflx(i,l_bc_ai)+sflx(i,l_bc_ac)) 
!cak_temp
      cam_out%bcphodry(i) = -(sflx(i,l_bc_n)+sflx(i,l_bc_ax))
!cak_old      cam_out%bcphodry(i) = 0._r8
!      cam_out%bcphodry(i) = 1.e-20_r8
!cak_temp

      ! organic carbon fluxes
!      cam_out%ocphidry(i) = aerdepdryis(i,idx_pom1)+aerdepdryis(i,idx_soa1)+aer!depdrycw(i,idx_pom1)+aerdepdrycw(i,idx_soa1)
!      cam_out%ocphodry(i) = aerdepdryis(i,idx_soa2)+aerdepdrycw(i,idx_soa2)
!cak_temp
      cam_out%ocphidry(i) = 0._r8
      cam_out%ocphodry(i) = 0._r8
!      cam_out%ocphidry(i) = 1.e-20_r8
!      cam_out%ocphodry(i) = 1.e-20_r8
!cak_temp

      ! dust fluxes
      !
      ! bulk bin1 (fine) dust deposition equals accumulation mode deposition:
!cak      cam_out%dstdry1(i) = -(sflx(i,l_dst_a2)+sflx(i,l_dst_a3))
      cam_out%dstdry1(i) = -(fdst1a2*sflx(i,l_dst_a2)+fdst1a3*sflx(i,l_dst_a3))
!cak
!aerdepdryis(i,idx_dst1)+aerdepdrycw(i,idx_dst1)
      
      ! Two options for partitioning deposition into bins 2-4:
      !  A. Simple: Assign all coarse-mode dust to bulk size bin 3:
!      cam_out%dstdry2(i) = 0._r8
!      cam_out%dstdry3(i) = aerdepdryis(i,idx_dst3)+aerdepdrycw(i,idx_dst3)
!      cam_out%dstdry4(i) = 0._r8

      ! in rare cases, integrated deposition tendency is upward
      if (cam_out%bcphidry(i) .lt. 0._r8) cam_out%bcphidry(i) = 0._r8
!t2      if (cam_out%bcphidry(i) .lt. 0._r8) cam_out%bcphidry(i) = 1.e-20_r8
!      if (cam_out%bcphidry(i) .le. 0._r8) cam_out%bcphidry(i) = 1.e-20_r8
      if (cam_out%dstdry1(i)  .lt. 0._r8) cam_out%dstdry1(i)  = 0._r8
!t2      if (cam_out%dstdry1(i)  .lt. 0._r8) cam_out%dstdry1(i)  = 1.e-20_r8
!      if (cam_out%dstdry1(i)  .le. 0._r8) cam_out%dstdry1(i)  = 1.e-20_r8
!cak_temp
!      cam_out%dstdry2(i) = 0._r8
!      cam_out%dstdry3(i) = 0._r8
!      cam_out%dstdry4(i) = 0._r8
      cam_out%dstdry2(i) = -(fdst2a2*sflx(i,l_dst_a2)+fdst2a3*sflx(i,l_dst_a3))
      cam_out%dstdry3(i) = -(fdst3a2*sflx(i,l_dst_a2)+fdst3a3*sflx(i,l_dst_a3))
      cam_out%dstdry4(i) = -(fdst4a2*sflx(i,l_dst_a2)+fdst4a3*sflx(i,l_dst_a3))
      if (cam_out%dstdry2(i).lt.0._r8) cam_out%dstdry2(i)  = 0._r8
      if (cam_out%dstdry3(i).lt.0._r8) cam_out%dstdry3(i)  = 0._r8
      if (cam_out%dstdry4(i).lt.0._r8) cam_out%dstdry4(i)  = 0._r8
!t2      if (cam_out%dstdry2(i).lt.0._r8) cam_out%dstdry2(i)  = 1.e-20_r8
!t2      if (cam_out%dstdry3(i).lt.0._r8) cam_out%dstdry3(i)  = 1.e-20_r8
!t2      if (cam_out%dstdry4(i).lt.0._r8) cam_out%dstdry4(i)  = 1.e-20_r8
!      if (cam_out%dstdry2(i).le.0._r8) cam_out%dstdry2(i)  = 1.e-20_r8
!      if (cam_out%dstdry3(i).le.0._r8) cam_out%dstdry3(i)  = 1.e-20_r8
!      if (cam_out%dstdry4(i).le.0._r8) cam_out%dstdry4(i)  = 1.e-20_r8
!cak_temp

!cak_0
!      cam_out%bcphidry(i) = 1.e-20_r8
!      cam_out%bcphodry(i) = 1.e-20_r8
!      cam_out%ocphidry(i) = 1.e-20_r8
!      cam_out%ocphodry(i) = 1.e-20_r8
!      cam_out%dstdry1(i) = 1.e-20_r8
!      cam_out%dstdry2(i) = 1.e-20_r8
!      cam_out%dstdry3(i) = 1.e-20_r8
!      cam_out%dstdry4(i) = 1.e-20_r8
!      cam_out%bcphidry(i) = 1.e-7_r8      ! TEST !!!
!      cam_out%dstdry1(i) = 1.e-11_r8
!      cam_out%dstdry2(i) = 1.e-10_r8
!      cam_out%dstdry3(i) = 1.e-09_r8
!      cam_out%dstdry4(i) = 1.e-08_r8
!    if(i==ncol) then
!       write(*,*) 'bcphidry = ', cam_out%bcphidry(i)
!       write(*,*) 'dstdry1 = ', cam_out%dstdry1(i)  
!       write(*,*) 'dstdry2 = ', cam_out%dstdry2(i)
!       write(*,*) 'dstdry3 = ', cam_out%dstdry3(i)
!       write(*,*) 'dstdry4 = ', cam_out%dstdry4(i)
!    endif
!cak_0

   enddo

end subroutine set_srf_drydep

!==============================================================================

end module aero_to_srf
