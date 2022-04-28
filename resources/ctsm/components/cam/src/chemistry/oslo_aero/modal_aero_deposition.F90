module modal_aero_deposition

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
! Jul 2011  F Vitt -- made avaliable to be used in a prescribed modal aerosol mode (no prognostic MAM)
! Mar 2012  F Vitt -- made changes for to prevent abort when 7-mode aeroslol model is used
!                     some of the needed consituents do not exist in 7-mode so bin_fluxes will be false
! May 2014  F Vitt -- included contributions from MAM4 aerosols and added soa_a2 to the ocphiwet fluxes
!------------------------------------------------------------------------------------------------

use shr_kind_mod,     only: r8 => shr_kind_r8
use camsrfexch,       only: cam_out_t     
use constituents,     only: cnst_get_ind, pcnst
use cam_abortutils,   only: endrun
use rad_constituents, only: rad_cnst_get_info
use aerosoldef,       only: l_bc_n,l_bc_ax,l_bc_ni,l_bc_a,l_bc_ai,l_bc_ac
use aerosoldef,       only: l_om_ni,l_om_ai,l_om_ac,l_dst_a2,l_dst_a3

implicit none
private
save

public :: &
   modal_aero_deposition_init, &
   set_srf_drydep,             &
   set_srf_wetdep

! Private module data

logical :: initialized = .false.
integer :: bcphi_ndx( pcnst ) = -1
integer :: bcpho_ndx( pcnst ) = -1
integer :: ocphi_ndx( pcnst ) = -1
integer :: ocpho_ndx( pcnst ) = -1
integer :: crse_dust_ndx( pcnst ) = -1
integer :: fine_dust_ndx( pcnst ) = -1
integer :: bcphi_cnt = 0
integer :: ocphi_cnt = 0
integer :: bcpho_cnt = 0
integer :: ocpho_cnt = 0
integer :: crse_dust_cnt = 0
integer :: fine_dust_cnt = 0

!==============================================================================
contains
!==============================================================================

subroutine modal_aero_deposition_init( bcphi_indices, bcpho_indices, ocphi_indices, &
                                ocpho_indices, fine_dust_indices, crse_dust_indices )

  ! set aerosol indices for re-mapping surface deposition fluxes:
  ! *_a1 = accumulation mode
  ! *_a2 = aitken mode
  ! *_a3 = coarse mode
  
  ! can be initialized with user specified indices
  ! if called from aerodep_flx module (for prescribed modal aerosol fluxes) then these indices are specified
  integer, optional, intent(in) :: bcphi_indices(:)     ! hydrophilic black carbon
  integer, optional, intent(in) :: bcpho_indices(:)     ! hydrophobic black carbon
  integer, optional, intent(in) :: ocphi_indices(:)     ! hydrophilic organic carbon
  integer, optional, intent(in) :: ocpho_indices(:)     ! hydrophobic organic carbon 
  integer, optional, intent(in) :: fine_dust_indices(:) ! fine dust
  integer, optional, intent(in) :: crse_dust_indices(:) ! coarse dust

  ! local vars
  integer :: i, pcnt, scnt

  character(len=16), parameter :: fine_dust_modes(2) =  (/ 'accum           ', 'fine_dust       '/)
  character(len=16), parameter :: crse_dust_modes(2) =  (/ 'coarse          ', 'coarse_dust     '/)
  character(len=16), parameter :: hydrophilic_carbon_modes(1) = (/'accum           '/)
  character(len=16), parameter :: hydrophobic_carbon_modes(3) = (/'aitken          ',  'coarse          ', 'primary_carbon  '/)

  ! if already initialized abort the run
  if (initialized) then
     call endrun('modal_aero_deposition is already initialized')
  endif


  initialized = .true.

end subroutine modal_aero_deposition_init

!==============================================================================
subroutine set_srf_wetdep(aerdepwetis, aerdepwetcw, cam_out)

! Set surface wet deposition fluxes passed to coupler.

   ! Arguments:
   real(r8), intent(in) :: aerdepwetis(:,:)  ! aerosol wet deposition (interstitial)
   real(r8), intent(in) :: aerdepwetcw(:,:)  ! aerosol wet deposition (cloud water)
   type(cam_out_t), intent(inout) :: cam_out     ! cam export state

   ! Local variables:
   integer :: i, ispec, idx
   integer :: ncol                      ! number of columns

   real(r8) :: bcphiwet_sum, ocphiwet_sum
   !----------------------------------------------------------------------------

  if (.not.initialized) call endrun('set_srf_wetdep: modal_aero_deposition has not been initialized')

   ncol = cam_out%ncol

   cam_out%bcphiwet(:) = 0._r8
   cam_out%ocphiwet(:) = 0._r8

   ! derive cam_out variables from deposition fluxes
   !  note: wet deposition fluxes are negative into surface, 
   !        dry deposition fluxes are positive into surface.
   !        srf models want positive definite fluxes.
   do i = 1, ncol

      ! black carbon fluxes
      ! djlo : added bc_n and bc_ax contribution
      ! djlo : bc_ax is assumed not to exist in cloud water
      cam_out%bcphiwet(i) = -(aerdepwetis(i,l_bc_ni)+aerdepwetcw(i,l_bc_ni)+ &
                              aerdepwetis(i,l_bc_ai)+aerdepwetcw(i,l_bc_ai)+ &
                              aerdepwetis(i,l_bc_a )+aerdepwetcw(i,l_bc_a )+ &
                              aerdepwetis(i,l_bc_ac)+aerdepwetcw(i,l_bc_ac)+ &
                              aerdepwetis(i,l_bc_n )+aerdepwetcw(i,l_bc_n )+ &
                              aerdepwetis(i,l_bc_ax))

      ! organic carbon fluxes
      cam_out%ocphiwet(i) = -(aerdepwetis(i,l_om_ni)+aerdepwetcw(i,l_om_ni)+ &
                              aerdepwetis(i,l_om_ai)+aerdepwetcw(i,l_om_ai)+ &
                              aerdepwetis(i,l_om_ac)+aerdepwetcw(i,l_om_ac))

      ! dust fluxes
      !
      ! bulk bin1 (fine) dust deposition equals accumulation mode deposition:
      cam_out%dstwet1(i) = -(aerdepwetis(i,l_dst_a2)+aerdepwetcw(i,l_dst_a2))
      
      !  A. Simple: Assign all coarse-mode dust to bulk size bin 3:
      cam_out%dstwet2(i) = 0._r8
      cam_out%dstwet3(i) = -(aerdepwetis(i,l_dst_a3)+aerdepwetcw(i,l_dst_a3))
      cam_out%dstwet4(i) = 0._r8

   enddo

end subroutine set_srf_wetdep

!==============================================================================

subroutine set_srf_drydep(aerdepdryis, aerdepdrycw, cam_out)

! Set surface dry deposition fluxes passed to coupler.
   
   ! Arguments:
   real(r8), intent(in) :: aerdepdryis(:,:)  ! aerosol dry deposition (interstitial)
   real(r8), intent(in) :: aerdepdrycw(:,:)  ! aerosol dry deposition (cloud water)
   type(cam_out_t), intent(inout) :: cam_out     ! cam export state

   ! Local variables:
   integer :: i, ispec, idx
   integer :: ncol                      ! number of columns
   real(r8):: bcphidry_sum, ocphidry_sum, ocphodry_sum
   !----------------------------------------------------------------------------

   if (.not.initialized) call endrun('set_srf_drydep: modal_aero_deposition has not been initialized')

   ncol = cam_out%ncol

   cam_out%bcphidry(:) = 0._r8
   cam_out%bcphodry(:) = 0._r8
   cam_out%ocphidry(:) = 0._r8
   cam_out%ocphodry(:) = 0._r8

   ! derive cam_out variables from deposition fluxes
   !  note: wet deposition fluxes are negative into surface, 
   !        dry deposition fluxes are positive into surface.
   !        srf models want positive definite fluxes.
   do i = 1, ncol
      ! black carbon fluxes
      cam_out%bcphidry(i) = aerdepdryis(i,l_bc_ni)+aerdepdrycw(i,l_bc_ni)+ &
                            aerdepdryis(i,l_bc_ai)+aerdepdrycw(i,l_bc_ai)+ &
                            aerdepdryis(i,l_bc_a )+aerdepdrycw(i,l_bc_a )+ &
                            aerdepdryis(i,l_bc_ac)+aerdepdrycw(i,l_bc_ac)
      cam_out%bcphodry(i) = aerdepdryis(i,l_bc_n )+aerdepdrycw(i,l_bc_n )+ &
                            aerdepdryis(i,l_bc_ax)+aerdepdrycw(i,l_bc_ax)

      ! organic carbon fluxes
      ! djlo : skipped the bc_a contribution (was about om !)
      cam_out%ocphidry(i) = aerdepdryis(i,l_om_ni)+aerdepdrycw(i,l_om_ni)+ &
                            aerdepdryis(i,l_om_ai)+aerdepdrycw(i,l_om_ai)+ &
                            aerdepdryis(i,l_om_ac)+aerdepdrycw(i,l_om_ac)
      cam_out%ocphidry(i) = 0._r8 !aerdepdryis(i,idx_pom1)+aerdepdryis(i,idx_soa1)+aerdepdrycw(i,idx_pom1)+aerdepdrycw(i,idx_soa1)
      cam_out%ocphodry(i) = 0._r8 !aerdepdryis(i,idx_soa2)+aerdepdrycw(i,idx_soa2)

      ! dust fluxes
      !
      ! bulk bin1 (fine) dust deposition equals accumulation mode deposition:
      cam_out%dstdry1(i) = aerdepdryis(i,l_dst_a2)+aerdepdrycw(i,l_dst_a2)
      
      ! Two options for partitioning deposition into bins 2-4:
      !  A. Simple: Assign all coarse-mode dust to bulk size bin 3:
      cam_out%dstdry2(i) = 0._r8
      cam_out%dstdry3(i) = aerdepdryis(i,l_dst_a3)+aerdepdrycw(i,l_dst_a3)
      cam_out%dstdry4(i) = 0._r8
   enddo

end subroutine set_srf_drydep


!==============================================================================

end module modal_aero_deposition
