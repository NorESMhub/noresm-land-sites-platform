!  SVN:$Id: ice_ocean.F90 897 2015-01-22 01:15:53Z tcraig $
!=======================================================================

! Ocean mixed layer calculation (internal to sea ice model).
! Allows heat storage in ocean for uncoupled runs.
!
! authors:   John Weatherly, CRREL
!            C.M. Bitz, UW
!            Elizabeth C. Hunke, LANL
!            Bruce P. Briegleb, NCAR
!            William H. Lipscomb, LANL
!
! 2004: Block structure added by William Lipscomb
! 2005: Ocean-to-atmosphere fluxes added as 3D arrays, William Lipscomb
! 2006: Converted to free source form (F90) by Elizabeth Hunke

      module ice_ocean

      use ice_kinds_mod
      use ice_constants

      implicit none

      private
      public :: ocean_mixed_layer

      logical (kind=log_kind), public :: &
         oceanmixed_ice           ! if true, use ocean mixed layer

      real (kind=dbl_kind), parameter :: &
         cprho = cp_ocn*rhow

      character(len=char_len), public :: &
         tfrz_option              ! form of ocean freezing temperature
                                  ! 'minus1p8' = -1.8 C
                                  ! 'linear_salt' = -depressT * sss
                                  ! 'mushy' conforms with ktherm=2

!=======================================================================

      contains

!=======================================================================

! Compute the mixed layer heat balance and update the SST.
! Compute the energy available to freeze or melt ice.
! NOTE: SST changes due to fluxes through the ice are computed in
!       ice_therm_vertical.

      subroutine ocean_mixed_layer (dt, iblk)

      use ice_blocks, only: nx_block, ny_block
      use ice_state, only: aice
      use ice_flux, only: sst, Tf, Qa, uatm, vatm, wind, potT, rhoa, zlvl, &
           frzmlt, fhocn, fswthru, flw, flwout_ocn, fsens_ocn, flat_ocn, evap_ocn, &
           alvdr_ocn, alidr_ocn, alvdf_ocn, alidf_ocn, swidf, swvdf, swidr, swvdr, &
           qdp, hmix, strairx_ocn, strairy_ocn, Tref_ocn, Qref_ocn
      use ice_grid, only: tmask
      use ice_atmo, only: atmo_boundary_layer, atmbndy, atmo_boundary_const, &
           Cdn_atm, Cd_atm

      real (kind=dbl_kind), intent(in) :: &
         dt      ! time step

      integer (kind=int_kind), intent(in) :: &
         iblk    ! block index

      ! local variables

      real (kind=dbl_kind) :: &
         TsfK , & ! surface temperature (K)
         swabs    ! surface absorbed shortwave heat flux (W/m^2)

      real (kind=dbl_kind), parameter :: &
         frzmlt_max = c1000   ! max magnitude of frzmlt (W/m^2)

      integer (kind=int_kind) :: &
         i, j           , & ! horizontal indices
         ij                 ! combined ij index

      real (kind=dbl_kind), dimension(nx_block,ny_block) :: &
         delt  , & ! potential temperature difference   (K)
         delq  , & ! specific humidity difference   (kg/kg)
         shcoef, & ! transfer coefficient for sensible heat
         lhcoef    ! transfer coefficient for latent heat

      integer (kind=int_kind) :: &
         icells    ! number of ocean cells

      integer (kind=int_kind), dimension(nx_block*ny_block) :: &
         indxi, indxj    ! compressed indices for ocean cells

      !-----------------------------------------------------------------
      ! Identify ocean cells.
      ! Set fluxes to zero in land cells.
      !-----------------------------------------------------------------

         icells = 0
         indxi(:) = 0
         indxj(:) = 0
         do j = 1, ny_block
         do i = 1, nx_block
            if (tmask(i,j,iblk)) then
               icells = icells + 1
               indxi(icells) = i
               indxj(icells) = j
            else
               sst       (i,j,iblk) = c0
               frzmlt    (i,j,iblk) = c0
               flwout_ocn(i,j,iblk) = c0
               fsens_ocn (i,j,iblk) = c0
               flat_ocn  (i,j,iblk) = c0
               evap_ocn  (i,j,iblk) = c0
            endif
         enddo                  ! i
         enddo                  ! j

      !-----------------------------------------------------------------
      ! Compute boundary layer quantities
      !-----------------------------------------------------------------

         if (trim(atmbndy) == 'constant') then
            call atmo_boundary_const (nx_block,  ny_block,   &
                                      'ocn',     icells,     &
                                      indxi,     indxj,      &
                                      uatm       (:,:,iblk), &   
                                      vatm       (:,:,iblk), &   
                                      wind       (:,:,iblk), &   
                                      rhoa       (:,:,iblk), &
                                      strairx_ocn(:,:,iblk), & 
                                      strairy_ocn(:,:,iblk), & 
                                      sst        (:,:,iblk), &    
                                      potT       (:,:,iblk), &
                                      Qa         (:,:,iblk), &     
                                      delt       (:,:),      &    
                                      delq       (:,:),      &
                                      lhcoef     (:,:),      &
                                      shcoef     (:,:),      &
                                      Cdn_atm(:,:,iblk)) 

         else ! default
            call atmo_boundary_layer (nx_block,  ny_block,   &
                                      'ocn',     icells,     &
                                      indxi,     indxj,      &
                                      sst        (:,:,iblk), &    
                                      potT       (:,:,iblk), &
                                      uatm       (:,:,iblk), &   
                                      vatm       (:,:,iblk), &   
                                      wind       (:,:,iblk), &   
                                      zlvl       (:,:,iblk), &   
                                      Qa         (:,:,iblk), &     
                                      rhoa       (:,:,iblk), &
                                      strairx_ocn(:,:,iblk), & 
                                      strairy_ocn(:,:,iblk), & 
                                      Tref_ocn   (:,:,iblk), & 
                                      Qref_ocn   (:,:,iblk), & 
                                      delt       (:,:),      &    
                                      delq       (:,:),      &
                                      lhcoef     (:,:),      &
                                      shcoef     (:,:),      &
                                      Cdn_atm(:,:,iblk),     & 
                                      Cd_atm(:,:,iblk))
         endif

      !-----------------------------------------------------------------
      ! Ocean  albedo
      ! For now, assume albedo = albocn in each spectral band.
      !-----------------------------------------------------------------

         alvdr_ocn(:,:,iblk) = albocn
         alidr_ocn(:,:,iblk) = albocn
         alvdf_ocn(:,:,iblk) = albocn
         alidf_ocn(:,:,iblk) = albocn

      !-----------------------------------------------------------------
      ! Compute ocean fluxes and update SST
      !-----------------------------------------------------------------

!DIR$ CONCURRENT !Cray
!cdir nodep      !NEC
!ocl novrec      !Fujitsu
      do ij = 1, icells
         i = indxi(ij)
         j = indxj(ij)

         ! shortwave radiative flux
         swabs = (c1-alvdr_ocn(i,j,iblk)) * swvdr(i,j,iblk) &
               + (c1-alidr_ocn(i,j,iblk)) * swidr(i,j,iblk) &
               + (c1-alvdf_ocn(i,j,iblk)) * swvdf(i,j,iblk) &
               + (c1-alidf_ocn(i,j,iblk)) * swidf(i,j,iblk) 

         ! ocean surface temperature in Kelvin
         TsfK = sst(i,j,iblk) + Tffresh

         ! longwave radiative flux
         flwout_ocn(i,j,iblk) = -stefan_boltzmann * TsfK**4

         ! downward latent and sensible heat fluxes
         fsens_ocn(i,j,iblk) =  shcoef(i,j) * delt(i,j)
         flat_ocn (i,j,iblk) =  lhcoef(i,j) * delq(i,j)
         evap_ocn (i,j,iblk) = -flat_ocn(i,j,iblk) / Lvap

         ! Compute sst change due to exchange with atm/ice above
         sst(i,j,iblk) = sst(i,j,iblk) + dt * ( &
              (fsens_ocn(i,j,iblk) + flat_ocn(i,j,iblk) + flwout_ocn(i,j,iblk) &
             + flw(i,j,iblk) + swabs) * (c1-aice(i,j,iblk)) &
             + fhocn(i,j,iblk) + fswthru(i,j,iblk))         &  ! these are *aice
             / (cprho*hmix(i,j,iblk))

         ! adjust qdp if cooling of mixed layer would occur when sst <= Tf
         if (sst(i,j,iblk) <= Tf(i,j,iblk) .and. qdp(i,j,iblk) > c0) qdp(i,j,iblk) = c0

         ! computed T change due to exchange with deep layers:
         sst(i,j,iblk) = sst(i,j,iblk) - qdp(i,j,iblk)*dt/(cprho*hmix(i,j,iblk))

         ! compute potential to freeze or melt ice
         frzmlt(i,j,iblk) = (Tf(i,j,iblk)-sst(i,j,iblk))*cprho*hmix(i,j,iblk)/dt
         frzmlt(i,j,iblk) = min(max(frzmlt(i,j,iblk),-frzmlt_max),frzmlt_max)

         ! if sst is below freezing, reset sst to Tf
         if (sst(i,j,iblk) <= Tf(i,j,iblk)) sst(i,j,iblk) = Tf(i,j,iblk)

      enddo                     ! ij

      end subroutine ocean_mixed_layer

!=======================================================================

      end module ice_ocean

!=======================================================================
