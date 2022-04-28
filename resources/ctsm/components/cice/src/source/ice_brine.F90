!  SVN:$Id: ice_brine.F90 897 2015-01-22 01:15:53Z tcraig $
!=======================================================================
!
! Computes ice microstructural information for use in biogeochemistry
!
! authors: Nicole Jeffery, LANL
!
      module ice_brine

      use ice_kinds_mod
      use ice_constants
      use ice_domain_size, only: nilyr, nblyr, max_blocks, ncat
      use ice_fileunits, only: nu_diag, nu_rst_pointer, nu_dump_hbrine, &
          nu_restart_hbrine, flush_fileunit
      use ice_blocks, only: nx_block, ny_block
      use ice_communicate, only: my_task, master_task
      use ice_exit, only: abort_ice
      use ice_state, only: ntrcr, nt_qice, nt_sice
      use ice_zbgc_shared, only: cgrid, bgrid, igrid, exp_h, k_o, rhosi, &
           hbr_min, thinS, min_salin, igrid, remap_layers_bgc, &
           phi_snow, restart_hbrine, first_ice

      implicit none

      private
      public :: preflushing_changes, compute_microS_mushy, &
                update_hbrine, init_hbrine, write_restart_hbrine, &
                hbrine_diags
 
      real (kind=dbl_kind), parameter :: &   
         maxhbr  = 1.25_dbl_kind  , & ! brine overflows if hbr > maxhbr*hin
         viscos  = 2.1e-6_dbl_kind, & ! kinematic viscosity (m^2/s) 
         ! Brine salinity as a cubic function of temperature
         a1      = -21.4_dbl_kind , & ! (psu/C)  
         a2      = -0.886_dbl_kind, & ! (psu/C^2)
         a3      = -0.012_dbl_kind, & ! (psu/C^3)
         ! Brine density as a quadratic of brine salinity
         b1      = 1000.0_dbl_kind, & ! (kg/m^3)  
         b2      = 0.8_dbl_kind       ! (kg/m^3/ppt)

      real (kind=dbl_kind), dimension (nx_block,ny_block,ncat,max_blocks) :: &
         first_ice_real     ! .true. = c1, .false. = c0

!=======================================================================

      contains

!=======================================================================

!  Initialize brine height tracer

      subroutine init_hbrine 

      use ice_state, only: nt_fbri, trcrn

      integer (kind=int_kind) :: &
           k               ! vertical index

      real (kind=dbl_kind) :: & 
         zspace            ! grid spacing for CICE vertical grid

      !-----------------------------------------------------------------
      ! Calculate bio gridn: 0 to 1 corresponds to ice top to bottom 
      !-----------------------------------------------------------------

      bgrid(:)       = c0 ! bgc grid points         
      bgrid(nblyr+2) = c1 ! bottom value
      igrid(:)       = c0 ! bgc interface grid points   
      igrid(1)       = c0 ! ice top
      igrid(nblyr+1) = c1 ! ice bottom
      
      zspace = c1/max(c1,(real(nblyr,kind=dbl_kind)))
      do k = 2, nblyr+1
         bgrid(k) = zspace*(real(k,kind=dbl_kind) - c1p5)
      enddo
      
      do k = 2, nblyr
         igrid(k) = p5*(bgrid(k+1)+bgrid(k))
      enddo

      !-----------------------------------------------------------------
      ! Calculate CICE cgrid for interpolation ice top (0) to bottom (1) 
      !-----------------------------------------------------------------
       
      cgrid(1) = c0                           ! CICE vertical grid top point
      zspace = c1/(real(nilyr,kind=dbl_kind)) ! CICE grid spacing
    
      do k = 2, nilyr+1
         cgrid(k) = zspace * (real(k,kind=dbl_kind) - c1p5) 
      enddo 

      !-----------------------------------------------------------------
      ! initialize restart variables
      !-----------------------------------------------------------------

      if (restart_hbrine) then
         call read_restart_hbrine
      else
         first_ice(:,:,:,:) = .true.            
         trcrn(:,:,nt_fbri,:,:) = c1
      endif

      end subroutine init_hbrine

!=======================================================================

! Computes the top and bottom brine boundary changes for flushing
! works for zsalinity and tr_salinity
!
! NOTE: In this subroutine, trcrn(nt_fbri) is  the volume fraction of ice with 
! dynamic salinity or the height ratio == hbr/vicen*aicen, where hbr is the 
! height of the brine surface relative to the bottom of the ice.  This volume fraction
! may be > 1 in which case there is brine above the ice surface (meltponds). 

      subroutine preflushing_changes (nx_block, ny_block,             &
                                      icells,   n_cat,                &
                                      indxii,   indxjj,               &
                                      aicen,    vicen,    vsnon,      &
                                      meltb,    meltt,    congel,     &
                                      snoice,   hice_old, fbri,       & 
                                      dhbr_top, dhbr_bot,             &
                                      hbr_old,  hin,hsn,  firstice)

      integer (kind=int_kind), intent(in) :: &
         nx_block, ny_block, & ! block dimensions
         icells            , & ! number of cells with aicen > 0
         n_cat                 ! category
                              
      integer (kind=int_kind), dimension(nx_block*ny_block), &
         intent(in) :: &
         indxii, indxjj ! compressed indices for icells with aicen > puny

      real (kind=dbl_kind), dimension (nx_block,ny_block), intent(in) :: &
         aicen       , & ! concentration of ice
         vicen       , & ! volume per unit area of ice          (m)
         vsnon       , & ! volume per unit area of snow         (m)
         meltb       , & ! bottom ice melt                      (m)
         meltt       , & ! top ice melt                         (m)
         congel      , & ! bottom ice growth                    (m)
         snoice          ! top ice growth from flooding         (m)
 
      real (kind=dbl_kind), dimension(nx_block*ny_block), intent(inout) :: &
         hin          , & ! ice thickness (m) 
         hsn          , & ! snow thickness (m) 
         hbr_old          ! old brine height (m)

      real (kind=dbl_kind), dimension (nx_block,ny_block), intent(inout) :: &
         fbri         , & ! trcrn(i,j,nt_fbri)
         dhbr_top     , & ! brine change in top for diagnostics (m)
         dhbr_bot     , & ! brine change in bottom for diagnostics (m)
         hice_old         ! old ice thickness (m)

      logical (kind=log_kind), dimension (nx_block,ny_block), intent(in) :: &
         firstice         ! if true, initialized values should be used     

      ! local variables

      integer (kind=int_kind) :: &
         i, j         , & ! horizontal indices
         ij               ! horizontal index, combines i and j loops
  
      real (kind=dbl_kind) :: &
         hin_old          ! ice thickness before current melt/growth (m)

      real (kind=dbl_kind):: &
         dhice            ! Change in hin due to subl/cond  (m)

      !-----------------------------------------------------------------
      ! initialize
      !-----------------------------------------------------------------

      dhbr_top(:,:) = c0
      dhbr_bot(:,:) = c0

!DIR$ CONCURRENT !Cray
!cdir nodep      !NEC
!ocl novrec      !Fujitsu
      do ij = 1, icells
      
         i = indxii(ij)
         j = indxjj(ij)
         if (fbri(i,j) <= c0) then
            write(nu_diag, *) 'preflushing: fbri <= c0:',i,j
            write(nu_diag, *) 'vicen, aicen', vicen(i,j), aicen(i,j)
            write(nu_diag, *) 'fbri, hice_old', fbri(i,j), hice_old(i,j)
            call abort_ice ('ice_brine error')
         endif

         hin(ij) = vicen(i,j) / aicen(i,j)
         hsn(ij) = vsnon(i,j) / aicen(i,j)
         hin_old = max(c0, hin(ij) + meltb (i,j) + meltt (i,j) &
                                   - congel(i,j) - snoice(i,j))
         dhice = hin_old - hice_old(i,j)   ! change due to subl/cond
         dhbr_top(i,j) = meltt (i,j) - dhice - snoice(i,j)
         dhbr_bot(i,j) = congel(i,j) - meltb(i,j)   

         if ((hice_old(i,j) < puny) .OR. (hin_old < puny) &
                                    .OR. firstice(i,j)) then
            hin_old         = hin(ij) 
            dhbr_top  (i,j) = c0
            dhbr_bot  (i,j) = c0
            fbri      (i,j) = c1 
         endif

         hbr_old(ij) = fbri(i,j) * hice_old(i,j)

      enddo  ! ij

      end subroutine preflushing_changes

!=======================================================================

! Computes ice microstructural properties for updating hbrine
!
! NOTE: This subroutine uses thermosaline_vertical output to compute
! average ice permeability and the surface ice porosity

      subroutine compute_microS_mushy (nx_block, ny_block,               &
                                       icells,   n_cat,                  &
                                       indxii,   indxjj,                 &
                                       trcrn,    hice_old,   hbr_old,     &
                                       sss,      sst,        bTin,       &
                                       bphin,    kperm,      zphi_min,   &
                                       bSin,     brine_sal,  brine_rho,  &
                                       iphin,    ibrine_rho, ibrine_sal)

      use ice_therm_mushy, only: temperature_mush, liquid_fraction, permeability

      integer (kind=int_kind), intent(in) :: &
         nx_block, ny_block, & ! block dimensions
         icells , &            ! number of cells with aicen > 0
         n_cat                 ! ice category
                              
      integer (kind=int_kind), dimension(nx_block*ny_block), &
         intent(in) :: &
         indxii, indxjj  ! compressed indices for icells with aicen > puny

      real (kind=dbl_kind), dimension (nx_block,ny_block), &
         intent(in) :: &
         hice_old    , & ! previous timestep ice height (m)
         sss         , & ! ocean salinity (ppt)
         sst             ! ocean temperature (C)
       
      real (kind=dbl_kind), dimension(nx_block,ny_block,ntrcr), &
         intent(in) :: &
         trcrn           

      real (kind=dbl_kind), dimension(nx_block*ny_block), intent(out) :: & 
         kperm       , & ! average ice permeability (m^2)
         zphi_min        ! surface porosity

      real (kind=dbl_kind), dimension (nx_block*ny_block), intent(inout) :: &
         hbr_old           ! previous timestep brine height (m)

      real (kind=dbl_kind), dimension (nx_block*ny_block,nblyr+1), &
         intent(inout)  :: &
         iphin       , & ! porosity on the igrid 
         ibrine_rho  , & ! brine rho on interface  
         ibrine_sal      ! brine sal on interface   

      real (kind=dbl_kind), dimension (nx_block*ny_block,nblyr+2), &
         intent(inout)  :: &
         bSin        , &    ! bulk salinity (ppt) on bgrid
         brine_sal   , & ! equilibrium brine salinity (ppt) 
         brine_rho       ! internal brine density (kg/m^3) 

      real (kind=dbl_kind), dimension (nx_block,ny_block,nblyr+2), &
         intent(inout)  :: &
         bTin        , & ! temperature on bgrid
         bphin           ! porosity on bgrid

      ! local variables

      real (kind=dbl_kind), dimension (nx_block*ny_block,nilyr) :: &
         cSin        , & ! bulk salinity (ppt)
         cqin            ! enthalpy ()

      real (kind=dbl_kind), dimension (nx_block*ny_block,nblyr+2) :: &
         zTin        , & ! Temperature of ice layers on bgrid (C) 
         zSin        , & ! Salinity of ice layers on bgrid (C) 
         bqin            ! enthalpy on the bgrid ()

      integer (kind=int_kind) :: &
         i, j        , & ! horizontal indices
         ij          , & ! horizontal index, combines i and j loops
         k               ! vertical biology layer index 
      
      real (kind=dbl_kind), dimension(icells) :: &
         surface_S   , & ! salinity of ice above hin > hbr 
         hinc_old    , & ! mean ice thickness before current melt/growth (m)
         hbrc_old        ! mean brine thickness before current melt/growth (m)
      
      real (kind=dbl_kind), dimension (nx_block*ny_block,nblyr+2) :: &
         trtmp_s     , & ! temporary, remapped tracers   
         trtmp_q         ! temporary, remapped tracers   
     
      !-----------------------------------------------------------------
      ! Define ice salinity and temperature on bgrid
      !-----------------------------------------------------------------

      do k = 1, nilyr
!DIR$ CONCURRENT !Cray
!cdir nodep      !NEC
!ocl novrec      !Fujitsu
         do ij = 1, icells
            i = indxii(ij)
            j = indxjj(ij)            
            cSin(ij,k) = trcrn(i,j,nt_sice+k-1)
            cqin(ij,k) = trcrn(i,j,nt_qice+k-1)
         enddo
      enddo
        
      trtmp_s(:,:) = c0
      trtmp_q(:,:) = c0
!DIR$ CONCURRENT !Cray
!cdir nodep      !NEC
!ocl novrec      !Fujitsu
      do ij = 1, icells   ! map Sin and qin profiles to bgc grid 
         i = indxii(ij)
         j = indxjj(ij)
         hbr_old  (ij) = min(hbr_old(ij), maxhbr*hice_old(i,j))
         hinc_old (ij) = hice_old(i,j)
         hbrc_old (ij) = hbr_old (ij)

         call remap_layers_bgc (ntrcr,            nilyr,          &
                                nt_sice,                          &
                                trcrn(i,j,:),     trtmp_s(ij,:),  &
                                0,                nblyr+1,        &
                                hinc_old(ij),     hinc_old(ij),   &
                                cgrid(2:nilyr+1),                 &
                                bgrid(1:nblyr+1), surface_S(ij))
     
         call remap_layers_bgc (ntrcr,            nilyr,          &
                                nt_qice,                          &
                                trcrn(i,j,:),     trtmp_q(ij,:),  &
                                0,                nblyr+1,        &
                                hinc_old(ij),     hinc_old(ij),   &
                                cgrid(2:nilyr+1),                 &
                                bgrid(1:nblyr+1), surface_S(ij))
      enddo

      do k = 1, nblyr+1
!DIR$ CONCURRENT !Cray
!cdir nodep      !NEC
!ocl novrec      !Fujitsu
         do ij = 1, icells   
            i = indxii(ij)
            j = indxjj(ij)
            bqin (ij, k) = min(c0,        trtmp_q(ij,k))
            bSin (ij, k) = max(min_salin, trtmp_s(ij,k))
            bTin (i,j,k) = temperature_mush(bqin(ij, k), bSin(ij,k))
            bphin(i,j,k) = liquid_fraction (bTin(i,j,k), bSin(ij,k))
         enddo ! ij                          
      enddo    ! k

!DIR$ CONCURRENT !Cray
!cdir nodep      !NEC
!ocl novrec      !Fujitsu
         do ij = 1, icells   
            i = indxii(ij)
            j = indxjj(ij)
            bSin (ij, nblyr+2) = sss(i,j) 
            bTin (i,j,nblyr+2) = sst(i,j)
            bphin(i,j,nblyr+2) = c1
         enddo ! ij                          

      !-----------------------------------------------------------------
      ! Define ice multiphase structure
      !-----------------------------------------------------------------
     
      call prepare_hbrine (icells,     indxii, indxjj, &
                           bSin,       bTin,           &
                           brine_sal,  brine_rho,      &
                           ibrine_sal, ibrine_rho,     &
                           bphin,      iphin,          &
                           kperm,      zphi_min,       &
                           igrid,      sss)
       
      end subroutine compute_microS_mushy

!=======================================================================

      subroutine prepare_hbrine (icells,     indxi,  indxj,  &
                                 bSin,       bTin,           &
                                 brine_sal,  brine_rho,      &
                                 ibrine_sal, ibrine_rho,     &
                                 bphin,      iphin,          &
                                 kperm,      zphi_min,       &
                                 igrid,      sss)

      use ice_therm_shared, only: calculate_Tin_from_qin

      integer (kind=int_kind), intent(in) :: &
         icells         ! number of cells with aicen > 0
                              
      integer (kind=int_kind), dimension(nx_block*ny_block), &
         intent(in) :: &
         indxi, indxj   ! compressed indices for icells with aicen > puny

      real (kind=dbl_kind), dimension (nx_block*ny_block,nblyr+2), &
         intent(in) :: &
         bSin           ! salinity of ice layers on bio grid (ppt)

      real (kind=dbl_kind), dimension (nx_block,ny_block,nblyr+2), &
         intent(in) :: &
         bTin           ! temperature of ice layers on bio grid for history (C)

      real (kind=dbl_kind), dimension (nx_block*ny_block,nblyr+2), &
         intent(inout) :: &
         brine_sal  , & ! equilibrium brine salinity (ppt)  
         brine_rho      ! internal brine density (kg/m^3)

      real (kind=dbl_kind), dimension (nx_block*ny_block,nblyr+1), &
         intent(inout) :: &
         ibrine_rho , & ! brine density on interface (kg/m^3)
         ibrine_sal , & ! brine salinity on interface (ppt)
         iphin          ! porosity on interface

      real (kind=dbl_kind), dimension (nx_block,ny_block,nblyr+2), &
         intent(inout) :: &
         bphin          ! porosity of layers

      real (kind=dbl_kind), dimension (nblyr+1), intent(in):: &
         igrid          ! biology grid interface points

      real (kind=dbl_kind), dimension (nx_block,ny_block), intent(in) :: &
         sss            ! sea surface salinity (ppt)

      real (kind=dbl_kind), dimension (nx_block*ny_block), intent(out) :: &
         kperm      , & ! harmonic average permeability (m^2)
         zphi_min       ! minimum porosity

      ! local variables

      real (kind=dbl_kind), dimension(icells, nblyr+1) :: &
          kin           !  permeability (m^2)
    
      real (kind=dbl_kind), dimension(icells) :: &
          k_min, ktemp

      real (kind=dbl_kind) :: &
          igrp, igrm, rigr  ! grid finite differences
     
      integer (kind=int_kind) :: &
           k, i, j, ij  ! tracer indices

      !-----------------------------------------------------------------
      ! calculate equilibrium brine density and gradients 
      !-----------------------------------------------------------------

      do k = 1, nblyr+1
!DIR$ CONCURRENT !Cray
!cdir nodep      !NEC
!ocl novrec      !Fujitsu
         do ij = 1,icells
            i = indxi(ij)
            j = indxj(ij) 

            brine_sal(ij, k)   = a1*bTin(i,j,k)    &
                               + a2*bTin(i,j,k)**2 &
                               + a3*bTin(i,j,k)**3
            brine_rho(ij, k)   = b1 + b2*brine_sal(ij,k)
            bphin    (i,j,k)   = min(c1, max(puny, bSin(ij,k)*rhosi &
                               /(brine_sal(ij,k)*brine_rho(ij,k)))) 
            kin      (ij, k)   = k_o*bphin(i,j,k)**exp_h 
         enddo ! ij
      enddo    ! k         

!DIR$ CONCURRENT !Cray
!cdir nodep      !NEC
!ocl novrec      !Fujitsu
         do ij = 1, icells
            i = indxi(ij)
            j = indxj(ij) 

            brine_sal (ij, nblyr+2) = sss       (i,j)
            brine_rho (ij, nblyr+2) = rhow
            bphin     (i,j,nblyr+2) = c1
            ibrine_sal(ij, 1)       = brine_sal (ij, 2)
            ibrine_sal(ij, nblyr+1) = brine_sal (ij, nblyr+2)
            ibrine_rho(ij, 1)       = brine_rho (ij, 2)
            ibrine_rho(ij, nblyr+1) = brine_rho (ij, nblyr+2)
            iphin     (ij, 1)       = bphin     (i,j,2)
            iphin     (ij, nblyr+1) = bphin     (i,j,nblyr+1)
            zphi_min  (ij)          = bphin     (i,j,2) 
            k_min     (ij)          = MINVAL(kin(ij, 2:nblyr+1))
            kperm     (ij)          = c0  ! initialize
            ktemp     (ij)          = c0
         enddo ! ij

      do k = 2, nblyr
!DIR$ CONCURRENT !Cray
!cdir nodep      !NEC
!ocl novrec      !Fujitsu
         do ij = 1, icells
            i = indxi(ij)
            j = indxj(ij) 
 
            if (k_min(ij) > c0) then
               ktemp(ij) = ktemp(ij) + c1/kin(ij,k)
               kperm(ij) = k_min(ij)
            endif

            igrp = igrid(k+1) - igrid(k  )
            igrm = igrid(k  ) - igrid(k-1)
            rigr = c1 / (igrid(k+1)-igrid(k-1))

            ibrine_sal(ij,k) = (brine_sal(ij,k+1)*igrp &
                             +  brine_sal(ij,k  )*igrm) * rigr
            ibrine_rho(ij,k) = (brine_rho(ij,k+1)*igrp &
                             +  brine_rho(ij,k  )*igrm) * rigr
            iphin     (ij,k) = min(c1, max(puny, &
                                  (bphin(i,j,k+1)*igrp &
                                +  bphin(i,j,k  )*igrm) * rigr))
         enddo ! ij
      enddo    ! k         

!DIR$ CONCURRENT !Cray
!cdir nodep      !NEC
!ocl novrec      !Fujitsu
         do ij = 1, icells
            i = indxi(ij)
            j = indxj(ij) 
 
            if (k_min(ij) > c0) then
                  ktemp(ij) = ktemp(ij) + c1/kin(ij,nblyr+1) 
                  kperm(ij) = real(nblyr,kind=dbl_kind)/ktemp(ij)
            endif
         enddo ! ij

      end subroutine prepare_hbrine

!=======================================================================

! Changes include brine height increases from ice and snow surface melt, 
! congelation growth, and upward pressure driven flow from snow loading.
!  
! Decreases arise from downward flushing and bottom melt.  
!
! NOTE: In this subroutine, trcrn(nt_fbri) is  the volume fraction of ice 
! with dynamic salinity or the height ratio == hbr/vicen*aicen, where 
! hbr is the height of the brine surface relative to the bottom of the 
! ice.  This volume fraction may be > 1 in which case there is brine 
! above the ice surface (ponds).

      subroutine update_hbrine (meltb,      meltt,       &
                                melts,      dt,          &
                                hin,        hsn,         &
                                hin_old,                 &
                                hbr,        hbr_old,     &
                                fbri,                    &
                                dhS_top,    dhS_bottom,  &
                                kperm,      zphi_min,    &
                                darcy_V)

      real (kind=dbl_kind), intent(in) :: &
         dt             ! timestep
        
      real (kind=dbl_kind), intent(in):: &
         meltb,       & ! bottom melt over dt (m)
         meltt,       & ! true top melt over dt (m)
         melts,       & ! true snow melt over dt (m)
         hin,         & ! ice thickness (m)
         hsn,         & ! snow thickness (m)
         hin_old,     & ! past timestep ice thickness (m)
         hbr_old,     & ! previous timestep hbr
         kperm          ! avg ice permeability

      real (kind=dbl_kind), intent(inout):: &
         darcy_V    , & ! Darcy velocity: m/s
         dhS_top    , & ! change in top hbr before darcy flow
         dhS_bottom , & ! change in bottom hbr initially before darcy flow
         hbr        , & ! thickness of brine (m) 
         fbri       , & ! brine height ratio tracer (hbr/hin) 
         zphi_min       ! surface porosity

      ! local variables

      real (kind=dbl_kind) :: &  
         hbr_min    , & ! thinS or hin 
         dhbr_hin   , & ! hbr-hin
         hbrocn     , & ! brine height above sea level (m) hbr-h_ocn
         dhbr       , & ! change in brine surface
         h_ocn      , & ! new ocean surface from ice bottom (m)
         darcy_coeff, & ! magnitude of the Darcy velocity/hbrocn (1/s)
         hbrocn_new     ! hbrocn after flushing

      real (kind=dbl_kind), parameter :: &
         dh_min = p001, & ! brine remains within dh_min of sea level
                          ! when ice thickness is less than thinS
!echmod USE NAMELIST PARAMETERS rfracmin, rfracmax 
         run_off = c0   ! fraction of melt that runs off directly to the ocean

         hbrocn     = c0
         darcy_V    = c0
         hbrocn_new = c0
         h_ocn = rhosi/rhow*hin + rhos/rhow*hsn 
       
         if (hbr_old > thinS .AND. hin_old > thinS) then

            dhS_top = -max(c0, min(hin_old-hbr_old, meltt)) * rhoi/rhow
            dhS_top = dhS_top - max(c0, melts) * rhos/rhow
            dhS_top = (c1 - run_off) * dhS_top
            dhbr    = dhS_bottom - dhS_top  
            hbr     = max(hbr_min, hbr_old + dhbr)
            hbrocn  = hbr - h_ocn
            darcy_coeff = max(c0, kperm*gravit/(viscos*hbr_old))

            if (hbrocn > c0 .AND. hbr > thinS ) then   
               hbrocn_new = hbrocn*exp(-darcy_coeff/zphi_min*dt)
               hbr = max(thinS, h_ocn + hbrocn_new)    
            elseif (hbrocn < c0) then
               if (hbr >= hin) zphi_min = phi_snow
               hbrocn_new = hbrocn*exp(-darcy_coeff/zphi_min*dt)
               hbr = max(hbr_min, h_ocn + hbrocn_new)
            endif

            hbrocn_new = hbr - h_ocn
            darcy_V  = -SIGN((hbrocn-hbrocn_new)/dt*zphi_min, hbrocn)
            dhS_top  = dhS_top + SIGN((hbrocn-hbrocn_new), hbrocn)

         else    ! very thin brine height 
            hbr_min  = min(thinS, hin)
            hbr = max(hbr_min, hbr_old+dhS_bottom-dhS_top)
            dhbr_hin = hbr - h_ocn
            if (abs(dhbr_hin) > dh_min) &
               hbr = max(hbr_min, h_ocn + SIGN(dh_min,dhbr_hin))
         endif 

         fbri = hbr/hin

      end subroutine update_hbrine

!=======================================================================

      subroutine read_restart_hbrine()

! Reads all values needed for hbrine
! author Elizabeth C. Hunke, LANL

      use ice_communicate, only: my_task, master_task
      use ice_domain, only: nblocks
      use ice_fileunits, only: nu_diag, nu_restart_hbrine
      use ice_state, only: trcrn, nt_fbri
      use ice_restart,only: read_restart_field

      ! local variables

      integer (kind=int_kind) :: &
         i, j, n, iblk          ! counting indices

      logical (kind=log_kind) :: &
         diag

      diag = .true.

      if (my_task == master_task) write(nu_diag,*) 'brine restart'

      call read_restart_field(nu_restart_hbrine,0,trcrn(:,:,nt_fbri,:,:),'ruf8', &
                              'fbrn',ncat,diag,field_loc_center,field_type_scalar)
      call read_restart_field(nu_restart_hbrine,0,first_ice_real(:,:,:,:),'ruf8', &
                              'first_ice',ncat,diag,field_loc_center,field_type_scalar)
       
      do iblk = 1, nblocks
         do n = 1,ncat
            do j = 1, ny_block
            do i = 1, nx_block
               if (first_ice_real(i,j,n,iblk) >= p5) then
                  first_ice (i,j,n,iblk) = .true.
               else
                  first_ice (i,j,n,iblk) = .false.
               endif
            enddo
            enddo
         enddo
      enddo

      end subroutine read_restart_hbrine

!=======================================================================

      subroutine write_restart_hbrine()

! Dumps all values needed for a hbrine restart
! author Elizabeth C. Hunke, LANL

      use ice_domain, only: nblocks
      use ice_state, only: trcrn, nt_fbri
      use ice_restart,only: write_restart_field

      ! local variables

      integer (kind=int_kind) :: &
         i, j, n, iblk

      logical (kind=log_kind) :: diag

      diag = .true.

      do iblk = 1, nblocks
       do n = 1,ncat
         do j = 1, ny_block
         do i = 1, nx_block
            if (first_ice(i,j,n,iblk)) then
               first_ice_real(i,j,n,iblk) = c1
            else
               first_ice_real(i,j,n,iblk) = c0
            endif
         enddo
         enddo
       enddo
      enddo

      call write_restart_field(nu_dump_hbrine,0,trcrn(:,:,nt_fbri,:,:),'ruf8', &
                               'fbrn',ncat,diag)
      call write_restart_field(nu_dump_hbrine,0,first_ice_real(:,:,:,:),'ruf8', &
                               'first_ice',ncat,diag)

      end subroutine write_restart_hbrine

!=======================================================================
!
! Writes diagnostic info (max, min, global sums, etc) to standard out
!
! authors: Elizabeth C. Hunke, LANL
!          Bruce P. Briegleb, NCAR
!          Cecilia M. Bitz, UW
!          Nicole Jeffery, LANL

      subroutine hbrine_diags (dt)
              
      use ice_broadcast, only: broadcast_scalar
      use ice_diagnostics, only: npnt, print_points, pmloc, piloc, pjloc, pbloc, &
                                plat, plon
      use ice_domain_size, only: ncat
      use ice_state, only: aice, aicen, vicen, vice, trcr, nt_fbri, &
                          trcrn, nt_sice
      use ice_zbgc_shared, only: darcy_V

      real (kind=dbl_kind), intent(in) :: &
         dt      ! time step

      ! local variables

      integer (kind=int_kind) :: &
         i, j, k, n, iblk

      ! fields at diagnostic points
      real (kind=dbl_kind), dimension(npnt) :: &
         phinS, phinS1, pdarcy_V, pfbri

      real (kind=dbl_kind), dimension(npnt,nilyr) :: &
         pSin

      !-----------------------------------------------------------------
      ! Dynamic brine height
      !-----------------------------------------------------------------

      if (print_points) then

      !-----------------------------------------------------------------
      ! state of the ice and associated fluxes for 2 defined points
      ! NOTE these are computed for the last timestep only (not avg)
      !-----------------------------------------------------------------

         do n = 1, npnt
           if (my_task == pmloc(n)) then
               i = piloc(n)
               j = pjloc(n)
               iblk = pbloc(n)            
               phinS1(n) = c0             
               phinS(n) = c0            
               pfbri(n) = trcrn(i,j,nt_fbri,1,iblk) 
               pdarcy_V(n) = darcy_V(i,j,1,iblk)
               if (aice(i,j,iblk) > c0) &
                       phinS (n) = trcr(i,j,nt_fbri,iblk) &
                                 * vice(i,j,iblk)/aice(i,j,iblk)
               if (aicen(i,j,1,iblk)> c0) &
                       phinS1(n) = trcrn(i,j,nt_fbri,1,iblk) &
                                 * vicen(i,j,1,iblk)/aicen(i,j,1,iblk)
               do k = 1,nilyr
                       pSin(n,k) = trcr(i,j,nt_sice+k-1,iblk)
               enddo
            endif                 ! my_task = pmloc
           
            do k = 1,nilyr
            call broadcast_scalar(pSin(n,k), pmloc(n))   
            enddo
            call broadcast_scalar(pfbri(n), pmloc(n))  
            call broadcast_scalar(phinS1(n), pmloc(n))  
            call broadcast_scalar(phinS(n), pmloc(n)) 
            call broadcast_scalar(pdarcy_V(n), pmloc(n))
         enddo                  ! npnt
      endif                     ! print_points

      !-----------------------------------------------------------------
      ! start spewing
      !-----------------------------------------------------------------

      if (my_task == master_task) then

      call flush_fileunit(nu_diag)

      !-----------------------------------------------------------------
      ! diagnostics for Arctic and Antarctic points
      !-----------------------------------------------------------------

      if (print_points) then
        write(nu_diag,*) '-------- hbrine -------'
        write(nu_diag,900) 'hbrine, (m)            = ',phinS(1),phinS(2)
        write(nu_diag,900) 'fbri, cat1 (m)         = ',pfbri(1),pfbri(2)
        write(nu_diag,900) 'hbrine cat1, (m)       = ',phinS1(1),phinS1(2)  
        write(nu_diag,900) 'darcy_V cat1, (m/s)    = ',pdarcy_V(1),pdarcy_V(2)
        do k = 1, nilyr
        write(nu_diag,900) 'salinity profile (ppt) = ',pSin(1,k),pSin(2,k)
        enddo
      endif                   ! print_points
      endif                   ! my_task = master_task 

  900 format (a25,2x,f24.17,2x,f24.17)

      end subroutine hbrine_diags

!=======================================================================

      end module ice_brine

!=======================================================================
