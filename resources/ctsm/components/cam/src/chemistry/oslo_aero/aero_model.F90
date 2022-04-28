!===============================================================================
! Modal Aerosol Model
!===============================================================================
module aero_model

#include <preprocessorDefinitions.h>

  use shr_kind_mod,   only: r8 => shr_kind_r8
  use constituents,   only: pcnst, cnst_name, cnst_get_ind
  use ppgrid,         only: pcols, pver, pverp
  use phys_control,   only: phys_getopts, cam_physpkg_is
  use cam_abortutils, only: endrun
  use cam_logfile,    only: iulog
  use perf_mod,       only: t_startf, t_stopf
  use camsrfexch,     only: cam_in_t, cam_out_t
  use aerodep_flx,    only: aerodep_flx_prescribed
  use physics_types,  only: physics_state, physics_ptend, physics_ptend_init
  use physics_buffer, only: physics_buffer_desc
  use physics_buffer, only: pbuf_get_field, pbuf_get_index, pbuf_set_field
  use physconst,      only: gravit, rair, rhoh2o
  use spmd_utils,     only: masterproc
  use infnan,         only: nan, assignment(=)

  use cam_history,    only: outfld, fieldname_len
  use chem_mods,      only: gas_pcnst, adv_mass
  use mo_tracname,    only: solsym
  use aerosoldef, only: chemistryIndex, physicsIndex &
                        , getCloudTracerIndexDirect &
                        , getCloudTracerName
  use condtend, only: N_COND_VAP, COND_VAP_ORG_SV, COND_VAP_ORG_LV, COND_VAP_H2SO4 &
                      , condtend_sub
  use koagsub, only: coagtend, clcoag 
  use sox_cldaero_mod, only: sox_cldaero_init
  !use modal_aero_data,only: cnst_name_cw, lptr_so4_cw_amode
  !use modal_aero_data,only: ntot_amode, modename_amode, nspec_max

  use ref_pres,       only: top_lev => clim_modal_aero_top_lev

  !use modal_aero_wateruptake, only: modal_strat_sulfate
  use mo_setsox,              only: setsox
  use mo_mass_xforms,         only: vmr2mmr, mmr2vmr, mmr2vmri

  implicit none
  private

  public :: aero_model_readnl
  public :: aero_model_register
  public :: aero_model_init
  public :: aero_model_gasaerexch ! create, grow, change, and shrink aerosols.
  public :: aero_model_drydep     ! aerosol dry deposition and sediment
  public :: aero_model_wetdep     ! aerosol wet removal
  public :: aero_model_emissions  ! aerosol emissions
  public :: aero_model_surfarea  ! tropopspheric aerosol wet surface area for chemistry
  public :: aero_model_strat_surfarea ! stratospheric aerosol wet surface area for chemistry

 ! Misc private data 

  ! number of modes
  integer :: nmodes
  integer :: pblh_idx            = 0
  integer :: dgnum_idx           = 0
  integer :: dgnumwet_idx        = 0
  integer :: rate1_cw2pr_st_idx  = 0  

  integer :: wetdens_ap_idx      = 0
  integer :: qaerwat_idx         = 0

  integer :: fracis_idx          = 0
  integer :: prain_idx           = 0
  integer :: rprddp_idx          = 0 
  integer :: rprdsh_idx          = 0 
  integer :: nevapr_shcu_idx     = 0 
  integer :: nevapr_dpcu_idx     = 0 

  integer :: sulfeq_idx = -1

  ! variables for table lookup of aerosol impaction/interception scavenging rates
  integer, parameter :: nimptblgrow_mind=-7, nimptblgrow_maxd=12
  real(r8) :: dlndg_nimptblgrow
  real(r8),allocatable :: scavimptblnum(:,:)
  real(r8),allocatable :: scavimptblvol(:,:)

  ! for surf_area_dens 
  integer,allocatable :: num_idx(:)
  integer,allocatable :: index_tot_mass(:,:)
  integer,allocatable :: index_chm_mass(:,:)

  integer :: ndx_h2so4, ndx_soa_lv, ndx_soa_sv

  ! Namelist variables
  character(len=16) :: wetdep_list(pcnst) = ' '
  character(len=16) :: drydep_list(pcnst) = ' '
  real(r8)          :: sol_facti_cloud_borne   = 1._r8
  real(r8)          :: sol_factb_interstitial  = 0.1_r8
  real(r8)          :: sol_factic_interstitial = 0.4_r8
  real(r8)          :: seasalt_emis_scale 

  integer :: ndrydep = 0
  integer,allocatable :: drydep_indices(:)
  integer :: nwetdep = 0
  integer,allocatable :: wetdep_indices(:)
  logical :: drydep_lq(pcnst)
  logical :: wetdep_lq(pcnst)


  logical :: convproc_do_aer

contains
  
  !=============================================================================
  ! reads aerosol namelist options
  !=============================================================================
  subroutine aero_model_readnl(nlfile)

    use namelist_utils,  only: find_group_name
    use units,           only: getunit, freeunit
    use mpishorthand

    character(len=*), intent(in) :: nlfile  ! filepath for file containing namelist input

    ! Local variables
    integer :: unitn, ierr
    character(len=*), parameter :: subname = 'aero_model_readnl'

    ! Namelist variables
    character(len=16) :: aer_wetdep_list(pcnst) = ' '
    character(len=16) :: aer_drydep_list(pcnst) = ' '

    namelist /aerosol_nl/ aer_wetdep_list, aer_drydep_list, sol_facti_cloud_borne, &
       sol_factb_interstitial, sol_factic_interstitial  

    !-----------------------------------------------------------------------------

    ! Read namelist
    if (masterproc) then
       unitn = getunit()
       open( unitn, file=trim(nlfile), status='old' )
       call find_group_name(unitn, 'aerosol_nl', status=ierr)
       if (ierr == 0) then
          read(unitn, aerosol_nl, iostat=ierr)
          if (ierr /= 0) then
             call endrun(subname // ':: ERROR reading namelist')
          end if
       end if
       close(unitn)
       call freeunit(unitn)
    end if

#ifdef SPMD
    ! Broadcast namelist variables
    call mpibcast(aer_wetdep_list,   len(aer_wetdep_list(1))*pcnst, mpichar, 0, mpicom)
    call mpibcast(aer_drydep_list,   len(aer_drydep_list(1))*pcnst, mpichar, 0, mpicom)
    call mpibcast(sol_facti_cloud_borne, 1,                         mpir8,   0, mpicom)
    call mpibcast(sol_factb_interstitial, 1,                        mpir8,   0, mpicom)
    call mpibcast(sol_factic_interstitial, 1,                       mpir8,   0, mpicom)
    call mpibcast(seasalt_emis_scale, 1,                            mpir8,   0, mpicom)
#endif

    wetdep_list = aer_wetdep_list
    drydep_list = aer_drydep_list

  end subroutine aero_model_readnl

  !=============================================================================
  !=============================================================================
  subroutine aero_model_register()
    use aerosoldef, only: aero_register
    use condtend, only: registerCondensation

    call aero_register()
    call registerCondensation()

  end subroutine aero_model_register

  !=============================================================================
  !=============================================================================
  subroutine aero_model_init( pbuf2d )

    !use mo_chem_utls,    only: get_inv_ndx
    use cam_history,     only: addfld, add_default, horiz_only
    use mo_chem_utls,    only: get_rxt_ndx, get_spc_ndx
    !use modal_aero_data, only: cnst_name_cw
    !use modal_aero_data, only: modal_aero_data_init
    !use rad_constituents,only: rad_cnst_get_info
    use dust_model,      only: dust_init, dust_active 
    use seasalt_model,   only: seasalt_init, seasalt_active
    use drydep_mod,      only: inidrydep
    use wetdep,          only: wetdep_init

    use condtend,            only: initializeCondensation
    use oslo_ocean_intr,     only: oslo_ocean_init  
    use oslo_aerosols_intr,      only: oslo_aero_initialize

    use opttab, only  :      initopt
    use opttab_lw, only: initopt_lw

    use modal_aero_deposition , only: modal_aero_deposition_init

    !use modal_aero_calcsize,   only: modal_aero_calcsize_init
    !use modal_aero_coag,       only: modal_aero_coag_init
    !use modal_aero_deposition, only: modal_aero_deposition_init
    !use modal_aero_gasaerexch, only: modal_aero_gasaerexch_init
    !use modal_aero_newnuc,     only: modal_aero_newnuc_init
    !use modal_aero_rename,     only: modal_aero_rename_init
    !use modal_aero_convproc,   only: ma_convproc_init  

    ! args
    type(physics_buffer_desc), pointer :: pbuf2d(:,:)

    ! local vars
    character(len=*), parameter :: subrname = 'aero_model_init'
    integer :: m, n, id
    character(len=20) :: dummy

    logical  :: history_aerosol ! Output MAM or SECT aerosol tendencies

    integer :: l
    character(len=6) :: test_name
    character(len=64) :: errmes

    character(len=2)  :: unit_basename  ! Units 'kg' or '1' 
    integer :: errcode
    character(len=fieldname_len) :: field_name

    character(len=32) :: spec_name
    character(len=32) :: spec_type
    character(len=32) :: mode_type
    integer :: nspec

    call phys_getopts(history_aerosol_out = history_aerosol, &
                      convproc_do_aer_out = convproc_do_aer)

#ifdef OSLO_AERO
   call constants
   call initopt
   call initlogn
   call initopt_lw
#ifdef AEROCOM
       call initaeropt
       call initdryp
#endif ! aerocom
#endif

    call initializeCondensation()
    call oslo_ocean_init()

    call oslo_aero_initialize(pbuf2d)

    call dust_init()
    call seasalt_init() !seasalt_emis_scale)
    call wetdep_init()
    call modal_aero_deposition_init()


    nwetdep = 0
    ndrydep = 0


    call inidrydep(rair, gravit)
    dummy = 'RAM1'
    call addfld (dummy,horiz_only, 'A','frac','RAM1')
    if ( history_aerosol ) then  
       call add_default (dummy, 1, ' ')
    endif
    dummy = 'airFV'
    call addfld (dummy,horiz_only, 'A','frac','FV')
    if ( history_aerosol ) then  
       call add_default (dummy, 1, ' ')
    endif

    !Get height of boundary layer for boundary layer nucleation
    pblh_idx            = pbuf_get_index('pblh')

    call cnst_get_ind ( "H2SO4", ndx_h2so4, abort=.true. )
    ndx_h2so4 = chemistryIndex(ndx_h2so4)
    call cnst_get_ind ( "SOA_LV", ndx_soa_lv,abort=.true.)
    ndx_soa_lv = chemistryIndex(ndx_soa_lv)
    call cnst_get_ind ( "SOA_SV", ndx_soa_sv, abort=.true.)
    ndx_soa_sv = chemistryIndex(ndx_soa_sv)

    do m = 1,gas_pcnst


       unit_basename = 'kg'  ! Units 'kg' or '1' 

       call addfld( 'GS_'//trim(solsym(m)),horiz_only, 'A', unit_basename//'/m2/s ', &
                    trim(solsym(m))//' gas chemistry/wet removal (for gas species)')
       call addfld( 'AQ_'//trim(solsym(m)),horiz_only, 'A', unit_basename//'/m2/s ', &
                    trim(solsym(m))//' aqueous chemistry (for gas species)')
       if(physicsIndex(m).le.pcnst) then
       if (getCloudTracerIndexDirect(physicsIndex(m)) .gt. 0)then
         call addfld( 'AQ_'//getCloudTracerName(physicsIndex(m)),horiz_only, 'A', unit_basename//'/m2/s ', &
                    trim(solsym(m))//' aqueous chemistry (for cloud species)')
       end if
       end if

       if ( history_aerosol ) then 
          call add_default( 'GS_'//trim(solsym(m)), 1, ' ')
          call add_default( 'AQ_'//trim(solsym(m)), 1, ' ')
       if(physicsIndex(m).le.pcnst) then
          if(getCloudTracerIndexDirect(physicsIndex(m)).gt.0)then
             call add_default( 'AQ_'//getCloudTracerName(physicsIndex(m)),1,' ')
          end if
       end if
       endif
    enddo

    call addfld ('NUCLRATE',(/'lev'/), 'A','#/cm3/s','Nucleation rate')
    call addfld ('FORMRATE',(/'lev'/), 'A','#/cm3/s','Formation rate of 12nm particles')
    call addfld ('COAGNUCL',(/'lev'/), 'A', '/s','Coagulation sink for nucleating particles')
    call addfld ('GRH2SO4',(/'lev'/), 'A', 'nm/hour','Growth rate H2SO4')
    call addfld ('GRSOA',(/'lev'/),'A','nm/hour','Growth rate SOA')
    call addfld ('GR',(/'lev'/), 'A', 'nm/hour','Growth rate, H2SO4+SOA')
    call addfld ('NUCLSOA',(/'lev'/),'A','kg/kg','SOA nucleate')
    call addfld ('ORGNUCL',(/'lev'/),'A','kg/kg','Organic gas available for nucleation')

    if(history_aerosol)then
       call add_default ('NUCLRATE', 1, ' ')
       call add_default ('FORMRATE', 1, ' ')
       call add_default ('COAGNUCL', 1, ' ')
       call add_default ('GRH2SO4', 1, ' ')
       call add_default ('GRSOA', 1, ' ')
       call add_default ('GR', 1, ' ')
       call add_default ('NUCLSOA', 1, ' ')
       call add_default ('ORGNUCL', 1, ' ')
    end if

    call addfld( 'XPH_LWC',    (/ 'lev' /), 'A','kg/kg',   'pH value multiplied by lwc')
    call addfld ('AQSO4_H2O2', horiz_only,  'A','kg/m2/s', 'SO4 aqueous phase chemistry due to H2O2')
    call addfld ('AQSO4_O3',   horiz_only,  'A','kg/m2/s', 'SO4 aqueous phase chemistry due to O3')

    if ( history_aerosol ) then
       call add_default ('XPH_LWC', 1, ' ')
       call add_default ('AQSO4_H2O2', 1, ' ')
       call add_default ('AQSO4_O3', 1, ' ')
    endif

    

end subroutine aero_model_init

  !=============================================================================
  !=============================================================================
  subroutine aero_model_drydep  ( state, pbuf, obklen, ustar, cam_in, dt, cam_out, ptend )

    use calcaersize
    use oslo_aerosols_intr, only: oslo_aero_dry_intr
    use aerosoldef        , only : numberOfProcessModeTracers
    use commondefinitions, only: oslo_nmodes=>nmodes

  ! args 
    type(physics_state),    intent(in)    :: state     ! Physics state variables
    real(r8),               intent(in)    :: obklen(:)          
    real(r8),               intent(in)    :: ustar(:)  ! sfc fric vel
    type(cam_in_t), target, intent(in)    :: cam_in    ! import state
    real(r8),               intent(in)    :: dt             ! time step
    type(cam_out_t),        intent(inout) :: cam_out   ! export state
    type(physics_ptend),    intent(out)   :: ptend     ! indivdual parameterization tendencies
    type(physics_buffer_desc),    pointer :: pbuf(:)

  ! local vars
    integer                                         :: ncol
    real(r8), dimension(pcols, pver, 0:oslo_nmodes) :: oslo_dgnumwet
    real(r8), dimension(pcols, pver, 0:oslo_nmodes) :: oslo_wetdens
    real(r8), dimension(pcols, pver, numberOfProcessModeTracers) :: oslo_dgnumwet_processmodes
    real(r8), dimension(pcols, pver, numberOfProcessModeTracers) :: oslo_wetdens_processmodes

    ncol  = state%ncol
    oslo_wetdens(:,:,:) = 0._r8
    call calcaersize_sub( ncol, &
                     state%t, state%q(1,1,1), state%pmid, state%pdel &
                     ,oslo_dgnumwet , oslo_wetdens                  &
                     ,oslo_dgnumwet_processmodes, oslo_wetdens_processmodes   &
                     )

    call oslo_aero_dry_intr(state, pbuf, obklen, ustar, cam_in, dt, cam_out,ptend &
            , oslo_dgnumwet, oslo_wetdens                        &
            , oslo_dgnumwet_processmodes, oslo_wetdens_processmodes, &
            cam_in%cflx ) !++alfgr

   return
  endsubroutine aero_model_drydep

  !=============================================================================
  !=============================================================================
  subroutine aero_model_wetdep( state, dt, dlf, cam_out, ptend, pbuf)

    use oslo_aerosols_intr, only: oslo_aero_wet_intr

    type(physics_state), intent(in)    :: state       ! Physics state variables
    real(r8),            intent(in)    :: dt          ! time step
    real(r8),            intent(in)    :: dlf(:,:)    ! shallow+deep convective detrainment [kg/kg/s]
    type(cam_out_t),     intent(inout) :: cam_out     ! export state
    type(physics_ptend), intent(out)   :: ptend       ! indivdual parameterization tendencies
    type(physics_buffer_desc), pointer :: pbuf(:)

    call oslo_aero_wet_intr( state, dt, dlf, cam_out, ptend, pbuf)

  endsubroutine aero_model_wetdep

  !-------------------------------------------------------------------------
  ! provides wet tropospheric aerosol surface area info for modal aerosols
  ! called from mo_usrrxt
  !-------------------------------------------------------------------------
  subroutine aero_model_surfarea( &
                  mmr, radmean, relhum, pmid, temp, strato_sad, sulfate, rho, ltrop, &
                  dlat, het1_ndx, pbuf, ncol, sfc, dm_aer, sad_trop, reff_trop )
   
    use commondefinitions, only: nmodes_oslo => nmodes
    use const            , only: numberToSurface
    use aerosoldef       , only: lifeCycleNumberMedianRadius
    use oslo_utils       , only: calculateNumberConcentration

    ! dummy args
    real(r8), intent(in)    :: pmid(:,:)
    real(r8), intent(in)    :: temp(:,:)
    real(r8), intent(in)    :: mmr(:,:,:)
    real(r8), intent(in)    :: radmean      ! mean radii in cm
    real(r8), intent(in)    :: strato_sad(:,:)
    integer,  intent(in)    :: ncol
    integer,  intent(in)    :: ltrop(:)
    real(r8), intent(in)    :: dlat(:)                    ! degrees latitude
    integer,  intent(in)    :: het1_ndx
    real(r8), intent(in)    :: relhum(:,:)
    real(r8), intent(in)    :: rho(:,:) ! total atm density (/cm^3)
    real(r8), intent(in)    :: sulfate(:,:)
    type(physics_buffer_desc), pointer :: pbuf(:)

    real(r8), intent(inout) :: sfc(:,:,:)
    real(r8), intent(inout) :: dm_aer(:,:,:)
    real(r8), intent(inout) :: sad_trop(:,:)
    real(r8), intent(out)   :: reff_trop(:,:)

    ! local vars
    !HAVE TO GET RID OF THIS MODE 0!! MESSES UP EVERYTHING!!
    real(r8)         :: numberConcentration(pcols,pver,0:nmodes_oslo)
    real(r8), target :: sad_mode(pcols,pver, nmodes_oslo)
    real(r8) :: rho_air(pcols,pver)
    integer :: l,m
    integer :: i,k

   !Get air density
    do k=1,pver
       do i=1,ncol
          rho_air(i,k) = pmid(i,k)/(temp(i,k)*287.04_r8)
       end do
    end do
    !    
    !Get number concentrations
    call calculateNumberConcentration(ncol, mmr, rho_air, numberConcentration) 
    
    !Convert to area using lifecycle-radius
    sad_mode = 0._r8
    sad_trop = 0._r8
    do m=1,nmodes_oslo
       do k=1,pver
         sad_mode(:ncol,k,m) = numberConcentration(:ncol,k,m)*numberToSurface(m)*1.e-2_r8 !m2/m3 ==> cm2/cm3
         sad_trop(:ncol,k) = sad_trop(:ncol,k) + sad_mode(:ncol,k,m)
       end do
    end do

    do m=1,nmodes_oslo
       do k=1,pver
         sfc(:ncol,k,m) = sad_mode(:ncol,k,m)     ! aitken_idx:aitken_idx)
         dm_aer(:ncol,k,m) = 2.0_r8*lifeCycleNumberMedianRadius(m)
       end do
    end do

    !++ need to implement reff_trop here
      reff_trop(:,:)=1.0e-6_r8
    !--


  end subroutine aero_model_surfarea

  !-------------------------------------------------------------------------
  ! provides WET stratospheric aerosol surface area info for modal aerosols
  ! if modal_strat_sulfate = TRUE -- called from mo_gas_phase_chemdr
  !-------------------------------------------------------------------------
  subroutine aero_model_strat_surfarea( ncol, mmr, pmid, temp, ltrop, pbuf, strato_sad, reff_strat )

    ! dummy args
    integer,  intent(in)    :: ncol
    real(r8), intent(in)    :: mmr(:,:,:)
    real(r8), intent(in)    :: pmid(:,:)
    real(r8), intent(in)    :: temp(:,:)
    integer,  intent(in)    :: ltrop(:) ! tropopause level indices
    type(physics_buffer_desc), pointer :: pbuf(:)
    real(r8), intent(out)   :: strato_sad(:,:)
    real(r8), intent(out)   :: reff_strat(:,:)

    ! local vars
    real(r8), pointer, dimension(:,:,:) :: dgnumwet
    integer :: beglev(ncol)
    integer :: endlev(ncol)

    reff_strat = 0.1e-6_r8
    strato_sad = 0._r8
    !do nothing
    return

  end subroutine aero_model_strat_surfarea

  !=============================================================================
  !=============================================================================
  subroutine aero_model_gasaerexch( loffset, ncol, lchnk, troplev, delt, reaction_rates, &
                                    tfld, pmid, pdel, mbar, relhum, &
                                    zm,  qh2o, cwat, cldfr, cldnum, &
                                    airdens, invariants, del_h2so4_gasprod,  &
                                    vmr0, vmr, pbuf )

    use time_manager,          only : get_nstep
    use condtend,              only : condtend_sub
    use aerosoldef,            only: getCloudTracerName
    !-----------------------------------------------------------------------
    !      ... dummy arguments
    !-----------------------------------------------------------------------
    integer,  intent(in) :: loffset                ! offset applied to modal aero "pointers"
    integer,  intent(in) :: ncol                   ! number columns in chunk
    integer,  intent(in) :: lchnk                  ! chunk index
    integer,  intent(in) :: troplev(pcols)
    real(r8), intent(in) :: delt                   ! time step size (sec)
    real(r8), intent(in) :: reaction_rates(:,:,:)  ! reaction rates
    real(r8), intent(in) :: tfld(:,:)              ! temperature (K)
    real(r8), intent(in) :: pmid(:,:)              ! pressure at model levels (Pa)
    real(r8), intent(in) :: pdel(:,:)              ! pressure thickness of levels (Pa)
    real(r8), intent(in) :: mbar(:,:)              ! mean wet atmospheric mass ( amu )
    real(r8), intent(in) :: relhum(:,:)            ! relative humidity
    real(r8), intent(in) :: airdens(:,:)           ! total atms density (molec/cm**3)
    real(r8), intent(in) :: invariants(:,:,:)
    real(r8), intent(inout) :: del_h2so4_gasprod(:,:)  ![molec/molec/sec] 
    real(r8), intent(in) :: zm(:,:) 
    real(r8), intent(in) :: qh2o(:,:) 
    real(r8), intent(in) :: cwat(:,:)          ! cloud liquid water content (kg/kg)
    real(r8), intent(in) :: cldfr(:,:) 
    real(r8), intent(in) :: cldnum(:,:)       ! droplet number concentration (#/kg)
    real(r8), intent(in) :: vmr0(:,:,:)       ! initial mixing ratios (before gas-phase chem changes)
    real(r8), intent(inout) :: vmr(:,:,:)         ! mixing ratios ( vmr )

    type(physics_buffer_desc), pointer :: pbuf(:)
   
    ! local vars 
    
    integer :: n, m
    integer :: i,k,l
    integer :: nstep

    integer, parameter :: nmodes_aq_chem = 1

    real(r8), dimension(ncol) :: wrk
    character(len=32)         :: name
    real(r8) :: dvmrcwdt(ncol,pver,gas_pcnst)
    real(r8) :: dvmrdt(ncol,pver,gas_pcnst)
    real(r8) :: vmrcw(ncol,pver,gas_pcnst)            ! cloud-borne aerosol (vmr)

    real(r8) :: del_h2so4_aeruptk(ncol,pver)
    real(r8) :: del_h2so4_aqchem(ncol,pver)
    real(r8) :: mmr_cond_vap_start_of_timestep(pcols,pver,N_COND_VAP)
    real(r8) :: mmr_cond_vap_gasprod(pcols,pver,N_COND_VAP)
    real(r8) :: del_soa_lv_gasprod(ncol,pver)
    real(r8) :: del_soa_sv_gasprod(ncol,pver)
    real(r8) :: dvmrdt_sv1(ncol,pver,gas_pcnst)
    real(r8) :: dvmrcwdt_sv1(ncol,pver,gas_pcnst)
    real(r8) :: mmr_tend_ncols(ncol, pver, gas_pcnst)
    real(r8) :: mmr_tend_pcols(pcols, pver, gas_pcnst)

    integer  :: cond_vap_idx
    real(r8) ::  aqso4(ncol,nmodes_aq_chem)           ! aqueous phase chemistry
    real(r8) ::  aqh2so4(ncol,nmodes_aq_chem)         ! aqueous phase chemistry
    real(r8) ::  aqso4_h2o2(ncol)                     ! SO4 aqueous phase chemistry due to H2O2
    real(r8) ::  aqso4_o3(ncol)                       ! SO4 aqueous phase chemistry due to O3
    real(r8) ::  xphlwc(ncol,pver)                    ! pH value multiplied by lwc
    real(r8) ::  delt_inverse                         ! 1 / timestep

    real(r8), pointer :: pblh(:)

    logical :: is_spcam_m2005
   
    nstep = get_nstep()


    is_spcam_m2005   = cam_physpkg_is('spcam_m2005')

    delt_inverse = 1.0_r8 / delt

    !Get height of boundary layer (needed for boundary layer nucleation)
    call pbuf_get_field(pbuf, pblh_idx,     pblh)

    ! calculate tendency due to gas phase chemistry and processes
    dvmrdt(:ncol,:,:) = (vmr(:ncol,:,:) - vmr0(:ncol,:,:)) / delt
    do m = 1, gas_pcnst
      wrk(:) = 0._r8
      do k = 1,pver
        wrk(:ncol) = wrk(:ncol) + dvmrdt(:ncol,k,m)*adv_mass(m)/mbar(:ncol,k)*pdel(:ncol,k)/gravit
      end do
      name = 'GS_'//trim(solsym(m))
      call outfld( name, wrk(:ncol), ncol, lchnk )
    enddo

!  Get mass mixing ratios at start of time step
   call vmr2mmr( vmr0, mmr_tend_ncols, mbar, ncol )
   mmr_cond_vap_start_of_timestep(:ncol,:,COND_VAP_H2SO4) = mmr_tend_ncols(1:ncol,:,ndx_h2so4)
   mmr_cond_vap_start_of_timestep(:ncol,:,COND_VAP_ORG_LV) = mmr_tend_ncols(1:ncol,:,ndx_soa_lv)
   mmr_cond_vap_start_of_timestep(:ncol,:,COND_VAP_ORG_SV) = mmr_tend_ncols(1:ncol,:,ndx_soa_sv)
!
! Aerosol processes ...
!
    call qqcw2vmr( lchnk, vmrcw, mbar, ncol, loffset, pbuf )

    ! save h2so4 change by gas phase chem (for later new particle nucleation)
    if (ndx_h2so4 > 0) then
       del_h2so4_gasprod(1:ncol,:) = vmr(1:ncol,:,ndx_h2so4) - vmr0(1:ncol,:,ndx_h2so4)
    endif

    del_soa_lv_gasprod(1:ncol,:) = vmr(1:ncol,:,ndx_soa_lv) - vmr0(1:ncol,:,ndx_soa_lv)
    del_soa_sv_gasprod(1:ncol,:) = vmr(1:ncol,:,ndx_soa_sv) - vmr0(1:ncol,:,ndx_soa_sv)

    if (.not. is_spcam_m2005) then  ! regular CAM
      dvmrdt(:ncol,:,:) = vmr(:ncol,:,:)
      dvmrcwdt(:ncol,:,:) = vmrcw(:ncol,:,:)

      !Save intermediate concentrations
      dvmrdt_sv1 = vmr
      dvmrcwdt_sv1 = vmrcw

    ! aqueous chemistry ...

    call setsox(   &
        ncol,     &
        lchnk,    &
        loffset,  &
        delt,     &
        pmid,     &
        pdel,     &
        tfld,     &
        mbar,     &
        cwat,     &
        cldfr,    &
        cldnum,   &
        airdens,  &
        invariants, &
        vmrcw,    &
        vmr,      &
        xphlwc,   &
        aqso4,    &
        aqh2so4,  &
        aqso4_h2o2, &
        aqso4_o3  &
        )

      call outfld( 'AQSO4_H2O2', aqso4_h2o2(:ncol), ncol, lchnk)
      call outfld( 'AQSO4_O3',   aqso4_o3(:ncol),   ncol, lchnk)
      call outfld( 'XPH_LWC',    xphlwc(:ncol,:),   ncol, lchnk )


    ! vmr tendency from aqchem and soa routines
    dvmrdt_sv1 = (vmr - dvmrdt_sv1)/delt
    dvmrcwdt_sv1 = (vmrcw - dvmrcwdt_sv1)/delt

    if(ndx_h2so4 .gt. 0)then
       del_h2so4_aqchem(:ncol,:) = dvmrdt_sv1(:ncol,:,ndx_h2so4)*delt !"production rate" of H2SO4
    else
       del_h2so4_aqchem(:ncol,:) = 0.0_r8
    end if

    do m = 1,gas_pcnst
       wrk(:ncol) = 0._r8
       do k = 1,pver
          wrk(:ncol) = wrk(:ncol) + dvmrdt_sv1(:ncol,k,m)*adv_mass(m)/mbar(:ncol,k)*pdel(:ncol,k)/gravit
       end do
       name = 'AQ_'//trim(solsym(m))
       call outfld( name, wrk(:ncol), ncol, lchnk )

       !In oslo aero also write out the tendencies for the 
       !cloud borne aerosols... 
       n = physicsIndex(m) 
       if (n.le.pcnst) then
       if(getCloudTracerIndexDirect(n) .gt. 0)then
          name = 'AQ_'//trim(getCloudTracerName(n))
          wrk(:ncol)=0.0_r8
          do k=1,pver
            wrk(:ncol) = wrk(:ncol) + dvmrcwdt_sv1(:ncol,k,m)*adv_mass(m)/mbar(:ncol,k)*pdel(:ncol,k)/gravit  
          end do
          call outfld( name, wrk(:ncol), ncol, lchnk )
       end if
       end if
    enddo

   else if (is_spcam_m2005) then  ! SPCAM ECPP
! when ECPP is used, aqueous chemistry is done in ECPP,
! and not updated here.
! Minghuai Wang, 2010-02 (Minghuai.Wang@pnl.gov)
!
      dvmrdt = 0.0_r8
      dvmrcwdt = 0.0_r8
   endif

   !condensation
   call vmr2mmr( vmr, mmr_tend_ncols, mbar, ncol )
   do k = 1,pver
      mmr_cond_vap_gasprod(:ncol,k,COND_VAP_H2SO4) = adv_mass(ndx_h2so4) * (del_h2so4_gasprod(:ncol,k)+del_h2so4_aqchem(:ncol,k)) / mbar(:ncol,k)/delt
      mmr_cond_vap_gasprod(:ncol,k,COND_VAP_ORG_LV) = adv_mass(ndx_soa_lv) * del_soa_lv_gasprod(:ncol,k) / mbar(:ncol,k)/delt !cka
      mmr_cond_vap_gasprod(:ncol,k,COND_VAP_ORG_SV) = adv_mass(ndx_soa_sv) * del_soa_sv_gasprod(:ncol,k) / mbar(:ncol,k)/delt !cka
   end do

   !This should not happen since there are only 
   !production terms for these gases!!
   do cond_vap_idx=1,N_COND_VAP
      where(mmr_cond_vap_gasprod(:ncol,:,cond_vap_idx).lt. 0.0_r8)
         mmr_cond_vap_gasprod(:ncol,:,cond_vap_idx) = 0.0_r8
      end where
   end do
   mmr_tend_ncols(:ncol,:,ndx_h2so4) = mmr_cond_vap_start_of_timestep(:ncol,:,COND_VAP_H2SO4)
   mmr_tend_ncols(:ncol,:,ndx_soa_lv) = mmr_cond_vap_start_of_timestep(:ncol,:,COND_VAP_ORG_LV)
   mmr_tend_ncols(:ncol,:,ndx_soa_sv) = mmr_cond_vap_start_of_timestep(:ncol,:,COND_VAP_ORG_SV)

   !Rest of microphysics have pcols dimension 
   mmr_tend_pcols(:ncol,:,:) = mmr_tend_ncols(:ncol,:,:) 
   !Note use of "zm" here. In CAM5.3-implementation "zi" was used.. 
   !zm is passed through the generic interface, and it should not change much
   !to check if "zm" is below boundary layer height instead of zi
   call condtend_sub( lchnk, mmr_tend_pcols, mmr_cond_vap_gasprod,tfld, pmid, &
                     pdel, delt, ncol, pblh, zm, qh2o)  !cka


   !coagulation
   ! OS 280415  Concentratiions in cloud water is in vmr space and as a
   ! temporary variable  (vmrcw) Coagulation between aerosol and cloud
   ! droplets moved to after vmrcw is moved into qqcw (in mmr spac)

   call coagtend( mmr_tend_pcols, pmid, pdel, tfld, delt_inverse, ncol, lchnk) 

   !Convert cloud water to mmr again ==> values in buffer
   call vmr2qqcw( lchnk, vmrcw, mbar, ncol, loffset, pbuf )

   !Call cloud coagulation routines (all in mass mixing ratios)
   call clcoag( mmr_tend_pcols, pmid, pdel, tfld, cldnum ,cldfr, delt_inverse, ncol, lchnk,loffset,pbuf)

   !Make sure mmr==> vmr is done correctly
   mmr_tend_ncols(:ncol,:,:) = mmr_tend_pcols(:ncol,:,:)

   !Go back to volume mixing ratio for chemistry
   call mmr2vmr( mmr_tend_ncols, vmr, mbar, ncol )

    return

  end subroutine aero_model_gasaerexch

  !=============================================================================
  !=============================================================================
  subroutine aero_model_emissions( state, cam_in )
    use seasalt_model, only: oslo_salt_emis_intr, seasalt_active, OMOceanSource
    use dust_model, only: oslo_dust_emis_intr, dust_active
    use oslo_ocean_intr, only: oslo_dms_emis_intr
    use aerosoldef, only: l_om_ni
    use physics_types, only: physics_state

    ! Arguments:

    type(physics_state),    intent(in)    :: state   ! Physics state variables
    type(cam_in_t),         intent(inout) :: cam_in  ! import state

    ! local vars

    integer :: lchnk, ncol
    real(r8) :: sflx(pcols)   ! accumulate over all bins for output
    real (r8), parameter :: z0=0.0001_r8  ! m roughness length over oceans--from ocean model

    lchnk = state%lchnk
    ncol = state%ncol

    if (dust_active) then

       call oslo_dust_emis_intr( state, cam_in)

       ! some dust emis diagnostics ...
    endif

    if (seasalt_active) then

       call oslo_salt_emis_intr(state, cam_in)

    endif

    !Add whatever OM ocean source was calculated in the seasalt module
    cam_in%cflx(:ncol,l_om_ni) = cam_in%cflx(:ncol,l_om_ni) + OMOceanSource(:ncol)

    !Pick up correct DMS emissions (replace values from file if requested)
    call oslo_dms_emis_intr(state, cam_in)

  end subroutine aero_model_emissions

  !===============================================================================
  ! private methods


  !=============================================================================
  !=============================================================================
  subroutine surf_area_dens( ncol, mmr, pmid, temp, diam, beglev, endlev, sad, sfc )
    use mo_constants,    only : pi

    ! dummy args
    integer,  intent(in)  :: ncol
    real(r8), intent(in)  :: mmr(:,:,:)
    real(r8), intent(in)  :: pmid(:,:)
    real(r8), intent(in)  :: temp(:,:)
    real(r8), intent(in)  :: diam(:,:,:)
    integer,  intent(in)  :: beglev(:)
    integer,  intent(in)  :: endlev(:)
    real(r8), intent(out) :: sad(:,:)
    real(r8),optional, intent(out) :: sfc(:,:,:)

    ! local vars

    !
    ! Compute surface aero for each mode.
    ! Total over all modes as the surface area for chemical reactions.
    !

    !oslo: do nothing for now
    return

  end subroutine surf_area_dens

  !===============================================================================
  !===============================================================================
  subroutine modal_aero_bcscavcoef_init
    !-----------------------------------------------------------------------
    !
    ! Purpose:
    ! Computes lookup table for aerosol impaction/interception scavenging rates
    !
    ! Authors: R. Easter
    !
    !-----------------------------------------------------------------------
    
    use shr_kind_mod,    only: r8 => shr_kind_r8
    use modal_aero_data
    use cam_abortutils,  only: endrun

    implicit none

    ! oslo : do nothing for now
    return
  end subroutine modal_aero_bcscavcoef_init

  !===============================================================================
  !===============================================================================
  subroutine modal_aero_depvel_part( ncol, t, pmid, ram1, fv, vlc_dry, vlc_trb, vlc_grv,  &
                                     radius_part, density_part, sig_part, moment, lchnk )

!    calculates surface deposition velocity of particles
!    L. Zhang, S. Gong, J. Padro, and L. Barrie
!    A size-seggregated particle dry deposition scheme for an atmospheric aerosol module
!    Atmospheric Environment, 35, 549-560, 2001.
!
!    Authors: X. Liu

    !
    ! !USES
    !
    use physconst,     only: pi,boltz, gravit, rair
    use mo_drydep,     only: n_land_type, fraction_landuse

    ! !ARGUMENTS:
    !
    implicit none
    !
    real(r8), intent(in) :: t(pcols,pver)       !atm temperature (K)
    real(r8), intent(in) :: pmid(pcols,pver)    !atm pressure (Pa)
    real(r8), intent(in) :: fv(pcols)           !friction velocity (m/s)
    real(r8), intent(in) :: ram1(pcols)         !aerodynamical resistance (s/m)
    real(r8), intent(in) :: radius_part(pcols,pver)    ! mean (volume/number) particle radius (m)
    real(r8), intent(in) :: density_part(pcols,pver)   ! density of particle material (kg/m3)
    real(r8), intent(in) :: sig_part(pcols,pver)       ! geometric standard deviation of particles
    integer,  intent(in) :: moment ! moment of size distribution (0 for number, 2 for surface area, 3 for volume)
    integer,  intent(in) :: ncol
    integer,  intent(in) :: lchnk

    real(r8), intent(out) :: vlc_trb(pcols)       !Turbulent deposn velocity (m/s)
    real(r8), intent(out) :: vlc_grv(pcols,pver)       !grav deposn velocity (m/s)
    real(r8), intent(out) :: vlc_dry(pcols,pver)       !dry deposn velocity (m/s)
    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    ! Local Variables
    integer  :: m,i,k,ix                !indices
    real(r8) :: rho     !atm density (kg/m**3)
    real(r8) :: vsc_dyn_atm(pcols,pver)   ![kg m-1 s-1] Dynamic viscosity of air
    real(r8) :: vsc_knm_atm(pcols,pver)   ![m2 s-1] Kinematic viscosity of atmosphere
    real(r8) :: shm_nbr       ![frc] Schmidt number
    real(r8) :: stk_nbr       ![frc] Stokes number
    real(r8) :: mfp_atm(pcols,pver)       ![m] Mean free path of air
    real(r8) :: dff_aer       ![m2 s-1] Brownian diffusivity of particle
    real(r8) :: slp_crc(pcols,pver) ![frc] Slip correction factor
    real(r8) :: rss_trb       ![s m-1] Resistance to turbulent deposition
    real(r8) :: rss_lmn       ![s m-1] Quasi-laminar layer resistance
    real(r8) :: brownian      ! collection efficiency for Browning diffusion
    real(r8) :: impaction     ! collection efficiency for impaction
    real(r8) :: interception  ! collection efficiency for interception
    real(r8) :: stickfrac     ! fraction of particles sticking to surface
    real(r8) :: radius_moment(pcols,pver) ! median radius (m) for moment
    real(r8) :: lnsig         ! ln(sig_part)
    real(r8) :: dispersion    ! accounts for influence of size dist dispersion on bulk settling velocity
                              ! assuming radius_part is number mode radius * exp(1.5 ln(sigma))

    integer  :: lt
    real(r8) :: lnd_frc
    real(r8) :: wrk1, wrk2, wrk3

    ! constants
    real(r8) gamma(11)      ! exponent of schmidt number
!   data gamma/0.54d+00,  0.56d+00,  0.57d+00,  0.54d+00,  0.54d+00, &
!              0.56d+00,  0.54d+00,  0.54d+00,  0.54d+00,  0.56d+00, &
!              0.50d+00/
    data gamma/0.56e+00_r8,  0.54e+00_r8,  0.54e+00_r8,  0.56e+00_r8,  0.56e+00_r8, &        
               0.56e+00_r8,  0.50e+00_r8,  0.54e+00_r8,  0.54e+00_r8,  0.54e+00_r8, &
               0.54e+00_r8/
    save gamma

    real(r8) alpha(11)      ! parameter for impaction
!   data alpha/50.00d+00,  0.95d+00,  0.80d+00,  1.20d+00,  1.30d+00, &
!               0.80d+00, 50.00d+00, 50.00d+00,  2.00d+00,  1.50d+00, &
!             100.00d+00/
    data alpha/1.50e+00_r8,   1.20e+00_r8,  1.20e+00_r8,  0.80e+00_r8,  1.00e+00_r8, &
               0.80e+00_r8, 100.00e+00_r8, 50.00e+00_r8,  2.00e+00_r8,  1.20e+00_r8, &
              50.00e+00_r8/
    save alpha

    real(r8) radius_collector(11) ! radius (m) of surface collectors
!   data radius_collector/-1.00d+00,  5.10d-03,  3.50d-03,  3.20d-03, 10.00d-03, &
!                          5.00d-03, -1.00d+00, -1.00d+00, 10.00d-03, 10.00d-03, &
!                         -1.00d+00/
    data radius_collector/10.00e-03_r8,  3.50e-03_r8,  3.50e-03_r8,  5.10e-03_r8,  2.00e-03_r8, &
                           5.00e-03_r8, -1.00e+00_r8, -1.00e+00_r8, 10.00e-03_r8,  3.50e-03_r8, &
                          -1.00e+00_r8/
    save radius_collector

    integer            :: iwet(11) ! flag for wet surface = 1, otherwise = -1
!   data iwet/1,   -1,   -1,   -1,   -1,  &
!            -1,   -1,   -1,    1,   -1,  &
!             1/
    data iwet/-1,  -1,   -1,   -1,   -1,  &
              -1,   1,   -1,    1,   -1,  &
              -1/
    save iwet


    !------------------------------------------------------------------------
    do k=1,pver
       do i=1,ncol

          lnsig = log(sig_part(i,k))
! use a maximum radius of 50 microns when calculating deposition velocity
          radius_moment(i,k) = min(50.0e-6_r8,radius_part(i,k))*   &
                          exp((float(moment)-1.5_r8)*lnsig*lnsig)
          dispersion = exp(2._r8*lnsig*lnsig)

          rho=pmid(i,k)/rair/t(i,k)

          ! Quasi-laminar layer resistance: call rss_lmn_get
          ! Size-independent thermokinetic properties
          vsc_dyn_atm(i,k) = 1.72e-5_r8 * ((t(i,k)/273.0_r8)**1.5_r8) * 393.0_r8 / &
               (t(i,k)+120.0_r8)      ![kg m-1 s-1] RoY94 p. 102
          mfp_atm(i,k) = 2.0_r8 * vsc_dyn_atm(i,k) / &   ![m] SeP97 p. 455
               (pmid(i,k)*sqrt(8.0_r8/(pi*rair*t(i,k))))
          vsc_knm_atm(i,k) = vsc_dyn_atm(i,k) / rho ![m2 s-1] Kinematic viscosity of air

          slp_crc(i,k) = 1.0_r8 + mfp_atm(i,k) * &
                  (1.257_r8+0.4_r8*exp(-1.1_r8*radius_moment(i,k)/(mfp_atm(i,k)))) / &
                  radius_moment(i,k)   ![frc] Slip correction factor SeP97 p. 464
          vlc_grv(i,k) = (4.0_r8/18.0_r8) * radius_moment(i,k)*radius_moment(i,k)*density_part(i,k)* &
                  gravit*slp_crc(i,k) / vsc_dyn_atm(i,k) ![m s-1] Stokes' settling velocity SeP97 p. 466
          vlc_grv(i,k) = vlc_grv(i,k) * dispersion

          vlc_dry(i,k)=vlc_grv(i,k)
       enddo
    enddo
    k=pver  ! only look at bottom level for next part
    do i=1,ncol
       dff_aer = boltz * t(i,k) * slp_crc(i,k) / &    ![m2 s-1]
                 (6.0_r8*pi*vsc_dyn_atm(i,k)*radius_moment(i,k)) !SeP97 p.474
       shm_nbr = vsc_knm_atm(i,k) / dff_aer                        ![frc] SeP97 p.972

       wrk2 = 0._r8
       wrk3 = 0._r8
       do lt = 1,n_land_type
          lnd_frc = fraction_landuse(i,lt,lchnk)
          if ( lnd_frc /= 0._r8 ) then
             brownian = shm_nbr**(-gamma(lt))
             if (radius_collector(lt) > 0.0_r8) then
!       vegetated surface
                stk_nbr = vlc_grv(i,k) * fv(i) / (gravit*radius_collector(lt))
                interception = 2.0_r8*(radius_moment(i,k)/radius_collector(lt))**2.0_r8
             else
!       non-vegetated surface
                stk_nbr = vlc_grv(i,k) * fv(i) * fv(i) / (gravit*vsc_knm_atm(i,k))  ![frc] SeP97 p.965
                interception = 0.0_r8
             endif
             impaction = (stk_nbr/(alpha(lt)+stk_nbr))**2.0_r8   

             if (iwet(lt) > 0) then
                stickfrac = 1.0_r8
             else
                stickfrac = exp(-sqrt(stk_nbr))
                if (stickfrac < 1.0e-10_r8) stickfrac = 1.0e-10_r8
             endif
             rss_lmn = 1.0_r8 / (3.0_r8 * fv(i) * stickfrac * (brownian+interception+impaction))
             rss_trb = ram1(i) + rss_lmn + ram1(i)*rss_lmn*vlc_grv(i,k)

             wrk1 = 1.0_r8 / rss_trb
             wrk2 = wrk2 + lnd_frc*( wrk1 )
             wrk3 = wrk3 + lnd_frc*( wrk1 + vlc_grv(i,k) )
          endif
       enddo  ! n_land_type
       vlc_trb(i) = wrk2
       vlc_dry(i,k) = wrk3
    enddo !ncol

    return
  end subroutine modal_aero_depvel_part

  !===============================================================================
  subroutine modal_aero_bcscavcoef_get( m, ncol, isprx, dgn_awet, scavcoefnum, scavcoefvol )

    use modal_aero_data
    !-----------------------------------------------------------------------
    implicit none

    integer,intent(in) :: m, ncol
    logical,intent(in):: isprx(pcols,pver)
    real(r8), intent(in) :: dgn_awet(pcols,pver,ntot_amode)
    real(r8), intent(out) :: scavcoefnum(pcols,pver), scavcoefvol(pcols,pver)

    integer i, k, jgrow

    return
  end subroutine modal_aero_bcscavcoef_get

  !=============================================================================
  !=============================================================================
  subroutine qqcw2vmr(lchnk, vmr, mbar, ncol, im, pbuf)
    use modal_aero_data, only : qqcw_get_field
    use physics_buffer, only : physics_buffer_desc
    !-----------------------------------------------------------------
    !	... Xfrom from mass to volume mixing ratio
    !-----------------------------------------------------------------

    use chem_mods, only : adv_mass, gas_pcnst

    implicit none

    !-----------------------------------------------------------------
    !	... Dummy args
    !-----------------------------------------------------------------
    integer, intent(in)     :: lchnk, ncol, im
    real(r8), intent(in)    :: mbar(ncol,pver)
    real(r8), intent(inout) :: vmr(ncol,pver,gas_pcnst)
    type(physics_buffer_desc), pointer :: pbuf(:)

    !-----------------------------------------------------------------
    !	... Local variables
    !-----------------------------------------------------------------
    integer :: k, m
    real(r8), pointer :: fldcw(:,:)

    do m=1,gas_pcnst
       if( adv_mass(m) /= 0._r8 ) then
          fldcw => qqcw_get_field(pbuf, m+im,lchnk,errorhandle=.true.)
          if(associated(fldcw)) then
             do k=1,pver
                vmr(:ncol,k,m) = mbar(:ncol,k) * fldcw(:ncol,k) / adv_mass(m)
             end do
          else
             vmr(:,:,m) = 0.0_r8
          end if
       end if
    end do
  end subroutine qqcw2vmr


  !=============================================================================
  !=============================================================================
  subroutine vmr2qqcw( lchnk, vmr, mbar, ncol, im, pbuf )
    !-----------------------------------------------------------------
    !	... Xfrom from volume to mass mixing ratio
    !-----------------------------------------------------------------

    use m_spc_id
    use chem_mods,       only : adv_mass, gas_pcnst
    use modal_aero_data, only : qqcw_get_field
    use physics_buffer,  only : physics_buffer_desc

    implicit none

    !-----------------------------------------------------------------
    !	... Dummy args
    !-----------------------------------------------------------------
    integer, intent(in)     :: lchnk, ncol, im
    real(r8), intent(in)    :: mbar(ncol,pver)
    real(r8), intent(in)    :: vmr(ncol,pver,gas_pcnst)
    type(physics_buffer_desc), pointer :: pbuf(:)

    !-----------------------------------------------------------------
    !	... Local variables
    !-----------------------------------------------------------------
    integer :: k, m
    real(r8), pointer :: fldcw(:,:)
    !-----------------------------------------------------------------
    !	... The non-group species
    !-----------------------------------------------------------------
    do m = 1,gas_pcnst
       fldcw => qqcw_get_field(pbuf, m+im,lchnk,errorhandle=.true.)
       if( adv_mass(m) /= 0._r8 .and. associated(fldcw)) then
          do k = 1,pver
             fldcw(:ncol,k) = adv_mass(m) * vmr(:ncol,k,m) / mbar(:ncol,k)
          end do
       end if
    end do

  end subroutine vmr2qqcw

end module aero_model
