module oslo_aerosols_intr

  use aerosoldef
  use commondefinitions
  use modal_aero_data, only: qqcw_get_field
  use shr_kind_mod,   only: r8 => shr_kind_r8
  use constituents,   only: pcnst, cnst_name, cnst_get_ind
  use ppgrid,         only: pcols, pver, pverp
  use phys_control,   only: phys_getopts
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

  use ref_pres,       only: top_lev => clim_modal_aero_top_lev

  use modal_aero_wateruptake, only: modal_strat_sulfate
  use mo_setsox,              only: setsox, has_sox

  implicit none

  private          ! Make default type private to the module

  save

  !
  ! Public interfaces
  !

  public :: oslo_aero_wet_intr                             ! interface to wet deposition
  public :: sol_facti_cloud_borne
  public :: oslo_aero_dry_intr                             ! interface to dry deposition
  public :: oslo_aero_initialize

  logical :: inv_o3, inv_oh, inv_no3, inv_ho2
  integer, pointer :: id_so2, id_so4, id_dms, id_o3, id_h2o2, id_oh, id_no3, id_ho2
  integer, target  :: spc_ids(8)

  integer :: fracis_idx          = 0
  integer :: prain_idx           = 0
  integer :: rprddp_idx          = 0 
  integer :: rprdsh_idx          = 0 
  integer :: nevapr_shcu_idx     = 0 
  integer :: nevapr_dpcu_idx     = 0 
  real(r8) :: sol_facti_cloud_borne

! variables for table lookup of aerosol impaction/interception scavenging rates
 integer, parameter :: nimptblgrow_mind=-7, nimptblgrow_maxd=12
 real(r8) dlndg_nimptblgrow
 real(r8) scavimptblnum(nimptblgrow_mind:nimptblgrow_maxd, nmodes)
 real(r8) scavimptblvol(nimptblgrow_mind:nimptblgrow_maxd, nmodes)
  
  
  integer :: ndrydep = 0
  integer,allocatable :: drydep_indices(:)
  integer :: nwetdep = 0
  integer,allocatable :: wetdep_indices(:)
  logical :: drydep_lq(pcnst)
  logical :: wetdep_lq(pcnst)

  logical :: convproc_do_aer = .FALSE.

contains

  !===============================================================================
  subroutine oslo_aero_initialize(pbuf2d )
    use cam_history,      only : addfld, add_default, horiz_only
    use mo_chem_utls,     only : get_inv_ndx
    use gas_wetdep_opts,  only : gas_wetdep_list, gas_wetdep_cnt
    use physics_buffer,        only: physics_buffer_desc, pbuf_get_chunk
    use ppgrid,                only: pcols, pver, begchunk, endchunk
    use time_manager,          only: is_first_step
    use modal_aero_data,       only: qqcw_get_field

    implicit none

    type(physics_buffer_desc), pointer :: pbuf2d(:,:)
    integer :: m                                
    integer :: l

    integer :: i
    integer :: lchnk
    integer :: tracerIndex
    integer :: astat, id
    real(r8), pointer :: qqcw(:,:)

    logical :: history_aerosol      ! Output the MAM aerosol tendencies
    character(len=2)  :: unit_basename='kg'  ! Units 'kg' or '1' 
    character(len=100) :: aName         ! tracer name
    logical            :: is_in_output(pcnst)
    !-----------------------------------------------------------------------

    fracis_idx      = pbuf_get_index('FRACIS') 
    prain_idx       = pbuf_get_index('PRAIN')  
    rprddp_idx      = pbuf_get_index('RPRDDP')  
    rprdsh_idx      = pbuf_get_index('RPRDSH')  
    nevapr_shcu_idx = pbuf_get_index('NEVAPR_SHCU') 

    call phys_getopts( history_aerosol_out        = history_aerosol   )

    is_in_output(:)=.false.
    drydep_lq(:)=.false.
    wetdep_lq(:)=.false.

    !Mode 0 is not subject to wet deposition? (check noresm1 code..)
    do m=0,nmodes
       do l=1,getNumberOfTracersInMode(m)

          tracerIndex = getTracerIndex(m,l,.false.)

          drydep_lq(tracerIndex)=.true.
          wetdep_lq(tracerIndex)=.true.

          if(is_in_output(tracerIndex))then
             cycle
          endif

          aName = cnst_name(tracerIndex)

          print*, m,l,tracerIndex, trim(aName)

          call addfld (trim(aName)//'SFWET',horiz_only, 'A', unit_basename//'/m2/s',  &
              'Wet deposition flux at surface')
          call addfld (trim(aName)//'SFSIC',horiz_only, 'A', unit_basename//'/m2/s ', &
              'Wet deposition flux (incloud, convective) at surface')
          call addfld (trim(aName)//'SFSIS',horiz_only, 'A', unit_basename//'/m2/s ', &
              'Wet deposition flux (incloud, stratiform) at surface')
          call addfld (trim(aName)//'SFSBC',horiz_only, 'A', unit_basename//'/m2/s ', &
              'Wet deposition flux (belowcloud, convective) at surface')
          call addfld (trim(aName)//'SFSBS',horiz_only, 'A', unit_basename//'/m2/s ', &
              'Wet deposition flux (belowcloud, stratiform) at surface')
          call addfld (trim(aName)//'WET',(/'lev'/), 'A', unit_basename//'/kg/s ','wet deposition tendency')
          call addfld (trim(aName)//'SIC',(/'lev'/), 'A', unit_basename//'/kg/s ', &
               trim(aName)//' ic wet deposition')
          call addfld (trim(aName)//'SIS',(/'lev'/), 'A', unit_basename//'/kg/s ', &
               trim(aName)//' is wet deposition')
          call addfld (trim(aName)//'SBC',(/'lev'/), 'A', unit_basename//'/kg/s ', &
               trim(aName)//' bc wet deposition')
          call addfld (trim(aName)//'SBS',(/'lev'/), 'A', unit_basename//'/kg/s ', &
               trim(aName)//' bs wet deposition')
       
          !Extra wd ouptut
          if ( history_aerosol ) then          
             call add_default (trim(aName)//'SFWET', 1, ' ')
             call add_default (trim(aName)//'SFSIC', 1, ' ')
             call add_default (trim(aName)//'SFSIS', 1, ' ')
             call add_default (trim(aName)//'SFSBC', 1, ' ')
             call add_default (trim(aName)//'SFSBS', 1, ' ')
          endif

          !ddep output
          call addfld (trim(aName)//'DDF',horiz_only, 'A', unit_basename//'/m2/s ', &
               trim(aName)//' dry deposition flux at bottom (grav + turb)')
          call addfld (trim(aName)//'TBF',horiz_only, 'A' ,unit_basename//'/m2/s', &
               trim(aName)//' turbulent dry deposition flux')
          call addfld (trim(aName)//'GVF',horiz_only, 'A', unit_basename//'/m2/s ', &
               trim(aName)//' gravitational dry deposition flux')
          call addfld (trim(aName)//'DTQ',(/'lev'/), 'A', unit_basename//'/kg/s ', &
               trim(aName)//' dry deposition')
          call addfld (trim(aName)//'DDV',(/'lev'/), 'A', 'm/s', &
               trim(aName)//' deposition velocity')

          !extra drydep output
          if ( history_aerosol ) then 
             call add_default (trim(aName)//'DDF', 1, ' ')
             call add_default (trim(aName)//'TBF', 1, ' ')
             call add_default (trim(aName)//'GVF', 1, ' ')
             !call add_default (trim(aName)//'DDV', 1, ' ')
          endif

          !some tracers are not in cloud water
          if(getCloudTracerIndexDirect(tracerIndex) .lt. 0)then
             cycle
          endif

          aName = trim(getCloudTracerName(tracerIndex))
          !Cloud water fields (from mo_chm_diags.F90)
          call addfld (trim(aName)//'SFWET', horiz_only, 'A', unit_basename//'/m2/s', &
              trim(aName)//' wet deposition flux at surface')
          call addfld (trim(aName)//'SFSIC', horiz_only, 'A',unit_basename//'/m2/s ', &
              trim(aName)//' wet deposition flux (incloud, convective) at surface')
          call addfld (trim(aName)//'SFSIS', horiz_only, 'A', unit_basename//'/m2/s ', &
               trim(aName)//' wet deposition flux (incloud, stratiform) at surface')
          call addfld (trim(aName)//'SFSBC', horiz_only, 'A', unit_basename//'/m2/s ' , &
               trim(aName)//' wet deposition flux (belowcloud, convective) at surface')
          call addfld (trim(aName)//'SFSBS', horiz_only, 'A', unit_basename//'/m2/s ' , &
               trim(aName)//' wet deposition flux (belowcloud, stratiform) at surface')
          !dry deposition
          call addfld (trim(aName)//'DDF',   horiz_only, 'A', unit_basename//'/m2/s ',  &
               trim(aName)//' dry deposition flux at bottom (grav + turb)')
          call addfld (trim(aName)//'TBF',   horiz_only, 'A', unit_basename//'/m2/s ',  &
               trim(aName)//' turbulent dry deposition flux')
          call addfld (trim(aName)//'GVF',   horiz_only, 'A', unit_basename//'/m2/s ',  &
               trim(aName)//' gravitational dry deposition flux')    

          is_in_output(tracerIndex) = .true.

      end do !tracers
    enddo    !modes

    !initialize cloud concentrations

    if (is_first_step()) then
       ! initialize cloud bourne constituents in physics buffer

       do i = 1, pcnst
          do lchnk = begchunk, endchunk
             qqcw => qqcw_get_field(pbuf_get_chunk(pbuf2d,lchnk), i, lchnk, .true.)
             if (associated(qqcw)) then
                qqcw = 1.e-38_r8
             end if
          end do
       end do
    end if

  end subroutine oslo_aero_initialize

  subroutine oslo_aero_dry_intr  ( state, pbuf, obklen, ustar, cam_in, dt, cam_out, ptend &
                                   , dgncur_awet, wetdens, dgncur_awet_processmode   &
                                   , wetdens_processmode, cflx &
                                  )

  !===============================================================================
    use cam_history,       only: outfld
    use ppgrid,            only: pverp
    use physics_types,     only: physics_state, physics_ptend
    use camsrfexch,        only: cam_out_t
    use physconst,         only: gravit, rair, rhoh2o
    use drydep_mod,        only: setdvel,  d3ddflux, calcram
    use dust_sediment_mod, only: dust_sediment_tend, dust_sediment_vel
    use modal_aero_deposition, only: set_srf_drydep
    use physics_buffer, only : physics_buffer_desc

    !-----------------------------------------------------------------------
    implicit none
    !-----------------------------------------------------------------------
    !
    ! Arguments:
    type(physics_state),    intent(in)    :: state     ! Physics state variables
    real(r8),               intent(in)    :: obklen(:)          
    real(r8),               intent(in)    :: ustar(:)  ! sfc fric vel
    type(cam_in_t), target, intent(in)    :: cam_in    ! import state
    real(r8),               intent(in)    :: dt             ! time step
    type(cam_out_t),        intent(inout) :: cam_out   ! export state
    type(physics_ptend),    intent(out)   :: ptend     ! indivdual parameterization tendencies
    type(physics_buffer_desc),    pointer :: pbuf(:)
    !
    real(r8), intent(in) :: dgncur_awet(pcols,pver,0:nmodes)
    real(r8), intent(in) :: wetdens(pcols,pver,0:nmodes)
    real(r8), intent(in) :: dgncur_awet_processmode(pcols, pver, numberOfProcessModeTracers)
    real(r8), intent(in) :: wetdens_processmode(pcols, pver, numberOfProcessModeTracers)
    real(r8), intent(in) :: cflx(pcols,pcnst) !Surface fluxes

  ! local vars
    real(r8), pointer :: landfrac(:) ! land fraction
    real(r8), pointer :: icefrac(:)  ! ice fraction
    real(r8), pointer :: ocnfrac(:)  ! ocean fraction
    real(r8), pointer :: fvin(:)     !
    real(r8), pointer :: ram1in(:)   ! for dry dep velocities from land model for progseasalts

    real(r8) :: fv(pcols)            ! for dry dep velocities, from land modified over ocean & ice
    real(r8) :: ram1(pcols)          ! for dry dep velocities, from land modified over ocean & ice

    integer :: lchnk                   ! chunk identifier
    integer :: ncol                    ! number of atmospheric columns
    integer :: jvlc                    ! index for last dimension of vlc_xxx arrays
    integer :: lphase                  ! index for interstitial / cloudborne aerosol
    integer :: lspec                   ! index for aerosol number / chem-mass / water-mass
    integer :: m                       ! aerosol mode index
    integer :: mm                      ! tracer index
    integer :: i

    real(r8) :: tvs(pcols,pver)
    real(r8) :: rho(pcols,pver)                    ! air density in kg/m3
    real(r8) :: sflx(pcols)            ! deposition flux
    real(r8)::  dep_trb(pcols)       !kg/m2/s
    real(r8)::  dep_grv(pcols)       !kg/m2/s (total of grav and trb)
    real(r8) :: pvmzaer(pcols,pverp)    ! sedimentation velocity in Pa
    real(r8) :: dqdt_tmp(pcols,pver)   ! temporary array to hold tendency for 1 species

    real(r8) :: rad_drop(pcols,pver)
    real(r8) :: dens_drop(pcols,pver)
    real(r8) :: sg_drop(pcols,pver)
    real(r8) :: rad_aer(pcols,pver)
    real(r8) :: dens_aer(pcols,pver)
    real(r8) :: sg_aer(pcols,pver)

    real(r8) :: vlc_dry(pcols,pver,4)       ! dep velocity
    real(r8) :: vlc_grv(pcols,pver,4)       ! dep velocity
    real(r8)::  vlc_trb(pcols,4)            ! dep velocity
    real(r8) :: aerdepdryis(pcols,pcnst)  ! aerosol dry deposition (interstitial)
    real(r8) :: aerdepdrycw(pcols,pcnst)  ! aerosol dry deposition (cloud water)
    real(r8), pointer :: fldcw(:,:)

    !++oslo aerosols
    real(r8) :: interfaceTendToLowestLayer(pcols)
    real(r8) :: deltaH(pcols)
    real(r8) :: massLostDD(pcols)
    real(r8) :: MMRNew(pcols)
    real(r8) :: lossRate(pcols)
    real(r8) :: totalProd(pcols)
    real(r8) :: fallFromAbove(pcols)

    real(r8) :: logSigma     
    logical  :: is_done(pcnst,2)
    !-----------------------------------------------------------------------

    landfrac => cam_in%landfrac(:)
    icefrac  => cam_in%icefrac(:)
    ocnfrac  => cam_in%ocnfrac(:)
    fvin     => cam_in%fv(:)
    ram1in   => cam_in%ram1(:)

    lchnk = state%lchnk
    ncol  = state%ncol
    aerdepdryis(:,:)=0._r8
    aerdepdrycw(:,:)=0._r8
    ! calc ram and fv over ocean and sea ice ...
    call calcram( ncol,landfrac,icefrac,ocnfrac,obklen,&
                  ustar,ram1in,ram1,state%t(:,pver),state%pmid(:,pver),&
                  state%pdel(:,pver),fvin,fv)

    call outfld( 'airFV', fv(:), pcols, lchnk )
    call outfld( 'RAM1', ram1(:), pcols, lchnk )
 
    ! note that tendencies are not only in sfc layer (because of sedimentation)
    ! and that ptend is updated within each subroutine for different species
    
    call physics_ptend_init(ptend, state%psetcols, 'aero_model_drydep', lq=drydep_lq)

    tvs(:ncol,:) = state%t(:ncol,:)!*(1+state%q(:ncol,k)
    rho(:ncol,:)=  state%pmid(:ncol,:)/(rair*state%t(:ncol,:))


    is_done(:,:) = .false.

!
! calc settling/deposition velocities for cloud droplets (and cloud-borne aerosols)
!
! *** mean drop radius should eventually be computed from ndrop and qcldwtr
    rad_drop(:,:) = 5.0e-6_r8
    dens_drop(:,:) = rhoh2o
    sg_drop(:,:) = 1.46_r8
    !jvlc = 3
    !call modal_aero_depvel_part( ncol,state%t(:,:), state%pmid(:,:), ram1, fv,  &
    !                 vlc_dry(:,:,jvlc), vlc_trb(:,jvlc), vlc_grv(:,:,jvlc),  &
    !                 rad_drop(:,:), dens_drop(:,:), sg_drop(:,:), 0, lchnk)
    jvlc = 4
    call modal_aero_depvel_part( ncol,state%t(:,:), state%pmid(:,:), ram1, fv,  &
                     vlc_dry(:,:,jvlc), vlc_trb(:,jvlc), vlc_grv(:,:,jvlc),  &
                     rad_drop(:,:), dens_drop(:,:), sg_drop(:,:), 3, lchnk)



    !At this point we really need to distribute the lifecycle-tracers over 
    !the actual modes (maybe according to surface available of background tracers?) 

    !in mam3, jvlc = 1 means number-concentration
    !in oslo_aero, jvlc = 1 means process-modes
    !The following logic is based on that process-mode tracers 
    !always follow AFTER the actual tracers!!

      dens_aer(:,:) = 0._r8
   do m = 0, nmodes   ! main loop over aerosol modes

       do lphase = 1, 2   ! loop over interstitial / cloud-borne forms

          if (lphase == 1) then   ! interstial aerosol - calc settling/dep velocities of mode

            logSigma = log(lifeCycleSigma(m))

             ! rad_aer = volume mean wet radius (m)
             ! dgncur_awet = geometric mean wet diameter for number distribution (m)
             if(top_lev.gt.1) then
               rad_aer(1:ncol,:top_lev-1) = 0._r8
             end if
             rad_aer(1:ncol,top_lev:) = 0.5_r8*dgncur_awet(1:ncol,top_lev:,m)   &
                                 *exp(1.5_r8*(logSigma))

             ! dens_aer(1:ncol,:) = wet density (kg/m3)
             if(top_lev.gt.1)then
               dens_aer(1:ncol,:top_lev-1) = 0._r8
             end if
             dens_aer(1:ncol,top_lev:) = wetdens(1:ncol,top_lev:,m)
             sg_aer(1:ncol,:) = lifecycleSigma(m)

             jvlc = 2
             call modal_aero_depvel_part( ncol, state%t(:,:), state%pmid(:,:), ram1, fv,  & 
                        vlc_dry(:,:,jvlc), vlc_trb(:,jvlc), vlc_grv(:,:,jvlc),  &
                        rad_aer(:,:), dens_aer(:,:), sg_aer(:,:), 3, lchnk)

!             if(m .eq. MODE_IDX_SS_A3)then
!               do i=1,ncol
!                  print*, "rad_aer", rad_aer(i,pver)*1.e6, ' um  ', vlc_dry(i,pver,jvlc)*1.e2, " cm/s"
!               end do
!            end if
          end if

          do lspec = 1, getNumberOfTracersInMode(m)   ! loop over number + constituents

            mm = getTracerIndex(m,lspec,.false.) 
            if(is_done(mm,lphase) .eqv. .true. )then
               cycle
            endif
            is_done(mm,lphase)=.true.

          if (lphase == 1) then
             jvlc = 2              !mass in clean air tracers

             !Process tracers have their own velocity based on fixed size / density
             !Calculate the velocity to use for this specie..
             if ( is_process_mode(mm, .false.) ) then
                jvlc = 1
                logSigma = log(processModeSigma(processModeMap(mm)))
                if(top_lev.gt.1)then
                  rad_aer(1:ncol, top_lev-1) = 0.0_r8
                end if
                rad_aer(1:ncol,top_lev:) = 0.5_r8*dgncur_awet_processmode(1:ncol,top_lev:,processModeMap(mm))   &
                                 *exp(1.5_r8*(logSigma))
                call modal_aero_depvel_part( ncol, state%t(:,:), state%pmid(:,:), ram1, fv,  & 
                           vlc_dry(:,:,jvlc), vlc_trb(:,jvlc), vlc_grv(:,:,jvlc),  &
                           rad_aer(:,:), dens_aer(:,:), sg_aer(:,:), 3, lchnk)
             endif

          else
             jvlc = 4              !mass in cloud tracers
          endif

          if (mm <= 0) cycle

!         if (lphase == 1) then
          if ((lphase == 1) .and. (lspec <= getNumberOfTracersInMode(m))) then
             ptend%lq(mm) = .TRUE.

             ! use pvprogseasalts instead (means making the top level 0)
             pvmzaer(:ncol,1)=0._r8
             pvmzaer(:ncol,2:pverp) = vlc_dry(:ncol,:,jvlc)

             call outfld( trim(cnst_name(mm))//'DDV', pvmzaer(:,2:pverp), pcols, lchnk )

             if(.true.) then ! use phil's method
             !      convert from meters/sec to pascals/sec
             !      pvprogseasalts(:,1) is assumed zero, use density from layer above in conversion
                pvmzaer(:ncol,2:pverp) = pvmzaer(:ncol,2:pverp) * rho(:ncol,:)*gravit

             !      calculate the tendencies and sfc fluxes from the above velocities
                call dust_sediment_tend( &
                     ncol,             dt,       state%pint(:,:), state%pmid, state%pdel, state%t , &
                     state%q(:,:,mm),  pvmzaer,  ptend%q(:,:,mm), sflx, interfaceTendToLowestLayer  )
             else   !use charlie's method
                call d3ddflux( ncol, vlc_dry(:,:,jvlc), state%q(:,:,mm), state%pmid, &
                               state%pdel, tvs, sflx, ptend%q(:,:,mm), dt )
             endif

             !write(iulog,*)"starting ddep proc", mm, pcnst
             !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
             !%%%%%% FIX FOR SHORT DRYDEP LIFE-TIMES
             !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
             !Some tracers have short lifetime with respect to dry dep:
             !Solve implicitly for eqn for emission and dry dep in lowest layer
             deltaH(:ncol)=state%pdel(:ncol,pver)/rho(:ncol,pver)/gravit     ![m] height of layer
             !print*, "deltaH", deltaH(:ncol)
             lossRate(:ncol) = vlc_dry(:ncol,pver,jvlc)/deltaH(:ncol)            ![1/s] loss rate out of layer
             !print*, "lossRate", lossRate(:ncol)
             !print*, "interfaceFluxesToLowestLayer", interfaceFluxToLowestLayer(:ncol)

             !OBS OBS OBS DIRTY FIX but need approx 2-3 weeks for proper solution
             !special treatment of BC_AX because BC_AX is not treated with
             !boundary mixing in activation (is by definition not activated!)
             !Therefor emissions are already added in "normal" boundary layer
             !mixing routine..
             !The proper fix to this is to skip the special treatment of BC_AX
             !and skip the index "0" for that mixture alltogether!
             if(mm .eq. l_bc_ax) then
               totalProd(:ncol) = interfaceTendToLowestLayer(:ncol)
             else
               totalProd(:ncol) = cflx(:ncol,mm)*gravit/state%pdel(:ncol,pver) + interfaceTendToLowestLayer(:ncol)
             end if

             !Do solution
             where(lossRate(:ncol)*dt .gt. 1.e-2_r8)
               MMRNew(:ncol) = state%q(:ncol,pver,mm)*exp(-lossRate(:ncol)*dt)   &
                              + totalProd(:ncol)/lossRate(:ncol)*(1.0_r8 - exp(-lossRate(:ncol)*dt))
             elsewhere
               MMRNew(:ncol) = state%q(:ncol,pver,mm)        &
                                   + totalProd(:ncol)*dt &
                                   - state%q(:ncol,pver,mm)*lossRate(:ncol)*dt
             end where

             !C0 + Pdt -massLostDD = CNew   ==>               
             massLostDD(:ncol) = state%q(:ncol,pver,mm) - MMRNew(:ncol) + totalProd(:ncol)*dt
             !Overwrite tendency in lowest layer to include emissions
             !They are then not included in vertical diffusion!!
             ptend%q(:ncol,pver,mm) = (MMRNew(:ncol)-state%q(:ncol,pver,mm))/dt
             sflx(:ncol) = massLostDD(:ncol)*state%pdel(:ncol,pver) / gravit / dt
             !write(iulog,*)"done ddep"
             !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

             ! apportion dry deposition into turb and gravitational settling for tapes
             dep_trb = 0._r8
             dep_grv = 0._r8
             do i=1,ncol
                if (vlc_dry(i,pver,jvlc) /= 0._r8) then
                   dep_trb(i)=sflx(i)*vlc_trb(i,jvlc)/vlc_dry(i,pver,jvlc)
                   dep_grv(i)=sflx(i)*vlc_grv(i,pver,jvlc)/vlc_dry(i,pver,jvlc)
                endif
             enddo

             call outfld( trim(cnst_name(mm))//'DDF', sflx, pcols, lchnk)
             call outfld( trim(cnst_name(mm))//'TBF', dep_trb, pcols, lchnk )
             call outfld( trim(cnst_name(mm))//'GVF', dep_grv, pcols, lchnk )
             call outfld( trim(cnst_name(mm))//'DTQ', ptend%q(:,:,mm), pcols, lchnk)
             aerdepdryis(:ncol,mm) = sflx(:ncol)

          else  ! lphase == 2

             !Pick up the cloud tracers (oslo)
             fldcw => qqcw_get_field(pbuf, mm,lchnk,.true.)
             if( .not. associated(fldcw))then
               cycle
             end if

             ! use pvprogseasalts instead (means making the top level 0)
             pvmzaer(:ncol,1)=0._r8
             pvmzaer(:ncol,2:pverp) = vlc_dry(:ncol,:,jvlc)


             if(.true.) then ! use phil's method
             !      convert from meters/sec to pascals/sec
             !      pvprogseasalts(:,1) is assumed zero, use density from layer above in conversion
                pvmzaer(:ncol,2:pverp) = pvmzaer(:ncol,2:pverp) * rho(:ncol,:)*gravit

             !      calculate the tendencies and sfc fluxes from the above velocities
                call dust_sediment_tend( &
                     ncol,             dt,       state%pint(:,:), state%pmid, state%pdel, state%t , &
                     fldcw(:,:),  pvmzaer,  dqdt_tmp(:,:), sflx  )
             else   !use charlie's method
                call d3ddflux( ncol, vlc_dry(:,:,jvlc), fldcw(:,:), state%pmid, &
                               state%pdel, tvs, sflx, dqdt_tmp(:,:), dt )
             endif

             ! apportion dry deposition into turb and gravitational settling for tapes
             dep_trb = 0._r8
             dep_grv = 0._r8
             do i=1,ncol
                if (vlc_dry(i,pver,jvlc) /= 0._r8) then
                   dep_trb(i)=sflx(i)*vlc_trb(i,jvlc)/vlc_dry(i,pver,jvlc)
                   dep_grv(i)=sflx(i)*vlc_grv(i,pver,jvlc)/vlc_dry(i,pver,jvlc)
                end if 
             enddo

             fldcw(1:ncol,:) = fldcw(1:ncol,:) + dqdt_tmp(1:ncol,:) * dt

             call outfld( trim(getCloudTracerName(mm))//'DDF', sflx, pcols, lchnk)
             call outfld( trim(getCloudTracerName(mm))//'TBF', dep_trb, pcols, lchnk )
             call outfld( trim(getCloudTracerName(mm))//'GVF', dep_grv, pcols, lchnk )
             aerdepdrycw(:ncol,mm) = sflx(:ncol)

          endif

          enddo   ! lspec = 0, nspec_amode(m)+1
       enddo   ! lphase = 1, 2
    enddo   ! m = 1, ntot_amode

    ! if the user has specified prescribed aerosol dep fluxes then 
    ! do not set cam_out dep fluxes according to the prognostic aerosols
    if (.not.aerodep_flx_prescribed()) then
       call set_srf_drydep(aerdepdryis, aerdepdrycw, cam_out)
    endif

    return
  end subroutine oslo_aero_dry_intr
  !===============================================================================
  subroutine oslo_aero_wet_intr ( state, dt, dlf, cam_out, ptend, pbuf) 
  
  
    !----------------------------------------------------------------------- 
    !-----------------------------------------------------------------------
    use cam_history,   only: outfld
    use physics_types, only: physics_state, physics_ptend
    use camsrfexch,    only: cam_out_t     
    use wetdep,        only: wetdepa_v2, wetdep_inputs_set, wetdep_inputs_t
    use physconst,     only: gravit
    use constituents,  only: cnst_mw
    use physconst,     only: mwdry    ! molecular weight dry air ~ kg/kmole
    use physconst,     only: boltz    ! J/K/molecule
    use tracer_cnst,   only: get_cnst_data
    use modal_aero_deposition, only: set_srf_wetdep
    use physics_buffer, only : physics_buffer_desc

    type(physics_state), intent(in)    :: state       ! Physics state variables
    real(r8),            intent(in)  :: dt             ! time step
    real(r8),            intent(in)    :: dlf(:,:)    ! shallow+deep convective detrainment [kg/kg/s]
    type(cam_out_t),     intent(inout) :: cam_out     ! export state
    type(physics_ptend), intent(out)   :: ptend       ! indivdual parameterization tendencies
    type(physics_buffer_desc), pointer :: pbuf(:)


    !
    ! Local variables
    !
    integer  :: m                                  ! tracer index
    integer  :: lchnk                              ! chunk identifier
    integer  :: ncol                               ! number of atmospheric columns
    real(r8) :: iscavt(pcols, pver)
    integer  :: mm

    real(r8) :: icscavt(pcols, pver)
    real(r8) :: isscavt(pcols, pver)
    real(r8) :: bcscavt(pcols, pver)
    real(r8) :: bsscavt(pcols, pver)
    real(r8) :: sol_factb, sol_facti
    real(r8) :: sol_factic(pcols,pver)
    real(r8) :: sflx(pcols)            ! deposition flux
    integer :: i,k
    real(r8) :: scavcoef(pcols,pver) ! Dana and Hales coefficient (/mm) (0.1)
    integer :: jnv                     ! index for scavcoefnv 3rd dimension
    integer :: lphase                  ! index for interstitial / cloudborne aerosol
    integer :: lspec                   ! index for aerosol number / chem-mass / water-mass
    integer :: lcoardust, lcoarnacl    ! indices for coarse mode dust and seasalt masses
    real(r8) :: dqdt_tmp(pcols,pver)   ! temporary array to hold tendency for 1 species
    real(r8) :: f_act_conv(pcols,pver) ! prescribed aerosol activation fraction for convective cloud   ! rce 2010/05/01
    real(r8) :: f_act_conv_coarse(pcols,pver) ! similar but for coarse mode                            ! rce 2010/05/02
    real(r8) :: f_act_conv_coarse_dust, f_act_conv_coarse_nacl                                         ! rce 2010/05/02
    real(r8) :: fracis_cw(pcols,pver)
    !real(r8) :: hygro_sum_old(pcols,pver)  ! before removal    [sum of (mass*hydro/dens)]
    !real(r8) :: hygro_sum_del(pcols,pver)  ! removal change to [sum of (mass*hydro/dens)]
    !real(r8) :: hygro_sum_old_ik, hygro_sum_new_ik
    real(r8) :: prec(pcols)                ! precipitation rate
    real(r8) :: q_tmp(pcols,pver)          ! temporary array to hold "most current" mixing ratio for 1 species
    real(r8) :: qqcw_tmp(pcols,pver)       ! temporary array to hold qqcw   ! rce 2010/05/01
    real(r8) :: scavcoefnv(pcols,pver,0:2) ! Dana and Hales coefficient (/mm) for
                                           ! cloud-borne num & vol (0), 
                                           ! interstitial num (1), interstitial vol (2)
    real(r8) :: tmpa, tmpb
    real(r8) :: tmpdust, tmpnacl
    real(r8) :: water_old, water_new   ! temporary old/new aerosol water mix-rat
    logical :: isprx(pcols,pver) ! true if precipation
    real(r8) :: aerdepwetis(pcols,pcnst)  ! aerosol wet deposition (interstitial)
    real(r8) :: aerdepwetcw(pcols,pcnst)  ! aerosol wet deposition (cloud water)

    real(r8), pointer :: rprddp(:,:)     ! rain production, deep convection
    real(r8), pointer :: rprdsh(:,:)     ! rain production, shallow convection
    real(r8), pointer :: evapcdp(:,:)    ! Evaporation rate of deep    convective precipitation >=0.
    real(r8), pointer :: evapcsh(:,:)    ! Evaporation rate of shallow convective precipitation >=0.

    real(r8) :: rprddpsum(pcols)
    real(r8) :: rprdshsum(pcols)
    real(r8) :: evapcdpsum(pcols)
    real(r8) :: evapcshsum(pcols)

    real(r8) :: tmp_resudp, tmp_resush

    real(r8) :: sflxec(pcols), sflxecdp(pcols)  ! deposition flux
    real(r8) :: sflxic(pcols), sflxicdp(pcols)  ! deposition flux
    real(r8) :: sflxbc(pcols), sflxbcdp(pcols)  ! deposition flux
    real(r8) :: rcscavt(pcols, pver)
    real(r8) :: rsscavt(pcols, pver)
    real(r8) :: qqcw_in(pcols,pver), qqcw_sav(pcols,pver,pcnst) ! temporary array to hold qqcw for the current mode
    real(r8), pointer :: fldcw(:,:)

    logical            :: is_done(pcnst,2)
    real(r8),target :: zeroAerosolConcentration(pcols,pver)

    real(r8), pointer :: fracis(:,:,:)   ! fraction of transported species that are insoluble

    type(wetdep_inputs_t) :: dep_inputs

    lchnk = state%lchnk
    ncol  = state%ncol

    call physics_ptend_init(ptend, state%psetcols, 'aero_model_wetdep', lq=wetdep_lq)

    is_done(:,:) = .false.


    zeroAerosolConcentration(:,:)=0.0_r8

    ! Wet deposition of mozart aerosol species.
    ptend%name  = ptend%name//'+mz_aero_wetdep'

    call wetdep_inputs_set( state, pbuf, dep_inputs )
    call pbuf_get_field(pbuf, fracis_idx,         fracis, start=(/1,1,1/), kount=(/pcols, pver, pcnst/) )

     prec(:ncol)=0._r8
     do k=1,pver
        where (prec(:ncol) >= 1.e-7_r8)
            isprx(:ncol,k) = .true.
        elsewhere
            isprx(:ncol,k) = .false.
        endwhere
       prec(:ncol) = prec(:ncol) + (dep_inputs%prain(:ncol,k) + dep_inputs%cmfdqr(:ncol,k) - dep_inputs%evapr(:ncol,k)) &
                    *state%pdel(:ncol,k)/gravit
     end do


! calculate the mass-weighted sol_factic for coarse mode species
!    sol_factic_coarse(:,:) = 0.30_r8   ! tuned 1/4
     f_act_conv_coarse(:,:) = 0.60_r8   ! rce 2010/05/02
     f_act_conv_coarse_dust = 0.40_r8   ! rce 2010/05/02
     f_act_conv_coarse_nacl = 0.80_r8   ! rce 2010/05/02
     !++ag
      f_act_conv_coarse(:,:) = 0.5_r8
     !--ag

    scavcoefnv(:,:,0) = 0.0_r8   ! below-cloud scavcoef = 0.0 for cloud-borne species

    do m = 0, nmodes  ! main loop over aerosol modes

       do lphase = 1, 2   ! loop over interstitial (1) and cloud-borne (2) forms

          ! sol_factb and sol_facti values
          ! sol_factb - currently this is basically a tuning factor
          ! sol_facti & sol_factic - currently has a physical basis, and reflects activation fraction
          !
          ! 2008-mar-07 rce - sol_factb (interstitial) changed from 0.3 to 0.1
          ! - sol_factic (interstitial, dust modes) changed from 1.0 to 0.5
          ! - sol_factic (cloud-borne, pcarb modes) no need to set it to 0.0
          ! because the cloud-borne pcarbon == 0 (no activation)
          !
          ! rce 2010/05/02
          ! prior to this date, sol_factic was used for convective in-cloud wet removal,
          ! and its value reflected a combination of an activation fraction (which varied between modes)
          ! and a tuning factor
          ! from this date forward, two parameters are used for convective in-cloud wet removal
          ! f_act_conv is the activation fraction
          ! note that "non-activation" of aerosol in air entrained into updrafts should
          ! be included here
          ! eventually we might use the activate routine (with w ~= 1 m/s) to calculate
          ! this, but there is still the entrainment issue
          ! sol_factic is strictly a tuning factor
          !
          if (lphase == 1) then   ! interstial aerosol
             !hygro_sum_old(:,:) = 0.0_r8
             !hygro_sum_del(:,:) = 0.0_r8
             !call modal_aero_bcscavcoef_get( m, ncol, isprx, dgncur_awet,   &
             !                                scavcoefnv(:,:,1), scavcoefnv(:,:,2) )

             scavcoefnv(:,:,1) = 0.1_r8  !Used by MAM for number concentration

             sol_factb  = 0.1_r8   ! all below-cloud scav ON (0.1 "tuning factor")
!            sol_factb  = 0.03_r8   ! all below-cloud scav ON (0.1 "tuning factor")  ! tuned 1/6

             sol_facti  = 0.0_r8   ! strat  in-cloud scav totally OFF for institial

             sol_factic = 0.4_r8      ! xl 2010/05/20

             !fxm: simplified relative to MAM
             f_act_conv = 0.8 !ag: Introduce tuning per component later


          else   ! cloud-borne aerosol (borne by stratiform cloud drops)
      
             !++ag
             !default 100 % is scavenged by cloud -borne
             sol_facti_cloud_borne = 1.0_r8
             !--ag

             sol_factb  = 0.0_r8   ! all below-cloud scav OFF (anything cloud-borne is located "in-cloud")
             sol_facti  = sol_facti_cloud_borne   ! strat  in-cloud scav cloud-borne tuning factor
             sol_factic = 0.0_r8   ! conv   in-cloud scav OFF (having this on would mean
                                   !        that conv precip collects strat droplets)
             f_act_conv = 0.0_r8   ! conv   in-cloud scav OFF (having this on would mean

          end if
          if (convproc_do_aer .and. lphase == 1) then
             ! if modal aero convproc is turned on for aerosols, then
             !    turn off the convective in-cloud removal for interstitial aerosols
             !    (but leave the below-cloud on, as convproc only does in-cloud)
             !    and turn off the outfld SFWET, SFSIC, SFSID, SFSEC, and SFSED calls 
             ! for (stratiform)-cloudborne aerosols, convective wet removal
             !    (all forms) is zero, so no action is needed
             sol_factic = 0.0_r8
          endif


          do lspec = 1,getNumberOfTracersInMode(m)   ! loop over number + chem constituents + water

           
            mm = getTracerIndex(m,lspec,.false.)
            if(is_done(mm,lphase) .eqv. .true. )then
               cycle
            endif
            is_done(mm,lphase)=.true.

            if (lphase == 1) then
               jnv = 2
               !Set correct below cloud scaveing coefficients
               !Hard-coded values per mode in NorESM
               if(is_process_mode(mm,.FALSE.))then
                  scavcoefnv(:,:,jnv) = belowCloudScavengingCoefficientProcessModes(processModeMap(mm))
               else
                  scavcoefnv(:,:,jnv) = belowCloudScavengingCoefficient(m)
               end if
            else
               jnv = 0  !==> below cloud scavenging coefficients are zero (see above)
            endif



          if ((lphase == 1) .and. (lspec <= getNumberOfTracersInMode(m))) then
             ptend%lq(mm) = .TRUE.
             dqdt_tmp(:,:) = 0.0_r8
             ! q_tmp reflects changes from modal_aero_calcsize and is the "most current" q
             q_tmp(1:ncol,:) = state%q(1:ncol,:,mm) + ptend%q(1:ncol,:,mm)*dt
                if(convproc_do_aer) then
                   !Feed in the saved cloudborne mixing ratios from phase 2
                   qqcw_in(:,:) = qqcw_sav(:,:,mm)
                   !Not implemented for oslo aerosols
                else
                   fldcw => qqcw_get_field(pbuf, mm,lchnk, .TRUE.)
                   if(.not. associated(fldcw))then
                      qqcw_in(:,:) = zeroAerosolConcentration(:,:)
                   else
                     qqcw_in(:,:) = fldcw(:,:)
                   end if
             endif

             call wetdepa_v2( state%pmid, state%q(:,:,1), state%pdel, &
                  dep_inputs%cldt, dep_inputs%cldcu, dep_inputs%cmfdqr, &
                  dep_inputs%evapc, dep_inputs%conicw, dep_inputs%prain, dep_inputs%qme, &
                  dep_inputs%evapr, dep_inputs%totcond, q_tmp, dt, &
                  dqdt_tmp, iscavt, dep_inputs%cldvcu, dep_inputs%cldvst, &
                  dlf, fracis(:,:,mm), sol_factb, ncol, &
                  scavcoefnv(:,:,jnv), &
                  is_strat_cloudborne=.false., &
                  qqcw=qqcw_in(:,:),  &
                  f_act_conv=f_act_conv, &
                  icscavt=icscavt, isscavt=isscavt, bcscavt=bcscavt, bsscavt=bsscavt, &
                  convproc_do_aer=.false., rcscavt=rcscavt, rsscavt=rsscavt,  &
                  sol_facti_in=sol_facti, sol_factic_in=sol_factic )

             ptend%q(1:ncol,:,mm) = ptend%q(1:ncol,:,mm) + dqdt_tmp(1:ncol,:)

             call outfld( trim(cnst_name(mm))//'WET', dqdt_tmp(:,:), pcols, lchnk)
             call outfld( trim(cnst_name(mm))//'SIC', icscavt, pcols, lchnk)
             call outfld( trim(cnst_name(mm))//'SIS', isscavt, pcols, lchnk)
             call outfld( trim(cnst_name(mm))//'SBC', bcscavt, pcols, lchnk)
             call outfld( trim(cnst_name(mm))//'SBS', bsscavt, pcols, lchnk)

             sflx(:)=0._r8
             do k=1,pver
                do i=1,ncol
                   sflx(i)=sflx(i)+dqdt_tmp(i,k)*state%pdel(i,k)/gravit
                enddo
             enddo
                if (.not.convproc_do_aer) call outfld( trim(cnst_name(mm))//'SFWET', sflx, pcols, lchnk)
             aerdepwetis(:ncol,mm) = sflx(:ncol)
         
             sflx(:)=0._r8
             do k=1,pver
                do i=1,ncol
                   sflx(i)=sflx(i)+icscavt(i,k)*state%pdel(i,k)/gravit
                enddo
             enddo
                if (.not.convproc_do_aer) call outfld( trim(cnst_name(mm))//'SFSIC', sflx, pcols, lchnk)
                if (convproc_do_aer) sflxic = sflx

             sflx(:)=0._r8
             do k=1,pver
                do i=1,ncol
                   sflx(i)=sflx(i)+isscavt(i,k)*state%pdel(i,k)/gravit
                enddo
             enddo
             call outfld( trim(cnst_name(mm))//'SFSIS', sflx, pcols, lchnk)

             sflx(:)=0._r8
             do k=1,pver
                do i=1,ncol
                   sflx(i)=sflx(i)+bcscavt(i,k)*state%pdel(i,k)/gravit
                enddo
             enddo
             call outfld( trim(cnst_name(mm))//'SFSBC', sflx, pcols, lchnk)
                if (convproc_do_aer)sflxbc = sflx

             sflx(:)=0._r8
             do k=1,pver
                do i=1,ncol
                   sflx(i)=sflx(i)+bsscavt(i,k)*state%pdel(i,k)/gravit
                enddo
             enddo
             call outfld( trim(cnst_name(mm))//'SFSBS', sflx, pcols, lchnk)


   

          else   ! lphase == 2
             dqdt_tmp(:,:) = 0.0_r8
             qqcw_tmp(:,:) = 0.0_r8    ! rce 2010/05/01

                   if (convproc_do_aer) then
                      fldcw => qqcw_get_field(pbuf,mm,lchnk)
                      qqcw_sav(1:ncol,:,mm) = fldcw(1:ncol,:)
                      !This option yet not implemented for OSLO_AERO
                   else
                      fldcw => qqcw_get_field(pbuf, mm,lchnk, .TRUE.)
                      if(.not. associated(fldcw))then
                         cycle
                      end if
                   endif

             call wetdepa_v2(state%pmid, state%q(:,:,1), state%pdel, &
                  dep_inputs%cldt, dep_inputs%cldcu, dep_inputs%cmfdqr, &
                  dep_inputs%evapc, dep_inputs%conicw, dep_inputs%prain, dep_inputs%qme, &
                  dep_inputs%evapr, dep_inputs%totcond, fldcw, dt, &
                  dqdt_tmp, iscavt, dep_inputs%cldvcu, dep_inputs%cldvst, &
                  dlf, fracis_cw, sol_factb, ncol, &
                  scavcoefnv(:,:,jnv), &
                  is_strat_cloudborne=.true.,  &
                  icscavt=icscavt, isscavt=isscavt, bcscavt=bcscavt, bsscavt=bsscavt, &
                  convproc_do_aer=.false., rcscavt=rcscavt, rsscavt=rsscavt,  &
                  sol_facti_in=sol_facti, sol_factic_in=sol_factic )

             fldcw(1:ncol,:) = fldcw(1:ncol,:) + dqdt_tmp(1:ncol,:) * dt

             sflx(:)=0._r8
             do k=1,pver
                do i=1,ncol
                   sflx(i)=sflx(i)+dqdt_tmp(i,k)*state%pdel(i,k)/gravit
                enddo
             enddo
             call outfld( trim(getCloudTracerName(mm))//'SFWET', sflx, pcols, lchnk)
             aerdepwetcw(:ncol,mm) = sflx(:ncol)

             sflx(:)=0._r8
             do k=1,pver
                do i=1,ncol
                   sflx(i)=sflx(i)+icscavt(i,k)*state%pdel(i,k)/gravit
                enddo
             enddo
             call outfld( trim(getCloudTracerName(mm))//'SFSIC', sflx, pcols, lchnk)
             sflx(:)=0._r8
             do k=1,pver
                do i=1,ncol
                   sflx(i)=sflx(i)+isscavt(i,k)*state%pdel(i,k)/gravit
                enddo
             enddo
             call outfld( trim(getCloudTracerName(mm))//'SFSIS', sflx, pcols, lchnk)
             sflx(:)=0._r8
             do k=1,pver
                do i=1,ncol
                   sflx(i)=sflx(i)+bcscavt(i,k)*state%pdel(i,k)/gravit
                enddo
             enddo
             call outfld( trim(getCloudTracerName(mm))//'SFSBC', sflx, pcols, lchnk)
             sflx(:)=0._r8
             do k=1,pver
                do i=1,ncol
                   sflx(i)=sflx(i)+bsscavt(i,k)*state%pdel(i,k)/gravit
                enddo
             enddo
             call outfld( trim(getCloudTracerName(mm))//'SFSBS', sflx, pcols, lchnk)

          endif

          enddo   ! lspec = 0, nspec_amode(m)+1
       enddo   ! lphase = 1, 2
    enddo   ! m = 1, ntot_amode

    ! if the user has specified prescribed aerosol dep fluxes then 
    ! do not set cam_out dep fluxes according to the prognostic aerosols
    if (.not. aerodep_flx_prescribed()) then
       call set_srf_wetdep(aerdepwetis, aerdepwetcw, cam_out)
    endif

    return

  end subroutine oslo_aero_wet_intr



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

    if(top_lev.gt.1) then
      vlc_grv(:ncol,:top_lev-1) = 0._r8
      vlc_dry(:ncol,:top_lev-1) = 0._r8
    endif

    do k=top_lev,pver
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



end module oslo_aerosols_intr
