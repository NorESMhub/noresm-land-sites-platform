module nucleate_ice_oslo

!---------------------------------------------------------------------------------
!
!  CAM Interfaces for nucleate_ice module.
!
!  B. Eaton - Sept 2014
!---------------------------------------------------------------------------------

use shr_kind_mod,   only: r8=>shr_kind_r8
use spmd_utils,     only: masterproc
use ppgrid,         only: pcols, pver
use physconst,      only: pi, rair, tmelt
use constituents,   only: pcnst, cnst_get_ind
use physics_types,  only: physics_state, physics_ptend, physics_ptend_init
use physics_buffer, only: physics_buffer_desc, pbuf_get_index, pbuf_old_tim_idx, pbuf_get_field
use phys_control,   only: use_hetfrz_classnuc
use physics_buffer, only: pbuf_add_field, dtype_r8, pbuf_old_tim_idx, &
                          pbuf_get_index, pbuf_get_field
use cam_history,    only: addfld, add_default, outfld

use ref_pres,       only: top_lev => trop_cloud_top_lev
use wv_saturation,  only: qsat_water, svp_water, svp_ice
use shr_spfn_mod,   only: erf => shr_spfn_erf

use cam_logfile,    only: iulog
use cam_abortutils, only: endrun

use nucleate_ice,   only: nucleati_init, nucleati

use aerosoldef,     only:  l_dst_a2, l_dst_a3, & 
                           MODE_IDX_DST_A2, MODE_IDX_DST_A3, &
                           rhopart
use modal_aero_data, only: qqcw_get_field
use const          , only: volumeToNumber

implicit none
private
save

public :: &
   nucleate_ice_oslo_readnl,   &
   nucleate_ice_oslo_register, &
   nucleate_ice_oslo_init,     &
   nucleate_ice_oslo_calc
   

! Namelist variables
logical, public, protected :: use_preexisting_ice = .false.
logical                    :: hist_preexisting_ice = .false.
logical                    :: nucleate_ice_incloud = .false.
logical                    :: nucleate_ice_use_troplev = .false.
real(r8)                   :: nucleate_ice_subgrid = -1._r8
real(r8)                   :: nucleate_ice_subgrid_strat = -1._r8
real(r8)                   :: nucleate_ice_strat = 0.0_r8

! Vars set via init method.
real(r8) :: mincld      ! minimum allowed cloud fraction
real(r8) :: bulk_scale  ! prescribed aerosol bulk sulfur scale factor

! constituent indices
integer :: &
   cldliq_idx = -1, &
   cldice_idx = -1, &
   numice_idx = -1

integer :: &
   naai_idx,     &
   naai_hom_idx

integer :: &
   ast_idx   = -1, &
   dgnum_idx = -1

integer :: &
    qsatfac_idx
! modal aerosols
logical :: clim_modal_aero = .TRUE.
logical :: lq(pcnst) = .false. ! set flags true for constituents with non-zero tendencies

!===============================================================================
contains
!===============================================================================

subroutine nucleate_ice_oslo_readnl(nlfile)

  use namelist_utils,  only: find_group_name
  use units,           only: getunit, freeunit
  use spmd_utils,      only: mpicom, masterprocid, mpi_logical, mpi_real8

  character(len=*), intent(in) :: nlfile  ! filepath for file containing namelist input

  ! Local variables
  integer :: unitn, ierr
  character(len=*), parameter :: subname = 'nucleate_ice_cam_readnl'

  namelist /nucleate_ice_nl/ use_preexisting_ice, hist_preexisting_ice, &
       nucleate_ice_subgrid, nucleate_ice_subgrid_strat, nucleate_ice_strat, &
       nucleate_ice_incloud, nucleate_ice_use_troplev

  !-----------------------------------------------------------------------------

  if (masterproc) then
     unitn = getunit()
     open( unitn, file=trim(nlfile), status='old' )
     call find_group_name(unitn, 'nucleate_ice_nl', status=ierr)
     if (ierr == 0) then
        read(unitn, nucleate_ice_nl, iostat=ierr)
        if (ierr /= 0) then
           call endrun(subname // ':: ERROR reading namelist')
        end if
     end if
     close(unitn)
     call freeunit(unitn)

  end if

  ! Broadcast namelist variables
  call mpi_bcast(use_preexisting_ice,  1, mpi_logical,masterprocid, mpicom, ierr)
  call mpi_bcast(hist_preexisting_ice, 1, mpi_logical,masterprocid, mpicom, ierr)
  call mpi_bcast(nucleate_ice_subgrid, 1, mpi_real8,  masterprocid, mpicom, ierr)
  call mpi_bcast(nucleate_ice_subgrid_strat, 1, mpi_real8,  masterprocid, mpicom, ierr)
  call mpi_bcast(nucleate_ice_strat,   1, mpi_real8,  masterprocid, mpicom, ierr)
  call mpi_bcast(nucleate_ice_incloud, 1, mpi_logical,masterprocid, mpicom, ierr)
  call mpi_bcast(nucleate_ice_use_troplev, 1, mpi_logical,masterprocid, mpicom, ierr)

end subroutine nucleate_ice_oslo_readnl

!================================================================================================

subroutine nucleate_ice_oslo_register()

   call pbuf_add_field('NAAI',     'physpkg', dtype_r8, (/pcols,pver/), naai_idx)
   call pbuf_add_field('NAAI_HOM', 'physpkg', dtype_r8, (/pcols,pver/), naai_hom_idx)

end subroutine nucleate_ice_oslo_register

!================================================================================================

subroutine nucleate_ice_oslo_init(mincld_in, bulk_scale_in)
   use phys_control, only: phys_getopts

   real(r8), intent(in) :: mincld_in
   real(r8), intent(in) :: bulk_scale_in

   ! local variables
   integer  :: iaer
   integer :: ierr
   integer  :: m, n, nspec

   character(len=32) :: str32
   character(len=*), parameter :: routine = 'nucleate_ice_cam_init'
   logical :: history_cesm_forcing
   !--------------------------------------------------------------------------------------------
   call phys_getopts(history_cesm_forcing_out = history_cesm_forcing)

   mincld     = mincld_in
   bulk_scale = bulk_scale_in

   if( masterproc ) then
      write(iulog,*) 'nucleate_ice parameters:'
      write(iulog,*) '  mincld                     = ', mincld_in
      write(iulog,*) '  bulk_scale                 = ', bulk_scale_in
      write(iulog,*) '  use_preexisiting_ice       = ', use_preexisting_ice
      write(iulog,*) '  hist_preexisiting_ice      = ', hist_preexisting_ice
      write(iulog,*) '  nucleate_ice_subgrid       = ', nucleate_ice_subgrid
      write(iulog,*) '  nucleate_ice_subgrid_strat = ', nucleate_ice_subgrid_strat
      write(iulog,*) '  nucleate_ice_strat         = ', nucleate_ice_strat
      write(iulog,*) '  nucleate_ice_incloud       = ', nucleate_ice_incloud
      write(iulog,*) '  nucleate_ice_use_troplev   = ', nucleate_ice_use_troplev
   end if

   call cnst_get_ind('CLDLIQ', cldliq_idx)
   call cnst_get_ind('CLDICE', cldice_idx)
   call cnst_get_ind('NUMICE', numice_idx)
   qsatfac_idx  = pbuf_get_index('QSATFAC', ierr)

   if (((nucleate_ice_subgrid .eq. -1._r8) .or. (nucleate_ice_subgrid_strat .eq. -1._r8)) .and. (qsatfac_idx .eq. -1)) then
     call endrun(routine//': ERROR qsatfac is required when subgrid = -1 or subgrid_strat = -1')
   end if
   
   call addfld('NIHF',  (/ 'lev' /), 'A', '1/m3', 'Activated Ice Number Concentation due to homogenous freezing')
   call addfld('NIDEP', (/ 'lev' /), 'A', '1/m3', 'Activated Ice Number Concentation due to deposition nucleation')
   call addfld('NIIMM', (/ 'lev' /), 'A', '1/m3', 'Activated Ice Number Concentation due to immersion freezing')
   call addfld('NIMEY', (/ 'lev' /), 'A', '1/m3', 'Activated Ice Number Concentation due to meyers deposition')

   call addfld('NIREGM',(/ 'lev' /), 'A', 'C', 'Ice Nucleation Temperature Threshold for Regime')
   call addfld('NISUBGRID',(/ 'lev' /), 'A', '', 'Ice Nucleation subgrid saturation factor')
   call addfld('NITROP_PD',(/ 'lev' /), 'A', '', 'Chemical Tropopause probability')
   if ( history_cesm_forcing ) then
      call add_default('NITROP_PD',8,' ')
   endif

   if (use_preexisting_ice) then
      call addfld('fhom',      (/ 'lev' /), 'A','fraction', 'Fraction of cirrus where homogeneous freezing occur'   ) 
      call addfld ('WICE',     (/ 'lev' /), 'A','m/s','Vertical velocity Reduction caused by preexisting ice'  )
      call addfld ('WEFF',     (/ 'lev' /), 'A','m/s','Effective Vertical velocity for ice nucleation' )
      call addfld ('INnso4',   (/ 'lev' /), 'A','1/m3','Number Concentation so4 (in) to ice_nucleation')
      call addfld ('INnbc',    (/ 'lev' /), 'A','1/m3','Number Concentation bc  (in) to ice_nucleation')
      call addfld ('INndust',  (/ 'lev' /), 'A','1/m3','Number Concentation dust (in) ice_nucleation')
      call addfld ('INondust',  (/ 'lev' /), 'A','1/m3','Number Concentation dust (out) from ice_nucleation')
      call addfld ('INhet',    (/ 'lev' /), 'A','1/m3', &
                'contribution for in-cloud ice number density increase by het nucleation in ice cloud')
      call addfld ('INhom',    (/ 'lev' /), 'A','1/m3', &
                'contribution for in-cloud ice number density increase by hom nucleation in ice cloud')
      call addfld ('INFrehom', (/ 'lev' /), 'A','frequency','hom IN frequency ice cloud')
      call addfld ('INFreIN',  (/ 'lev' /), 'A','frequency','frequency of ice nucleation occur')

      if (hist_preexisting_ice) then
         call add_default ('WSUBI   ', 1, ' ')  ! addfld/outfld calls are in microp_aero

         call add_default ('fhom    ', 1, ' ') 
         call add_default ('WICE    ', 1, ' ')
         call add_default ('WEFF    ', 1, ' ')
         call add_default ('INnso4  ', 1, ' ')
         call add_default ('INnbc   ', 1, ' ')
         call add_default ('INndust ', 1, ' ')
         call add_default ('INhet   ', 1, ' ')
         call add_default ('INhom   ', 1, ' ')
         call add_default ('INFrehom', 1, ' ')
         call add_default ('INFreIN ', 1, ' ')
      end if
   end if


   lq(l_dst_a2) = .TRUE.
   lq(l_dst_a3) = .TRUE.

   call nucleati_init(use_preexisting_ice, use_hetfrz_classnuc, nucleate_ice_incloud, iulog, pi, &
        mincld)

   ! get indices for fields in the physics buffer
   ast_idx      = pbuf_get_index('AST')

end subroutine nucleate_ice_oslo_init

!================================================================================================

subroutine nucleate_ice_oslo_calc( &
   state, wsubi, pbuf, dtime, ptend  &
   , numberConcentration)

   use aerosoldef, only : MODE_IDX_DST_A2, MODE_IDX_DST_A3 &
                        , MODE_IDX_SO4_AC,MODE_IDX_OMBC_INTMIX_COAT_AIT 
   use commondefinitions, only: nmodes

   use tropopause,     only: tropopause_findChemTrop

   ! arguments
   real(r8), intent(in)                       :: numberConcentration(pcols,pver,0:nmodes)
   type(physics_state), target, intent(in)    :: state
   real(r8),                    intent(in)    :: wsubi(:,:)
   type(physics_buffer_desc),   pointer       :: pbuf(:)
   real(r8),                    intent(in)    :: dtime
   type(physics_ptend),         intent(out)   :: ptend
 
   ! local workspace

   ! naai and naai_hom are the outputs shared with the microphysics
   real(r8), pointer :: naai(:,:)       ! number of activated aerosol for ice nucleation 
   real(r8), pointer :: naai_hom(:,:)   ! number of activated aerosol for ice nucleation (homogeneous freezing only)

   integer :: lchnk, ncol
   integer :: itim_old
   integer :: i, k, m

   real(r8), pointer :: t(:,:)          ! input temperature (K)
   real(r8), pointer :: qn(:,:)         ! input water vapor mixing ratio (kg/kg)
   real(r8), pointer :: qc(:,:)         ! cloud water mixing ratio (kg/kg)
   real(r8), pointer :: qi(:,:)         ! cloud ice mixing ratio (kg/kg)
   real(r8), pointer :: ni(:,:)         ! cloud ice number conc (1/kg)
   real(r8), pointer :: pmid(:,:)       ! pressure at layer midpoints (pa)

   real(r8), pointer :: cld_dst_a2(:,:) ! mmr cld dst a2
   real(r8), pointer :: cld_dst_a3(:,:) ! mass m.r. of coarse dust

   real(r8), pointer :: ast(:,:)
   real(r8) :: icecldf(pcols,pver)  ! ice cloud fraction
   real(r8), pointer :: qsatfac(:,:)      ! Subgrid cloud water saturation scaling factor.

   real(r8) :: rho(pcols,pver)      ! air density (kg m-3)

   real(r8), allocatable :: naer2(:,:,:)    ! bulk aerosol number concentration (1/m3)
   real(r8), allocatable :: maerosol(:,:,:) ! bulk aerosol mass conc (kg/m3)

   real(r8) :: qs(pcols)            ! liquid-ice weighted sat mixing rat (kg/kg)
   real(r8) :: es(pcols)            ! liquid-ice weighted sat vapor press (pa)
   real(r8) :: gammas(pcols)        ! parameter for cond/evap of cloud water
   integer  :: troplev(pcols)       ! tropopause level

   real(r8) :: relhum(pcols,pver)  ! relative humidity
   real(r8) :: icldm(pcols,pver)   ! ice cloud fraction

   real(r8) :: so4_num                               ! so4 aerosol number (#/cm^3)
   real(r8) :: soot_num                              ! soot (hydrophilic) aerosol number (#/cm^3)
   real(r8) :: dst1_num,dst2_num,dst3_num,dst4_num   ! dust aerosol number (#/cm^3)
   real(r8) :: dst_num                               ! total dust aerosol number (#/cm^3)
   real(r8) :: wght
   real(r8) :: dmc
   real(r8) :: ssmc
   real(r8) :: oso4_num
   real(r8) :: odst_num
   real(r8) :: osoot_num
   real(r8) :: dso4_num                              ! tuning factor for increased so4
   real(r8) :: ramp                                  ! ---------- " ----------------
   real(r8) :: dust_coarse_fraction                  ! fraction of dust in coarse (a3) mode
   real(r8) :: masslost                              ! [kg/kg] tmp variable for mass lost
   real(r8) :: numberFromSmallDustMode               ! [#/cm3] number of dust activated from small mode

   real(r8) :: subgrid(pcols,pver)
   real(r8) :: trop_pd(pcols,pver)

   ! For pre-existing ice
   real(r8) :: fhom(pcols,pver)    ! how much fraction of cloud can reach Shom
   real(r8) :: wice(pcols,pver)    ! diagnosed Vertical velocity Reduction caused by preexisting ice (m/s), at Shom 
   real(r8) :: weff(pcols,pver)    ! effective Vertical velocity for ice nucleation (m/s); weff=wsubi-wice 
   real(r8) :: INnso4(pcols,pver)   ! #/m3, so4 aerosol number used for ice nucleation
   real(r8) :: INnbc(pcols,pver)    ! #/m3, bc aerosol number used for ice nucleation
   real(r8) :: INndust(pcols,pver)  ! #/m3, dust aerosol number used for ice nucleation
   real(r8) :: INondust(pcols,pver)  ! #/m3, dust aerosol number used for ice nucleation
   real(r8) :: INhet(pcols,pver)    ! #/m3, ice number from het freezing
   real(r8) :: INhom(pcols,pver)    ! #/m3, ice number from hom freezing
   real(r8) :: INFrehom(pcols,pver) !  hom freezing occurence frequency.  1 occur, 0 not occur.
   real(r8) :: INFreIN(pcols,pver)  !  ice nucleation occerence frequency.   1 occur, 0 not occur.

   ! history output for ice nucleation
   real(r8) :: nihf(pcols,pver)  !output number conc of ice nuclei due to heterogenous freezing (1/m3)
   real(r8) :: niimm(pcols,pver) !output number conc of ice nuclei due to immersion freezing (hetero nuc) (1/m3)
   real(r8) :: nidep(pcols,pver) !output number conc of ice nuclei due to deoposion nucleation (hetero nuc) (1/m3)
   real(r8) :: nimey(pcols,pver) !output number conc of ice nuclei due to meyers deposition (1/m3)
   real(r8) :: regm(pcols,pver)  !output temperature thershold for nucleation regime

   real(r8) :: so4_num_ac
   real(r8) :: so4_num_cr

   !-------------------------------------------------------------------------------

   lchnk = state%lchnk
   ncol  = state%ncol
   t     => state%t
   qn    => state%q(:,:,1)
   qc    => state%q(:,:,cldliq_idx)
   qi    => state%q(:,:,cldice_idx)
   ni    => state%q(:,:,numice_idx)
   pmid  => state%pmid

   do k = top_lev, pver
      do i = 1, ncol
         rho(i,k) = pmid(i,k)/(rair*t(i,k))
      end do
   end do

   call physics_ptend_init(ptend, state%psetcols, 'nucleatei', lq=lq) 

   cld_dst_a2 => qqcw_get_field(pbuf, l_dst_a2, lchnk, .true.)
   cld_dst_a3 => qqcw_get_field(pbuf, l_dst_a2, lchnk, .true.) 

   itim_old = pbuf_old_tim_idx()
   call pbuf_get_field(pbuf, ast_idx, ast, start=(/1,1,itim_old/), kount=(/pcols,pver,1/))

   icecldf(:ncol,:pver) = ast(:ncol,:pver)

   ! naai and naai_hom are the outputs from this parameterization
   call pbuf_get_field(pbuf, naai_idx, naai)
   call pbuf_get_field(pbuf, naai_hom_idx, naai_hom)
   naai(1:ncol,1:pver)     = 0._r8  
   naai_hom(1:ncol,1:pver) = 0._r8  

   ! Use the same criteria that is used in chemistry and in CLUBB (for cloud fraction)
   ! to determine whether to use tropospheric or stratospheric settings. Include the
   ! tropopause level so that the cold point tropopause will use the stratospheric values.
   call tropopause_findChemTrop(state, troplev)
   
   if ((nucleate_ice_subgrid .eq. -1._r8) .or. (nucleate_ice_subgrid_strat .eq. -1._r8)) then
      call pbuf_get_field(pbuf, qsatfac_idx, qsatfac)
   end if
   
   trop_pd(:,:) = 0._r8
   
   do k = top_lev, pver
      do i = 1, ncol
         trop_pd(i, troplev(i)) = 1._r8
         
         if (k <= troplev(i)) then
            if (nucleate_ice_subgrid_strat .eq. -1._r8) then
               subgrid(i, k) = 1._r8 / qsatfac(i, k)
            else
               subgrid(i, k) = nucleate_ice_subgrid_strat
            end if
         else
            if (nucleate_ice_subgrid .eq. -1._r8) then
               subgrid(i, k) = 1._r8 / qsatfac(i, k)
            else
               subgrid(i, k) = nucleate_ice_subgrid
            end if
         end if
      end do
   end do


   ! initialize history output fields for ice nucleation
   nihf(1:ncol,1:pver)  = 0._r8  
   niimm(1:ncol,1:pver) = 0._r8  
   nidep(1:ncol,1:pver) = 0._r8 
   nimey(1:ncol,1:pver) = 0._r8 

   if (use_preexisting_ice) then
      fhom(:,:)     = 0.0_r8
      wice(:,:)     = 0.0_r8
      weff(:,:)     = 0.0_r8
      INnso4(:,:)   = 0.0_r8
      INnbc(:,:)    = 0.0_r8
      INndust(:,:)  = 0.0_r8
      INondust(:,:)  = 0.0_r8
      INhet(:,:)    = 0.0_r8
      INhom(:,:)    = 0.0_r8
      INFrehom(:,:) = 0.0_r8
      INFreIN(:,:)  = 0.0_r8
   endif

   do k = top_lev, pver

      ! Get humidity and saturation vapor pressures
      call qsat_water(t(:ncol,k), pmid(:ncol,k), &
           es(:ncol), qs(:ncol), gam=gammas(:ncol))

      do i = 1, ncol

         relhum(i,k) = qn(i,k)/qs(i)

         ! get cloud fraction, check for minimum
         icldm(i,k) = max(icecldf(i,k), mincld)

      end do
   end do


   do k = top_lev, pver
      do i = 1, ncol

         if (t(i,k) < tmelt - 5._r8) then

            ! compute aerosol number for so4, soot, and dust with units #/cm^3
            so4_num  = 0._r8
            soot_num = 0._r8
            dst1_num = 0._r8
            dst2_num = 0._r8
            dst3_num = 0._r8
            dst4_num = 0._r8
            dst_num  = 0._r8

            if (clim_modal_aero) then
               !For modal aerosols, assume for the upper troposphere:
               ! soot = accumulation mode
               ! sulfate = aiken mode
               ! dust = coarse mode
               ! since modal has internal mixtures.
               soot_num =  numberConcentration(i,k,MODE_IDX_OMBC_INTMIX_COAT_AIT)*1.0e-6_r8

               dst_num = (numberConcentration(i,k,MODE_IDX_DST_A2) &
                          + numberConcentration(i,k,MODE_IDX_DST_A3))*1.0e-6_r8
               !Oslo aerosols have two modes.. Need mode-fractions
               dust_coarse_fraction = numberConcentration(i,k,MODE_IDX_DST_A3)*1.e-6_r8 / (dst_num+1.e-100_r8)
   

               so4_num = (numberConcentration(i,k,MODE_IDX_SO4_AC))*1.0e-6_r8 

            end if !clim modal aero
            ! *** Turn off soot nucleation ***
            soot_num = 0.0_r8

            call nucleati( &
               wsubi(i,k), t(i,k), pmid(i,k), relhum(i,k), icldm(i,k),   &
               qc(i,k), qi(i,k), ni(i,k), rho(i,k),                      &
               so4_num, dst_num, soot_num, subgrid(i,k),                 &
               naai(i,k), nihf(i,k), niimm(i,k), nidep(i,k), nimey(i,k), &
               wice(i,k), weff(i,k), fhom(i,k), regm(i,k),               &
               oso4_num, odst_num, osoot_num)

            ! Move aerosol used for nucleation from interstial to cloudborne, 
            ! otherwise the same coarse mode aerosols will be available again
            ! in the next timestep and will supress homogeneous freezing.
            if (use_preexisting_ice) then

               numberFromSmallDustMode = 0.0_r8
               
               !Assume the coarse aerosols were activated first
               !so only remove small ones if more than large ones are activated
               if(odst_num .gt. dst_num*dust_coarse_fraction)then

                  !A2-mode
                  numberFromSmallDustMode = odst_num - dst_num*dust_coarse_fraction

                  masslost = (odst_num               & !all removed
                     - dst_num*dust_coarse_fraction) & !fraction to coarse mode
                     / volumeToNumber(MODE_IDX_DST_A2) &
                     * rhopart(l_dst_a2) & 
                     /rho(i,k)*1e6_r8 

                  ptend%q(i,k,l_dst_a2) = -masslost*icldm(i,k)/ dtime
                  cld_dst_a2(i,k) = cld_dst_a2(i,k) + masslost*icldm(i,k)

               end if

               ! Coarse mode (is always lost)  
               masslost = (odst_num - numberFromSmallDustMode) &
                  / volumeToNumber(MODE_IDX_DST_A3) &
                  * rhopart(l_dst_a3) & 
                  / rho(i,k)*1e6_r8 
                 
                  ptend%q(i,k,l_dst_a3) = -masslost * icldm(i,k) / dtime
                  cld_dst_a3(i,k) = cld_dst_a3(i,k) + masslost*icldm(i,k)

            end if

            !Oslo aerosols do not have explicit treatment of coarse sulfate
            so4_num_cr = 0.0_r8

            ! Liu&Penner does not generate enough nucleation in the polar winter
            ! stratosphere, which affects surface area density, dehydration and
            ! ozone chemistry. Part of this is that there are a larger number of
            ! particles in the accumulation mode than in the Aitken mode. In volcanic
            ! periods, the coarse mode may also be important. As a short
            ! term work around, include the accumulation and coarse mode particles
            ! and assume a larger fraction of the sulfates nucleate in the polar
            ! stratosphere.
            !
            ! Do not include the tropopause level, as stratospheric aerosols
            ! only exist above the tropopause level.
            !
            ! NOTE: This may still not represent the proper particles that
            ! participate in nucleation, because it doesn't include STS and NAT
            ! particles. It may not represent the proper saturation threshold for
            ! nucleation, and wsubi from CLUBB is probably not representative of
            ! wave driven varaibility in the polar stratosphere.
            if (nucleate_ice_use_troplev) then 
              if ((k < troplev(i)) .and. (nucleate_ice_strat > 0._r8)) then
                 if (oso4_num > 0._r8) then
                    so4_num_ac = so4_num*rho(i,k)*1.0e-6_r8 !This is maximum sulfate which can activate
                    !! NCAR/MAM4-version
                    !!!so4_num_ac = num_accum(i,k)*rho(i,k)*1.0e-6_r8
                    !! NCAR/MAM4-version
                    dso4_num = max(0._r8, (nucleate_ice_strat * (so4_num_cr + so4_num_ac)) - oso4_num) * 1e6_r8 / rho(i,k)
                    naai(i,k) = naai(i,k) + dso4_num
                    nihf(i,k) = nihf(i,k) + dso4_num
                 end if
              end if
            else
            
              ! This maintains backwards compatibility with the previous version.
              if (pmid(i,k) <= 12500._r8 .and. pmid(i,k) > 100._r8 .and. abs(state%lat(i)) >= 60._r8 * pi / 180._r8) then
                 ramp = 1._r8 - min(1._r8, max(0._r8, (pmid(i,k) - 10000._r8) / 2500._r8))

                 if (oso4_num > 0._r8) then
                    dso4_num = (max(oso4_num, ramp * nucleate_ice_strat * so4_num) - oso4_num) * 1e6_r8 / rho(i,k)
                    naai(i,k) = naai(i,k) + dso4_num
                    nihf(i,k) = nihf(i,k) + dso4_num
                 end if
              end if
            end if

            naai_hom(i,k) = nihf(i,k)

            ! output activated ice (convert from #/kg -> #/m3)
            nihf(i,k)     = nihf(i,k) *rho(i,k)
            niimm(i,k)    = niimm(i,k)*rho(i,k)
            nidep(i,k)    = nidep(i,k)*rho(i,k)
            nimey(i,k)    = nimey(i,k)*rho(i,k)

            if (use_preexisting_ice) then
               INnso4(i,k) =so4_num*1e6_r8  ! (convert from #/cm3 -> #/m3)
               INnbc(i,k)  =soot_num*1e6_r8
               INndust(i,k)=dst_num*1e6_r8
               INondust(i,k)=odst_num*1e6_r8
               INFreIN(i,k)=1.0_r8          ! 1,ice nucleation occur
               INhet(i,k) = (niimm(i,k) + nidep(i,k))   ! #/m3, nimey not in cirrus
               INhom(i,k) = nihf(i,k)                 ! #/m3
               if (INhom(i,k).gt.1e3_r8)   then ! > 1/L
                  INFrehom(i,k)=1.0_r8       ! 1, hom freezing occur
               endif

               ! exclude  no ice nucleaton 
               if ((INFrehom(i,k) < 0.5_r8) .and. (INhet(i,k) < 1.0_r8))   then   
                  INnso4(i,k) =0.0_r8
                  INnbc(i,k)  =0.0_r8
                  INndust(i,k)=0.0_r8
                  INondust(i,k)=0.0_r8
                  INFreIN(i,k)=0.0_r8
                  INhet(i,k) = 0.0_r8
                  INhom(i,k) = 0.0_r8
                  INFrehom(i,k)=0.0_r8    
                  wice(i,k) = 0.0_r8
                  weff(i,k) = 0.0_r8 
                  fhom(i,k) = 0.0_r8
               endif
            end if

         end if
      end do
   end do


   call outfld('NIHF',   nihf, pcols, lchnk)
   call outfld('NIIMM', niimm, pcols, lchnk)
   call outfld('NIDEP', nidep, pcols, lchnk)
   call outfld('NIMEY', nimey, pcols, lchnk)
   call outfld('NIREGM', regm, pcols, lchnk)
   call outfld('NISUBGRID', subgrid, pcols, lchnk)
   call outfld('NITROP_PD', trop_pd, pcols, lchnk)

   if (use_preexisting_ice) then
      call outfld( 'fhom' , fhom, pcols, lchnk)
      call outfld( 'WICE' , wice, pcols, lchnk)
      call outfld( 'WEFF' , weff, pcols, lchnk)
      call outfld('INnso4  ',INnso4 , pcols,lchnk)
      call outfld('INnbc   ',INnbc  , pcols,lchnk)
      call outfld('INndust ',INndust, pcols,lchnk)
      call outfld('INondust ',INondust, pcols,lchnk)
      call outfld('INhet   ',INhet  , pcols,lchnk)
      call outfld('INhom   ',INhom  , pcols,lchnk)
      call outfld('INFrehom',INFrehom,pcols,lchnk)
      call outfld('INFreIN ',INFreIN, pcols,lchnk)
   end if

end subroutine nucleate_ice_oslo_calc

!================================================================================================

end module nucleate_ice_oslo
