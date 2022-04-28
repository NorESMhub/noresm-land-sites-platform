module ndrop

!---------------------------------------------------------------------------------
! Purpose:
!   CAM Interface for droplet activation by modal aerosols
!
! ***N.B.*** This module is currently hardcoded to recognize only the modes that
!            affect the climate calculation.  This is implemented by using list
!            index 0 in all the calls to rad_constituent interfaces.
!---------------------------------------------------------------------------------

use shr_kind_mod,     only: r8 => shr_kind_r8
use spmd_utils,       only: masterproc
use ppgrid,           only: pcols, pver, pverp
use physconst,        only: pi, rhoh2o, mwh2o, r_universal, rh2o, &
                            gravit, latvap, cpair, epsilo, rair
use constituents,     only: pcnst, cnst_get_ind, cnst_name, cnst_spec_class_gas, cnst_species_class
use physics_types,    only: physics_state, physics_ptend, physics_ptend_init
use physics_buffer,   only: physics_buffer_desc, pbuf_get_index, pbuf_get_field

use wv_saturation,    only: qsat
use phys_control,     only: phys_getopts
use ref_pres,         only: top_lev => trop_cloud_top_lev
use shr_spfn_mod,     only: erf => shr_spfn_erf
use rad_constituents, only: rad_cnst_get_info, rad_cnst_get_mode_num, rad_cnst_get_aer_mmr, &
                            rad_cnst_get_aer_props, rad_cnst_get_mode_props,                &
                            rad_cnst_get_mam_mmr_idx, rad_cnst_get_mode_num_idx
use cam_history,      only: addfld, add_default, horiz_only, fieldname_len, outfld
use cam_abortutils,   only: endrun
use cam_logfile,      only: iulog
!++ MH_2015/09/09
use phys_control,   only: use_hetfrz_classnuc
!-- MH_2015/09/09

#ifdef OSLO_AERO
!++oslo
use aerosoldef 
use parmix_progncdnc
use oslo_utils, only: calculateNumberMedianRadius
!--oslo
#endif


implicit none
private
save

public ndrop_init, dropmixnuc, activate_modal, loadaer

#ifndef OSLO_AERO
real(r8), allocatable :: alogsig(:)     ! natl log of geometric standard dev of aerosol
real(r8), allocatable :: exp45logsig(:)
real(r8), allocatable, target :: f1(:)          ! abdul-razzak functions of width
real(r8), allocatable, target :: f2(:)          ! abdul-razzak functions of width
#endif

real(r8) :: t0            ! reference temperature
real(r8) :: aten
real(r8) :: surften       ! surface tension of water w/respect to air (N/m)
real(r8) :: alog2, alog3, alogaten
real(r8) :: third, twothird, sixth, zero
real(r8) :: sq2, sqpi

! CCN diagnostic fields
!integer,  parameter :: psat=6    ! number of supersaturations to calc ccn concentration
!real(r8), parameter :: supersat(psat)= & ! supersaturation (%) to determine ccn concentration
!                       (/ 0.02_r8, 0.05_r8, 0.1_r8, 0.2_r8, 0.5_r8, 1.0_r8 /)
!character(len=8) :: ccn_name(psat)= &
!                    (/'CCN1','CCN2','CCN3','CCN4','CCN5','CCN6'/)
!akc6+
integer,  parameter :: psat=7    ! number of supersaturations to calc ccn concentration
real(r8), parameter :: supersat(psat)= & ! supersaturation (%) to determine ccn concentration
                       (/ 0.02_r8, 0.05_r8, 0.1_r8, 0.15_r8, 0.2_r8, 0.5_r8, 1.0_r8 /)
character(len=8) :: ccn_name(psat)= &
                    (/'CCN1','CCN2','CCN3','CCN4','CCN5','CCN6','CCN7'/)
!akc6-

! indices in state and pbuf structures
integer :: numliq_idx = -1
integer :: kvh_idx    = -1

! description of modal aerosols
integer               :: ntot_amode     ! number of aerosol modes
integer,  allocatable :: nspec_amode(:) ! number of chemical species in each aerosol mode
real(r8), allocatable :: sigmag_amode(:)! geometric standard deviation for each aerosol mode
real(r8), allocatable :: dgnumlo_amode(:)
real(r8), allocatable :: dgnumhi_amode(:)
real(r8), allocatable :: voltonumblo_amode(:)
real(r8), allocatable :: voltonumbhi_amode(:)

logical :: history_aerosol      ! Output the MAM aerosol tendencies
character(len=fieldname_len), allocatable :: fieldname(:)    ! names for drop nuc tendency output fields
character(len=fieldname_len), allocatable :: fieldname_cw(:) ! names for drop nuc tendency output fields

! local indexing for MAM
integer, allocatable :: mam_idx(:,:) ! table for local indexing of modal aero number and mmr
integer :: ncnst_tot                  ! total number of mode number conc + mode species

! Indices for MAM species in the ptend%q array.  Needed for prognostic aerosol case.
integer, allocatable :: mam_cnst_idx(:,:)

#ifdef OSLO_AERO
logical :: tendencyCounted(pcnst) = .false. ! set flags true for constituents with non-zero tendencies
integer :: n_aerosol_tracers
integer :: aerosolTracerList(pcnst)        !List where indexes 1...n_aerosol_tracers are the indexes in pcnst
                                           !..something like (/ l_so4_a1, l_bc_a, .../)etc
integer :: inverseAerosolTracerList(pcnst) !List where you can back the place in aerosolTracerList if you know the 
                                           !tracer index. So in the example above inverseAerosolTracerList(l_so4_a1) = 1
#endif

! ptr2d_t is used to create arrays of pointers to 2D fields
type ptr2d_t
   real(r8), pointer :: fld(:,:)
end type ptr2d_t

! modal aerosols
logical :: prog_modal_aero     ! true when modal aerosols are prognostic
logical :: lq(pcnst) = .false. ! set flags true for constituents with non-zero tendencies
                               ! in the ptend object

!===============================================================================
contains
!===============================================================================

subroutine ndrop_init

   integer  :: ii, l, lptr, m, mm
   integer  :: nspec_max            ! max number of species in a mode
   character(len=32)   :: tmpname
   character(len=32)   :: tmpname_cw
   character(len=128)  :: long_name
   character(len=8)    :: unit
   logical :: history_amwg         ! output the variables used by the AMWG diag package
#ifdef OSLO_AERO
   character(len=10)                :: modeString
   character(len=20)                :: varname
#endif 

   !-------------------------------------------------------------------------------

   ! get indices into state%q and pbuf structures
   call cnst_get_ind('NUMLIQ', numliq_idx)

   kvh_idx      = pbuf_get_index('kvh')

   zero     = 0._r8
   third    = 1._r8/3._r8
   twothird = 2._r8*third
   sixth    = 1._r8/6._r8
   sq2      = sqrt(2._r8)
   sqpi     = sqrt(pi)

   t0       = 273._r8
   surften  = 0.076_r8
   aten     = 2._r8*mwh2o*surften/(r_universal*t0*rhoh2o)
   alogaten = log(aten)
   alog2    = log(2._r8)
   alog3    = log(3._r8)

   ! get info about the modal aerosols
   ! get ntot_amode
#ifdef OSLO_AERO
   ntot_amode = nmodes !from opttab
#else
   call rad_cnst_get_info(0, nmodes=ntot_amode)
#endif
   allocate( &
      nspec_amode(ntot_amode),  &
      sigmag_amode(ntot_amode), &
      dgnumlo_amode(ntot_amode), &
      dgnumhi_amode(ntot_amode), &
#ifndef OSLO_AERO
      alogsig(ntot_amode),      &
      exp45logsig(ntot_amode),  &
      f1(ntot_amode),           &
      f2(ntot_amode),           &
#endif
      voltonumblo_amode(ntot_amode), &
      voltonumbhi_amode(ntot_amode)  )

#ifdef OSLO_AERO
   do m = 1,ntot_amode
      nspec_amode(m) = getNumberOfTracersInMode(m)
   enddo
#else 
   do m = 1, ntot_amode
      ! use only if width of size distribution is prescribed

      ! get mode info
      call rad_cnst_get_info(0, m, nspec=nspec_amode(m))

      ! get mode properties
      call rad_cnst_get_mode_props(0, m, sigmag=sigmag_amode(m),  &
         dgnumhi=dgnumhi_amode(m), dgnumlo=dgnumlo_amode(m))

      alogsig(m)     = log(sigmag_amode(m))
      exp45logsig(m) = exp(4.5_r8*alogsig(m)*alogsig(m))
      f1(m)          = 0.5_r8*exp(2.5_r8*alogsig(m)*alogsig(m))
      f2(m)          = 1._r8 + 0.25_r8*alogsig(m)

      voltonumblo_amode(m) = 1._r8 / ( (pi/6._r8)*                          &
                             (dgnumlo_amode(m)**3._r8)*exp(4.5_r8*alogsig(m)**2._r8) )
      voltonumbhi_amode(m) = 1._r8 / ( (pi/6._r8)*                          &
                             (dgnumhi_amode(m)**3._r8)*exp(4.5_r8*alogsig(m)**2._r8) )
   end do
#endif 
   ! Init the table for local indexing of mam number conc and mmr.
   ! This table uses species index 0 for the number conc.

   ! Find max number of species in all the modes, and the total
   ! number of mode number concentrations + mode species
   nspec_max = nspec_amode(1)
   ncnst_tot = nspec_amode(1) + 1
   do m = 2, ntot_amode
      nspec_max = max(nspec_max, nspec_amode(m))
      ncnst_tot = ncnst_tot + nspec_amode(m) + 1
   end do

   allocate( &
      mam_idx(ntot_amode,0:nspec_max),      &
      mam_cnst_idx(ntot_amode,0:nspec_max), &
      fieldname(ncnst_tot),                 &
      fieldname_cw(ncnst_tot)               )

   ! Local indexing compresses the mode and number/mass indicies into one index.
   ! This indexing is used by the pointer arrays used to reference state and pbuf
   ! fields.
   ii = 0
   do m = 1, ntot_amode
      do l = 0, nspec_amode(m)
         ii = ii + 1
         mam_idx(m,l) = ii
      end do
   end do

   ! Add dropmixnuc tendencies for all modal aerosol species

   call phys_getopts(history_amwg_out = history_amwg, &
                     history_aerosol_out = history_aerosol, &
                     prog_modal_aero_out=prog_modal_aero)

#ifdef OSLO_AERO
   prog_modal_aero = .TRUE.
   n_aerosol_tracers = getNumberOfAerosolTracers()
   call fillAerosolTracerList(aerosolTracerList)
   call fillInverseAerosolTracerList(aerosolTracerList, inverseAerosolTracerList, n_aerosol_tracers)
   do ii=1,n_aerosol_tracers
      print*, "aerosolTracerList", ii, aerosolTracerList(ii), inverseAerosolTracerList(aerosolTracerList(ii))
   end do
#endif

#ifdef OSLO_AERO
   lq(:)=.FALSE.  !Initialize

   !Set up tendencies for tracers (output)
   do m=1,ntot_amode
      do l=1,nspec_amode(m)
         lptr = getTracerIndex(m,l,.false.)

         if(.NOT. lq(lptr))then
            !add dropmixnuc tendencies
            mm=mam_idx(m,l)
            fieldname(mm)=trim(cnst_name(lptr))//"_mixnuc1"
            fieldname_cw(mm)=trim(getCloudTracerName(lptr))//"_mixnuc1"

            long_name = trim(fieldname(mm)) // ' dropmixnuc column tendency'
            call addfld(trim(fieldname(mm)), horiz_only ,'A', "kg/m2/s",long_name)

            long_name = trim(fieldname_cw(mm)) // ' dropmixnuc column tendency'
            call addfld(trim(fieldname_cw(mm)), horiz_only, 'A', "kg/m2/s",long_name)

            if (history_aerosol) then
               call add_default(trim(fieldname(mm)), 1, ' ')
               call add_default(trim(fieldname_cw(mm)),1,' ')
            endif

            !Do tendencies of this tracer
            lq(lptr)=.TRUE.
         endif
      enddo
   enddo
   do m=1,ntot_amode
      modeString="  "
      write(modeString,"(I2)"),m
      if(m .lt. 10) modeString="0"//adjustl(modeString)
      varName = "NMR"//trim(modeString)
      call addfld(varName, (/ 'lev' /),'A', 'm  ', 'number median radius mode '//modeString)
      if(history_aerosol)call add_default(varName, 1, ' ')
      varName = "NCONC"//trim(modeString)
      call addfld(varName, (/ 'lev' /),'A', '#/m3  ', 'number concentration mode '//modeString)
      if(history_aerosol)call add_default(varName, 1, ' ')
      varName = "VCONC"//trim(modeString)
      call addfld(varName, (/ 'lev' /),'A',  'm3/m3  ','volume concentration mode '//modeString)
      if(history_aerosol)call add_default(varName, 1, ' ')
      varName = "SIGMA"//trim(modeString)
      call addfld(varName, (/ 'lev' /),'A', '-','Std. dev. mode '//modeString)
      if(history_aerosol)call add_default(varName, 1, ' ')
      varName = "HYGRO"//trim(modeString)
      call addfld(varName, (/ 'lev' /),'A','-','Hygroscopicity '//modeString)
      if(history_aerosol)call add_default(varName, 1, ' ')
   end do
#else
   do m = 1, ntot_amode
      do l = 0, nspec_amode(m)   ! loop over number + chem constituents

         mm = mam_idx(m,l)

         unit = 'kg/m2/s'
         if (l == 0) then   ! number
            unit = '#/m2/s'
         end if

         if (l == 0) then   ! number
            call rad_cnst_get_info(0, m, num_name=tmpname, num_name_cw=tmpname_cw)
         else
            call rad_cnst_get_info(0, m, l, spec_name=tmpname, spec_name_cw=tmpname_cw)
         end if

         fieldname(mm)    = trim(tmpname) // '_mixnuc1'
         fieldname_cw(mm) = trim(tmpname_cw) // '_mixnuc1'

         if (prog_modal_aero) then

            ! To set tendencies in the ptend object need to get the constituent indices
            ! for the prognostic species
            if (l == 0) then   ! number
               call rad_cnst_get_mode_num_idx(m, lptr)
            else
               call rad_cnst_get_mam_mmr_idx(m, l, lptr)
            end if
            mam_cnst_idx(m,l) = lptr
            lq(lptr)          = .true.

            ! Add tendency fields to the history only when prognostic MAM is enabled.
            long_name = trim(tmpname) // ' dropmixnuc mixnuc column tendency'
            call addfld(fieldname(mm),    horiz_only, 'A', unit, long_name)

            long_name = trim(tmpname_cw) // ' dropmixnuc mixnuc column tendency'
            call addfld(fieldname_cw(mm), horiz_only, 'A', unit, long_name)

            if (history_aerosol) then
               call add_default(fieldname(mm), 1, ' ')
               call add_default(fieldname_cw(mm), 1, ' ')
            end if



         end if
            
      end do
   end do

#endif

!   call addfld('CCN1',(/ 'lev' /), 'A','#/cm3','CCN concentration at S=0.02%')
!   call addfld('CCN2',(/ 'lev' /), 'A','#/cm3','CCN concentration at S=0.05%')
!   call addfld('CCN3',(/ 'lev' /), 'A','#/cm3','CCN concentration at S=0.1%')
!   call addfld('CCN4',(/ 'lev' /), 'A','#/cm3','CCN concentration at S=0.2%')
!   call addfld('CCN5',(/ 'lev' /), 'A','#/cm3','CCN concentration at S=0.5%')
!   call addfld('CCN6',(/ 'lev' /), 'A','#/cm3','CCN concentration at S=1.0%')
!akc6+
   call addfld('CCN1',(/ 'lev' /), 'A','#/cm3','CCN concentration at S=0.02%')
   call addfld('CCN2',(/ 'lev' /), 'A','#/cm3','CCN concentration at S=0.05%')
   call addfld('CCN3',(/ 'lev' /), 'A','#/cm3','CCN concentration at S=0.1%')
   call addfld('CCN4',(/ 'lev' /), 'A','#/cm3','CCN concentration at S=0.15%')
   call addfld('CCN5',(/ 'lev' /), 'A','#/cm3','CCN concentration at S=0.2%')
   call addfld('CCN6',(/ 'lev' /), 'A','#/cm3','CCN concentration at S=0.5%')
   call addfld('CCN7',(/ 'lev' /), 'A','#/cm3','CCN concentration at S=1.0%')
!akc6-

#ifdef OSLO_AERO
   if(history_aerosol)then
      do l = 1, psat 
         call add_default(ccn_name(l), 1, ' ')
      enddo
   end if
#endif

   call addfld('WTKE',     (/ 'lev' /), 'A', 'm/s', 'Standard deviation of updraft velocity')
   call addfld('NDROPMIX', (/ 'lev' /), 'A', '#/kg/s', 'Droplet number mixing')
   call addfld('NDROPSRC', (/ 'lev' /), 'A', '#/kg/s', 'Droplet number source')
   call addfld('NDROPSNK', (/ 'lev' /), 'A', '#/kg/s', 'Droplet number loss by microphysics')
   call addfld('NDROPCOL', horiz_only,  'A', '#/m2', 'Column droplet number')

#ifndef OSLO_AERO

   ! set the add_default fields  
   if (history_amwg) then
      call add_default('CCN3', 1, ' ')
   endif

   if (history_aerosol .and. prog_modal_aero) then
     do m = 1, ntot_amode
        do l = 0, nspec_amode(m)   ! loop over number + chem constituents
           mm = mam_idx(m,l)
           if (l == 0) then   ! number
              call rad_cnst_get_info(0, m, num_name=tmpname, num_name_cw=tmpname_cw)
           else
              call rad_cnst_get_info(0, m, l, spec_name=tmpname, spec_name_cw=tmpname_cw)
           end if
           fieldname(mm)    = trim(tmpname) // '_mixnuc1'
           fieldname_cw(mm) = trim(tmpname_cw) // '_mixnuc1'
        end do
     end do
   endif

#endif

end subroutine ndrop_init

!===============================================================================

subroutine dropmixnuc( &
   state, ptend, dtmicro, pbuf, wsub,          &  ! Input
   cldn, cldo, cldliqf,                                &
   !++ MH_2015/09/07
   hasAerosol, &
   CProcessModes, f_c, f_bc, f_aq, f_so4_cond, &
   f_soa,                                      &
   cam, f_acm, f_bcm, f_aqm, f_so4_condm,      &
   f_soam,                                     &
   numberConcentration, volumeConcentration,   &
   hygroscopicity, lnsigma,                    &
   !-- MH_2015/09/07
   tendnd,                                     &  ! Output
   !++ MH_2015/04/10
   fn_in,                                      & 
   from_spcam )
   !-- MH_2015/04/10

   ! vertical diffusion and nucleation of cloud droplets
   ! assume cloud presence controlled by cloud fraction
   ! doesn't distinguish between warm, cold clouds

   ! arguments
   type(physics_state), target, intent(in)    :: state
   type(physics_ptend),         intent(out)   :: ptend
   real(r8),                    intent(in)    :: dtmicro     ! time step for microphysics (s)

   type(physics_buffer_desc), pointer :: pbuf(:)

   ! arguments
   real(r8), intent(in) :: wsub(pcols,pver)    ! subgrid vertical velocity
   real(r8), intent(in) :: cldn(pcols,pver)    ! cloud fraction
   real(r8), intent(in) :: cldo(pcols,pver)    ! cloud fraction on previous time step
   real(r8), intent(in) :: cldliqf(pcols,pver) ! liquid cloud fraction (liquid / (liquid + ice))
   logical,  intent(in),optional :: from_spcam ! value insignificant - if variable present, is called from spcam

!++ MH_2015/09/07
   logical, intent(in)  :: hasAerosol(pcols, pver, nmodes)
   real(r8), intent(in) :: CProcessModes(pcols,pver)
   real(r8), intent(in) :: cam(pcols,pver,nbmodes)
   real(r8), intent(in) :: f_c(pcols,pver)
   real(r8), intent(in) :: f_aq(pcols,pver)
   real(r8), intent(in) :: f_bc(pcols,pver)
   real(r8), intent(in) :: f_so4_cond(pcols,pver)
   real(r8), intent(in) :: f_soa(pcols,pver)
   real(r8), intent(in) :: f_acm(pcols,pver, nbmodes)
   real(r8), intent(in) :: f_bcm(pcols,pver, nbmodes)
   real(r8), intent(in) :: f_aqm(pcols, pver, nbmodes)
   real(r8), intent(in) :: f_so4_condm(pcols, pver, nbmodes)            !Needed in "get component fraction
   real(r8), intent(in) :: f_soam(pcols,pver,nbmodes)               
   real(r8), intent(in) :: numberConcentration(pcols,pver,0:nmodes) ![#/m3] number concentraiton
   real(r8), intent(in) :: volumeConcentration(pcols,pver,nmodes)   ![m3/m3] volume concentration
   real(r8), intent(in) :: hygroscopicity(pcols,pver,nmodes)        ![-] hygroscopicity
   real(r8), intent(in) :: lnsigma(pcols,pver,nmodes)               ![-] log(base e) sigma
!-- MH_2015/09/07

   ! output arguments
   real(r8), intent(out) :: tendnd(pcols,pver) ! change in droplet number concentration (#/kg/s)

   !--------------------Local storage-------------------------------------

   integer  :: lchnk               ! chunk identifier
   integer  :: ncol                ! number of columns

   real(r8), pointer :: ncldwtr(:,:) ! droplet number concentration (#/kg)
   real(r8), pointer :: temp(:,:)    ! temperature (K)
   real(r8), pointer :: omega(:,:)   ! vertical velocity (Pa/s)
   real(r8), pointer :: pmid(:,:)    ! mid-level pressure (Pa)
   real(r8), pointer :: pint(:,:)    ! pressure at layer interfaces (Pa)
   real(r8), pointer :: pdel(:,:)    ! pressure thickess of layer (Pa)
   real(r8), pointer :: rpdel(:,:)   ! inverse of pressure thickess of layer (/Pa)
   real(r8), pointer :: zm(:,:)      ! geopotential height of level (m)

   real(r8), pointer :: kvh(:,:)     ! vertical diffusivity (m2/s)

   type(ptr2d_t), allocatable :: raer(:)     ! aerosol mass, number mixing ratios
   type(ptr2d_t), allocatable :: qqcw(:)
   real(r8) :: raertend(pver)  ! tendency of aerosol mass, number mixing ratios
   real(r8) :: qqcwtend(pver)  ! tendency of cloudborne aerosol mass, number mixing ratios


   real(r8), parameter :: zkmin = 0.01_r8, zkmax = 100._r8
   real(r8), parameter :: wmixmin = 0.1_r8        ! minimum turbulence vertical velocity (m/s)
   real(r8) :: sq2pi

   integer  :: i, k, l, m, mm, n
   integer  :: km1, kp1
   integer  :: nnew, nsav, ntemp
   integer  :: lptr
   integer  :: nsubmix, nsubmix_bnd
   integer, save :: count_submix(100)
   integer  :: phase ! phase of aerosol

   real(r8) :: arg
   real(r8) :: dtinv
   real(r8) :: dtmin, tinv, dtt
   real(r8) :: lcldn(pcols,pver)
   real(r8) :: lcldo(pcols,pver)

   real(r8) :: zs(pver) ! inverse of distance between levels (m)
   real(r8) :: qcld(pver) ! cloud droplet number mixing ratio (#/kg)
   real(r8) :: qncld(pver)     ! droplet number nucleated on cloud boundaries
   real(r8) :: srcn(pver)       ! droplet source rate (/s)
   real(r8) :: cs(pcols,pver)      ! air density (kg/m3)
   real(r8) :: csbot(pver)       ! air density at bottom (interface) of layer (kg/m3)
   real(r8) :: csbot_cscen(pver) ! csbot(i)/cs(i,k)
   real(r8) :: dz(pcols,pver)      ! geometric thickness of layers (m)

   real(r8) :: wtke(pcols,pver)     ! turbulent vertical velocity at base of layer k (m/s)
   real(r8) :: wtke_cen(pcols,pver) ! turbulent vertical velocity at center of layer k (m/s)
   real(r8) :: wbar, wmix, wmin, wmax

   real(r8) :: zn(pver)   ! g/pdel (m2/g) for layer
   real(r8) :: flxconv    ! convergence of flux into lowest layer

   real(r8) :: wdiab           ! diabatic vertical velocity
   real(r8) :: ekd(pver)       ! diffusivity for droplets (m2/s)
   real(r8) :: ekk(0:pver)     ! density*diffusivity for droplets (kg/m3 m2/s)
   real(r8) :: ekkp(pver)      ! zn*zs*density*diffusivity
   real(r8) :: ekkm(pver)      ! zn*zs*density*diffusivity

   real(r8) :: dum, dumc
   real(r8) :: tmpa
   real(r8) :: dact
   real(r8) :: fluxntot         ! (#/cm2/s)
   real(r8) :: dtmix
   real(r8) :: alogarg
   real(r8) :: overlapp(pver), overlapm(pver) ! cloud overlap

   real(r8) :: nsource(pcols,pver)            ! droplet number source (#/kg/s)
   real(r8) :: ndropmix(pcols,pver)           ! droplet number mixing (#/kg/s)
   real(r8) :: ndropcol(pcols)               ! column droplet number (#/m2)
   real(r8) :: cldo_tmp, cldn_tmp
   real(r8) :: tau_cld_regenerate
   real(r8) :: zeroaer(pver)
   real(r8) :: taumix_internal_pver_inv ! 1/(internal mixing time scale for k=pver) (1/s)


   real(r8), allocatable :: nact(:,:)  ! fractional aero. number  activation rate (/s)
   real(r8), allocatable :: mact(:,:)  ! fractional aero. mass    activation rate (/s)

   real(r8), allocatable :: raercol(:,:,:)    ! single column of aerosol mass, number mixing ratios
   real(r8), allocatable :: raercol_cw(:,:,:) ! same as raercol but for cloud-borne phase
#ifdef OSLO_AERO
   !to avoid excessive calls to boundary layer scheme
   real(r8), allocatable :: raercol_tracer(:,:,:)
   real(r8), allocatable :: raercol_cw_tracer(:,:,:)
   real(r8), allocatable :: mact_tracer(:,:)
   real(r8), allocatable :: mfullact_tracer(:,:)
#endif

   real(r8) :: na(pcols), va(pcols), hy(pcols)
   real(r8), allocatable :: naermod(:)  ! (1/m3)
   real(r8), allocatable :: hygro(:)    ! hygroscopicity of aerosol mode
   real(r8), allocatable :: vaerosol(:) ! interstit+activated aerosol volume conc (cm3/cm3)

   real(r8) :: source(pver)

!++ MH_2015/04/10
   real(r8), allocatable :: fn(:)              ! activation fraction for aerosol number
   real(r8), intent(out) :: fn_in(pcols,pver,0:nmodes)
!-- MH_2015/04/10
   real(r8), allocatable :: fm(:)              ! activation fraction for aerosol mass

   real(r8), allocatable :: fluxn(:)           ! number  activation fraction flux (cm/s)
   real(r8), allocatable :: fluxm(:)           ! mass    activation fraction flux (cm/s)
   real(r8)              :: flux_fullact(pver) ! 100%    activation fraction flux (cm/s)
   !     note:  activation fraction fluxes are defined as 
   !     fluxn = [flux of activated aero. number into cloud (#/cm2/s)]
   !           / [aero. number conc. in updraft, just below cloudbase (#/cm3)]


   real(r8), allocatable :: coltend(:,:)       ! column tendency for diagnostic output
   real(r8), allocatable :: coltend_cw(:,:)    ! column tendency
   real(r8) :: ccn(pcols,pver,psat)    ! number conc of aerosols activated at supersat

   !for gas species turbulent mixing
   real(r8), pointer :: rgas(:, :, :)
   real(r8), allocatable :: rgascol(:, :, :)
   real(r8), allocatable :: coltendgas(:)
   real(r8) :: zerogas(pver)
   character*200 fieldnamegas

   logical  :: called_from_spcam
   !-------------------------------------------------------------------------------
#ifdef OSLO_AERO
   real(r8)       :: numberMedianRadius(pcols,pver,nmodes)
   real(r8)       :: sigma(pcols,pver,nmodes)                 ![-] sigma
   real(r8)       :: constituentFraction
   !++ MH_2015/04/10
   real(r8)       :: volumeCore(pcols,pver,nmodes)
   real(r8)       :: volumeCoat(pcols,pver,nmodes)
   !-- MH_2015/04/10
   integer                              :: tracerIndex
   integer                              :: cloudTracerIndex
   integer                              :: kcomp
   integer                              :: speciesMap(nmodes)
   !++ MH_2015/04/10
!   real(r8)                             :: fn_tmp(pcols,pver,nmodes)
   real(r8), allocatable :: fn_tmp(:), fm_tmp(:)
   !-- MH_2015/04/10
   real(r8), allocatable :: fluxn_tmp(:), fluxm_tmp(:)
   real(r8)                             :: componentFraction
   real(r8)                             :: componentFractionOK(pver,nmodes,pcnst)
   real(r8)                             :: sumFraction
   logical                              :: alert
   real(r8), dimension(pver, pcnst) :: massBalance
   real(r8), dimension(pver, pcnst) :: newMass
   real(r8), dimension(pver,pcnst)  :: newCloud, oldCloud, newAerosol, oldAerosol, deltaCloud
   integer                          :: kCrit, lptr2
   logical                          :: stopMe
   integer                          :: iDebug=1, lDebug=15
   real(r8)                         :: mixRatioToMass
   real(r8),dimension(pcnst)        :: debugSumFraction
   real(r8), allocatable            :: lnsigman(:)
   character(len=2)                 :: modeString
   character(len=20)                :: varname
#endif
   integer                              :: numberOfModes
!-------------------------------------------------------------------------------
#undef EXTRATESTS
#undef MASS_BALANCE_CHECK

   sq2pi = sqrt(2._r8*pi)

   lchnk = state%lchnk
   ncol  = state%ncol

   ncldwtr  => state%q(:,:,numliq_idx)
   temp     => state%t
   omega    => state%omega
   pmid     => state%pmid
   pint     => state%pint
   pdel     => state%pdel
   rpdel    => state%rpdel
   zm       => state%zm

   call pbuf_get_field(pbuf, kvh_idx, kvh)

   ! Create the liquid weighted cloud fractions that were passsed in
   ! before. This doesn't seem like the best variable, since the cloud could
   ! have liquid condensate, but the part of it that is changing could be the
   ! ice portion; however, this is what was done before.
   lcldo(:ncol,:)  = cldo(:ncol,:)  * cldliqf(:ncol,:)
   lcldn(:ncol,:) = cldn(:ncol,:) * cldliqf(:ncol,:)


   arg = 1.0_r8
   if (abs(0.8427_r8 - erf(arg))/0.8427_r8 > 0.001_r8) then
      write(iulog,*) 'erf(1.0) = ',ERF(arg)
      call endrun('dropmixnuc: Error function error')
   endif
   arg = 0.0_r8
   if (erf(arg) /= 0.0_r8) then
      write(iulog,*) 'erf(0.0) = ',erf(arg)
      write(iulog,*) 'dropmixnuc: Error function error'
      call endrun('dropmixnuc: Error function error')
   endif

   dtinv = 1._r8/dtmicro

   allocate( &
      nact(pver,ntot_amode),          &
      mact(pver,ntot_amode),          &
      raer(ncnst_tot),                &
      qqcw(ncnst_tot),                &
      raercol(pver,ncnst_tot,2),      &
      raercol_cw(pver,ncnst_tot,2),   &
      coltend(pcols,ncnst_tot),       &
      coltend_cw(pcols,ncnst_tot),    &
      naermod(ntot_amode),            &
      hygro(ntot_amode),              &
#ifdef OSLO_AERO
      lnsigman(ntot_amode),           &           !variable std. deviation (CAM-Oslo)
      raercol_tracer(pver,n_aerosol_tracers,2), &
      raercol_cw_tracer(pver,n_aerosol_tracers,2), &
      mact_tracer(pver,n_aerosol_tracers),      &
      mfullact_tracer(pver,n_aerosol_tracers),  &
#endif
      vaerosol(ntot_amode),           &
      fn(ntot_amode),                 &
      fm(ntot_amode),                 &
      fluxn(ntot_amode),              &
      fluxm(ntot_amode)               )

   ! Init pointers to mode number and specie mass mixing ratios in 
   ! intersitial and cloud borne phases.
#ifdef OSLO_AERO  
   !Need a list of all aerosol species ==> store in raer (mm) 
   ! or qqcw for cloud-borne aerosols (?)
   do m=1,nmodes  !All aerosol modes

      !NOTE: SEVERAL POINTERS POINT TO SAME FIELD, E.G. CONDENSATE WHICH IS IN SEVERAL MODES
      do l = 1, nspec_amode(m)
         tracerIndex      =  getTracerIndex(m,l,.false.)                   !Index in q
         cloudTracerIndex =  getCloudTracerIndex(m,l)              !Index in phys-buffer
         mm               =  mam_idx(m,l)                          !Index in raer/qqcw
         raer(mm)%fld =>  state%q(:,:,tracerIndex)                 !NOTE: These are total fields (for example condensate)
         call pbuf_get_field(pbuf, CloudTracerIndex, qqcw(mm)%fld) !NOTE: These are total fields (for example condensate)
#ifdef EXTRATESTS
!         if(tracerIndex .eq. ldebug)then
!            do k=1,pver
!               print*,"pointer check",k,m,l,mm,tracerIndex, raer(mm)%fld(idebug,k), state%q(idebug,k,tracerIndex)
!            end do
!         endf
#endif
      enddo
   enddo
   allocate(                             &
      fn_tmp(ntot_amode),                 &
      fm_tmp(ntot_amode),                 &
      fluxn_tmp(ntot_amode),              &
      fluxm_tmp(ntot_amode)               )
#else
   do m = 1, ntot_amode
      mm = mam_idx(m, 0)
      call rad_cnst_get_mode_num(0, m, 'a', state, pbuf, raer(mm)%fld)
      call rad_cnst_get_mode_num(0, m, 'c', state, pbuf, qqcw(mm)%fld)  ! cloud-borne aerosol
      do l = 1, nspec_amode(m)
         mm = mam_idx(m, l)
         call rad_cnst_get_aer_mmr(0, m, l, 'a', state, pbuf, raer(mm)%fld)
         call rad_cnst_get_aer_mmr(0, m, l, 'c', state, pbuf, qqcw(mm)%fld)  ! cloud-borne aerosol
      end do
   end do
#endif

   called_from_spcam = (present(from_spcam)) 

   if (called_from_spcam) then
      rgas  => state%q
      allocate(rgascol(pver, pcnst, 2))
      allocate(coltendgas(pcols))
   endif
   wtke = 0._r8

   if (prog_modal_aero) then
      ! aerosol tendencies
      call physics_ptend_init(ptend, state%psetcols, 'ndrop', lq=lq)
   else
      ! no aerosol tendencies
      call physics_ptend_init(ptend, state%psetcols, 'ndrop')
   end if

#ifdef OSLO_AERO
      !Improve this later by using only cloud points ? 
      do k = top_lev, pver
         do i=1,ncol
            cs(i,k)  = pmid(i,k)/(rair*temp(i,k))        ! air density (kg/m3) 
         end do
      end do

      !Output this
      call calculateNumberMedianRadius(numberConcentration, volumeConcentration, lnSigma, numberMedianRadius, ncol)
      do n=1,nmodes
         sigma(:ncol,:,n) = DEXP(lnSigma(:ncol,:,n))
         modeString="  "
         write(modeString,"(I2)"),n
         if(n .lt. 10) modeString="0"//adjustl(modeString)
         varName = "NMR"//trim(modeString)
         call outfld(varName, numberMedianRadius(:,:,n), pcols, lchnk)
         varName = "NCONC"//trim(modeString)
         call outfld(varName, numberConcentration(:,:,n),pcols, lchnk)
         varName = "VCONC"//trim(modeString)
         call outfld(varName, volumeConcentration(:,:,n), pcols,lchnk)
         varName = "SIGMA"//trim(modeString)
         call outfld(varName, sigma(:,:,n), pcols,lchnk)
         varName = "HYGRO"//trim(modeString)
         call outfld(varName, hygroscopicity(:,:,n), pcols,lchnk)
       end do

      alert = .FALSE.
      do k=top_lev,pver
         mm = k - top_lev + 1 
         do m=1,nmodes
            if(.NOT. alert .and. &
               ANY(numberConcentration(:ncol,k,m) .lt. 0.0_r8 ))then
               alert = .TRUE.
               lptr = k
               print*,"STRANGE numberconc", m, minval(numberConcentration(:,:,:))*1.e-6_r8, "#/cm3", k, mm
            endif
         enddo
      enddo


      if(alert)then
            print*,"strange stuff here "
            stop

         !do m=1,nmodes
         !   print*,"numberconc (after alert)", m, modedefs(1)%nnatk(m)*1.e-6_r8, "#/cm3" &
         !         ,modedefs(1)%C(m)*1.0e9_r8, "ug/m3"

         !   if(modedefs(1)%nnatk(m) > 1.e-30_r8)then
         !      print*, "final weight per particle ",m, modedefs(1)%C(m)/modedefs(1)%nnatk(m)
         !   endif
         !end do
         !stop
      endif

#endif

   ! overall_main_i_loop
   do i = 1, ncol

#ifdef OSLO_AERO
   coltend(i,:)=0.0_r8
   coltend_cw(i,:) = 0.0_r8
#endif

      do k = top_lev, pver-1
         zs(k) = 1._r8/(zm(i,k) - zm(i,k+1))
      end do
      zs(pver) = zs(pver-1)

      ! load number nucleated into qcld on cloud boundaries

      do k = top_lev, pver

         qcld(k)  = ncldwtr(i,k)
         qncld(k) = 0._r8
         srcn(k)  = 0._r8
         cs(i,k)  = pmid(i,k)/(rair*temp(i,k))        ! air density (kg/m3)
         dz(i,k)  = 1._r8/(cs(i,k)*gravit*rpdel(i,k)) ! layer thickness in m

         do m = 1, ntot_amode
            nact(k,m) = 0._r8
            mact(k,m) = 0._r8
         end do

         zn(k) = gravit*rpdel(i,k)

         if (k < pver) then
            ekd(k)   = kvh(i,k+1)
            ekd(k)   = max(ekd(k), zkmin)
            ekd(k)   = min(ekd(k), zkmax)
            csbot(k) = 2.0_r8*pint(i,k+1)/(rair*(temp(i,k) + temp(i,k+1)))
            csbot_cscen(k) = csbot(k)/cs(i,k)
         else
            ekd(k)   = 0._r8
            csbot(k) = cs(i,k)
            csbot_cscen(k) = 1.0_r8
         end if

         ! rce-comment - define wtke at layer centers for new-cloud activation
         !    and at layer boundaries for old-cloud activation
         !++ag
         wtke_cen(i,k) = wsub(i,k)
         wtke(i,k)     = wsub(i,k)
         !--ag
         wtke_cen(i,k) = max(wtke_cen(i,k), wmixmin)
         wtke(i,k)     = max(wtke(i,k), wmixmin)

         nsource(i,k) = 0._r8

      end do  ! k

      nsav = 1
      nnew = 2
#ifdef OSLO_AERO

      !get constituent fraction
      componentFractionOK(:,:,:) = 0.0_r8
      do k=top_lev, pver
         do m = 1,ntot_amode
            if(m .le. nbmodes)then
               do l = 1, nspec_amode(m)
                  !calculate fraction of component "l" in mode "m" based on concentrations in clear air
                  componentFractionOK(k,m,getTracerIndex(m,l,.false.))       &
                         = getConstituentFraction(CProcessModes(i,k), f_c(i,k), f_bc(i,k), f_aq(i,k), f_so4_cond(i,k), f_soa(i,k)  &
                         ,Cam(i,k,m), f_acm(i,k,m), f_bcm(i,k,m), f_aqm(i,k,m), f_so4_condm(i,k,m) , f_soam(i,k,m), getTracerIndex(m,l,.false.)  )
               end do
            else
               do l = 1, nspec_amode(m)
                  componentFractionOK(k,m,getTracerIndex(m,l,.false.)) = 1.0_r8
               end do
            endif
         end do

         !Loop over all tracers ==> check that sums to one
         !for all tracers which exist in the oslo-modes
         do l=1,pcnst
            sumFraction = 0.0_r8
            do m=1,ntot_amode
               sumFraction = sumFraction + componentFractionOK(k,m,l)
            end do
            if(sumFraction .gt. 1.e-2_r8)then  !Just scale what comes out if componentFraction is larger than 1%
               do m=1,ntot_amode
                  componentFractionOK(k,m,l) = &
                     componentFractionOK(k,m,l)/sumFraction
               end do
            else       !negative or zero fraction for this species
                       !distribute equal fraction to all receiver modes
               sumFraction = 0.0_r8
               do m=1,ntot_amode
                  do lptr=1,getNumberOfTracersInMode(m)
                     if(getTracerIndex(m,lptr,.FALSE.) .eq. l ) then  
                        sumFraction = sumFraction + 1.0_r8
                     endif
                  end do ! tracers in mode
               end do    ! mode
               do m=1,ntot_amode
                  componentFractionOK(k,m,l)=1.0_r8/max(1.e-30_r8, sumFraction)
               end do !modes
            endif
         end do !tracers
      end do    !levels
      !debug sum fraction for "i" done



     debugSumFraction(:) = 0.0_r8 !sum of component lDebug in level k
      do m = 1, nmodes ! Number of modes
         !Get number concentration of this mode
         mm =mam_idx(m,0)
         do k= top_lev,pver
            raercol(k,mm,nsav) = numberConcentration(i,k,m)/cs(i,k) !#/kg air
            !In oslo model, number concentrations are diagnostics, so 
            !Approximate number concentration in each mode by total 
            !cloud number concentration scaled by how much is available of 
            !each mode
            raercol_cw(k,mm,nsav) = ncldwtr(i,k)*numberConcentration(i,k,m)&
                           /max(1.e-30_r8, sum(numberConcentration(i,k,1:nmodes)))
         enddo

         !These are the mass mixing ratios
         do l = 1, nspec_amode(m)
            mm = mam_idx(m,l)      !index of tracer (all unique) 
            raercol(:,mm,nsav) = 0.0_r8
            raercol_cw(:,mm,nsav) = 0.0_r8
            !Several of the fields (raer(mm)%fld point to the same
            !field in q. To avoid double counting, we take into
            !account the component fraction in the mode
            do k=top_lev,pver
               if(m .gt. nbmodes) then
                  componentFraction = 1.0_r8
               else
                  componentFraction = componentFractionOK(k,m,getTracerIndex(m,l,.false.))
               endif
#ifdef EXTRATESTS
            if(i .eq. iDebug .and. getTracerIndex(m,l,.false.) .eq. lDebug)then
               !print*,"componentFraction", i,cnst_name(oslo_cnst_idx(m,l)),componentFraction
               print*,"assigning cloud/aerosol", k,m,l,qqcw(mm)%fld(i,k), raer(mm)%fld(i,k) &
                     ,componentFraction
               debugSumFraction(k) = debugSumFraction(k) + componentFraction
            endif
            if(componentFraction > 1.0_r8)then
               print*, "wrong component fraction", componentFraction
               stop
               call endrun("wrong component fraction")
            endif
#endif
               !Assign to the components used here i.e. distribute condensate/coagulate to modes
               raercol_cw(k,mm,nsav) = qqcw(mm)%fld(i,k)*componentFraction
               raercol(k,mm,nsav)    = raer(mm)%fld(i,k)*componentFraction
            enddo ! k (levels)
         end do   ! l (species)
      end do      ! m (modes)
#ifdef EXTRATESTS
     do k=top_lev,pver
      if(i .eq. iDebug .and. (abs(debugSumFraction(k)-1.0_r8).gt.1.e-2_r8) .and. debugSumFraction(k).gt.1.e-6_r8)then
         print*, "debugSumFraction", cnst_name(getTracerIndex(m,l,.false.)),i, k, debugSumFraction(k), abs(debugSumFraction(k)-1.0_r8)
         componentFraction=0.0_r8
         do m=1,nbmodes
            componentFraction = componentFraction + cam(i,k,m)
            print*, "MODECONC", m, cam(i,k,m), numberConcentration(i,k,m)
         end do
         print*, "CS, sumCAM", CProcessModes(i,k), sum(cam(i,k,1:nbmodes)), componentFraction
         print*, "q (cond)", state%q(i,k,lDebug)*cs(i,k)!mass in q
         print*, "q (aq) " ,state%q(i,k,l_so4_a2)*cs(i,k)
         print*, "bulk fractions", f_so4_cond(i,k),f_c(i,k), f_bc(i,k), f_aq(i,k)
         !print*, "other levels", debugSumFraction(:)
         do m=1,nmodes
            do l=1,nspec_amode(m)
               if(getTracerIndex(m,l,.false.) == ldebug)then
                  if(m .gt. nbmodes)then
                     componentFraction = 1.0_r8
                  else
                     componentFraction = componentFractionOK(k,m,getTracerIndex(m,l,.false.))
                  endif
                  print*, "nmode, l,k, ", m,l,k , lDebug, componentFraction, cam(i,k,m), f_aqm(i,k,m), f_acm(i,k,m), f_so4_condm(i,k,m)
                  print*, "fraction2 ", cam(i,k,m), cam(i,k,m)/CProcessModes(i,k)*100.0_r8, " %"
               endif
            enddo
         enddo
         call endrun("wrong debugsumfraction")
      endif  !idebug/ldebug
     enddo
#endif
     !END OSLO-STUFF, BELOW IS MAM 3
#else
   do m = 1, ntot_amode
      mm = mam_idx(m,0)
      raercol_cw(:,mm,nsav) = 0.0_r8
      raercol(:,mm,nsav)    = 0.0_r8
      raercol_cw(top_lev:pver,mm,nsav) = qqcw(mm)%fld(i,top_lev:pver)
      raercol(top_lev:pver,mm,nsav)    = raer(mm)%fld(i,top_lev:pver)
      do l = 1, nspec_amode(m)
         mm = mam_idx(m,l)
         raercol_cw(top_lev:pver,mm,nsav) = qqcw(mm)%fld(i,top_lev:pver)
         raercol(top_lev:pver,mm,nsav)    = raer(mm)%fld(i,top_lev:pver)
      end do
   end do
#endif


      if (called_from_spcam) then
      !
      ! In the MMF model, turbulent mixing for tracer species are turned off.
      ! So the turbulent for gas species mixing are added here.
      ! (Previously, it had the turbulent mixing for aerosol species)
      !
         do m=1, pcnst
            if (cnst_species_class(m) == cnst_spec_class_gas) rgascol(:,m,nsav) = rgas(i,:,m)
         end do

      endif

      ! droplet nucleation/aerosol activation

      ! tau_cld_regenerate = time scale for regeneration of cloudy air 
      !    by (horizontal) exchange with clear air
      tau_cld_regenerate = 3600.0_r8 * 3.0_r8 

      if (called_from_spcam) then
      ! when this is called  in the MMF part, no cloud regeneration and decay.
      ! set the time scale be very long so that no cloud regeneration.
           tau_cld_regenerate = 3600.0_r8 * 24.0_r8 * 365.0_r8
      endif


      ! k-loop for growing/shrinking cloud calcs .............................
      ! grow_shrink_main_k_loop: &
      do k = top_lev, pver

         ! This code was designed for liquid clouds, but the cloudbourne
         ! aerosol can be either from liquid or ice clouds. For the ice clouds,
         ! we do not do regeneration, but as cloud fraction decreases the
         ! aerosols should be returned interstitial. The lack of a liquid cloud
         ! should not mean that all of the aerosol is realease. Therefor a
         ! section has been added for shrinking ice clouds and checks were added
         ! to protect ice cloudbourne aerosols from being released when no
         ! liquid cloud is present.

         ! shrinking ice cloud ......................................................
         cldo_tmp = cldo(i,k) * (1._r8 - cldliqf(i,k))
         cldn_tmp = cldn(i,k) * (1._r8 - cldliqf(i,k))

         if (cldn_tmp < cldo_tmp) then

            ! convert activated aerosol to interstitial in decaying cloud

            dumc = (cldn_tmp - cldo_tmp)/cldo_tmp * (1._r8 - cldliqf(i,k))
            do m = 1, ntot_amode
               mm = mam_idx(m,0)
               dact   = raercol_cw(k,mm,nsav)*dumc
               raercol_cw(k,mm,nsav) = raercol_cw(k,mm,nsav) + dact   ! cloud-borne aerosol
               raercol(k,mm,nsav)    = raercol(k,mm,nsav) - dact
               do l = 1, nspec_amode(m)
                  mm = mam_idx(m,l)
                  dact    = raercol_cw(k,mm,nsav)*dumc
                  raercol_cw(k,mm,nsav) = raercol_cw(k,mm,nsav) + dact  ! cloud-borne aerosol
                  raercol(k,mm,nsav)    = raercol(k,mm,nsav) - dact
               end do
            end do
         end if

         ! shrinking liquid cloud ......................................................
         !    treat the reduction of cloud fraction from when cldn(i,k) < cldo(i,k)
         !    and also dissipate the portion of the cloud that will be regenerated
         cldo_tmp = lcldo(i,k)
         cldn_tmp = lcldn(i,k) * exp( -dtmicro/tau_cld_regenerate )
         !    alternate formulation
         !    cldn_tmp = cldn(i,k) * max( 0.0_r8, (1.0_r8-dtmicro/tau_cld_regenerate) )

         ! fraction is also provided. 
         if (cldn_tmp < cldo_tmp) then
            !  droplet loss in decaying cloud
            !++ sungsup
            nsource(i,k) = nsource(i,k) + qcld(k)*(cldn_tmp - cldo_tmp)/cldo_tmp*cldliqf(i,k)*dtinv
            qcld(k)      = qcld(k)*(1._r8 + (cldn_tmp - cldo_tmp)/cldo_tmp)
            !-- sungsup

            ! convert activated aerosol to interstitial in decaying cloud

            dumc = (cldn_tmp - cldo_tmp)/cldo_tmp * cldliqf(i,k)
            do m = 1, ntot_amode
               mm = mam_idx(m,0)
               dact   = raercol_cw(k,mm,nsav)*dumc
               raercol_cw(k,mm,nsav) = raercol_cw(k,mm,nsav) + dact   ! cloud-borne aerosol
               raercol(k,mm,nsav)    = raercol(k,mm,nsav) - dact
               do l = 1, nspec_amode(m)
                  mm = mam_idx(m,l)
                  dact    = raercol_cw(k,mm,nsav)*dumc
                  raercol_cw(k,mm,nsav) = raercol_cw(k,mm,nsav) + dact  ! cloud-borne aerosol
                  raercol(k,mm,nsav)    = raercol(k,mm,nsav) - dact
#ifdef EXTRATESTS
              if(i.eq. iDebug .and. getTracerIndex(m,l,.false.).eq.lDebug)then
               print*,"decaying cloud", k, dact, cldn_tmp, cldo_tmp
              endif
#endif
               end do
            end do
         end if

         ! growing liquid cloud ......................................................
         !    treat the increase of cloud fraction from when cldn(i,k) > cldo(i,k)
         !    and also regenerate part of the cloud 
         cldo_tmp = cldn_tmp
         cldn_tmp = lcldn(i,k)

         if (cldn_tmp-cldo_tmp > 0.01_r8) then

            ! rce-comment - use wtke at layer centers for new-cloud activation
            wbar  = wtke_cen(i,k)
            wmix  = 0._r8
            wmin  = 0._r8
            wmax  = 10._r8
            wdiab = 0._r8

            ! load aerosol properties, assuming external mixtures

#ifdef OSLO_AERO
   naermod(:) = 0.0_r8
   vaerosol(:) = 0.0_r8
   hygro(:) = 0.0_r8
   lnsigman(:) = log(2.0_r8)

   m=0
   do kcomp = 1,nmodes
      if(hasAerosol(i,k,kcomp) .eqv. .TRUE.)then
         m = m + 1
         naermod(m) = numberConcentration(i,k,kcomp)
         vaerosol(m) = volumeConcentration(i,k,kcomp)
         hygro(m) =    hygroscopicity(i,k,kcomp)
         lnsigman(m) = lnsigma(i,k,kcomp)
         speciesMap(m) = kcomp
      end if
   end do
   numberOfModes = m
#else
         numberOfModes = ntot_amode
            phase = 1 ! interstitial
            do m = 1, ntot_amode
               call loadaer( &
                  state, pbuf, i, i, k, &
                  m, cs, phase, na, va, &
                  hy)
               naermod(m)  = na(i)
               vaerosol(m) = va(i)
               hygro(m)    = hy(i)
            end do
#endif
         !++ MH_2015/04/10
         !Call the activation procedure
         if(numberOfModes .gt. 0)then
		    if (use_hetfrz_classnuc) then
               call activate_modal( &
               wbar, wmix, wdiab, wmin, wmax,                       &
               temp(i,k), cs(i,k), naermod, numberOfModes,          &
               vaerosol, hygro, fn_in(i,k,1:nmodes), fm, fluxn,            &
               fluxm,flux_fullact(k)                                &
#ifdef OSLO_AERO
               ,lnsigman                                            &
#endif
               )
            else
               call activate_modal( &
               wbar, wmix, wdiab, wmin, wmax,                       &
               temp(i,k), cs(i,k), naermod, numberOfModes,          &
               vaerosol, hygro, fn, fm, fluxn,                      &
               fluxm,flux_fullact(k)                                &
#ifdef OSLO_AERO
               ,lnsigman                                            &
#endif
               )
            end if
	     !-- MH_2015/04/10
         endif

            dumc = (cldn_tmp - cldo_tmp)
#ifdef OSLO_AERO
      if (use_hetfrz_classnuc) then
	     fn_tmp(:) = fn_in(i,k,1:nmodes)
      else
         fn_tmp(:) = fn(:)
      end if
      fm_tmp(:) = fm(:)
      fluxn_tmp(:) = fluxn(:)
      fluxm_tmp(:) = fluxm(:)
      fn(:) = 0.0_r8
	  fn_in(i,k,:) = 0.0_r8
      fm(:) = 0.0_r8
      fluxn(:)=0.0_r8
      fluxm(:)= 0.0_r8
      do m = 1, numberOfModes   !Number of coexisting modes to be used for activation
         kcomp = speciesMap(m)       !This is the CAM-oslo mode (modes 1-14 may be activated, mode 0 not)
         if (use_hetfrz_classnuc) then
		    fn_in(i,k,kcomp) = fn_tmp(m)
         else
            fn(kcomp) = fn_tmp(m)
         end if
         fm(kcomp) = fm_tmp(m)
         fluxn(kcomp) = fluxn_tmp(m)
         fluxm(kcomp) = fluxm_tmp(m)
      enddo
#endif
            do m = 1, ntot_amode
               mm = mam_idx(m,0)
#ifdef OSLO_AERO
               if (use_hetfrz_classnuc) then
                  dact   = dumc*fn_in(i,k,m)*numberConcentration(i,k,m)/cs(i,k) !#/kg_{air}
               else
                  dact   = dumc*fn(m)*numberConcentration(i,k,m)/cs(i,k) !#/kg_{air}
               end if
#else
               if (use_hetfrz_classnuc) then
                  dact   = dumc*fn_in(i,k,m)*raer(mm)%fld(i,k) ! interstitial only
               else
                  dact   = dumc*fn(m)*raer(mm)%fld(i,k) ! interstitial only
               end if
#endif
               qcld(k) = qcld(k) + dact
               nsource(i,k) = nsource(i,k) + dact*dtinv
               raercol_cw(k,mm,nsav) = raercol_cw(k,mm,nsav) + dact  ! cloud-borne aerosol
               raercol(k,mm,nsav)    = raercol(k,mm,nsav) - dact
               dum = dumc*fm(m)
               do l = 1, nspec_amode(m)
                  mm = mam_idx(m,l)
#ifdef OSLO_AERO
                  if(m .gt. nbmodes)then
                     constituentFraction = 1.0_r8
                  else
                     constituentFraction = componentFractionOK(k,m,getTracerIndex(m,l,.false.)  ) 
                  endif

                  dact    = dum*raer(mm)%fld(i,k)*constituentFraction
#else
                  dact    = dum*raer(mm)%fld(i,k) ! interstitial only
#endif
                  raercol_cw(k,mm,nsav) = raercol_cw(k,mm,nsav) + dact  ! cloud-borne aerosol
                  raercol(k,mm,nsav)    = raercol(k,mm,nsav) - dact
#ifdef EXTRATESTS
              if(i.eq.iDebug .and. getTracerIndex(m,l,.false.).eq.lDebug)then
               print*,"growing cloud (new/old)", k, raercol_cw(k,mm,nsav), raercol_cw(k,mm,nsav)-dact &
                                                   ,raercol(k,mm,nsav),raercol(k,mm,nsav)+dact,dact
              endif
#endif
               enddo
            enddo
         endif  ! cldn_tmp-cldo_tmp > 0.01_r8

      enddo  ! grow_shrink_main_k_loop
      ! end of k-loop for growing/shrinking cloud calcs ......................

      ! ......................................................................
      ! start of k-loop for calc of old cloud activation tendencies ..........
      !
      ! rce-comment
      !    changed this part of code to use current cloud fraction (cldn) exclusively
      !    consider case of cldo(:)=0, cldn(k)=1, cldn(k+1)=0
      !    previous code (which used cldo below here) would have no cloud-base activation
      !       into layer k.  however, activated particles in k mix out to k+1,
      !       so they are incorrectly depleted with no replacement

      ! old_cloud_main_k_loop
      do k = top_lev, pver
         kp1 = min0(k+1, pver)
         taumix_internal_pver_inv = 0.0_r8

         if (lcldn(i,k) > 0.01_r8) then

            wdiab = 0._r8
            wmix  = 0._r8                       ! single updraft
            wbar  = wtke(i,k)                   ! single updraft
            if (k == pver) wbar = wtke_cen(i,k) ! single updraft
            wmax  = 10._r8
            wmin  = 0._r8

            if (lcldn(i,k) - lcldn(i,kp1) > 0.01_r8 .or. k == pver) then

               ! cloud base

               ! ekd(k) = wtke(i,k)*dz(i,k)/sq2pi
               ! rce-comments
               !   first, should probably have 1/zs(k) here rather than dz(i,k) because
               !      the turbulent flux is proportional to ekd(k)*zs(k),
               !      while the dz(i,k) is used to get flux divergences
               !      and mixing ratio tendency/change
               !   second and more importantly, using a single updraft velocity here
               !      means having monodisperse turbulent updraft and downdrafts.
               !      The sq2pi factor assumes a normal draft spectrum.
               !      The fluxn/fluxm from activate must be consistent with the
               !      fluxes calculated in explmix.
               ekd(k) = wbar/zs(k)

               alogarg = max(1.e-20_r8, 1._r8/lcldn(i,k) - 1._r8)
               wmin    = wbar + wmix*0.25_r8*sq2pi*log(alogarg)
               phase   = 1   ! interstitial
#ifdef OSLO_AERO
            naermod(:) = 0.0_r8
            vaerosol(:) = 0.0_r8
            hygro(:) = 0.0_r8
            lnsigman(:) = log(2.0_r8)

            m=0
            do kcomp = 1,nmodes
               if(hasAerosol(i,kp1,kcomp) .eqv. .TRUE.)then
                  m = m + 1
                  naermod(m) = numberConcentration(i,kp1,kcomp)
                  vaerosol(m) = volumeConcentration(i,kp1,kcomp)
                  hygro(m) =    hygroscopicity(i,kp1,kcomp)
                  lnsigman(m) = lnsigma(i,kp1,kcomp)
                  speciesMap(m) = kcomp
               end if
            end do
            numberOfModes = m
#else
            numberOfModes = ntot_amode

               do m = 1, ntot_amode
                  ! rce-comment - use kp1 here as old-cloud activation involves 
                  !   aerosol from layer below
                  call loadaer( &
                     state, pbuf, i, i, kp1,  &
                     m, cs, phase, na, va,   &
                     hy)
                  naermod(m)  = na(i)
                  vaerosol(m) = va(i)
                  hygro(m)    = hy(i)
               end do
#endif
         !++ MH_2015/04/10
         if(numberOfModes .gt. 0)then
		    if (use_hetfrz_classnuc) then
               call activate_modal( &
                  wbar, wmix, wdiab, wmin, wmax,                       &
                  temp(i,k), cs(i,k), naermod, numberOfModes , &
                  vaerosol, hygro, fn_in(i,k,:), fm, fluxn,                      &
                  fluxm, flux_fullact(k)                       &
#ifdef OSLO_AERO
                  ,lnsigman                                    &
#endif
                   )
            else
               call activate_modal( &
                  wbar, wmix, wdiab, wmin, wmax,                       &
                  temp(i,k), cs(i,k), naermod, numberOfModes , &
                  vaerosol, hygro, fn, fm, fluxn,                      &
                  fluxm, flux_fullact(k)                       &
#ifdef OSLO_AERO
                  ,lnsigman                                    &
#endif
                   )
            end if
		 !-- MH_2015/04/10
         endif

            !Difference in cloud fraction this layer and above!
            !we are here because there are more clouds above, and some
            !aerosols go into  that layer! ==> calculate additional cloud fraction
               if (k < pver) then
                  dumc = lcldn(i,k) - lcldn(i,kp1)
               else
                  dumc = lcldn(i,k)
               endif

#ifdef OSLO_AERO
            if (use_hetfrz_classnuc) then
               fn_tmp(:) = fn_in(i,k,1:nmodes)
            else
               fn_tmp(:) = fn(:)
            end if
            fm_tmp(:) = fm(:)
            fluxn_tmp(:) = fluxn(:)
            fluxm_tmp(:) = fluxm(:)
            fn(:) = 0.0_r8
			fn_in(i,k,:) = 0.0_r8
            fm(:) = 0.0_r8
            fluxn(:)=0.0_r8
            fluxm(:)= 0.0_r8
            do m = 1, numberOfModes   !Number of coexisting modes to be used for activation
               kcomp = speciesMap(m)       !This is the CAM-oslo mode (modes 1-14 may be activated, mode 0 not)
               if (use_hetfrz_classnuc) then
                  fn_in(i,k,kcomp) = fn_tmp(m)
               else
                  fn(kcomp) = fn_tmp(m)
               end if
               fm(kcomp) = fm_tmp(m)
               fluxn(kcomp) = fluxn_tmp(m)
               fluxm(kcomp) = fluxm_tmp(m)
            enddo
#endif

               fluxntot = 0.0_r8

               ! rce-comment 1
               !    flux of activated mass into layer k (in kg/m2/s)
               !       = "actmassflux" = dumc*fluxm*raercol(kp1,lmass)*csbot(k)
               !    source of activated mass (in kg/kg/s) = flux divergence
               !       = actmassflux/(cs(i,k)*dz(i,k))
               !    so need factor of csbot_cscen = csbot(k)/cs(i,k)
               !                   dum=1./(dz(i,k))
               dum=csbot_cscen(k)/(dz(i,k))

               ! rce-comment 2
               !    code for k=pver was changed to use the following conceptual model
               !    in k=pver, there can be no cloud-base activation unless one considers
               !       a scenario such as the layer being partially cloudy, 
               !       with clear air at bottom and cloudy air at top
               !    assume this scenario, and that the clear/cloudy portions mix with 
               !       a timescale taumix_internal = dz(i,pver)/wtke_cen(i,pver)
               !    in the absence of other sources/sinks, qact (the activated particle 
               !       mixratio) attains a steady state value given by
               !          qact_ss = fcloud*fact*qtot
               !       where fcloud is cloud fraction, fact is activation fraction, 
               !       qtot=qact+qint, qint is interstitial particle mixratio
               !    the activation rate (from mixing within the layer) can now be
               !       written as
               !          d(qact)/dt = (qact_ss - qact)/taumix_internal
               !                     = qtot*(fcloud*fact*wtke/dz) - qact*(wtke/dz)
               !    note that (fcloud*fact*wtke/dz) is equal to the nact/mact
               !    also, d(qact)/dt can be negative.  in the code below
               !       it is forced to be >= 0
               !
               ! steve -- 
               !    you will likely want to change this.  i did not really understand 
               !       what was previously being done in k=pver
               !    in the cam3_5_3 code, wtke(i,pver) appears to be equal to the
               !       droplet deposition velocity which is quite small
               !    in the cam3_5_37 version, wtke is done differently and is much
               !       larger in k=pver, so the activation is stronger there
               !
               if (k == pver) then
                  taumix_internal_pver_inv = flux_fullact(k)/dz(i,k)
               end if

               do m = 1, ntot_amode
                  mm = mam_idx(m,0)
                  fluxn(m) = fluxn(m)*dumc
                  fluxm(m) = fluxm(m)*dumc
                  nact(k,m) = nact(k,m) + fluxn(m)*dum
                  mact(k,m) = mact(k,m) + fluxm(m)*dum
                  if (k < pver) then
                     ! note that kp1 is used here
                     fluxntot = fluxntot &
                        + fluxn(m)*raercol(kp1,mm,nsav)*cs(i,k)
                  else
                     tmpa = raercol(kp1,mm,nsav)*fluxn(m) &
                          + raercol_cw(kp1,mm,nsav)*(fluxn(m) &
                          - taumix_internal_pver_inv*dz(i,k))
                     fluxntot = fluxntot + max(0.0_r8, tmpa)*cs(i,k)
                  end if
               end do
               srcn(k)      = srcn(k) + fluxntot/(cs(i,k)*dz(i,k))
               nsource(i,k) = nsource(i,k) + fluxntot/(cs(i,k)*dz(i,k))
#ifdef EXTRATESTS
            if(fluxntot/(cs(i,k)*dz(i,k)) > 0.0_r8 )then
               print*,"activated/available(from below)",i,k,m,fluxntot/(cs(i,k)*dz(i,k))  
            endif
#endif
            endif  ! (cldn(i,k) - cldn(i,kp1) > 0.01 .or. k == pver)

         else  ! i.e: cldn(i,k) < 0.01_r8

            ! no liquid cloud

            nsource(i,k) = nsource(i,k) - qcld(k)*dtinv
            qcld(k)      = 0.0_r8

            if (cldn(i,k) < 0.01_r8) then
               ! no ice cloud either

               ! convert activated aerosol to interstitial in decaying cloud

               do m = 1, ntot_amode
                  mm = mam_idx(m,0)
                  raercol(k,mm,nsav)    = raercol(k,mm,nsav) + raercol_cw(k,mm,nsav)  ! cloud-borne aerosol
                  raercol_cw(k,mm,nsav) = 0._r8

                  do l = 1, nspec_amode(m)
                     mm = mam_idx(m,l)
#ifdef EXTRATESTS
                 if(i.eq.iDebug .and. getTracerIndex(m,l,.false.).eq.lDebug)then
                  print*,"no cloud", k,  raercol(k,mm,nsav) , raercol_cw(k,mm,nsav)
                 endif
#endif
                     raercol(k,mm,nsav)    = raercol(k,mm,nsav) + raercol_cw(k,mm,nsav) ! cloud-borne aerosol
                     raercol_cw(k,mm,nsav) = 0._r8
                  end do
               end do
            end if
         end if

      end do  ! old_cloud_main_k_loop

      ! switch nsav, nnew so that nnew is the updated aerosol
      ntemp = nsav
      nsav  = nnew
      nnew  = ntemp

      ! load new droplets in layers above, below clouds

      dtmin     = dtmicro
      ekk(top_lev-1)    = 0.0_r8
      ekk(pver) = 0.0_r8
      do k = top_lev, pver-1
         ! rce-comment -- ekd(k) is eddy-diffusivity at k/k+1 interface
         !   want ekk(k) = ekd(k) * (density at k/k+1 interface)
         !   so use pint(i,k+1) as pint is 1:pverp 
         !           ekk(k)=ekd(k)*2.*pint(i,k)/(rair*(temp(i,k)+temp(i,k+1)))
         !           ekk(k)=ekd(k)*2.*pint(i,k+1)/(rair*(temp(i,k)+temp(i,k+1)))
         ekk(k) = ekd(k)*csbot(k)
      end do

      do k = top_lev, pver
         km1     = max0(k-1, top_lev)
         ekkp(k) = zn(k)*ekk(k)*zs(k)
         ekkm(k) = zn(k)*ekk(k-1)*zs(km1)
         tinv    = ekkp(k) + ekkm(k)

         ! rce-comment -- tinv is the sum of all first-order-loss-rates
         !    for the layer.  for most layers, the activation loss rate
         !    (for interstitial particles) is accounted for by the loss by
         !    turb-transfer to the layer above.
         !    k=pver is special, and the loss rate for activation within 
         !    the layer must be added to tinv.  if not, the time step
         !    can be too big, and explmix can produce negative values.
         !    the negative values are reset to zero, resulting in an 
         !    artificial source.
         if (k == pver) tinv = tinv + taumix_internal_pver_inv

         if (tinv .gt. 1.e-6_r8) then
            dtt   = 1._r8/tinv
            dtmin = min(dtmin, dtt)
         end if
      end do

      dtmix   = 0.9_r8*dtmin
      nsubmix = dtmicro/dtmix + 1
      if (nsubmix > 100) then
         nsubmix_bnd = 100
      else
         nsubmix_bnd = nsubmix
      end if
      count_submix(nsubmix_bnd) = count_submix(nsubmix_bnd) + 1
      dtmix = dtmicro/nsubmix

      do k = top_lev, pver
         kp1 = min(k+1, pver)
         km1 = max(k-1, top_lev)
         ! maximum overlap assumption
         if (cldn(i,kp1) > 1.e-10_r8) then
            overlapp(k) = min(cldn(i,k)/cldn(i,kp1), 1._r8)
         else
            overlapp(k) = 1._r8
         end if
         if (cldn(i,km1) > 1.e-10_r8) then
            overlapm(k) = min(cldn(i,k)/cldn(i,km1), 1._r8)
         else
            overlapm(k) = 1._r8
         end if
      end do


      ! rce-comment
      !    the activation source(k) = mact(k,m)*raercol(kp1,lmass)
      !       should not exceed the rate of transfer of unactivated particles
      !       from kp1 to k which = ekkp(k)*raercol(kp1,lmass)
      !    however it might if things are not "just right" in subr activate
      !    the following is a safety measure to avoid negatives in explmix
      do k = top_lev, pver-1
         do m = 1, ntot_amode
            nact(k,m) = min( nact(k,m), ekkp(k) )
            mact(k,m) = min( mact(k,m), ekkp(k) )
         end do
      end do

!Don't need the mixing per mode in OSLO_AERO ==> only per tracer
!Note that nsav/nnew is switched above, so operate on nnew here
!nnew is the updated aerosol
#ifdef OSLO_AERO
    raercol_tracer(:,:,:) = 0.0_r8
    raercol_cw_tracer(:,:,:) = 0.0_r8
    mact_tracer(:,:) = 0.0_r8
    mfullact_tracer(:,:) = 0.0_r8
    do m=1,ntot_amode
       do l=1,nspec_amode(m)
          lptr = getTracerIndex(m,l,.FALSE.)  !which tracer are we talking about
          lptr2  = inverseAerosolTracerList(lptr)    !which index is this in the list of aerosol-tracers
          mm = mam_idx(m,l)
          raercol_tracer(:,lptr2,nnew) = raercol_tracer(:,lptr2,nnew) &
                                        + raercol(:,mm,nnew)

          raercol_cw_tracer(:,lptr2,nnew) = raercol_cw_tracer(:,lptr2,nnew)&
                                        + raercol_cw(:,mm,nnew)

          mact_tracer(:,lptr2) = mact_tracer(:,lptr2) + mact(:,m)*raercol(:,mm,nnew)
          mfullact_tracer(:,lptr2) = mfullact_tracer(:,lptr2) + raercol(:,mm,nnew)

#ifdef EXTRATESTS
          if(lptr.eq.lDebug .and. i.eq.iDebug)then
             do k=pver,top_lev,-1
               print*, "assigning to tracer space",lptr, raercol(k,mm,nnew) &
                        , raercol_tracer(k,lptr2,nnew) &
                        , raercol_cw(k,mm,nnew) &
                        , raercol_cw_tracer(k,lptr2,nnew)
             end do
          end if
#endif
       end do !l
    end do    !m

    do lptr2=1,n_aerosol_tracers
       mact_tracer(:,lptr2) = mact_tracer(:,lptr2)  &
               /(mfullact_tracer(:,lptr2) + smallNumber)
    end do
#endif OSLO_AERO

      ! old_cloud_nsubmix_loop
      do n = 1, nsubmix
         qncld(:) = qcld(:)
         ! switch nsav, nnew so that nsav is the updated aerosol
         ntemp   = nsav
         nsav    = nnew
         nnew    = ntemp
         srcn(:) = 0.0_r8

         !First mix cloud droplet number concentration
         do m = 1, ntot_amode
            mm = mam_idx(m,0)

            ! update droplet source
            ! rce-comment- activation source in layer k involves particles from k+1
            !	       srcn(:)=srcn(:)+nact(:,m)*(raercol(:,mm,nsav))
            srcn(top_lev:pver-1) = srcn(top_lev:pver-1) + nact(top_lev:pver-1,m)*(raercol(top_lev+1:pver,mm,nsav))

            ! rce-comment- new formulation for k=pver
            !              srcn(  pver  )=srcn(  pver  )+nact(  pver  ,m)*(raercol(  pver,mm,nsav))
            tmpa = raercol(pver,mm,nsav)*nact(pver,m) &
                 + raercol_cw(pver,mm,nsav)*(nact(pver,m) - taumix_internal_pver_inv)
            srcn(pver) = srcn(pver) + max(0.0_r8,tmpa)
         end do

         !mixing of cloud droplets
         call explmix(  &
            qcld, srcn, ekkp, ekkm, overlapp,  &
            overlapm, qncld, zero, zero, pver, &
            dtmix, .false.)

#ifdef OSLO_AERO
         !Mix number concentrations consistently!!
         do m = 1, ntot_amode
            mm = mam_idx(m,0)
            ! rce-comment -   activation source in layer k involves particles from k+1
            !	              source(:)= nact(:,m)*(raercol(:,mm,nsav))
            source(top_lev:pver-1) = nact(top_lev:pver-1,m)*(raercol(top_lev+1:pver,mm,nsav))
            ! rce-comment - new formulation for k=pver
            !               source(  pver  )= nact(  pver,  m)*(raercol(  pver,mm,nsav))
            tmpa = raercol(pver,mm,nsav)*nact(pver,m) &
                 + raercol_cw(pver,mm,nsav)*(nact(pver,m) - taumix_internal_pver_inv)
            source(pver) = max(0.0_r8, tmpa)
            flxconv = 0._r8

            call explmix( &
               raercol_cw(:,mm,nnew), source, ekkp, ekkm, overlapp, &
               overlapm, raercol_cw(:,mm,nsav), zero, zero, pver,   &
               dtmix, .false.)

            call explmix( &
               raercol(:,mm,nnew), source, ekkp, ekkm, overlapp,  &
               overlapm, raercol(:,mm,nsav), zero, flxconv, pver, &
               dtmix, .true., raercol_cw(:,mm,nsav))
         end do
#endif

#ifndef OSLO_AERO
         ! rce-comment
         !    the interstitial particle mixratio is different in clear/cloudy portions
         !    of a layer, and generally higher in the clear portion.  (we have/had
         !    a method for diagnosing the the clear/cloudy mixratios.)  the activation
         !    source terms involve clear air (from below) moving into cloudy air (above).
         !    in theory, the clear-portion mixratio should be used when calculating 
         !    source terms
         do m = 1, ntot_amode
            mm = mam_idx(m,0)
            ! rce-comment -   activation source in layer k involves particles from k+1
            !	              source(:)= nact(:,m)*(raercol(:,mm,nsav))
            source(top_lev:pver-1) = nact(top_lev:pver-1,m)*(raercol(top_lev+1:pver,mm,nsav))
            ! rce-comment - new formulation for k=pver
            !               source(  pver  )= nact(  pver,  m)*(raercol(  pver,mm,nsav))
            tmpa = raercol(pver,mm,nsav)*nact(pver,m) &
                 + raercol_cw(pver,mm,nsav)*(nact(pver,m) - taumix_internal_pver_inv)
            source(pver) = max(0.0_r8, tmpa)
            flxconv = 0._r8

            call explmix( &
               raercol_cw(:,mm,nnew), source, ekkp, ekkm, overlapp, &
               overlapm, raercol_cw(:,mm,nsav), zero, zero, pver,   &
               dtmix, .false.)

            call explmix( &
               raercol(:,mm,nnew), source, ekkp, ekkm, overlapp,  &
               overlapm, raercol(:,mm,nsav), zero, flxconv, pver, &
               dtmix, .true., raercol_cw(:,mm,nsav))

            do l = 1, nspec_amode(m)
               mm = mam_idx(m,l)
               ! rce-comment -   activation source in layer k involves particles from k+1
               !	          source(:)= mact(:,m)*(raercol(:,mm,nsav))
               source(top_lev:pver-1) = mact(top_lev:pver-1,m)*(raercol(top_lev+1:pver,mm,nsav))
               ! rce-comment- new formulation for k=pver
               !                 source(  pver  )= mact(  pver  ,m)*(raercol(  pver,mm,nsav))
               tmpa = raercol(pver,mm,nsav)*mact(pver,m) &
                    + raercol_cw(pver,mm,nsav)*(mact(pver,m) - taumix_internal_pver_inv)
               source(pver) = max(0.0_r8, tmpa)
               flxconv = 0._r8

               call explmix( &
                  raercol_cw(:,mm,nnew), source, ekkp, ekkm, overlapp, &
                  overlapm, raercol_cw(:,mm,nsav), zero, zero, pver,   &
                  dtmix, .false.)

               call explmix( &
                  raercol(:,mm,nnew), source, ekkp, ekkm, overlapp,  &
                  overlapm, raercol(:,mm,nsav), zero, flxconv, pver, &
                  dtmix, .true., raercol_cw(:,mm,nsav))

            end do
         end do
#endif
         if (called_from_spcam) then
         !
         ! turbulent mixing for gas species .
         !
               do m=1, pcnst
                  if (cnst_species_class(m) == cnst_spec_class_gas) then
                    flxconv = 0.0_r8
                    zerogas(:) = 0.0_r8
                    call explmix(rgascol(1,m,nnew),zerogas,ekkp,ekkm,overlapp,overlapm,  &
                                 rgascol(1,m,nsav),zero, flxconv, pver,dtmix,&
                                   .true., zerogas)
                  end if
               end do
         endif

#ifdef OSLO_AERO
         do lptr2=1,n_aerosol_tracers
            source(top_lev:pver-1) = mact_tracer(top_lev:pver-1,lptr2) &
                           *(raercol_tracer(top_lev+1:pver,lptr2,nsav))

            tmpa = raercol_tracer(pver,lptr2,nsav)*mact_tracer(pver,lptr2) &
              + raercol_cw_tracer(pver,lptr2,nsav)*(mact_tracer(pver,lptr2) - taumix_internal_pver_inv)
            
            source(pver) = max(0.0_r8, tmpa)
            flxconv = 0.0_r8
   
            call explmix( &
               raercol_cw_tracer(:,lptr2,nnew), source, ekkp, ekkm, overlapp, &
               overlapm, raercol_cw_tracer(:,lptr2,nsav), zero, zero, pver,   &
               dtmix, .false.)

            call explmix( &
               raercol_tracer(:,lptr2,nnew), source, ekkp, ekkm, overlapp,  &
               overlapm, raercol_tracer(:,lptr2,nsav), zero, flxconv, pver, &
               dtmix, .true., raercol_cw_tracer(:,lptr2,nsav))

#ifdef EXTRATESTS
            lptr = aerosolTracerList(lptr2)
            if(i.eq.iDebug .and. lptr.eq.lDebug)then
               print*, "bugeds for ",trim(cnst_name(lptr)), n, nsubmix
               do k=pver,1,-1
                  print*, "source (aerosol/cloud) ",k, raercol_cw_tracer(k,lptr2,nnew),raercol_cw_tracer(k,lptr2,nsav) &
                        , raercol_tracer(k,lptr2,nnew),raercol_tracer(k,lptr2,nsav),source(k)
               end do
               if(m .le. nbmodes)then
                  print*, " ", mm, lptr, componentFractionOK(k,m,getTracerIndex(m,l,.false.))
               endif
            endif
#endif
         end do !Number of aerosol tracers
      end do ! old_cloud_nsubmix_loop

      !Set back to the original framework
      !Could probably continue in tracer-space from here
      !but return back to mixture for easier use of std. NCAR code
      tendencyCounted(:)=.FALSE. 
      do m = 1, ntot_amode
         do l=1,nspec_amode(m)
            mm=mam_idx(m,l)
            lptr = getTracerIndex(m,l,.FALSE.)
            lptr2 = inverseAerosolTracerList(lptr)
            !All the tracer-space contains sum of all
            !modes ==> put in first available component
            !and zero in others.
            if(.not.tendencyCounted(lptr))then
               raercol(:,mm,nnew) = raercol_tracer(:,lptr2,nnew)
               raercol_cw(:,mm,nnew) = raercol_cw_tracer(:,lptr2,nnew)
               tendencyCounted(lptr) = .TRUE.
            else
               raercol(:,mm,nnew) = 0.0_r8
               raercol_cw(:,mm,nnew) = 0.0_r8
            end if
        end do
     end do
#endif
      ! evaporate particles again if no cloud

      do k = top_lev, pver
         if (cldn(i,k) == 0._r8) then
            ! no ice or liquid cloud
            qcld(k)=0._r8

            ! convert activated aerosol to interstitial in decaying cloud
            do m = 1, ntot_amode
               mm = mam_idx(m,0)
               raercol(k,mm,nnew)    = raercol(k,mm,nnew) + raercol_cw(k,mm,nnew)
               raercol_cw(k,mm,nnew) = 0._r8

               do l = 1, nspec_amode(m)
                  mm = mam_idx(m,l)
                  raercol(k,mm,nnew)    = raercol(k,mm,nnew) + raercol_cw(k,mm,nnew)
                  raercol_cw(k,mm,nnew) = 0._r8
               end do
            end do
         end if
      end do

      ! droplet number

      ndropcol(i) = 0._r8

      !Initialize tendnd to zero in all layers since values are set in only top_lev,pver
      !Without this the layers above top_lev would be un-initialized
      tendnd(i,:) = 0.0_r8

      do k = top_lev, pver
         ndropmix(i,k) = (qcld(k) - ncldwtr(i,k))*dtinv - nsource(i,k)
         tendnd(i,k)   = (max(qcld(k), 1.e-6_r8) - ncldwtr(i,k))*dtinv
         !print*, "tendnd",i,k, "new /old/tend", qcld(k), ncldwtr(i,k), tendnd(i,k)
         ndropcol(i)   = ndropcol(i) + ncldwtr(i,k)*pdel(i,k)
      end do
      ndropcol(i) = ndropcol(i)/gravit

#ifdef EXTRATESTS
      print*, "tendnd (#/kg/sec)", minval(tendnd(i,:)), maxval(tendnd(i,:))
#endif

      if (prog_modal_aero) then

#ifdef OSLO_AERO

#ifdef MASS_BALANCE_CHECK
      !test for correct transfer between in-cloud / no-cloud..
      newCloud(:,:) = 0.0_r8
      oldCloud(:,:) = 0.0_r8
      newAerosol(:,:) = 0.0_r8   
      oldAerosol(:,:) = 0.0_r8
      deltaCloud(:,:) = 0.0_r8
      !Check mass balances #2 (all new cloud droplet species are taken from aerosols or from layer below
      do k=pver,1,-1
         mixRatioToMass = cs(i,k)*dz(i,k)
         !First sum up cloud tracer in this layer
         tendencyCounted(:)=.FALSE.
         do m=1,ntot_amode
            do l=1,nspec_amode(m)
               mm = mam_idx(m,l)
               lptr = getTracerIndex(m,l,.false.) !lptr occurs several times
               newCloud(k, lptr)  = newCloud(k, lptr) + raercol_cw(k, mm, nnew)*mixRatioToMass
               newAerosol(k, lptr) = newAerosol(k, lptr) + raercol(k,mm,nnew)*mixRatioToMass
               if(.NOT. tendencyCounted(lptr))then
                  oldAerosol(k, lptr) = raer(mm)%fld(i,k)*mixRatioToMass
                  oldCloud(k, lptr)  = qqcw(mm)%fld(i,k)*mixRatioToMass
                  tendencyCounted(lptr)=.TRUE.
               endif
            enddo
         enddo
      enddo! k
      
      k = pver
      !Check imbalance in bottom layer

      !Any change in cloud species is either from aerosol concentration or from change in layer below
      do m=1,ntot_amode
         do l=1,nspec_amode(m)
            lptr = getTracerIndex(m,l,.false.)

            !This is the mass which must go to layer above!
            deltaCloud(k,lptr) = (oldAerosol(k,lptr) - newAerosol(k,lptr)) &!used to create cloud species
                           -(newCloud(k,lptr) - oldCloud(k,lptr))    !created cloud species
         enddo
      enddo

      !if "deltaCloud" is positive in layer below it means that some aerosol species were sent up
      
      !Move upwards
      do k=pver-1,1,-1
         kp1 = k + 1
         do m=1,ntot_amode
            do l=1,nspec_amode(m)
               lptr = getTracerIndex(m,l,.false.)
               deltaCloud(k,lptr) = (oldAerosol(k,lptr)-newAerosol(k,lptr)) & !used to create cloud species
                           -   (newCloud(k,lptr) - oldCloud(k,lptr))    & !created cloud species
                           -   0.0_r8 ! deltaCloud(kp1,lptr)                      !species received from below 
            enddo
         enddo
      enddo !layers

      stopMe = .FALSE.
      tendencyCounted(:) = .FALSE.
      do m=1,ntot_amode
         do l=1,nspec_amode(m)
            lptr= getTracerIndex(m,l,.false.)
            if(abs(sum(deltaCloud(:,lptr))) > 1.e-8_r8 .and. (.NOT. tendencyCounted(lptr)))then
               stopMe = .TRUE.
               lptr2 = lptr
               print*, "wrong mass budget",i,lptr,cnst_name(lptr), sum(deltaCloud(:,lptr))
            endif
            tendencyCounted(lptr) = .TRUE.
         enddo
      enddo
      if(stopMe)then
         print*,"error in species : ", cnst_name(lptr2)
         do k=pver,1,-1
            print*, "budgets new/old ",k, newCloud(k,lptr2),oldCloud(k,lptr2),newaerosol(k,lptr2),oldAerosol(k,lptr2), deltaCloud(k,lptr2)
         enddo 
         call endrun ("wrong mass budget in column")
      endif
#endif
#endif
         raertend = 0._r8
         qqcwtend = 0._r8


#ifndef OSLO_AERO
         do m = 1, ntot_amode
            do l = 0, nspec_amode(m)

               mm   = mam_idx(m,l)
               lptr = mam_cnst_idx(m,l)

               raertend(top_lev:pver) = (raercol(top_lev:pver,mm,nnew) - raer(mm)%fld(i,top_lev:pver))*dtinv
               qqcwtend(top_lev:pver) = (raercol_cw(top_lev:pver,mm,nnew) - qqcw(mm)%fld(i,top_lev:pver))*dtinv

               coltend(i,mm)    = sum( pdel(i,:)*raertend )/gravit
               coltend_cw(i,mm) = sum( pdel(i,:)*qqcwtend )/gravit

               ptend%q(i,:,lptr) = 0.0_r8
               ptend%q(i,top_lev:pver,lptr) = raertend(top_lev:pver)           ! set tendencies for interstitial aerosol
               qqcw(mm)%fld(i,:) = 0.0_r8
               qqcw(mm)%fld(i,top_lev:pver) = raercol_cw(top_lev:pver,mm,nnew) ! update cloud-borne aerosol
            end do
         end do
#else
      !OSLO AEROSOLS ...

      coltend_cw(i,:)=0.0_r8
      coltend(i,:) = 0.0_r8

      !Need to initialize first because process modes arrive several times
      tendencyCounted(:) = .FALSE.
      do m=1,ntot_amode
         do l = 1,getNumberOfTracersInMode(m)
            lptr = getTracerIndex(m,l,.false.)
            mm = mam_idx(m,l)

            !column tendencies for output
            if(.NOT. tendencyCounted(lptr))then
               coltend_cw(i,lptr) = coltend_cw(i,lptr) & 
                                   + sum( pdel(i,top_lev:pver)*(raercol_cw(top_lev:pver,mm,nnew) & !New, splitted, 
                                   - qqcw(mm)%fld(i,top_lev:pver) ) )/gravit*dtinv      !Old, total
               tendencyCounted(lptr) = .TRUE.
            else  !Already subtracted total old value, just add new
               coltend_cw(i,lptr) = coltend_cw(i,lptr)  &
                                   + sum(pdel(i,top_lev:pver)*raercol_cw(top_lev:pver,mm,nnew))/gravit*dtinv !total already subtracted
            end if

            ptend%q(i,:,lptr) = 0.0_r8  !Initialize tendencies
            qqcw(mm)%fld(i,:) = 0.0_r8  !Throw out old concentrations before summing new ones
         end do  ! Tracers
      end do     ! Modes

      !First, sum up all the tracer mass concentrations 
      do m = 1, ntot_amode
         do l = 1, nspec_amode(m)
            mm   = mam_idx(m,l)                !tracer indices for aerosol mass mixing ratios in raer-arrays
            lptr = getTracerIndex(m,l,.false.) !index in q-array (1-pcnst)

            !This is a bit tricky since in our scheme the tracers can arrive several times 
            !the same tracer can exist in several modes, e.g. condensate!!
            !Here we sum this into "qqcw" and "ptend" so that they contain TOTAL of those tracers
   
            !raercol and raercol_cw do not have totals, they have process-tracers splitted onto modes

            !Tendency at this point is the sum (original value subtracted below)
            ptend%q(i,top_lev:pver,lptr)   =    ptend%q(i,top_lev:pver,lptr) + raercol(top_lev:pver,mm,nnew) 
            !for cloud water concentrations, we don't get tendency , only new concentration
            qqcw(mm)%fld(i,top_lev:pver)   =    qqcw(mm)%fld(i,top_lev:pver) + raercol_cw(top_lev:pver,mm,nnew) 

         end do
      end do

      !Need this check due to some tracers (e.g. condensate) several times
      tendencyCounted(:) = .FALSE.

       ! Recalculating cloud-borne aerosol number mixing ratios 
       do m=1,ntot_amode

         !Now that all new aerosol masses are summed up, we subtract the original concentrations to obtain the tendencies
         do l= 1,nspec_amode(m)
            mm = mam_idx(m,l)
            lptr = getTracerIndex(m,l,.false.)
            if(.NOT. tendencyCounted(lptr)) then 
               ptend%q(i,top_lev:pver,lptr) = (ptend%q(i,top_lev:pver,lptr) - raer(mm)%fld(i,top_lev:pver))*dtinv
               coltend(i,lptr) = sum(pdel(i,top_lev:pver)*ptend%q(i,top_lev:pver,lptr))/gravit !Save column tendency
               tendencyCounted(lptr) = .TRUE.
            endif
         end do !species
      end do    !modes
#endif

#ifdef MASS_BALANCE_CHECK
      !Check mass balances (all removed should be in tendencies)
      massBalance(:,:) = 0.0_r8
      newMass(:,:) = 0.0_r8
      do m=1,ntot_amode
         do l=1,nspec_amode(m)
            mm = mam_idx(m,l)  !unique index, for example sulfate condendsate in "x mode" or sulf cond in "y mode"
            lptr = getTracerIndex(m,l,.false.)
            !add up all new values for this tracer
            newMass(top_lev:pver,lptr) = newMass(top_lev:pver,lptr) + raercol(top_lev:pver, mm,nnew)
         enddo
      enddo
      tendencyCounted(:)=.FALSE.
      do m=1,ntot_amode
         do l=1,nspec_amode(m)
            mm = mam_idx(m,l)
            lptr = getTracerIndex(m,l,.false.)
            if(.NOT. tendencyCounted(lptr))then
               massBalance(top_lev:pver, lptr)  =   newMass(top_lev:pver,lptr) &
                            - raer(mm)%fld(i,top_lev:pver) &     !previous value
                            - ptend%q(i,top_lev:pver,lptr)/dtinv !added during time step 
               tendencyCounted(lptr) = .TRUE.
            endif
         enddo
      enddo
      tendencyCounted(:) = .FALSE.
      do m=1,ntot_amode
         do l=1,nspec_amode(m)
            lptr = getTracerIndex(m,l,.false.)
            !Check for large deviation in mass balance for this tracer
            if(.NOT. tendencyCounted(lptr) .and. &
                  (maxval(massBalance(:,lptr)) > 1.e-30_r8 .or. minval(massBalance(:,lptr)) < -1.0e-30_r8))then
               tendencyCounted(lptr) = .TRUE.
               print*, "massBalance error", i, lptr, maxVal(massBalance(:,lptr)), minVal(massBalance(:,lptr))
               if(maxVal(massBalance(:,lptr)) > 1.e-30_r8)then
                  kCrit = maxLoc(massBalance(:,lptr),1)
               else
                  kCrit = minLoc(massBalance(:,lptr),1)
               endif
               print*, "massBalance error loc", massBalance(kCrit, lptr), newMass(kCrit,lptr), raer(mm)%fld(i,kCrit)
               !If mass balance error is larger than 1.e-10 times new or original value ==> stop
               if(abs(massBalance(kCrit,lptr)) .gt. 1.e-10_r8*raer(mm)%fld(i,kCrit) & 
                  .and. abs(massBalance(kCrit,lptr)).gt.1.e-10_r8*newMass(kCrit,lptr) )then
                  stop
               endif
            endif
         enddo
      enddo
#endif


      end if  !prog_modal_aero

      if (called_from_spcam) then
      !
      ! Gas tendency
      !
           do m=1, pcnst
              if (cnst_species_class(m) == cnst_spec_class_gas) then
                ptend%lq(m) = .true.
                ptend%q(i, :, m) = (rgascol(:,m,nnew)-rgas(i,:,m)) * dtinv
              end if
           end do
      endif

   end do  ! overall_main_i_loop

#ifdef EXTRATESTS
   !check reasonable values for ncldwtr!
   do k=top_lev,pver
      if(maxval(ncldwtr(:ncol,k)) .gt. 1.e20_r8)then
         print*, "stopping (after dropmixnuc) wrong ncldwtr", maxloc(ncldwtr(:ncol,k))
            do i=1,ncol
               print*, "ncldwtr",i,k,ncldwtr(i,k)
            enddo
         call endrun("wrong ncldwtr (end of dropmixnuc)")
      end if
   end do !loop on layers
#endif

   ! end of main loop over i/longitude ....................................

   call outfld('NDROPCOL', ndropcol, pcols, lchnk)
   call outfld('NDROPSRC', nsource,  pcols, lchnk)
   call outfld('NDROPMIX', ndropmix, pcols, lchnk)
   call outfld('WTKE    ', wtke,     pcols, lchnk)

#ifndef OSLO_AERO
   !fxm: Make this work with the oslo aerosols also!
   call ccncalc(state, pbuf, cs, ccn)
#else
   if (history_aerosol) then
   call ccncalc_oslo(state &
                    , pbuf &
                    , cs     &
!+tht
                    , hasAerosol &
!-tht
                    , numberConcentration &
                    , volumeConcentration &
                    , hygroscopicity &
                    , lnSigma        &
                    , ccn           )
  end if
#endif
   if(history_aerosol) then
   do l = 1, psat
      call outfld(ccn_name(l), ccn(1,1,l), pcols, lchnk)
   enddo
   end if
#ifndef OSLO_AERO
   ! do column tendencies
   if (prog_modal_aero) then
      do m = 1, ntot_amode
         do l = 0, nspec_amode(m)
            mm = mam_idx(m,l)
            call outfld(fieldname(mm),    coltend(:,mm),    pcols, lchnk)
            call outfld(fieldname_cw(mm), coltend_cw(:,mm), pcols, lchnk)
         end do
      end do
   end if
#endif

   if(called_from_spcam) then
   !
   ! output column-integrated Gas tendency (this should be zero)
   !
        do m=1, pcnst
            if (cnst_species_class(m) == cnst_spec_class_gas) then
              do i=1, ncol
                 coltendgas(i) = sum( pdel(i,:)*ptend%q(i,:,m) )/gravit
              end do
              fieldnamegas = trim(cnst_name(m)) // '_mixnuc1sp'
              call outfld( trim(fieldnamegas), coltendgas, pcols, lchnk)
            end if
        end do
        deallocate(rgascol, coltendgas)
   end if

#ifdef OSLO_AERO
   tendencyCounted(:)=.FALSE.
   do m = 1, ntot_amode
      do l = 1, nspec_amode(m)
         mm = mam_idx(m,l)
         lptr = getTracerIndex(m,l,.false.)
         if(.NOT. tendencyCounted(lptr))then
            call outfld(fieldname(mm), coltend(:,lptr), pcols,lchnk)
            call outfld(fieldname_cw(mm), coltend_cw(:,lptr), pcols,lchnk)
            tendencyCounted(lptr)=.TRUE.
         endif
      end do
   end do
#endif

   deallocate( &
      nact,       &
      mact,       &
      raer,       &
      qqcw,       &
      raercol,    &
      raercol_cw, &
      coltend,    &
      coltend_cw, &
      naermod,    &
      hygro,      &
#ifdef OSLO_AERO
      lnsigman,   &  !Variable std. dev (CAM-Oslo)
#endif
      vaerosol,   &
      fn,         &
      fm,         &
      fluxn,      &
      fluxm       )

#ifdef OSLO_AERO
    deallocate (fluxm_tmp)
    deallocate (fluxn_tmp)
    deallocate (fm_tmp)
    deallocate (fn_tmp)
    deallocate(raercol_tracer)
    deallocate(raercol_cw_tracer)
    deallocate(mact_tracer)
    deallocate(mfullact_tracer)
#endif


end subroutine dropmixnuc

!===============================================================================

subroutine explmix( q, src, ekkp, ekkm, overlapp, overlapm, &
   qold, surfrate, flxconv, pver, dt, is_unact, qactold )

   !  explicit integration of droplet/aerosol mixing
   !     with source due to activation/nucleation


   integer, intent(in) :: pver ! number of levels
   real(r8), intent(out) :: q(pver) ! mixing ratio to be updated
   real(r8), intent(in) :: qold(pver) ! mixing ratio from previous time step
   real(r8), intent(in) :: src(pver) ! source due to activation/nucleation (/s)
   real(r8), intent(in) :: ekkp(pver) ! zn*zs*density*diffusivity (kg/m3 m2/s) at interface
   ! below layer k  (k,k+1 interface)
   real(r8), intent(in) :: ekkm(pver) ! zn*zs*density*diffusivity (kg/m3 m2/s) at interface
   ! above layer k  (k,k+1 interface)
   real(r8), intent(in) :: overlapp(pver) ! cloud overlap below
   real(r8), intent(in) :: overlapm(pver) ! cloud overlap above
   real(r8), intent(in) :: surfrate ! surface exchange rate (/s)
   real(r8), intent(in) :: flxconv ! convergence of flux from surface
   real(r8), intent(in) :: dt ! time step (s)
   logical, intent(in) :: is_unact ! true if this is an unactivated species
   real(r8), intent(in),optional :: qactold(pver)
   ! mixing ratio of ACTIVATED species from previous step
   ! *** this should only be present
   !     if the current species is unactivated number/sfc/mass

   integer k,kp1,km1

   if ( is_unact ) then
      !     the qactold*(1-overlap) terms are resuspension of activated material
      do k=top_lev,pver
         kp1=min(k+1,pver)
         km1=max(k-1,top_lev)
         q(k) = qold(k) + dt*( - src(k) + ekkp(k)*(qold(kp1) - qold(k) +       &
            qactold(kp1)*(1.0_r8-overlapp(k)))               &
            + ekkm(k)*(qold(km1) - qold(k) +     &
            qactold(km1)*(1.0_r8-overlapm(k))) )
         !        force to non-negative
         !        if(q(k)<-1.e-30)then
         !           write(iulog,*)'q=',q(k),' in explmix'
         q(k)=max(q(k),0._r8)
         !        endif
      end do

      !     diffusion loss at base of lowest layer
      q(pver)=q(pver)-surfrate*qold(pver)*dt+flxconv*dt
      !        force to non-negative
      !        if(q(pver)<-1.e-30)then
      !           write(iulog,*)'q=',q(pver),' in explmix'
      q(pver)=max(q(pver),0._r8)
      !        endif
   else
      do k=top_lev,pver
         kp1=min(k+1,pver)
         km1=max(k-1,top_lev)
         q(k) = qold(k) + dt*(src(k) + ekkp(k)*(overlapp(k)*qold(kp1)-qold(k)) +      &
            ekkm(k)*(overlapm(k)*qold(km1)-qold(k)) )
         !        force to non-negative
         !        if(q(k)<-1.e-30)then
         !           write(iulog,*)'q=',q(k),' in explmix'
         q(k)=max(q(k),0._r8)
         !        endif
      end do
      !     diffusion loss at base of lowest layer
      q(pver)=q(pver)-surfrate*qold(pver)*dt+flxconv*dt
      !        force to non-negative
      !        if(q(pver)<-1.e-30)then
      !           write(iulog,*)'q=',q(pver),' in explmix'
      q(pver)=max(q(pver),0._r8)

   end if

end subroutine explmix

!===============================================================================

subroutine activate_modal(wbar, sigw, wdiab, wminf, wmaxf, tair, rhoair,  &
   na, nmode, volume, hygro,  &
   fn, fm, fluxn, fluxm, flux_fullact, lnsigman )

   !      calculates number, surface, and mass fraction of aerosols activated as CCN
   !      calculates flux of cloud droplets, surface area, and aerosol mass into cloud
   !      assumes an internal mixture within each of up to nmode multiple aerosol modes
   !      a gaussiam spectrum of updrafts can be treated.

   !      mks units

   !      Abdul-Razzak and Ghan, A parameterization of aerosol activation.
   !      2. Multiple aerosol types. J. Geophys. Res., 105, 6837-6844.


   !      input

   real(r8), intent(in) :: wbar          ! grid cell mean vertical velocity (m/s)
   real(r8), intent(in) :: sigw          ! subgrid standard deviation of vertical vel (m/s)
   real(r8), intent(in) :: wdiab         ! diabatic vertical velocity (0 if adiabatic)
   real(r8), intent(in) :: wminf         ! minimum updraft velocity for integration (m/s)
   real(r8), intent(in) :: wmaxf         ! maximum updraft velocity for integration (m/s)
   real(r8), intent(in) :: tair          ! air temperature (K)
   real(r8), intent(in) :: rhoair        ! air density (kg/m3)
   real(r8), intent(in) :: na(:)      ! aerosol number concentration (/m3)
   integer,  intent(in) :: nmode      ! number of aerosol modes
   real(r8), intent(in) :: volume(:)  ! aerosol volume concentration (m3/m3)
   real(r8), intent(in) :: hygro(:)   ! hygroscopicity of aerosol mode
   real(r8), intent(in), optional :: lnsigman(:)

   !      output

   real(r8), intent(out) :: fn(:)      ! number fraction of aerosols activated
   real(r8), intent(out) :: fm(:)      ! mass fraction of aerosols activated
   real(r8), intent(out) :: fluxn(:)   ! flux of activated aerosol number fraction into cloud (cm/s)
   real(r8), intent(out) :: fluxm(:)   ! flux of activated aerosol mass fraction into cloud (cm/s)
   real(r8), intent(out) :: flux_fullact   ! flux of activated aerosol fraction assuming 100% activation (cm/s)
   !    rce-comment
   !    used for consistency check -- this should match (ekd(k)*zs(k))
   !    also, fluxm/flux_fullact gives fraction of aerosol mass flux
   !       that is activated

   !      local

   integer, parameter:: nx=200
   integer iquasisect_option, isectional
   real(r8) integ,integf
   real(r8), parameter :: p0 = 1013.25e2_r8    ! reference pressure (Pa)
   real(r8) xmin(nmode),xmax(nmode) ! ln(r) at section interfaces
   real(r8) volmin(nmode),volmax(nmode) ! volume at interfaces
   real(r8) tmass ! total aerosol mass concentration (g/cm3)
   real(r8) sign(nmode)    ! geometric standard deviation of size distribution
   real(r8) rm ! number mode radius of aerosol at max supersat (cm)
   real(r8) pres ! pressure (Pa)
   real(r8) path ! mean free path (m)
   real(r8) diff ! diffusivity (m2/s)
   real(r8) conduct ! thermal conductivity (Joule/m/sec/deg)
   real(r8) diff0,conduct0
   real(r8) es ! saturation vapor pressure
   real(r8) qs ! water vapor saturation mixing ratio
   real(r8) dqsdt ! change in qs with temperature
   real(r8) dqsdp ! change in qs with pressure
   real(r8) g ! thermodynamic function (m2/s)
   real(r8) zeta(nmode), eta(nmode)
   real(r8) lnsmax ! ln(smax)
   real(r8) alpha
   real(r8) gamma
   real(r8) beta
   real(r8) sqrtg
   real(r8) :: amcube(nmode) ! cube of dry mode radius (m)
   !++alfgr (ununsed) real(r8) :: smcrit(nmode) ! critical supersatuation for activation
   real(r8) :: lnsm(nmode) ! ln(smcrit)
   real(r8) smc(nmode) ! critical supersaturation for number mode radius
   real(r8) sumflx_fullact
   real(r8) sumflxn(nmode)
   real(r8) sumflxm(nmode)
   real(r8) sumfn(nmode)
   real(r8) sumfm(nmode)
   real(r8) fnold(nmode)   ! number fraction activated
   real(r8) fmold(nmode)   ! mass fraction activated
   real(r8) exp45logsig_var(nmode)  !variable std. dev (CAM-Oslo)
   real(r8), target :: f1_var(nmode), f2_var(nmode)
   real(r8) wold,gold
   real(r8) alogam
   real(r8) rlo,rhi,xint1,xint2,xint3,xint4
   real(r8) wmin,wmax,w,dw,dwmax,dwmin,wnuc,dwnew,wb
   real(r8) dfmin,dfmax,fnew,fold,fnmin,fnbar,fsbar,fmbar
   real(r8) alw,sqrtalw
   real(r8) smax
   real(r8) x,arg
   real(r8) xmincoeff,xcut,volcut,surfcut
   real(r8) z,z1,z2,wf1,wf2,zf1,zf2,gf1,gf2,gf
   real(r8) etafactor1,etafactor2(nmode),etafactor2max
   real(r8) grow
   character(len=*), parameter :: subname='activate_modal'
   integer m,n
   !      numerical integration parameters
   real(r8), parameter :: eps=0.3_r8,fmax=0.99_r8,sds=3._r8

   real(r8), parameter :: namin=1.e6_r8   ! minimum aerosol number concentration (/m3)

   integer ndist(nx)  ! accumulates frequency distribution of integration bins required
   data ndist/nx*0/
   save ndist

   fn(:)=0._r8
   fm(:)=0._r8
   fluxn(:)=0._r8
   fluxm(:)=0._r8
   flux_fullact=0._r8

   if(nmode.eq.1.and.na(1).lt.1.e-20_r8)return

   if(sigw.le.1.e-5_r8.and.wbar.le.0._r8)return

   pres=rair*rhoair*tair
   diff0=0.211e-4_r8*(p0/pres)*(tair/t0)**1.94_r8
   conduct0=(5.69_r8+0.017_r8*(tair-t0))*4.186e2_r8*1.e-5_r8 ! convert to J/m/s/deg
   call qsat(tair, pres, es, qs)
   dqsdt=latvap/(rh2o*tair*tair)*qs
   alpha=gravit*(latvap/(cpair*rh2o*tair*tair)-1._r8/(rair*tair))
   gamma=(1.0_r8+latvap/cpair*dqsdt)/(rhoair*qs)
   etafactor2max=1.e10_r8/(alpha*wmaxf)**1.5_r8 ! this should make eta big if na is very small.

   grow  = 1._r8/(rhoh2o/(diff0*rhoair*qs)  &
           + latvap*rhoh2o/(conduct0*tair)*(latvap/(rh2o*tair) - 1._r8))
   sqrtg = sqrt(grow)
   beta  = 2._r8*pi*rhoh2o*grow*gamma

   do m=1,nmode

      if(volume(m).gt.1.e-39_r8.and.na(m).gt.1.e-39_r8)then
         !            number mode radius (m)
         !           write(iulog,*)'alogsig,volc,na=',alogsig(m),volc(m),na(m)
#ifdef OSLO_AERO
         if(present(lnsigman))then
            exp45logsig_var(m) = exp(4.5_r8*lnsigman(m)*lnsigman(m))
            amcube(m)=(3._r8*volume(m)/(4._r8*pi*exp45logsig_var(m)*na(m)))  ! only if variable size dist
            f1_var(m)          = 0.5_r8*exp(2.5_r8*lnsigman(m)*lnsigman(m))
            f2_var(m)          = 1._r8 + 0.25_r8*lnsigman(m)
         else
            call endrun("Problem with variable std. dev")
         endif
#else 
   !Std cam
         amcube(m)=(3._r8*volume(m)/(4._r8*pi*exp45logsig(m)*na(m)))  ! only if variable size dist
#endif
         !           growth coefficent Abdul-Razzak & Ghan 1998 eqn 16
         !           should depend on mean radius of mode to account for gas kinetic effects
         !           see Fountoukis and Nenes, JGR2005 and Meskhidze et al., JGR2006
         !           for approriate size to use for effective diffusivity.
         etafactor2(m)=1._r8/(na(m)*beta*sqrtg)
         if(hygro(m).gt.1.e-10_r8)then
            smc(m)=2._r8*aten*sqrt(aten/(27._r8*hygro(m)*amcube(m))) ! only if variable size dist
         else
            smc(m)=100._r8
         endif
         !	    write(iulog,*)'sm,hygro,amcube=',smcrit(m),hygro(m),amcube(m)
      else
         smc(m)=1._r8
         etafactor2(m)=etafactor2max ! this should make eta big if na is very small.
      endif
      lnsm(m)=log(smc(m)) ! only if variable size dist
      !	 write(iulog,'(a,i4,4g12.2)')'m,na,amcube,hygro,sm,lnsm=', &
      !                   m,na(m),amcube(m),hygro(m),sm(m),lnsm(m)
   enddo

   if(sigw.gt.1.e-5_r8)then ! spectrum of updrafts

      wmax=min(wmaxf,wbar+sds*sigw)
      wmin=max(wminf,-wdiab)
      wmin=max(wmin,wbar-sds*sigw)
      w=wmin
      dwmax=eps*sigw
      dw=dwmax
      dfmax=0.2_r8
      dfmin=0.1_r8
      if (wmax <= w) return
      do m=1,nmode
         sumflxn(m)=0._r8
         sumfn(m)=0._r8
         fnold(m)=0._r8
         sumflxm(m)=0._r8
         sumfm(m)=0._r8
         fmold(m)=0._r8
      enddo
      sumflx_fullact=0._r8

      fold=0._r8
      wold=0._r8
      gold=0._r8

      dwmin = min( dwmax, 0.01_r8 )
      do n = 1, nx

100      wnuc=w+wdiab
         !           write(iulog,*)'wnuc=',wnuc
         alw=alpha*wnuc
         sqrtalw=sqrt(alw)
         etafactor1=alw*sqrtalw

         do m=1,nmode
            eta(m)=etafactor1*etafactor2(m)
            zeta(m)=twothird*sqrtalw*aten/sqrtg
         enddo

         call maxsat(zeta,eta,nmode,smc,smax &
#ifdef OSLO_AERO
                     ,f1_var, f2_var         &
#endif
                     )
         !	      write(iulog,*)'w,smax=',w,smax

         lnsmax=log(smax)

#ifdef OSLO_AERO
         x=twothird*(lnsm(nmode)-lnsmax)/(sq2*lnsigman(nmode))
#else
         x=twothird*(lnsm(nmode)-lnsmax)/(sq2*alogsig(nmode))
#endif
         fnew=0.5_r8*(1._r8-erf(x))


         dwnew = dw
         if(fnew-fold.gt.dfmax.and.n.gt.1)then
            !              reduce updraft increment for greater accuracy in integration
            if (dw .gt. 1.01_r8*dwmin) then
               dw=0.7_r8*dw
               dw=max(dw,dwmin)
               w=wold+dw
               go to 100
            else
               dwnew = dwmin
            endif
         endif

         if(fnew-fold.lt.dfmin)then
            !              increase updraft increment to accelerate integration
            dwnew=min(1.5_r8*dw,dwmax)
         endif
         fold=fnew

         z=(w-wbar)/(sigw*sq2)
         g=exp(-z*z)
         fnmin=1._r8
         xmincoeff=alogaten-twothird*(lnsmax-alog2)-alog3

         do m=1,nmode
            !              modal
#ifdef OSLO_AERO
            x=twothird*(lnsm(m)-lnsmax)/(sq2*lnsigman(m))
#else
            x=twothird*(lnsm(m)-lnsmax)/(sq2*alogsig(m))
#endif
            fn(m)=0.5_r8*(1._r8-erf(x))
            fnmin=min(fn(m),fnmin)
            !               integration is second order accurate
            !               assumes linear variation of f*g with w
            fnbar=(fn(m)*g+fnold(m)*gold)
#ifdef OSLO_AERO
            arg=x-1.5_r8*sq2*lnsigman(m)
#else
            arg=x-1.5_r8*sq2*alogsig(m)
#endif
            fm(m)=0.5_r8*(1._r8-erf(arg))
            fmbar=(fm(m)*g+fmold(m)*gold)
            wb=(w+wold)
            if(w.gt.0._r8)then
               sumflxn(m)=sumflxn(m)+sixth*(wb*fnbar           &
                  +(fn(m)*g*w+fnold(m)*gold*wold))*dw
               sumflxm(m)=sumflxm(m)+sixth*(wb*fmbar           &
                  +(fm(m)*g*w+fmold(m)*gold*wold))*dw
            endif
            sumfn(m)=sumfn(m)+0.5_r8*fnbar*dw
            !	       write(iulog,'(a,9g10.2)')'lnsmax,lnsm(m),x,fn(m),fnold(m),g,gold,fnbar,dw=',lnsmax,lnsm(m),x,fn(m),fnold(m),g,gold,fnbar,dw
            fnold(m)=fn(m)
            sumfm(m)=sumfm(m)+0.5_r8*fmbar*dw
            fmold(m)=fm(m)
         enddo
         !           same form as sumflxm but replace the fm with 1.0
         sumflx_fullact = sumflx_fullact &
            + sixth*(wb*(g+gold) + (g*w+gold*wold))*dw
         !            sumg=sumg+0.5_r8*(g+gold)*dw
         gold=g
         wold=w
         dw=dwnew
         if (n > 1 .and. (w > wmax .or. fnmin > fmax)) exit
         w=w+dw
         if (n == nx) then
            write(iulog,*)'do loop is too short in activate'
            write(iulog,*)'wmin=',wmin,' w=',w,' wmax=',wmax,' dw=',dw
            write(iulog,*)'wbar=',wbar,' sigw=',sigw,' wdiab=',wdiab
            write(iulog,*)'wnuc=',wnuc
            write(iulog,*)'na=',(na(m),m=1,nmode)
            write(iulog,*)'fn=',(fn(m),m=1,nmode)
            !   dump all subr parameters to allow testing with standalone code
            !   (build a driver that will read input and call activate)
            write(iulog,*)'wbar,sigw,wdiab,tair,rhoair,nmode='
            write(iulog,*) wbar,sigw,wdiab,tair,rhoair,nmode
            write(iulog,*)'na=',na
            write(iulog,*)'volume=', (volume(m),m=1,nmode)
            write(iulog,*)'hydro='
            write(iulog,*) hygro
            call endrun(subname)
         end if

      enddo

      ndist(n)=ndist(n)+1
      if(w.lt.wmaxf)then

         !            contribution from all updrafts stronger than wmax
         !            assuming constant f (close to fmax)
         wnuc=w+wdiab

         z1=(w-wbar)/(sigw*sq2)
         z2=(wmaxf-wbar)/(sigw*sq2)
         g=exp(-z1*z1)
         integ=sigw*0.5_r8*sq2*sqpi*(erf(z2)-erf(z1))
         !            consider only upward flow into cloud base when estimating flux
         wf1=max(w,zero)
         zf1=(wf1-wbar)/(sigw*sq2)
         gf1=exp(-zf1*zf1)
         wf2=max(wmaxf,zero)
         zf2=(wf2-wbar)/(sigw*sq2)
         gf2=exp(-zf2*zf2)
         gf=(gf1-gf2)
         integf=wbar*sigw*0.5_r8*sq2*sqpi*(erf(zf2)-erf(zf1))+sigw*sigw*gf

         do m=1,nmode
            sumflxn(m)=sumflxn(m)+integf*fn(m)
            sumfn(m)=sumfn(m)+fn(m)*integ
            sumflxm(m)=sumflxm(m)+integf*fm(m)
            sumfm(m)=sumfm(m)+fm(m)*integ
         enddo
         !           same form as sumflxm but replace the fm with 1.0
         sumflx_fullact = sumflx_fullact + integf
         !            sumg=sumg+integ
      endif


      do m=1,nmode
         fn(m)=sumfn(m)/(sq2*sqpi*sigw)
         !            fn(m)=sumfn(m)/(sumg)
         if(fn(m).gt.1.01_r8)then
            write(iulog,*)'fn=',fn(m),' > 1 in activate'
            write(iulog,*)'w,m,na,amcube=',w,m,na(m),amcube(m)
            write(iulog,*)'integ,sumfn,sigw=',integ,sumfn(m),sigw
            call endrun('activate')
         endif
         fluxn(m)=sumflxn(m)/(sq2*sqpi*sigw)
         fm(m)=sumfm(m)/(sq2*sqpi*sigw)
         !            fm(m)=sumfm(m)/(sumg)
         if(fm(m).gt.1.01_r8)then
            write(iulog,*)'fm=',fm(m),' > 1 in activate'
         endif
         fluxm(m)=sumflxm(m)/(sq2*sqpi*sigw)
      enddo
      !        same form as fluxm
      flux_fullact = sumflx_fullact/(sq2*sqpi*sigw)

   else

      !        single updraft
      wnuc=wbar+wdiab

      if(wnuc.gt.0._r8)then

         w=wbar
         alw=alpha*wnuc
         sqrtalw=sqrt(alw)
         etafactor1=alw*sqrtalw

         do m=1,nmode
            eta(m)=etafactor1*etafactor2(m)
            zeta(m)=twothird*sqrtalw*aten/sqrtg
#ifdef OSLO_AERO
            if(present(lnsigman))then
               f1_var(m)          = 0.5_r8*exp(2.5_r8*lnsigman(m)*lnsigman(m))
               f2_var(m)          = 1._r8 + 0.25_r8*lnsigman(m)
            else
               call endrun("Problem with variable std. dev single updraft")
             endif
#endif
         enddo

         call maxsat(zeta,eta,nmode,smc,smax &
#ifdef OSLO_AERO
                     ,f1_var, f2_var         &
#endif
                     )

         lnsmax=log(smax)
         xmincoeff=alogaten-twothird*(lnsmax-alog2)-alog3


         do m=1,nmode
#ifdef OSLO_AERO
            x=twothird*(lnsm(m)-lnsmax)/(sq2*lnsigman(m))
#else
            x=twothird*(lnsm(m)-lnsmax)/(sq2*alogsig(m))
#endif
            fn(m)=0.5_r8*(1._r8-erf(x))
#ifdef OSLO_AERO
            arg=x-1.5_r8*sq2*lnsigman(m)
#else
            arg=x-1.5_r8*sq2*alogsig(m)
#endif
            fm(m)=0.5_r8*(1._r8-erf(arg))
            if(wbar.gt.0._r8)then
               fluxn(m)=fn(m)*w
               fluxm(m)=fm(m)*w
            endif
         enddo
         flux_fullact = w
      endif

   endif

end subroutine activate_modal

!===============================================================================

subroutine maxsat(zeta,eta,nmode,smc,smax, f1_in, f2_in)

   !      calculates maximum supersaturation for multiple
   !      competing aerosol modes.

   !      Abdul-Razzak and Ghan, A parameterization of aerosol activation.
   !      2. Multiple aerosol types. J. Geophys. Res., 105, 6837-6844.

   integer,  intent(in)  :: nmode ! number of modes
   real(r8), intent(in)  :: smc(nmode) ! critical supersaturation for number mode radius
   real(r8), intent(in)  :: zeta(nmode)
   real(r8), intent(in)  :: eta(nmode)
   real(r8), intent(in), optional, target :: f1_in(:)
   real(r8), intent(in), optional, target :: f2_in(:)

   real(r8), intent(out) :: smax ! maximum supersaturation
   integer  :: m  ! mode index
   real(r8) :: sum, g1, g2, g1sqrt, g2sqrt
   real(r8), pointer :: f1_used(:), f2_used(:)

#ifdef OSLO_AERO
      f1_used => f1_in
      f2_used => f2_in
#else
      f1_used => f1
      f2_used => f2
#endif


   do m=1,nmode
      if(zeta(m).gt.1.e5_r8*eta(m).or.smc(m)*smc(m).gt.1.e5_r8*eta(m))then
         !            weak forcing. essentially none activated
         smax=1.e-20_r8
      else
         !            significant activation of this mode. calc activation all modes.
         exit
      endif
          ! No significant activation in any mode.  Do nothing.
      if (m == nmode) return

   enddo

   sum=0.0_r8
   do m=1,nmode
      if(eta(m).gt.1.e-20_r8)then
         g1=zeta(m)/eta(m)
         g1sqrt=sqrt(g1)
         g1=g1sqrt*g1
         g2=smc(m)/sqrt(eta(m)+3._r8*zeta(m))
         g2sqrt=sqrt(g2)
         g2=g2sqrt*g2
         sum=sum+(f1_used(m)*g1+f2_used(m)*g2)/(smc(m)*smc(m))
      else
         sum=1.e20_r8
      endif
   enddo

   smax=1._r8/sqrt(sum)

end subroutine maxsat

!===============================================================================

#ifndef OSLO_AERO
subroutine ccncalc(state, pbuf, cs, ccn)

   ! calculates number concentration of aerosols activated as CCN at
   ! supersaturation supersat.
   ! assumes an internal mixture of a multiple externally-mixed aerosol modes
   ! cgs units

   ! Ghan et al., Atmos. Res., 1993, 198-221.

   ! arguments

   type(physics_state), target, intent(in)    :: state
   type(physics_buffer_desc),   pointer       :: pbuf(:)


   real(r8), intent(in)  :: cs(pcols,pver)       ! air density (kg/m3)
   real(r8), intent(out) :: ccn(pcols,pver,psat) ! number conc of aerosols activated at supersat (#/m3)

   ! local

   integer :: lchnk ! chunk index
   integer :: ncol  ! number of columns
   real(r8), pointer :: tair(:,:)     ! air temperature (K)

   real(r8) naerosol(pcols) ! interstit+activated aerosol number conc (/m3)
   real(r8) vaerosol(pcols) ! interstit+activated aerosol volume conc (m3/m3)

   real(r8) amcube(pcols)
   real(r8) super(psat) ! supersaturation
   real(r8), allocatable :: amcubecoef(:)
   real(r8), allocatable :: argfactor(:)
   real(r8) :: surften       ! surface tension of water w/respect to air (N/m)
   real(r8) surften_coef
   real(r8) a(pcols) ! surface tension parameter
   real(r8) hygro(pcols)  ! aerosol hygroscopicity
   real(r8) sm(pcols)  ! critical supersaturation at mode radius
   real(r8) arg(pcols)
   !     mathematical constants
   real(r8) twothird,sq2
   integer l,m,n,i,k
   real(r8) log,cc
   real(r8) smcoefcoef,smcoef(pcols)
   integer phase ! phase of aerosol
   !-------------------------------------------------------------------------------

   lchnk = state%lchnk
   ncol  = state%ncol
   tair  => state%t

   allocate( &
      amcubecoef(ntot_amode), &
      argfactor(ntot_amode)   )

   super(:)=supersat(:)*0.01_r8
   sq2=sqrt(2._r8)
   twothird=2._r8/3._r8
   surften=0.076_r8
   surften_coef=2._r8*mwh2o*surften/(r_universal*rhoh2o)
   smcoefcoef=2._r8/sqrt(27._r8)

   do m=1,ntot_amode
      amcubecoef(m)=3._r8/(4._r8*pi*exp45logsig(m))
      argfactor(m)=twothird/(sq2*alogsig(m))
   end do

   ccn = 0._r8
   do k=top_lev,pver

      do i=1,ncol
         a(i)=surften_coef/tair(i,k)
         smcoef(i)=smcoefcoef*a(i)*sqrt(a(i))
      end do

      do m=1,ntot_amode

         phase=3 ! interstitial+cloudborne

         call loadaer( &
            state, pbuf, 1, ncol, k, &
            m, cs, phase, naerosol, vaerosol, &
            hygro)

         where(naerosol(:ncol)>1.e-3_r8)
            amcube(:ncol)=amcubecoef(m)*vaerosol(:ncol)/naerosol(:ncol)
            sm(:ncol)=smcoef(:ncol)/sqrt(hygro(:ncol)*amcube(:ncol)) ! critical supersaturation
         elsewhere
            sm(:ncol)=1._r8 ! value shouldn't matter much since naerosol is small
         endwhere
         do l=1,psat
            do i=1,ncol
               arg(i)=argfactor(m)*log(sm(i)/super(l))
               ccn(i,k,l)=ccn(i,k,l)+naerosol(i)*0.5_r8*(1._r8-erf(arg(i)))
            enddo
         enddo
      enddo
   enddo
   ccn(:ncol,:,:)=ccn(:ncol,:,:)*1.e-6_r8 ! convert from #/m3 to #/cm3

   deallocate( &
      amcubecoef, &
      argfactor   )

end subroutine ccncalc

#else

subroutine ccncalc_oslo(state &
                     , pbuf   &
                     , cs     &
!+tht
                     , hasAerosol &
!-tht
                     , numberConcentration &
                     , volumeConcentration &
                     , hygroscopicity &
                     , lnSigma        &
                     , ccn           )

   ! calculates number concentration of aerosols activated as CCN at
   ! supersaturation supersat.
   ! assumes an internal mixture of a multiple externally-mixed aerosol modes
   ! cgs units

   ! This was used in the BACCHUS-project where it was agreed that 
   ! CCN would not include cloud-borne aerosols. It is possible to 
   ! calculate cloud-borne aerosols, but it is complicated, and it was
   ! not needed when this code was made.

   ! arguments

   type(physics_state), target, intent(in)    :: state
   type(physics_buffer_desc),   pointer       :: pbuf(:)

   real(r8), intent(in)  :: cs(pcols,pver)       ! air density (kg/m3)
   real(r8), intent(out) :: ccn(pcols,pver,psat) ! number conc of aerosols activated at supersat (#/m3)
!+tht
   logical, intent(in)  :: hasAerosol(pcols, pver, nmodes)
!-tht
!akc6   real(r8), intent(in) ::  numberConcentration(pcols,pver, nmodes) ! interstit+activated aerosol number conc (/m3)
   real(r8), intent(in) ::  numberConcentration(pcols,pver,0:nmodes) !   interstit+activated aerosol number conc (/m3)
!akc6-
   real(r8), intent(in) ::  volumeConcentration(pcols,pver,nmodes)  ! interstit+activated aerosol volume conc (m3/m3)
   real(r8), intent(in) ::  hygroscopicity(pcols,pver,nmodes)
   real(r8), intent(in) ::  lnSigma(pcols,pver,nmodes)

   ! local
   integer :: lchnk ! chunk index
   integer :: ncol  ! number of columns
   real(r8), pointer :: tair(:,:)     ! air temperature (K)


   real(r8) super(psat) ! supersaturation
   real(r8) surften_coef !Coefficient in ARGI / ARGII
   real(r8) amcube       !number median radius qubed
   real(r8) a    ! surface tension parameter
   real(r8) sm   ! critical supersaturation at mode radius
   real(r8) arg  ! factor in eqn 15 ARGII
   real(r8) argfactor !Coefficient in ARGI/ARGII 
   !     mathematical constants
   real(r8), parameter:: twothird=2.0_r8/3.0_r8
   real(r8), parameter:: sq2=sqrt(2.0_r8)
   real(r8), parameter :: surften=0.076_r8 !surface tension of water (J/m2)
   real(r8) exp45logsig_var
   integer lsat,m,i,k
   real(r8) smcoefcoef,smcoef
   !-------------------------------------------------------------------------------

   lchnk = state%lchnk
   ncol  = state%ncol
   tair  => state%t

   super(:)=supersat(:)*0.01_r8
 
   !This is curvature effect (A) in ARGI
   !eqn 5 in ARG1 (missing division by temperature, see below)
   surften_coef=2._r8*mwh2o*surften/(r_universal*rhoh2o)

   !This is part of eqn 9 in ARGII 
   !where A smcoefcoef is 2/3^(3/2) 
   smcoefcoef=2._r8/sqrt(27._r8)

   ccn(:,:,:) = 0._r8

   do m=1,nmodes
      do k=top_lev,pver

         do i=1,ncol

!+tht
          if (hasAerosol(i,k,m)) then
!-tht
            !Curvature-parameter "A" in ARGI (eqn 5)
            a = surften_coef/tair(i,k)

            !standard factor for transforming size distr
            !volume ==> number (google psd.pdf by zender)
            exp45logsig_var = & 
               exp(4.5_r8*lnsigma(i,k,m)*lnsigma(i,k,m))

            !Numbe rmedian radius (power of three)
            !By definition of lognormal distribution
            amcube =(3._r8*volumeConcentration(i,k,m) & 
               /(4._r8*pi*exp45logsig_var*numberConcentration(i,k,m)))  ! only if variable size dist


            !This is part of eqn 9 in ARGII 
            !where A smcoefcoef is 2/3^(3/2) 
            smcoef = smcoefcoef * a * sqrt(a)

            !This is finally solving eqn 9 
            !(solve for critical supersat of mode)
            sm=smcoef   &
               / sqrt(hygroscopicity(i,k,m)*amcube) ! critical supersaturation

            !Solve eqn 13 in ARGII
            do lsat = 1,psat
   
               !eqn 15 in ARGII
               argfactor=twothird/(sq2*lnSigma(i,k,m))

               !eqn 15 in ARGII
               arg=argfactor*log(sm/super(lsat))

               !eqn 13 i ARGII
               ccn(i,k,lsat)=ccn(i,k,lsat) &
                  +numberConcentration(i,k,m)&
                  *0.5_r8*(1._r8-erf(arg))
            end do
!+tht
          else
            do lsat = 1, psat
               ccn(i,k,lsat)=0._r8
            enddo
          endif
!-tht
         end do
      end do
   end do

   ccn(:ncol,:,:)=ccn(:ncol,:,:)*1.e-6_r8 ! convert from #/m3 to #/cm3

end subroutine ccncalc_oslo
#endif

!===============================================================================

subroutine loadaer( &
   state, pbuf, istart, istop, k, &
   m, cs, phase, naerosol, &
   vaerosol, hygro)

   ! return aerosol number, volume concentrations, and bulk hygroscopicity

   ! input arguments
   type(physics_state), target, intent(in) :: state
   type(physics_buffer_desc),   pointer    :: pbuf(:)

   integer,  intent(in) :: istart      ! start column index (1 <= istart <= istop <= pcols)
   integer,  intent(in) :: istop       ! stop column index  
   integer,  intent(in) :: m           ! mode index
   integer,  intent(in) :: k           ! level index
   real(r8), intent(in) :: cs(:,:)     ! air density (kg/m3)
   integer,  intent(in) :: phase       ! phase of aerosol: 1 for interstitial, 2 for cloud-borne, 3 for sum

   ! output arguments
   real(r8), intent(out) :: naerosol(:)  ! number conc (1/m3)
   real(r8), intent(out) :: vaerosol(:)  ! volume conc (m3/m3)
   real(r8), intent(out) :: hygro(:)     ! bulk hygroscopicity of mode

   ! internal
   integer  :: lchnk               ! chunk identifier

   real(r8), pointer :: raer(:,:) ! interstitial aerosol mass, number mixing ratios
   real(r8), pointer :: qqcw(:,:) ! cloud-borne aerosol mass, number mixing ratios
   real(r8) :: specdens, spechygro

   real(r8) :: vol(pcols) ! aerosol volume mixing ratio
   integer  :: i, l
   !-------------------------------------------------------------------------------

   lchnk = state%lchnk

   do i = istart, istop
      vaerosol(i) = 0._r8
      hygro(i)    = 0._r8
   end do

   do l = 1, nspec_amode(m)

      call rad_cnst_get_aer_mmr(0, m, l, 'a', state, pbuf, raer)
      call rad_cnst_get_aer_mmr(0, m, l, 'c', state, pbuf, qqcw)
      call rad_cnst_get_aer_props(0, m, l, density_aer=specdens, hygro_aer=spechygro)

      if (phase == 3) then
         do i = istart, istop
            vol(i) = max(raer(i,k) + qqcw(i,k), 0._r8)/specdens
         end do
      else if (phase == 2) then
         do i = istart, istop
            vol(i) = max(qqcw(i,k), 0._r8)/specdens
         end do
      else if (phase == 1) then
         do i = istart, istop
            vol(i) = max(raer(i,k), 0._r8)/specdens
         end do
      else
         write(iulog,*)'phase=',phase,' in loadaer'
         call endrun('phase error in loadaer')
      end if

      do i = istart, istop
         vaerosol(i) = vaerosol(i) + vol(i)
         hygro(i)    = hygro(i) + vol(i)*spechygro
      end do

   end do

   do i = istart, istop
      if (vaerosol(i) > 1.0e-30_r8) then   ! +++xl add 8/2/2007
         hygro(i)    = hygro(i)/(vaerosol(i))
         vaerosol(i) = vaerosol(i)*cs(i,k)
      else
         hygro(i)    = 0.0_r8
         vaerosol(i) = 0.0_r8
      end if
   end do

   ! aerosol number
   call rad_cnst_get_mode_num(0, m, 'a', state, pbuf, raer)
   call rad_cnst_get_mode_num(0, m, 'c', state, pbuf, qqcw)
   if (phase == 3) then
      do i = istart, istop
         naerosol(i) = (raer(i,k) + qqcw(i,k))*cs(i,k)
      end do
   else if (phase == 2) then
      do i = istart, istop
         naerosol(i) = qqcw(i,k)*cs(i,k)
      end do
   else
      do i = istart, istop
         naerosol(i) = raer(i,k)*cs(i,k)
      end do
   end if
   ! adjust number so that dgnumlo < dgnum < dgnumhi
   do i = istart, istop
      naerosol(i) = max(naerosol(i), vaerosol(i)*voltonumbhi_amode(m))
      naerosol(i) = min(naerosol(i), vaerosol(i)*voltonumblo_amode(m))
   end do

end subroutine loadaer

!===============================================================================

end module ndrop




