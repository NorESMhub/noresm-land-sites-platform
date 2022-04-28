module aerosoldef

!---------------------------------------------------------------------------------
! Module to set up register aerosols indexes, number of gas and particle 
! species and their scavenging rates. Tables for humidity growth
!---------------------------------------------------------------------------------
! Modified Spring 2015 by cka to include a version of RM's treatment of soa. (Makkonen et al. 2012)
! Modified Summer 2015 by ak to include a new treatment of sea-salt (Salter et al. 2015)

   use commondefinitions
   use modal_aero_data, only: qqcw_set_ptr
   use mo_tracname, only : solsym
   use shr_kind_mod, only: r8 => shr_kind_r8
   use constituents, only: pcnst, cnst_name,cnst_get_ind
   use cam_abortutils,       only: endrun

  implicit none
  save
  private          ! Make default type private to the module

   integer, public, parameter  :: max_tracers_per_mode = 7
   real(r8), public,dimension (pcnst) ::rhopart
   real(r8), public,dimension (pcnst) ::sgpart
   real(r8), public,dimension (pcnst) ::osmoticCoefficient
   real(r8), public,dimension (pcnst) ::numberOfIons
   real(r8), public,dimension (pcnst) ::solubleMassFraction
   integer, public,dimension (pcnst) ::aerosolType
   real(r8), public, dimension(nbmodes) :: numberFractionAvailableAqChem
   real(r8), public,dimension (pcnst) :: invrhopart


   real(r8), public, parameter :: smallConcentration = 1.e-100_r8 !duplicate, sync with smallNumber in Const
!
! Public interfaces
!
   public aero_register           ! register consituents
   public is_process_mode         ! Check is an aerosol specie is a process mode
   public isAerosol               ! Check is specie is aerosol (i.e. gases get .FALSE. here)
   public getTracerIndex
   public getNumberOfTracersInMode 
   public getNumberOfBackgroundTracersInMode
   public getCloudTracerIndex
   public getCloudTracerIndexDirect
   public getCloudTracerName
   public chemistryIndex
   public physicsIndex
   public getDryDensity
   public getConstituentFraction
   public isTracerInMode
   public fillAerosolTracerList
   public getNumberOfAerosolTracers
   public fillInverseAerosolTracerList

!cka: Add SOA particles to mode 1 and 11
    integer, parameter, public :: MODE_IDX_BC_EXT_AC             = 0   !Externally mixed BC accumulation mode
    integer, parameter, public :: MODE_IDX_SO4SOA_AIT            = 1   !SO4 and SOA in aitken mode, Created from 11 by growth (condensation) of SO4
!cka    integer, parameter, public :: MODE_IDX_SO4_AIT               = 1   !Pure SO4 in aitken mode, Created from 11 by growth (condensation) of SO4
    integer, parameter, public :: MODE_IDX_BC_AIT                = 2   !Created from 12 by growth (condensation)  SO4
    integer, parameter, public :: MODE_IDX_NOT_USED              = 3   !Not used
    integer, parameter, public :: MODE_IDX_OMBC_INTMIX_COAT_AIT  = 4   !Created from 14 by growth (condensation) of SO4 and from cloud processing/wet-phas
    integer, parameter, public :: MODE_IDX_SO4_AC                = 5   !Accumulation mode SO4 (mode will have other comps added)
    integer, parameter, public :: MODE_IDX_DST_A2                = 6   !Accumulation mode dust (mode will have other comps added)
    integer, parameter, public :: MODE_IDX_DST_A3                = 7   !Coarse mode dust (mode will have other comps added)
    integer, parameter, public :: MODE_IDX_SS_A1                 = 8   !Fine mode sea-salt (mode will have other comps added)
    integer, parameter, public :: MODE_IDX_SS_A2                 = 9   !Accumulation mode sea-salt (mode will have other comps added)
    integer, parameter, public :: MODE_IDX_SS_A3                 = 10  !Coarse mode sea-salt (mode will have other comps added)
    integer, parameter, public :: MODE_IDX_SO4SOA_NUC            = 11  !SO4 and SOA nucleation mode
!cka    integer, parameter, public :: MODE_IDX_SO4_NUC               = 11  !SO4 nucleation mode
    integer, parameter, public :: MODE_IDX_BC_NUC                = 12  !BC nucleation mode
    integer, parameter, public :: MODE_IDX_LUMPED_ORGANICS       = 13  !not used in lifecycle, but some extra mass goes here when max. allowed LUT conc. are too small 
    integer, parameter, public :: MODE_IDX_OMBC_INTMIX_AIT       = 14  !mix quickly formed in fire-plumes

   integer, parameter, public :: numberOfExternallyMixedModes = 4   !Modes 0;11-14 (13 is not used in lifecycle)
   integer, parameter, public :: numberOfInternallyMIxedMOdes = 9   !Modes 1-10 (3 is not used in lifecycle)
   
   integer, parameter, public :: numberOfProcessModeTracers    = 6
   integer, public, dimension(numberOfProcessModeTracers) :: tracerInProcessMode
   integer, public, dimension(pcnst)                      :: processModeMap
   
   !These tables describe how the tracers behave chemically
   integer, dimension(numberOfExternallyMixedModes), public :: externallyMixedMode = (/MODE_IDX_BC_EXT_AC,MODE_IDX_SO4SOA_NUC, MODE_IDX_BC_NUC, MODE_IDX_OMBC_INTMIX_AIT /)
   integer, dimension(numberOfInternallyMixedMOdes), public :: internallyMixedMode = (/MODE_IDX_SO4SOA_AIT, MODE_IDX_BC_AIT, MODE_IDX_OMBC_INTMIX_COAT_AIT &
                                                                                       ,MODE_IDX_SO4_AC, MODE_IDX_DST_A2, MODE_IDX_DST_A3, MODE_IDX_SS_A1 &
                                                                                       ,MODE_IDX_SS_A2, MODE_IDX_SS_A3 /)

!cka: add l_soa_n, l_soa_na (particles) l_soa_a1 (condensate) and l_soa_lv, l_soa_sv (SOA precursors)
! following are species indices for individual camuio species
  integer,public :: &
  l_so4_na,   l_so4_a1, l_so4_a2, l_so4_ac,             &
  l_bc_n,     l_bc_ax,    l_bc_ni,  l_bc_a,  l_bc_ai,l_bc_ac,     &
  l_om_ni,    l_om_ai  ,l_om_ac,               &
  l_so4_pr,   &
  l_dst_a2,   l_dst_a3,              &
  l_ss_a1,    l_ss_a2,    l_ss_a3, l_h2so4, &
  l_soa_na, l_soa_a1, l_soa_lv, l_soa_sv 

! some code here has been moved to commondefinitions...

   integer                                 :: n_aerosol_tracers !number of aerosol tracers

   integer                                 :: imozart

   !Number of transported tracers in each mode 
   integer, parameter, dimension(0:nmodes) :: n_tracers_in_mode = (/ 1, 4, 3, 0, 5, 7, 7, 7, 7, 7, 7, 0, 1, 0, 2 /) !cka: added organic condensate to mode 1,2,4-10
   integer, parameter, dimension(0:nmodes) :: n_background_tracers_in_mode = (/ 1,2,1,0,2,1,1,1,1,1,1,0,1,0,2 /) !cka: added soa to mode 1 and 11

   integer, dimension(0:nmodes, max_tracers_per_mode) :: tracer_in_mode


   !Radius used for the modes in the lifeCycle MAY ASSUME SOME GROWTH ALREADY HAPPENED
   real(r8), parameter, public, dimension(0:nmodes) :: lifeCycleNumberMedianRadius = &
!BCsizes                                1.e-6_r8*(/ 0.1_r8, 0.02_r8, 0.0118_r8, 0.04_r8,   0.04_r8,   0.075_r8, &
                                1.e-6_r8*(/ 0.0626_r8, 0.025_r8, 0.025_r8, 0.04_r8,   0.06_r8,   0.075_r8, &
                                              0.22_r8,   0.63_r8,   0.0475_r8,  0.30_r8,   0.75_r8,  &    ! Salter et al. (2015)
!BCsizes                                             0.0118_r8, 0.0118_r8, 0.04_r8,   0.04_r8    /)
                                             0.0118_r8, 0.024_r8, 0.04_r8,   0.04_r8    /)

   !Sigma based on original lifecycle code (taken from "sigmak" used previously in lifecycle code)
   real(r8), parameter, public, dimension(0:nmodes) :: lifeCycleSigma =  (/1.6_r8, 1.8_r8, 1.8_r8, 1.8_r8, 1.8_r8 &   !0-4
                                                                 ,1.59_r8, 1.59_r8, 2.0_r8               &   !5,6,7 (SO4+dust)
                                                                 ,2.1_r8, 1.72_r8, 1.6_r8                &   !8-10  (SS)     ! Salter et al. (2015)
                                                                 ,1.8_r8, 1.8_r8, 1.8_r8, 1.8_r8         &   !11-14
                                                                  /)

   !Below cloud scavenging coefficients for modes which have an actual size
   real(r8), parameter, public, dimension(0:nmodes) :: belowCloudScavengingCoefficient=                    &
                                       (/ 0.01_r8  ,  0.02_r8 , 0.02_r8  ,  0.0_r8 ,   0.02_r8,   0.01_r8, & !(0-5)
                                          0.02_r8  ,  0.2_r8  , 0.02_r8  ,  0.02_r8,   0.5_r8,             & !6-10 (DUST+SS)
                                          0.04_r8  ,  0.08_r8 , 0.0_r8   ,  0.02_r8    /)                    ! SO4_n, bc_n, N/A og bc/oc

   !Treatment of process-modes!
   !The tracers indices can not be set here since they are not known on compile time
   !tracerInProcessMode = (/l_so4_a1, l_so4_a2, l_so4_ac, l_om_ac, l_bc_ac, l_soa_a1 /)

   !The process modes need an "efficient size" (Why does A1 have a different size than the others??)
   real(r8), parameter, public, dimension(numberOfProcessModeTracers) :: processModeNumberMedianRadius = &
                                       (/ 0.04e-6_r8, 0.1e-6_r8, 0.1e-6_r8, 0.1e-6_r8, 0.1e-6_r8, 0.04e-6_r8 /) 

   !The process modes need an "efficient sigma"   
   real(r8), parameter, public, dimension(numberOfProcessModeTracers) :: processModeSigma =   & 
                                       (/ 1.8_r8, 1.59_r8, 1.59_r8, 1.59_r8, 1.59_r8, 1.8_r8  /)
                                                                       

   real(r8), parameter, public, dimension(numberOfProcessModeTracers) :: belowCloudScavengingCoefficientProcessModes = &
                                       (/0.02_r8, 0.01_r8, 0.02_r8, 0.02_r8, 0.02_r8, 0.02_r8 /)

   !Growth of aerosols, duplicated in opttab!!  AK: NB oppdaterte tall i opttab, rh der er ikke helt lik rhtab...
   real(r8), public,dimension (10)      :: rhtab
   real(r8), public,dimension (10,pcnst):: rdivr0(10,pcnst)

   data rhtab/ 0.0_r8, 0.37_r8, 0.47_r8, 0.65_r8, 0.75_r8, 0.80_r8, 0.85_r8, 0.90_r8, 0.95_r8, 0.98_r8 /

   integer, dimension(pcnst) :: cloudTracerIndex
   character(len=20) :: cloudTracerName(pcnst)
contains


   !For a tracer in an aerosol mode, check if this is 
   !actually a real tracer or a process mode
   function is_process_mode(l_index_in, isChemistry) result(answer)
      implicit none
      integer, intent(in)  :: l_index_in
      logical, intent(in)  :: isChemistry  !true if called from chemistry
      integer              :: l_index_phys
      logical              :: answer 

      l_index_phys = l_index_in
      if(isChemistry .eqv. .true.)then
         l_index_phys = l_index_phys + iMozart - 1
      endif

      !answer becomes true if tracer is a "process mode"
      answer = .FALSE.
      if(l_index_phys .eq. l_so4_a1 &
         .OR. l_index_phys .eq. l_so4_a2 &
         .OR. l_index_phys .eq. l_so4_ac &
         .OR. l_index_phys .eq. l_bc_ac  &
         .OR. l_index_phys .eq. l_om_ac  &
         .OR. l_index_phys .eq. l_soa_a1 ) then
         answer = .TRUE.
      endif
      
      return 
   end function is_process_mode

!===============================================================================
  subroutine aero_register
!----------------------------------------------------------------------- 
! 
! Register aerosol modes and indices, should be changed to read in values 
! instead of hard-coding it. 
! 
!-----------------------------------------------------------------------

  use mpishorthand
  use physics_buffer,  only: pbuf_add_field, dtype_r8
  use ppgrid,          only: pcols, pver, pverp


    implicit none
    integer :: idx_dum, l,m,mm
    logical isAlreadyCounted(pcnst)

!   register the species

    call cnst_get_ind('SO4_NA',l_so4_na, abort=.true.)   !Aitken mode sulfate (growth from so4_n)
    call cnst_get_ind('SO4_A1',l_so4_a1, abort=.true.)   !sulfate condensate (gas phase production)
    call cnst_get_ind('SO4_A2',l_so4_a2, abort=.true.)   !sulfate produced in aq. chemistry
    call cnst_get_ind('SO4_AC',l_so4_ac, abort=.true.)   !sulfate from coagulation processes
    call cnst_get_ind('SO4_PR',l_so4_pr, abort=.true.)   !sulfate emitted as primary

    call cnst_get_ind('BC_N',l_bc_n, abort=.true.)       !emissions (mainly industry) lost through coagulation
    call cnst_get_ind('BC_AX',l_bc_ax, abort=.true.)     !externally mixed (fluffy and impossible to activate)
    call cnst_get_ind('BC_NI',l_bc_ni, abort=.true.)     !mixed with oc (mainly biomass), externally mixed otherwise (before condensation etc)  
    call cnst_get_ind('BC_A',l_bc_a, abort=.true.)       !formed when bc_n grows by condensation 
    call cnst_get_ind('BC_AI',l_bc_ai, abort=.true.)     !formed when bc_ni grows by condensation
    call cnst_get_ind('BC_AC',l_bc_ac, abort=.true.)     !bc from coagulation processes

    call cnst_get_ind('OM_NI',l_om_ni, abort=.true.)     !om (mainly from biomass), emitted
    call cnst_get_ind('OM_AI',l_om_ai, abort=.true.)     !om formed when condensation growth of om_ni
    call cnst_get_ind('OM_AC',l_om_ac, abort=.true.)     !om from coagulation processes

    call cnst_get_ind('DST_A2',l_dst_a2, abort=.true.)    !Dust accumulation mode
    call cnst_get_ind('DST_A3',l_dst_a3, abort=.true.)    !Dust coarse mode

    call cnst_get_ind('SS_A1',l_ss_a1, abort=.true.)      !Sea salt fine mode
    call cnst_get_ind('SS_A2',l_ss_a2, abort=.true.)      !Sea salt accumulation mode
    call cnst_get_ind('SS_A3',l_ss_a3, abort=.true.)      !Sea salt coarse mode

!cka: register SOA species
    call cnst_get_ind('SOA_NA',l_soa_na, abort=.true.) !Aitken mode SOA with SO4 and SOA condensate
    call cnst_get_ind('SOA_A1',l_soa_a1, abort=.true.) !SOA condensate 
    call cnst_get_ind('SOA_LV',l_soa_lv, abort=.true.) !Gas phase low volatile SOA
    call cnst_get_ind('SOA_SV',l_soa_sv, abort=.true.) !Gas phase semi volatile SOA

    !gas phase h2so4
    call cnst_get_ind('H2SO4', l_h2so4, abort=.true.)

    !Register the tracers in modes
    call registerTracersInMode()

    !Set the aerosol types
    aerosolType(:)=-99
    aerosolType(l_so4_na)=AEROSOL_TYPE_SULFATE
    aerosolType(l_so4_a1)=AEROSOL_TYPE_SULFATE
    aerosolType(l_so4_a2)=AEROSOL_TYPE_SULFATE
    aerosolType(l_so4_ac)=AEROSOL_TYPE_SULFATE
    aerosolType(l_so4_pr)=AEROSOL_TYPE_SULFATE
    aerosolType(l_bc_n)=AEROSOL_TYPE_BC
    aerosolType(l_bc_ax)=AEROSOL_TYPE_BC
    aerosolType(l_bc_ni)=AEROSOL_TYPE_BC
    aerosolType(l_bc_a) =AEROSOL_TYPE_BC
    aerosolType(l_bc_ai)=AEROSOL_TYPE_BC
    aerosolType(l_bc_ac)=AEROSOL_TYPE_BC
    aerosolType(l_om_ni)=AEROSOL_TYPE_OM
    aerosolType(l_om_ai)=AEROSOL_TYPE_OM
    aerosolType(l_om_ac)=AEROSOL_TYPE_OM
    aerosolType(l_dst_a2)=AEROSOL_TYPE_DUST
    aerosolType(l_dst_a3)=AEROSOL_TYPE_DUST
    aerosolType(l_ss_a1)=AEROSOL_TYPE_SALT
    aerosolType(l_ss_a2)=AEROSOL_TYPE_SALT
    aerosolType(l_ss_a3)=AEROSOL_TYPE_SALT
    aerosolType(l_soa_na)=AEROSOL_TYPE_OM 
    aerosolType(l_soa_a1)=AEROSOL_TYPE_OM 

    rhopart(:)= 1000.0_r8
    !assign values based on aerosol type
    do m=0,nmodes
       do l=1,n_tracers_in_mode(m)
          mm= getTracerIndex(m,l,.false.)
          osmoticCoefficient(mm) = aerosol_type_osmotic_coefficient(aerosolType(mm))
          rhopart(mm)            = aerosol_type_density(aerosolType(mm))
          solubleMassFraction(mm)    = aerosol_type_soluble_mass_fraction(aerosolType(mm))
          numberOfIons(mm)           = aerosol_type_number_of_ions(aerosolType(mm)) 
        end do
    end do
    !SPECIAL CASES OF AEROSOL PROPERTIES: 
    !Density of bc_ax is rewritten later (calculated from fractal dimension)
    !so4_a2 is different since it is ammonium sulfate and not sulf. acid.
    rhopart(l_so4_a2) = 1769.0_r8

    !These are not really particles, but set densities for the condenseable vapours
    !used by condtend
    rhopart(l_h2so4)= 1841.0_r8
    rhopart(l_soa_lv) = aerosol_type_density(AEROSOL_TYPE_OM)
    rhopart(l_soa_sv) = aerosol_type_density(AEROSOL_TYPE_OM)
! Inverse calculated to avoid unneeded divisions in loop
    invrhopart(:)=1._r8/rhopart(:)
    !Set process mode sizes
    tracerInProcessMode = (/l_so4_a1, l_so4_a2, l_so4_ac, l_om_ac, l_bc_ac, l_soa_a1 /)
    processModeMap(:)=-99 !Force error if using unset values
    do l =1,pcnst
       do m=1,numberOfProcessModeTracers
          if(tracerInProcessMode(m) .eq. l)then
             processModeMap(l)=m
          end if
       end do
    end do

    !Find out first mozart tracers (fxm: short lived species might mess up this!)
    call cnst_get_ind(trim(solsym(1)), imozart, abort=.true.)

   !Add the cloud-tracers
    isAlreadyCounted(:) = .false.
    cloudTracerIndex(:) = -1
    do m=1,nmodes
       do l=1,n_tracers_in_mode(m)
          mm= getTracerIndex(m,l,.false.)
          if(.not. isAlreadyCounted(mm))then
            cloudTracerName(mm) = trim(cnst_name(mm))//"_OCW"
            !print*, "CTN ", trim(cloudTracerName(mm))
            call pbuf_add_field(trim(cloudTracerName(mm)), 'global', dtype_r8, (/pcols,pver/), idx_dum)
            call qqcw_set_ptr(mm,idx_dum)
            cloudTracerIndex(mm) = idx_dum
            isAlreadyCounted(mm) = .true.
          endif
      end do
    end do


    !Find out how many aerosol-tracers we carry
    isAlreadyCounted(:) = .FALSE.
    n_aerosol_tracers=0
    do m=1,nmodes
       do l=1,n_tracers_in_mode(m)
          mm=getTracerIndex(m,l,.false.)
          if(.not. isAlreadyCounted(mm))then
             n_aerosol_tracers = n_aerosol_tracers + 1
             isAlreadyCounted(mm)=.true.
          endif
       end do
    end do

   !Tabulated rh-growth for all species
   call inittabrh


    return
  end subroutine aero_register

   function getNumberOfAerosolTracers()RESULT(numberOfTracers)
      implicit none
      integer :: numberOfTracers
      numberOfTracers = n_aerosol_tracers
   end function getNumberOfAerosolTracers

   function chemistryIndex(phys_index) RESULT (chemistryIndexOut)
      implicit none
      integer, intent(in) :: phys_index
      integer             :: chemistryIndexOut

      chemistryIndexOut = phys_index - imozart + 1
   end function chemistryIndex
 
  function physicsIndex(chem_index) RESULT(physIndexOut)
      implicit none
      integer, intent(in) :: chem_index
      integer             :: physIndexOut

      physIndexOut = chem_index + imozart - 1
  end function physicsIndex

  function isAerosol(phys_index) RESULT(answer)
      integer, intent(in) :: phys_index
      logical answer
      answer=.FALSE.
      if(aerosolType(phys_index) .gt. 0)then
         answer = .TRUE.
      endif
      return
  end function isAerosol
!=============================================================================

   function getNumberOfTracersInMode(modeIndex) RESULT(numberOfSpecies)
      implicit none
      integer, intent(in)        :: modeIndex
      integer numberOfSpecies
      numberOfSpecies = n_tracers_in_mode(modeIndex)
   end function getNumberOfTracersInMode

   function getNumberOfBackgroundTracersInMode(modeIndex) RESULT (numberOfBackgroundSpecies)
      implicit none
      integer, intent(in)       :: modeIndex
      integer numberOfBackgroundSpecies
      numberOfBackgroundSpecies = n_background_tracers_in_mode(modeIndex)
   end function getNumberOfBackgroundTracersInMode

   !purpose: Ask for an index in mode
   !The index is the index in the q-array
   !Some tracers may exist in several modes (is that a problem??)
   function getTracerIndex(modeIndex, componentIndex, isChemistry) RESULT(tracerIndex)
      implicit none
      integer, intent(in)   :: modeIndex
      integer, intent(in)   :: componentIndex
      logical, intent(in)   :: isChemistry
      integer tracerIndex

      if(isChemistry)then
         !This is tracer index in physics array
         tracerIndex = tracer_in_mode(modeIndex,componentIndex)-imozart+1
      else
         tracerIndex = tracer_in_mode(modeIndex,componentIndex)
      endif

   end function getTracerIndex

   !Obtain an index in the physics-buffer for a component in the lifecycle scheme
   function getCloudTracerIndex(modeIndex, componentIndex) RESULT(cloud_tracer_index)
      implicit none
      integer, intent(in)   :: modeIndex
      integer, intent(in)   :: componentIndex
      integer               :: tracerIndex
      integer cloud_tracer_index

      if(componentIndex == 0)then
         !Special key for number concentration of a mode
         print*,"error no such species"
         stop
      else if (componentIndex > 0)then
         !Lifecycle specie in a mode
         tracerIndex = getTracerIndex(modeIndex,componentIndex,.false.)
         cloud_tracer_index = cloudTracerIndex(tracerIndex)       !ak: Index in phys-buffer
      else
         !error, negative component index
         call endrun("negative componentindex in getCloudTracerIndex")
      endif
   end function getCloudTracerIndex

   !returns index in pbuf for the corresponding cloud tracer with physics index "tracerIndex"
   !returns "-1" if the tracer does not have any corresponding cloud tracer
   function getCloudTracerIndexDirect(tracerIndex) RESULT(cloudTracerIndexOut)
      implicit none
      integer, intent(in) :: tracerIndex
      integer             :: cloudTracerIndexOut

      cloudTracerIndexOut = cloudTracerIndex(tracerIndex)

   end function getCloudTracerIndexDirect

   function getDryDensity(m,l) RESULT(density)
      implicit none
      integer, intent(in) :: m !mode index
      integer, intent(in) :: l !tracer index
      real(r8) :: density
      density =  rhopart(tracer_in_mode(m,l))
   end function


   function getCloudTracerName(tracerIndex) RESULT(cloudTracerNameOut)
      implicit none
      integer, intent(in) :: tracerIndex
      character(len=20)   :: cloudTracerNameOut
      cloudTracerNameOut = trim(cloudTracerName(tracerIndex))
      return
   end function getCloudTracerName

   subroutine fillAerosolTracerList(aerosolTracerList)
      implicit none
      integer, dimension (:), intent(out) :: aerosolTracerList
      logical, dimension(pcnst)          :: alreadyFound

      integer :: m,l,mm,nTracer

      alreadyFound(:) = .FALSE.
      
      nTracer = 0
      do m=1,nmodes
         do l=1,n_tracers_in_mode(m)
            mm=getTracerIndex(m,l,.FALSE.)
            if(.NOT.alreadyFound(mm))then
               nTracer = nTracer + 1
               alreadyFound(mm) = .TRUE.
               aerosolTracerList(nTracer) = mm
            end if
         end do
      end do
   end subroutine fillAerosolTracerList

   subroutine fillInverseAerosolTracerList(aerosolTracerList, inverseAerosolTracerList, n_aerosol_tracers)
      implicit none
      integer, dimension(:), intent(in)  :: aerosolTracerList
      integer, intent(in)                :: n_aerosol_tracers
      integer, dimension(pcnst), intent(out) :: inverseAerosolTracerList
      integer                                :: i

      inverseAerosolTracerList(:) = -99
      do i=1,n_aerosol_tracers
         inverseAerosolTracerList(aerosolTracerList(i)) = i
      end do    

   end subroutine

   !Register tracer index in modes
   subroutine registerTracersInMode()
      
      implicit none

      tracer_in_mode(:,:) = -1 !undefined
      !externally mixed bc
      tracer_in_mode(MODE_IDX_BC_EXT_AC, 1:n_tracers_in_mode(MODE_IDX_BC_EXT_AC)) = (/l_bc_ax/)
!cka      !sulphate + sulfate condensate
!cka      tracer_in_mode(MODE_IDX_SO4_AIT, 1:n_tracers_in_mode(MODE_IDX_SO4_AIT) ) = (/l_so4_na, l_so4_a1/)
      !sulphate + soa, sulfate condensate. 
      tracer_in_mode(MODE_IDX_SO4SOA_AIT, 1:n_tracers_in_mode(MODE_IDX_SO4SOA_AIT) ) = (/l_so4_na, l_soa_na, l_so4_a1, l_soa_a1/) 
      !bc + sulfate condensate	
      tracer_in_mode(MODE_IDX_BC_AIT,1:n_tracers_in_mode(MODE_IDX_BC_AIT))  = (/l_bc_a, l_so4_a1, l_soa_a1/)    
      !index not used
      !tracer_in_mode(MODE_IDX_NOT_USED, 1:n_tracers_in_mode(MODE_IDX_NOT_USED)) = (/-1/)
      !om / bc internally mixed with sulfate condensate and aquous phase sulfate 
      tracer_in_mode(MODE_IDX_OMBC_INTMIX_COAT_AIT, 1:n_tracers_in_mode(MODE_IDX_OMBC_INTMIX_COAT_AIT))= (/l_bc_ai, l_om_ai, l_so4_a1, l_so4_a2, l_soa_a1 /)
      !accumulation mode sulfate with coagulate, condensate and aquous phase sulfate
      tracer_in_mode(MODE_IDX_SO4_AC, 1:n_tracers_in_mode(MODE_IDX_SO4_AC)) = (/l_so4_pr, l_bc_ac, l_om_ac, l_so4_a1, l_so4_ac, l_so4_a2, l_soa_a1  /)
      !ac-mode dust with sulfate coagulate, condensate sulfate and wet-phase sulfate
      tracer_in_mode(MODE_IDX_DST_A2, 1:n_tracers_in_mode(MODE_IDX_DST_A2)) = (/l_dst_a2, l_bc_ac, l_om_ac, l_so4_a1, l_so4_ac, l_so4_a2, l_soa_a1 /)
      !coarse mode dust  with sulfate coagulate, condensate sulfate and wet-phase sulfate
      tracer_in_mode(MODE_IDX_DST_A3, 1:n_tracers_in_mode(MODE_IDX_DST_A3)) = (/l_dst_a3, l_bc_ac, l_om_ac, l_so4_a1, l_so4_ac, l_so4_a2, l_soa_a1 /)
      !at-mode ss with sulfate coagulate, condensate sulfate and wet-phase sulfate
      tracer_in_mode(MODE_IDX_SS_A1, 1:n_tracers_in_mode(MODE_IDX_SS_A1))   = (/l_ss_a1, l_bc_ac, l_om_ac, l_so4_a1, l_so4_ac, l_so4_a2, l_soa_a1 /)
      !ac mode ss with sulfate coagulate, condensate sulfate and wet-phase sulfate
      tracer_in_mode(MODE_IDX_SS_A2, 1:n_tracers_in_mode(MODE_IDX_SS_A2))   = (/l_ss_a2, l_bc_ac, l_om_ac, l_so4_a1, l_so4_ac, l_so4_a2, l_soa_a1 /)
      !coarse mode ss sulfate coagulate, condensate sulfate and wet-phase sulfate
      tracer_in_mode(MODE_IDX_SS_A3, 1:n_tracers_in_mode(MODE_IDX_SS_A3))   = (/l_ss_a3, l_bc_ac, l_om_ac, l_so4_a1, l_so4_ac, l_so4_a2, l_soa_a1 /)
      !sulfate + soa nucleation mode (mode no longer used)
      !tracer_in_mode(MODE_IDX_SO4SOA_NUC, 1:n_tracers_in_mode(MODE_IDX_SO4SOA_NUC)) = (/ -1 /)
      !bc in nucleation mode
      tracer_in_mode(MODE_IDX_BC_NUC, 1:n_tracers_in_mode(MODE_IDX_BC_NUC)) = (/l_bc_n/)
      !lumped organics
      !tracer_in_mode(MODE_IDX_LUMPED_ORGANICS, 1:n_tracers_in_mode(MODE_IDX_LUMPED_ORGANICS)) = (/-1/)
      !intermal mixture bc/oc coated
      tracer_in_mode(MODE_IDX_OMBC_INTMIX_AIT, 1:n_tracers_in_mode(MODE_IDX_OMBC_INTMIX_AIT)) = (/l_bc_ni, l_om_ni/)

   end subroutine registerTracersInMode
   !

   function isTracerInMode(modeIndex, constituentIndex)RESULT(answer)
      implicit none
      integer, intent(in) :: modeIndex
      integer, intent(in) :: constituentIndex
      integer             :: i
      logical             :: answer 
      answer = .FALSE.
      do i=1,n_tracers_in_mode(modeIndex)
         if(tracer_in_mode(modeIndex,i) == constituentIndex)then
            answer = .TRUE.
         endif
      enddo
      return
   end function isTracerInMode
   !

   function getConstituentFraction(CProcessModes, f_c, f_bc, f_aq, f_so4_cond,f_soa &
                                   ,Cam, f_acm, f_bcm, f_aqm, f_so4_condm,f_soam, constituentIndex,debugPrint ) RESULT(fraction)   ! mass fraction
      implicit none
      real(r8), intent(in) :: CProcessModes
      real(r8), intent(in) :: f_c
      real(r8), intent(in) :: f_bc
      real(r8), intent(in) :: f_aq
      real(r8), intent(in) :: f_so4_cond
      real(r8), intent(in) :: f_soa
      real(r8), intent(in) :: cam
      real(r8), intent(in) :: f_aqm
      real(r8), intent(in) :: f_bcm
      real(r8), intent(in) :: f_acm
      real(r8), intent(in) :: f_so4_condm
      real(r8), intent(in) :: f_soam
      integer, intent(in)  :: constituentIndex
      logical, optional, intent(in) :: debugPrint
      logical                       :: doPrint = .false.
      real(r8) :: fraction

      if(present(debugPrint))then
         if(debugPrint .eqv. .true.)then
            doPrint=.true.
         endif
      endif


      fraction = 1.0_r8              ! fraction = 1 for all tracers, except special cases (process modes) below 

      !This fraction is the mass of a certain tracer in a specific size-mode divided by the total
      !mass of the same tracer for (i.e. summed up over) all size-modes. This total mass is what 
      !is transported in the model, in the life cycle scheme. The word size-mode is here used for a mode in the  
      !aerosol size-distribution, which is assumed to be log-normal prior to growth. 
      if((l_so4_a1 .eq. constituentIndex))then !so4 condensation
         fraction= (cam                 &
                  *(1.0_r8-f_acm)       & !sulfate fraction 
                  *(1.0_r8-f_aqm)       & !fraction not from aq phase
                  *(f_so4_condm)        & !fraction being condensate
                  )                                     &
                  /                                     & 
                  (CProcessModes*(1.0_r8-f_c)*(1.0_r8-f_aq)*f_so4_cond+smallConcentration) !total so4 condensate 

         if(doPrint .eqv. .true.)then
            print*, " "
            print*, "conc     ==>", CProcessmodes, cam
            print*, "modefrc  ==>", f_acm, f_aqm, f_so4_condm
            print*, "totfrc   ==>", f_c, f_aq, f_so4_cond
            print*, "fraction ==>", cam/(CProcessModes+smallConcentration)*100.0, fraction*100 , "%"
         endif

      else if(l_so4_ac .eq. constituentIndex)then         !so4 coagulation
         fraction = (cam                         &
                  * (1.0_r8 - f_acm)  & !sulfate fraction
                  * (1.0_r8 - f_aqm)  & !fraction not from aq phase
                  * (1.0_r8 - f_so4_condm) & !fraction not being condensate
                  )                                        &
                  /                                   &
                  (CProcessModes*(1.0_r8-f_c)*(1.0_r8-f_aq)*(1.0_r8-f_so4_cond) & !total non-aq sulf
                     +smallConcentration)

      else if(l_so4_a2 .eq. constituentIndex) then  !so4 wet phase
         fraction = (cam            &
                  *(1.0_r8-f_acm)     & !sulfate fraction
                  *f_aqm)             & !aq phase fraction of sulfate
                  /                                   &
                  (CProcessModes*(1.0_r8-f_c)*(f_aq)+smallConcentration)

      else if(l_bc_ac .eq. constituentIndex)then  !bc coagulated
         fraction = (cam              &
                  *f_acm           & ! carbonaceous fraction 
                  *f_bcm)          & ! bc fraction of carbonaceous
                  /                                &
                  (CProcessModes*f_c*f_bc+smallConcentration) 

      else if(l_om_ac .eq. constituentIndex ) then  !oc coagulated
         fraction =  (cam          &
                  *f_acm           &       ! carbonaceous fraction
                  *(1.0_r8-f_bcm)  &       ! oc fraction of carbonaceous
                  *(1.0_r8-f_soam))&       ! oc fraction which is soa
                  /                                &
                  (CProcessModes*f_c*(1.0_r8-f_bc)*(1.0_r8-f_soa)+smallConcentration)

      else if (l_soa_a1 .eq. constituentIndex) then !SOA condensate
         fraction = cam              &
                  *f_acm             &  !carbonaceous fraction
                  *(1.0_r8 -f_bcm)   &  !om fraction
                  *(f_soam)          &  !fraction of OM is SOA
                  /                  &
                  (CProcessModes * f_c* (1.0_r8 -f_bc)*f_soa + smallConcentration)
      end if

      !if(fraction .gt. 1.2_r8)then
      !   if(cam .gt. 1.e-8 *CprocessModes)then
      !      print*, "warning, fraction > 1.2 in getConstituentFraction", constituentIndex, fraction
      !      print*, "  ==> ", CprocessModes, cam ,f_c, f_bc, f_aq,f_so4_cond
      !      print*, "  ==> ", f_acm, f_bcm, f_aq, f_so4_condm
      !      print*, "  ==> ", cam/CprocessModes
      !      !stop
      !   endif
      !   fraction = 1.0_r8
      if (fraction .gt. 1.0_r8)then
         fraction = 1.0_r8
      endif

      return
   end function getConstituentFraction

!**********************************************


   subroutine inittabrh

  ! Tables for hygroscopic growth
   
    integer :: i


    real(r8) :: rr0ss(10),rr0so4(10),rr0bcoc(10)

    data rr0ss / 1.00_r8, 1.00_r8, 1.02_r8, 1.57_r8, 1.88_r8, 1.97_r8, 2.12_r8, 2.35_r8, 2.88_r8, 3.62_r8 /
    data rr0so4 / 1.00_r8, 1.34_r8, 1.39_r8, 1.52_r8, 1.62_r8, 1.69_r8, 1.78_r8, 1.92_r8, 2.22_r8, 2.79_r8 /    
    data rr0bcoc / 1.00_r8, 1.02_r8, 1.03_r8, 1.12_r8, 1.17_r8, 1.20_r8, 1.25_r8, 1.31_r8, 1.46_r8, 1.71_r8 /

    rdivr0(:,:)=1._r8

    do i=1,10
       rdivr0(i,l_so4_na)=rr0so4(i)
       rdivr0(i,l_so4_a1)=rr0so4(i)
       rdivr0(i,l_so4_a2)=rr0so4(i)
       rdivr0(i,l_so4_ac)=rr0so4(i)
       rdivr0(i,l_so4_pr)=rr0so4(i)

       rdivr0(i,l_bc_a)=rr0bcoc(i) 

!      rdivr0(i,l_bc_n)=rr0bcoc(i)
       rdivr0(i,l_bc_ni)=rr0bcoc(i)
       rdivr0(i,l_bc_ai)=rr0bcoc(i)
       rdivr0(i,l_bc_ac)=rr0bcoc(i)

!       rdivr0(i,l_om_n)=rr0bcoc(i)
       rdivr0(i,l_om_ni)=rr0bcoc(i)
       rdivr0(i,l_om_ai)=rr0bcoc(i)
       rdivr0(i,l_om_ac)=rr0bcoc(i)

       rdivr0(i,l_ss_a1)=rr0ss(i)
       rdivr0(i,l_ss_a2)=rr0ss(i)
       rdivr0(i,l_ss_a3)=rr0ss(i)

!cka: Add hygroscopic properties for soa. Assume identical to bcoc properties.
       rdivr0(i,l_soa_na)=rr0bcoc(i)
!       rdivr0(i,l_soa_a1)=rr0bcoc(i)
	
    end do
    return
  end subroutine inittabrh

end module aerosoldef


