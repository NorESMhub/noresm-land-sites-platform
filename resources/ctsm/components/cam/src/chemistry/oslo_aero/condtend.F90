module condtend

   use phys_control, only: phys_getopts
   use chem_mods,    only: gas_pcnst
   use mo_tracname,  only: solsym
   use shr_kind_mod, only: r8 => shr_kind_r8
   use ppgrid
   use const
   use cam_history,  only: outfld
   use aerosoldef
   use physconst,    only: rair, gravit, pi
   use commondefinitions
   use chem_mods, only: adv_mass !molecular weights from mozart
!soa

   save

   integer, parameter :: N_COND_VAP = 3
   integer, parameter :: COND_VAP_H2SO4 = 1
   integer, parameter :: COND_VAP_ORG_LV = 2
   integer, parameter :: COND_VAP_ORG_SV = 3

   real(r8), public, dimension(0:nmodes,N_COND_VAP)  :: normalizedCondensationSink       ![m3/#/s] condensation sink per particle in mode i

   integer, private, dimension(gas_pcnst) :: lifeCycleReceiver                ! [-] array of transformation of life cycle tracers
   real(r8), private, dimension(0:nmodes,N_COND_VAP) :: stickingCoefficient              ! [-] stickingCoefficient for H2SO4 on a mode
   integer, private, dimension(N_COND_VAP) :: cond_vap_map

! Assumed number of monolayers
  real(r8), parameter, private :: n_so4_monolayers_age = 3.0_r8

  real(r8), parameter, public :: &
              dr_so4_monolayers_age = n_so4_monolayers_age * 4.76e-10_r8
! thickness of the so4 monolayers (m)
! for so4(+nh4), use bi-sulfate mw and 1.77 g/cm3 as in MAM


contains

   subroutine registerCondensation()

      implicit none
   

      integer                        :: iDonor
      integer                        :: l_donor
      integer                        :: tracerIndex
      integer                        :: mode_index_donor

      !These are the lifecycle-species which receive mass when  
      !the externally mixed modes receive condensate,
      !e.g. the receiver of l_so4_n mass is the tracer l_so4_na 
      lifeCycleReceiver(:) = -99
      lifeCycleReceiver(chemistryIndex(l_bc_n))   = chemistryIndex(l_bc_a)    !create bc int mix from bc in mode 12
      lifeCycleReceiver(chemistryIndex(l_bc_ni))  = chemistryIndex(l_bc_ai)   !create bc int mix from bc in mode 14 
      lifeCycleReceiver(chemistryIndex(l_om_ni))  = chemistryIndex(l_om_ai)
      !!create om int mix from om in mode 14
      lifeCycleReceiver(chemistryIndex(l_bc_ax))  = chemistryIndex(l_bc_ai)
      !!create bc int mix from bc in mode 0. Note Mass is conserved but not number 

      !Sticking coeffcients for H2SO4 condensation
      !See table 1 in Kirkevag et al (2013) 
      !http://www.geosci-model-dev.net/6/207/2013/gmd-6-207-2013.html
      !Note: In NorESM1, sticking coefficients of the externally mixed modes were
      !used for the internally mixed modes in modallapp. In condtend the internally
      !mixed modes had sticking coefficient = 1.0
      !This might be correct, but is too confusing, so here just
      !assign based on background aerosol and table 1 in Kirkevag et al
      stickingCoefficient(:,:) = 1.0_r8
      stickingCoefficient(MODE_IDX_BC_EXT_AC,:) = 0.3_r8
      stickingCoefficient(MODE_IDX_BC_AIT,:) = 0.3_r8
      stickingCoefficient(MODE_IDX_OMBC_INTMIX_COAT_AIT,:) = 0.5_r8
      stickingCoefficient(MODE_IDX_DST_A2,:) = 0.3_r8
      stickingCoefficient(MODE_IDX_DST_A3,:) = 0.3_r8
      stickingCoefficient(MODE_IDX_BC_NUC,:) = 0.3_r8
      stickingCoefficient(MODE_IDX_OMBC_INTMIX_AIT,:) = 0.5_r8


   end subroutine registerCondensation

!===============================================================================

   subroutine initializeCondensation()

      !condensation coefficients: 
      !Theory: Poling et al, "The properties of gases and liquids"
      !5th edition, eqn 11-4-4

      use cam_history,     only: addfld, add_default, fieldname_len, horiz_only 
      implicit none

      real(r8), parameter :: aunit = 1.6606e-27_r8  ![kg] Atomic mass unit
      real(r8), parameter :: boltz = 1.3806e-23_r8   ![J/K/molec] 
      real(r8), parameter :: t0 = 273.15_r8         ![K] standard temperature
      real(r8), parameter :: p0 = 101325.0_r8       ! [Pa] Standard pressure
      real(r8), parameter :: radair = 1.73e-10_r8   ![m] Typical air molecule collision radius
      real(r8), parameter :: Mair = 28.97_r8        ![amu/molec] Molecular weight for dry air
      !Diffusion volumes for simple molecules [Poling et al], table 11-1
      real(r8), dimension(N_COND_VAP), parameter :: vad = (/51.96_r8, 208.18_r8, 208.18_r8/) ![cm3/mol]
      real(r8), parameter :: vadAir       = 19.7_r8                                          ![cm3/mol]
      real(r8), parameter :: aThird = 1.0_r8/3.0_r8
      real(r8), parameter :: cm2Tom2 = 1.e-4_r8       !convert from cm2 ==> m2

      real(r8), dimension(0:100,0:nmodes,N_COND_VAP) :: DiffusionCoefficient   ! [m2/s] Diffusion coefficient
      character(len=fieldname_len+3) :: fieldname_donor
      character(len=fieldname_len+3) :: fieldname_receiver
      character(128)                 :: long_name
      character(8)                   :: unit

      integer                        :: nsiz !counter for aerotab sizes
      integer                        :: iChem             !counter for chemical species
      integer                        :: mode_index_donor  !index for mode
      integer                        :: iMode             !Counter for mode
      integer                        :: tracerIndex       !counter for chem. spec
     
      logical                        :: history_aerosol
      logical                        :: isAlreadyOnList(gas_pcnst)
      integer                        :: cond_vap_idx

      real(r8), dimension(N_COND_VAP) :: mfv  ![m] mean free path
      real(r8), dimension(N_COND_VAP) :: diff ![m2/s] diffusion coefficient for cond. vap
      real(r8) :: molecularWeight !amu/molec molecular weight
      real(r8) :: Mdual ![molec/amu] 1/M_1 + 1/M_2
      real(r8) :: rho   ![kg/m3] density of component in question
      real(r8) :: radmol ![m] radius molecule
      real(r8), dimension(N_COND_VAP) :: th     !thermal velocity

      !Couple the condenseable vapours to chemical species for properties and indexes
      cond_vap_map(COND_VAP_H2SO4) = chemistryIndex(l_h2so4)
      cond_vap_map(COND_VAP_ORG_LV) = chemistryIndex(l_soa_lv)
      cond_vap_map(COND_VAP_ORG_SV) = chemistryIndex(l_soa_sv)

      do cond_vap_idx = 1, N_COND_VAP

         rho = rhopart(physicsIndex(cond_vap_map(cond_vap_idx))) !pick up densities from aerosoldef 

         molecularWeight=adv_mass(cond_vap_map(cond_vap_idx))    !pick up molecular weights from mozart

         !https://en.wikipedia.org/wiki/Thermal_velocity
         th(cond_vap_idx) = sqrt(8.0_r8*boltz*t0/(pi*molecularweight*aunit))   ! thermal velocity for H2SO4 in air (m/s) 

         !Radius of molecul (straight forward assuming spherical)
         radmol=(3.0_r8*molecularWeight*aunit/(4.0_r8*pi*rho))**aThird    ! molecule radius 

         Mdual=2.0_r8/(1.0_r8/Mair+1.0_r8/molecularWeight) !factor of [1/m_1 + 1_m2]

         !calculating microphysical parameters from equations in Ch. 8 of Seinfeld & Pandis (1998): 
         mfv(cond_vap_idx)=1.0_r8/(pi*sqrt(1.0_r8+MolecularWeight/Mair)*(radair+radmol)**2*p0/(boltz*t0)) ! mean free path for molec in air (m)  
         
         !Solve eqn 11-4.4 in Poling et al
         !(A bit hard to follow units here, but result in the book is in cm2/s)..
         !so scale by "cm2Tom2" to get m2/sec
         diff(cond_vap_idx) = cm2Tom2   &
            *0.00143_r8*t0**1.75_r8     &
          /((p0/1.0e5_r8)*sqrt(Mdual)   &    
          *(((Vad(cond_vap_idx))**aThird+(Vadair)**aThird)**2))  

         !Values used in noresm1:
         !real(r8), parameter :: diff = 9.5e-6    !m2/s  diffusion coefficient (H2SO4)
         !real(r8), parameter :: th   = 243.0_r8  !m/s   thermal velocity (H2SO4)
         !real(r8), parameter :: mfv  = 1.65e-8   !m     mean free path (H2SO4)

         !Check values obtained here (H2SO4 / SOA) 
         !write(*,*) 'mfv =   ', mfv(cond_vap_idx)     !2.800830854409093E-008 / 1.633546464678737E-008
         !write(*,*) ' diff = ', diff(cond_vap_idx)    !->  9.360361706957621E-006 / !->  4.185923463242946E-006
         !write(*,*) ' th = ', th                      !->  242.818542922924 / 185.421069430852
      end do

      do cond_vap_idx = 1, N_COND_VAP
         do imode = 0, nmodes         !all modes receive condensation 
            do nsiz = 1, nBinsTab     !aerotab sizes
            !Correct for non-continuum effects, formula is from 
            !Chuang and Penner, Tellus, 1995, sticking coeffient from 
            !Vignati et al, JGR, 2004
            !fxm: make "diff ==> diff (cond_vap_idx)
            DiffusionCoefficient(nsiz,imode,cond_vap_idx) = diff(cond_vap_idx)  &    !original diffusion coefficient 
               /(                                    &       
                  rBinMidPoint(nsiz)/(rBinMidPoint(nsiz)+mfv(cond_vap_idx))  &  !non-continuum correction factor
                  +4.0_r8*diff(cond_vap_idx)/(stickingCoefficient(imode,cond_vap_idx)*th(cond_vap_idx)*rBinMidPoint(nsiz)) & 
                 )
            enddo
         end do !receiver modes
      end do

      normalizedCondensationSink(:,:) = 0.0_r8
      !Find sink per particle in mode "imode"
      !Eqn 13 in Kulmala et al, Tellus 53B, 2001, pp 479
      !http://onlinelibrary.wiley.com/doi/10.1034/j.1600-0889.2001.530411.x/abstract
      do cond_vap_idx =1, N_COND_VAP
         do imode = 0, nmodes
            do nsiz = 1, nBinsTab
               normalizedCondensationSink(imode,cond_vap_idx) =  &
                                                normalizedCondensationSink(imode,cond_vap_idx)  &
                                                + 4.0_r8*pi                                    &
                                                * DiffusionCoefficient(nsiz,imode,cond_vap_idx) &    ![m2/s] diffusion coefficient
                                                * rBinMidPoint(nsiz)                &    ![m] look up table radius
                                                * normnk(imode,nsiz)                     ![frc]
            end do
         end do
      end do

      !Initialize output
      call phys_getopts(history_aerosol_out = history_aerosol)

      isAlreadyOnList(:) = .FALSE.
      do iChem = 1,gas_pcnst
         !Does this tracer have a receiver? If yes: It participate in condensation tendencies
         if(lifeCycleReceiver(iChem) .gt. 0)then
            unit = "kg/m2/s"
            fieldname_donor = trim(solsym(iChem))//"condTend"
            fieldname_receiver = trim(solsym(lifeCycleReceiver(iChem)))//"condTend"
            if(.not. isAlreadyOnList(lifeCycleReceiver(iChem)))then
               call addfld( fieldname_receiver, horiz_only, "A", unit, "condensation tendency" )
               isAlreadyOnList(lifeCycleReceiver(iChem))=.TRUE.
            end if
            call addfld( fieldname_donor, horiz_only, "A", unit, "condensation tendency" )
            if(history_aerosol)then
               call add_default( fieldname_receiver, 1, ' ' )
               call add_default( fieldname_donor   , 1, ' ')
            end if
         end if
      end do
      !Need to add so4_a1, soa_na, so4_na, soa_a1 also (which are not parts of the donor-receiver stuff) 
      fieldname_receiver = trim(solsym(chemistryIndex(l_so4_a1)))//"condTend"
      call addfld( fieldname_receiver, horiz_only, 'A', unit, "condensation tendency")
      if(history_aerosol)then
         call add_default( fieldname_receiver, 1, ' ' )
      end if
      fieldname_receiver = trim(solsym(chemistryIndex(l_soa_a1)))//"condTend"
      call addfld( fieldname_receiver, horiz_only, "A", unit, "condensation tendency" )
      if(history_aerosol)then
         call add_default( fieldname_receiver, 1, ' ' )
      end if
      fieldname_receiver = trim(solsym(chemistryIndex(l_so4_na)))//"condTend"
      call addfld( fieldname_receiver, horiz_only, 'A', unit , "condensation tendency" )
      if(history_aerosol)then
         call add_default( fieldname_receiver, 1, ' ' )
      end if
      fieldname_receiver = trim(solsym(chemistryIndex(l_soa_na)))//"condTend"
      call addfld( fieldname_receiver, horiz_only, 'A', unit, "condensation tendency" )
      if(history_aerosol)then
         call add_default( fieldname_receiver, 1, ' ' )
      end if



   end subroutine initializeCondensation



   subroutine condtend_sub(lchnk,  q, cond_vap_gasprod, temperature, &
               pmid, pdel, dt, ncol, pblh,zm,qh20) 

! Calculate the sulphate nucleation rate, and condensation rate of 
! aerosols used for parameterising the transfer of externally mixed 
! aitken mode particles into an internal mixture.
! Note the parameterisation for conversion of externally mixed particles 
! used the h2so4 lifetime onto the particles, and not a given 
! increase in particle radius. Will be improved in future versions of the model
! Added input for h2so4 and soa nucleation: soa_lv_gasprod, soa_sv_gasprod, pblh,zm,qh20 (cka)

   use cam_history,     only: outfld,fieldname_len
!nuctst3+   use koagsub,         only: normalizedCoagulationSink,receiverMode,numberOfCoagulationReceivers ! h2so4 and soa nucleation(cka)
!   use koagsub,         only: normCoagSinkMode1,normalizedCoagulationSink,receiverMode,numberOfCoagulationReceivers ! h2so4 and soa nucleation(cka)
!nuctst3-
!ak+ 
    use koagsub,         only: normalizedCoagulationSink,receiverMode,numberOfCoagulationReceivers, &
                               numberOfAddCoagReceivers,addReceiverMode,normCoagSinkAdd
!ak-
   use constituents,    only: pcnst  ! h2so4 and soa nucleation (cka)

   implicit none

   ! arguments
   integer,  intent(in) :: lchnk                      ! chunk identifier
   integer,  intent(in) :: ncol                       ! number of columns
   real(r8), intent(in) :: temperature(pcols,pver)    ! Temperature (K)
   real(r8), intent(in) :: pmid(pcols,pver)           ! [Pa] pressure at mid point
   real(r8), intent(in) :: pdel(pcols,pver)           ! [Pa] difference in grid cell
   real(r8), intent(inout) :: q(pcols,pver,gas_pcnst) ! TMR [kg/kg] including moisture
   real(r8), intent(in) :: cond_vap_gasprod(pcols,pver,N_COND_VAP) ! TMR [kg/kg/sec]] production rate of H2SO4 (gas prod - aq phase uptake)
   real(r8), intent(in) :: dt                         ! Time step
   ! Needed for soa nucleation treatment
   real(r8), intent(in)    :: pblh(pcols)               ! pbl height (m)
   real(r8), intent(in)    :: zm(pcols,pverp)           ! midlayer geopotential height above the surface (m) (pver+1)
   real(r8), intent(in)    :: qh20(pcols,pver)          ! specific humidity (kg/kg)

   ! local
   character(len=fieldname_len+3) :: fieldname
   integer :: i,k,nsiz
   integer :: mode_index_donor            ![idx] index of mode donating mass
   integer :: mode_index_receiver         ![idx] index of mode receiving mass
   integer :: tracerIndex
   integer :: l_donor
   integer :: l_receiver
   integer :: iDonor                                 ![idx] counter for externally mixed modes
   real(r8) :: condensationSink(0:nmodes, N_COND_VAP)![1/s] loss rate per mode (mixture)
   real(r8) :: condensationSinkFraction(pcols,pver,numberOfExternallyMixedModes,N_COND_VAP) ![frc]
   real(r8) :: sumCondensationSink(pcols,pver, N_COND_VAP)       ![1/s] sum of condensation sink
   real(r8) :: totalLoss(pcols,pver,gas_pcnst) ![kg/kg] tracer lost
   real(r8) :: numberConcentration(0:nmodes) ![#/m3] number concentration
   real(r8) :: numberConcentrationExtMix(pcols,pver,numberOfExternallyMixedModes)
   real(r8), dimension(pcols, gas_pcnst)            :: coltend
   real(r8), dimension(pcols)                       :: tracer_coltend

   real(r8) :: intermediateConcentration(pcols,pver,N_COND_VAP)
   real(r8) :: rhoAir(pcols,pver)                           ![kg/m3] density of air
! Volume of added  material from condensate;  surface area of core particle; 
   real(r8) :: volume_shell, area_core,vol_monolayer 
   real (r8) :: frac_transfer                   ! Fraction of hydrophobic material converted to an internally mixed mode 
   logical  :: history_aerosol
   character(128)                 :: long_name                              ![-] needed for diagnostics

!cka:+   
   ! needed for h2so4 and soa nucleation treatment
   integer  :: modeIndexReceiverCoag             !Index of modes receiving coagulate
   integer  :: iCoagReceiver                     !counter for species receiving coagulate
   real(r8) :: coagulationSink(pcols,pver)       ![1/s] coaglation loss for SO4_n and soa_n   
!nuctst3+
!   real(r8) :: normCSmode1(pcols,pver)           !normalized coagulation from self coagulation (simplified)
!nuctst3-
   real(r8), parameter :: lvocfrac=0.5           !Fraction of organic oxidation products with low enough 
                                                  !volatility to enter nucleation mode particles (1-24 nm)
   real(r8) :: soa_lv_forNucleation(pcols,pver)  ![kg/kg] soa gas available for nucleation
   real(r8) :: gasLost(pcols,pver,N_COND_VAP)          ![kg/kg] budget terms on H2SO4 (gas) 
   real(r8) :: fracNucl(pcols,pver,N_COND_VAP)               ! [frc] fraction of gas nucleated
   real(r8) :: firstOrderLossRateNucl(pcols,pver,N_COND_VAP) ![1/s] first order loss rate due to nucleation 
   real(r8) :: nuclso4(pcols,pver)               ![kg/kg/s] Nucleated so4 mass tendency from RM's parameterization 
   real(r8) :: nuclsoa(pcols,pver)               ![kg/kg/s] Nucleated soa mass tendency from RM's parameterization 
   integer  :: cond_vap_idx
   
    !Initialize h2so4 and soa nucl variables
    coagulationSink(:,:)=0.0_r8             
    condensationSinkFraction(:,:,:,:) = 0.0_r8  !Sink to the coming "receiver" of any vapour
    numberConcentrationExtMix(:,:,:) = 0.0_r8
!ak+
!    normCSmode1(:,:)=0.0_r8 
!ak-

    do k=1,pver
      do i=1,ncol

         condensationSink(:,:) = 0.0_r8  !Sink to the coming "receiver" of any vapour

         !NB: The following is duplicated code, coordinate with koagsub!!
         !Initialize number concentration for this receiver

         !Air density
         rhoAir(i,k) = pmid(i,k)/rair/temperature(i,k)


         numberConcentration(:) = 0.0_r8

         !Go though all modes receiving condensation
         do mode_index_receiver = 0, nmodes

            !Go through all core species in that mode
            do tracerIndex = 1, getNumberOfBackgroundTracersInMode(mode_index_receiver)

               !Find the lifecycle-specie receiving the condensation
               l_receiver = getTracerIndex(mode_index_receiver, tracerIndex, .true.)

               !Add up the number concentration of the receiving mode [#/m3]
               numberConcentration(mode_index_receiver) = numberConcentration(mode_index_receiver) &  !previous value
                                             + q(i,k,l_receiver)                   &  !kg/kg
                                             / rhopart(physicsIndex(l_receiver))   &  !m3/kg ==> m3_{aer}/kg_{air} 
                                             * volumeToNumber(mode_index_receiver) &  !#/m3 ==> #/kg_{air}
                                             * rhoAir(i,k)                                 !kg/m3 ==> #/m3_{air}
            end do !Lifecycle "core" species in this mode
         enddo


         !All modes are condensation receivers
         do cond_vap_idx=1,N_COND_VAP
            do mode_index_receiver = 0, nmodes

               !This is the loss rate a gas molecule will see due to aerosol surface area 
               condensationSink(mode_index_receiver,cond_vap_idx)   = normalizedCondensationSink(mode_index_receiver,cond_vap_idx)  & ![m3/#/s]
                                                   * numberConcentration(mode_index_receiver)             ![#/m3]
                                                   !==> [1/s]
            end do !Loop over receivers
         end do

         !Find concentration after condensation of all
         !condenseable vapours
         do cond_vap_idx=1,N_COND_VAP

            !sum of cond. sink for this vapour [1/s]
            sumCondensationSink(i,k,cond_vap_idx) = sum(condensationSink(:,cond_vap_idx))


            !Solve the intermediate (end of timestep) concentration using
            !euler backward solution C_{old} + P *dt - L*C_{new}*dt = C_{new} ==> 
            !Cnew -Cold = prod - loss ==>
            intermediateConcentration(i,k,cond_vap_idx) = &
                                 ( q(i,k,cond_vap_map(cond_vap_idx)) + cond_vap_gasprod(i,k,cond_vap_idx)*dt ) & 
                                 / (1.0_r8 + sumCondensationSink(i,k,cond_vap_idx)*dt)
         end do

         !Save the fraction of condensation sink for the externally mixed modes
         !(Needed below to find volume shell)
         do cond_vap_idx=1,N_COND_VAP

            do iDonor = 1,numberOfExternallyMixedModes 
            !Find the mode in question
            mode_index_donor    = externallyMixedMode(iDonor) 

               !Remember fraction of cond sink for this mode
               condensationSinkFraction(i,k,iDonor,cond_vap_idx) = & 
                  condensationSink(mode_index_donor,cond_vap_idx)    &
                   / sumCondensationSink(i,k,cond_vap_idx)

               !Remember number concentration in this mode
               numberConcentrationExtMix(i,k,iDonor) =  &
                     numberConcentration(mode_index_donor)
            end do
         end do

         !Assume only a fraction of ORG_LV left can contribute to nucleation
         soa_lv_forNucleation(i,k) = lvocfrac*intermediateConcentration(i,k,COND_VAP_ORG_LV) !fraction of soa_lv left that is assumend to have low enough
                                            !volatility to nucleate.

         modeIndexReceiverCoag = 0
         !Sum coagulation sink for nucleated so4 and soa particles over all receivers of coagulate. Needed for RM's nucleation code
         !OBS - looks like RM's coagulation sink is multiplied by 10^-12??
         do iCoagReceiver = 1, numberOfCoagulationReceivers

            modeIndexReceiverCoag = receiverMode(iCoagReceiver)

            coagulationSink(i,k) =   &                                                ![1/s]
               coagulationSink(i,k) + &                                               ![1/] previous value
               normalizedCoagulationSink(modeIndexReceiverCoag,MODE_IDX_SO4SOA_AIT) & ![m3/#/s] 
                              * numberConcentration(modeIndexReceiverCoag)            !numberConcentration (#/m3) 
         end do    !coagulation sink

!nuctst3+
!         coagulationSink(i,k) = coagulationSink(i,k) + &
!               normCoagSinkMode1*numberConcentration(1)
!         if (i.eq.1.and.k.eq.30) write(*,*) 'cSink, dcSink = ', coagulationSink(i,k), normCoagSinkMode1*numberConcentration(1)
!         if (i.eq.1.and.k.eq.30) write(*,*) 'nConc1 = ', numberConcentration(1)
!nuctst3-
!ak+
         !Sum coagulation sink for nucleated so4 and soa particles over all additional
         !receivers od coagulate (not directly affecting the life-cycle). 
         do iCoagReceiver = 1, numberOfAddCoagReceivers

            modeIndexReceiverCoag = addReceiverMode(iCoagReceiver)

            coagulationSink(i,k) =   &                                                ![1/s]
               coagulationSink(i,k) + &                                               ![1/] previous value
               normCoagSinkAdd(iCoagReceiver) & ![m3/#/s] 
                              * numberConcentration(modeIndexReceiverCoag)            !numberConcentration (#/m3) 
         end do    !coagulation sink
!ak-

      end do !index i
   end do !index k

   !Calculate nucleated masses of so4 and soa (nuclso4, nuclsoa)
   !following RM's parameterization (cka)
   call aeronucl(lchnk,ncol,temperature, pmid, qh20, &
               intermediateConcentration(:,:,COND_VAP_H2SO4), soa_lv_forNucleation, &
               coagulationSink, nuclso4, nuclsoa, zm, pblh)


   firstOrderLossRateNucl(:,:,:)=0.0_r8
   do k=1,pver
      do i=1,ncol

         !First order loss rate (1/s) for nucleation
         firstOrderLossRateNucl(i,k,COND_VAP_H2SO4) = nuclSo4(i,k)/intermediateConcentration(i,k,COND_VAP_H2SO4)

         !First order loss rate (1/s) for nucleation
         firstOrderLossRateNucl(i,k,COND_VAP_ORG_LV) = nuclSOA(i,k)/intermediateConcentration(i,k,COND_VAP_ORG_LV)

         do cond_vap_idx = 1,N_COND_VAP
            !Solve implicitly (again)
            !C_new - C_old =  PROD_{gas} - CS*C_new*dt - LR_{nucl}*C_new =>
            intermediateConcentration(i,k,cond_vap_idx) = &
                           ( q(i,k,cond_vap_map(cond_vap_idx)) + cond_vap_gasprod(i,k,cond_vap_idx)*dt ) & 
                           / (1.0_r8 + sumCondensationSink(i,k,cond_vap_idx)*dt + firstOrderLossRateNucl(i,k,cond_vap_idx)*dt)

            !fraction nucleated
            fracNucl(i,k,cond_vap_idx) = firstOrderLossRateNucl(i,k,cond_vap_idx) &
                                 /(firstOrderLossRateNucl(i,k,cond_vap_idx) + sumCondensationSink(i,k,cond_vap_idx))
            !From budget, we get: lost = prod -cnew + cold
            gasLost(i,k,cond_vap_idx) = cond_vap_gasprod(i,k,cond_vap_idx)*dt   & !Produced
                                 + q(i,k,cond_vap_map(cond_vap_idx))            & !cold
                                 - intermediateConcentration(i,k,cond_vap_idx)    !cnew

         end do !cond_vap_idx
         
         !Add nuceated mass to so4_na mode
         q(i,k,chemistryIndex(l_so4_na)) =  q(i,k,chemistryIndex(l_so4_na))       &
                     + gasLost(i,k,COND_VAP_H2SO4)*fracNucl(i,k,COND_VAP_H2SO4)

         !H2SO4 condensate
         q(i,k,chemistryIndex(l_so4_a1)) = q(i,k,chemistryIndex(l_so4_a1))         & 
                        + gasLost(i,k,COND_VAP_H2SO4)*(1.0_r8-fracNucl(i,k,COND_VAP_H2SO4))   

         !Add nucleated mass to soa_na mode
         q(i,k,chemistryIndex(l_soa_na)) =  q(i,k,chemistryIndex(l_soa_na))       &
                     + gasLost(i,k,COND_VAP_ORG_LV)*fracNucl(i,k,COND_VAP_ORG_LV)

         !Organic condensate (from both soa_lv and soa_sv) goes to the soaCondensateReceiver tracer (cka)
         q(i,k,chemistryIndex(l_soa_a1)) = q(i,k,chemistryIndex(l_soa_a1))         & 
                        + gasLost(i,k,COND_VAP_ORG_SV)                             &           ! "semi volatile" can not nucleate 
                        + gasLost(i,k,COND_VAP_ORG_LV)*(1.0_r8-fracNucl(i,k,COND_VAP_ORG_LV))  ! part of low volatile which does not nucleate
  
         !condenseable vapours
         q(i,k,chemistryIndex(l_h2so4))  = intermediateConcentration(i,k,COND_VAP_H2SO4)
         q(i,k,chemistryIndex(l_soa_lv)) = intermediateConcentration(i,k,COND_VAP_ORG_LV)
         q(i,k,chemistryIndex(l_soa_sv)) = intermediateConcentration(i,k,COND_VAP_ORG_SV)


         !Condensation transfers mass from externally mixed to internally mixed modes 
         do iDonor = 1,numberOfExternallyMixedModes 

            !Find the mode in question
            mode_index_donor    = externallyMixedMode(iDonor) 

            if(getNumberOfTracersInMode(mode_index_donor) .eq. 0)then
               cycle
            end if

            volume_shell = 0.0_r8
            do cond_vap_idx = 1, N_COND_VAP
               
               !Add up volume shell for this 
               !condenseable vapour
               volume_shell = volume_shell                                               & 
                     + condensationSinkFraction(i,k,iDonor,cond_vap_idx)                 & ![frc]
                     * gasLost(i,k,cond_vap_idx)*(1.0_r8-fracNucl(i,k,cond_vap_idx))     & ![kg/kg]
                     * invRhoPart(physicsIndex(cond_vap_map(cond_vap_idx)))              & !*[m3/kg] ==> [m3/kg_{air}
                     * rhoAir(i,k)                                                         !*[kg/m3] ==> m3/m3
        
            end do
                           
            area_core=numberConcentrationExtMix(i,k,iDonor)*numberToSurface(mode_index_donor)   !#/m3 * m2/# ==> m2/m3
            vol_monolayer=area_core*dr_so4_monolayers_age

            ! Small fraction retained to avoid numerical irregularities
            frac_transfer=min((volume_shell/vol_monolayer),0.999_r8)

            !How many tracers exist in donor mode?
            !The "donor" is the externally mixed mode which will soon
            !become internally mixed. The externally mixed is donating mass
            !and the internally mixed is receiving...
            do tracerIndex = 1, getNumberOfTracersInMode(mode_index_donor)

               !Indexes here are in "chemistry space"
               l_donor    = getTracerIndex(mode_index_donor, tracerIndex,.true.)
               l_receiver = lifeCycleReceiver(l_donor) 

               if( l_receiver .le. 0)then
                  stop !something wrong
               endif

               !Transfer from donor to receiver takes into account 
               !fraction transferred
               totalLoss(i,k,l_donor) = frac_transfer*q(i,k,l_donor)
               q(i,k,l_donor) = q(i,k,l_donor) - totalLoss(i,k,l_donor)
               q(i,k,l_receiver) = q(i,k,l_receiver) + totalLoss(i,k,l_donor)
            end do !tracers in mode
         end do    !loop over receivers
      end do !physical index k 
   end do    !physical index i

   !Output for diagnostics
   call phys_getopts(history_aerosol_out = history_aerosol)
 
   if(history_aerosol)then
      coltend(:ncol,:) = 0.0_r8
      do i=1,gas_pcnst 
         !Check if species contributes to condensation
         if(lifeCycleReceiver(i) .gt. 0)then
           !Loss from the donor specie
           tracer_coltend(:ncol) = sum(totalLoss(:ncol, :,i)*pdel(:ncol,:),2)/gravit/dt
           coltend(:ncol,i) = coltend(:ncol,i) - tracer_coltend(:ncol) !negative (loss for donor)
           coltend(:ncol,lifeCycleReceiver(i)) = coltend(:ncol,lifeCycleReceiver(i)) + tracer_coltend(:ncol) 
         endif
      end do

      ! Remove so4_n ---> directly into so4_na
      coltend(:ncol,chemistryIndex(l_so4_na)) = coltend(:ncol,chemistryIndex(l_so4_na)) + &
                                             sum(                                         &
                                                gasLost(:ncol,:,COND_VAP_H2SO4)           &
                                                *fracNucl(:ncol,:,COND_VAP_H2SO4)*pdel(:ncol,:) , 2 &
                                                )/gravit/dt

      !Take into account H2SO4 (gas) condensed in budget
      coltend(:ncol,chemistryIndex(l_so4_a1)) = coltend(:ncol,chemistryIndex(l_so4_a1)) + &
                                             sum(                                         &
                                                gasLost(:ncol,:,COND_VAP_H2SO4)           &
                                                *(1.0_r8 - fracNucl(:ncol,:,COND_VAP_H2SO4))*pdel(:ncol,:) , 2 &
                                                )/gravit/dt

      !Take into account soa_lv (gas) nucleated in budget 
      coltend(:ncol,chemistryIndex(l_soa_na)) = coltend(:ncol,chemistryIndex(l_soa_na)) + &
                                             sum(                                         &
                                                gasLost(:ncol,:,COND_VAP_ORG_LV)              &
                                                *fracNucl(:ncol,:,COND_VAP_ORG_LV)*pdel(:ncol,:) , 2 &
                                                )/gravit/dt

      !Take into account soa gas condensed in the budget (both LV and SV)
      coltend(:ncol,chemistryIndex(l_soa_a1)) = coltend(:ncol,chemistryIndex(l_soa_a1)) + &
                                             sum(                                         &
                                                gasLost(:ncol,:,COND_VAP_ORG_LV)           &
                                                *(1.0_r8 - fracNucl(:ncol,:,COND_VAP_ORG_LV))*pdel(:ncol,:) , 2 &
                                                )/gravit/dt                        &
                                                +                                  & 
                                             sum(                                         &
                                                gasLost(:ncol,:,COND_VAP_ORG_SV)*pdel(:ncol,:) , 2 &
                                                )/gravit/dt                        
                                                
      do i=1,gas_pcnst
         if(lifeCycleReceiver(i) .gt. 0 )then
            long_name= trim(solsym(i))//"condTend"
            call outfld(long_name, coltend(:ncol,i), pcols, lchnk)
            long_name= trim(solsym(lifeCycleReceiver(i)))//"condTend"
            call outfld(long_name, coltend(:ncol,lifeCycleReceiver(i)),pcols,lchnk)
         end if
      end do
      long_name=trim(solsym(chemistryIndex(l_so4_a1)))//"condTend"
      call outfld(long_name, coltend(:ncol,chemistryIndex(l_so4_a1)),pcols,lchnk)
      long_name=trim(solsym(chemistryIndex(l_soa_a1)))//"condTend"
      call outfld(long_name, coltend(:ncol,chemistryIndex(l_soa_a1)),pcols,lchnk)
      long_name=trim(solsym(chemistryIndex(l_so4_na)))//"condTend"
      call outfld(long_name, coltend(:ncol,chemistryIndex(l_so4_na)),pcols,lchnk)
      long_name=trim(solsym(chemistryIndex(l_soa_na)))//"condTend"
      call outfld(long_name, coltend(:ncol,chemistryIndex(l_soa_na)),pcols,lchnk)

   endif


   return
   end subroutine condtend_sub


end module condtend
