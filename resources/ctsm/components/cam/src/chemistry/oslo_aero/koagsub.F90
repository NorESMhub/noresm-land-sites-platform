module koagsub

   use phys_control,     only: phys_getopts
   use aerosoldef
   use chem_mods,     only: gas_pcnst
   use mo_tracname,    only: solsym
   use const
   use shr_kind_mod,          only: r8 => shr_kind_r8
   use physconst,          only: rair, gravit
   use cam_logfile,      only : iulog
   save

   real(r8), parameter :: kboltzmann = 1.3806488e-23_r8          ![m2 kg s-2 K-1]
   real(r8), parameter :: temperatureLookupTables = 293.15_r8    !Temperature used in look up tables
   real(r8), parameter :: mfpAir = 63.3e-9_r8                    ![m] mean free path air
   real(r8), parameter :: viscosityAir = 1.983e-5_r8             ![Pa s] viscosity of air

   real(r8), parameter :: rhoh2o = 1000._r8 ! Density of water

   integer, parameter :: numberOfCoagulatingModes = 6
   integer, parameter :: numberOfCoagulationReceivers = 6

   real(r8), dimension(0:nmodes,0:nmodes) :: normalizedCoagulationSink ![m3/#/s]
   real(r8), dimension(0:nmodes)          :: NCloudCoagulationSink    ![m3/#/s]

!nuctst3+
   real(r8) normCoagSinkMode1 ![m3/#/s]
!nuctst3-
!aktest+
   integer, parameter :: numberOfAddCoagReceivers = 6
   real(r8), dimension(numberOfAddCoagReceivers) :: normCoagSinkAdd ![m3/#/s]
!aktest-

   !These are the modes which are coagulating (belonging to mixtures no. 0, 1, 2, 4, 12, 14)
   integer, dimension(numberOfCoagulatingModes) :: coagulatingMode =    & 
     (/MODE_IDX_BC_EXT_AC                                               &  !inert mode
     , MODE_IDX_SO4SOA_AIT, MODE_IDX_BC_AIT, MODE_IDX_OMBC_INTMIX_COAT_AIT &  !internally mixed small modes
     , MODE_IDX_BC_NUC, MODE_IDX_OMBC_INTMIX_AIT /)      !externally mixed small modes

   !These are the modes which are receiving coagulating material in OsloAero 
   ! (belonging to mixtures no. 5, 6, 7, 8, 9, 10)
   integer, dimension(numberOfCoagulationReceivers) :: receiverMode = & 
       (/MODE_IDX_SO4_AC,MODE_IDX_DST_A2, MODE_IDX_DST_A3, MODE_IDX_SS_A1, MODE_IDX_SS_A2, MODE_IDX_SS_A3 /)

!aktest+
   !And these are the additional modes which are allowed to contribute to the
   ! coagulation sink, defined here and to be used only in the nucleation code in condtend.F90  
   ! (belonging to mixtures no. 0, 1, 2, 4, 12, 14)
   integer, dimension(numberOfAddCoagReceivers) :: addReceiverMode = & 
       (/MODE_IDX_BC_EXT_AC,MODE_IDX_SO4SOA_AIT,MODE_IDX_BC_AIT, &
         MODE_IDX_OMBC_INTMIX_COAT_AIT,MODE_IDX_BC_NUC,MODE_IDX_OMBC_INTMIX_AIT /) 
!aktest-

   !Coagulation moves aerosol mass to the "coagulate" species, so some
   !lifecycle species will receive mass in this routine!
   integer, dimension(gas_pcnst) :: lifeCycleReceiver

   ! Coagulation between aerosol and cloud droplets move coagulate into
   ! the  equivalent value for aerosol concentration in cloud water.
   ! Exception: Sulphate coagulation with cloud droplets is merged with
   ! component from aqueous phase chemistry in order to take advantage of the 
   ! more detailed addition onto larger particles.

   integer, dimension(gas_pcnst) :: CloudAerReceiver

! Closest Table index for assumed size of droplets used in coagulation
   integer :: tableindexcloud 
   real(r8),parameter :: rcoagdroplet = 10.e-6   ! m


contains

subroutine initializeCoagulationOutput()

   use ppgrid, only: pver
   use cam_history,     only: addfld, add_default, fieldname_len, horiz_only
   implicit none
   integer :: imode
   integer :: iChem
   integer :: modeIndexCoagulator

   character(len=fieldname_len+3) :: fieldname_receiver
   character(len=fieldname_len+3) :: fieldname_donor
   character(8)                   :: unit
   logical                                          :: history_aerosol
   logical, dimension(gas_pcnst)  :: isAlreadyOnList

   call phys_getopts(history_aerosol_out = history_aerosol)

   isAlreadyOnList(:) = .FALSE.
   do iChem = 1,gas_pcnst
      !Does this tracer have a receiver? If yes: It contributes to coagulation
      if(lifeCycleReceiver(iChem) .gt. 0)then
         unit = "kg/m2/s"
         fieldname_donor = trim(solsym(iChem))//"coagTend"
         fieldname_receiver    = trim(solsym(lifeCycleReceiver(iChem)))//"coagTend"
         if(.not. isAlreadyOnList(lifeCycleReceiver(iChem)))then
            call addfld( fieldname_receiver, horiz_only ,"A", unit, "coagulation tendency")
            isAlreadyOnList(lifeCycleReceiver(iChem))=.TRUE.
         end if
         call addfld( fieldname_donor, horiz_only, 'A', unit, "coagulation tendency" )
         if(history_aerosol)then
            call add_default( fieldname_receiver, 1, ' ' )
            call add_default( fieldname_donor   , 1, ' ')
         end if
      end if
   end do

   isAlreadyOnList(:) = .FALSE.
   do iChem = 1,gas_pcnst
      if(CloudAerReceiver(iChem) .gt. 0)then
         unit = "kg/m2/s"
         fieldname_donor = trim(solsym(iChem))//"clcoagTend"
         fieldname_receiver    = trim(solsym(CloudAerReceiver(iChem)))//"_OCWclcoagTend"
         if(.not. isAlreadyOnList(CloudAerReceiver(iChem)))then
            call addfld( fieldname_receiver, horiz_only, 'A', unit, "coagulation tendency" )
            isAlreadyOnList(CloudAerReceiver(iChem))=.TRUE.
         end if
         call addfld( fieldname_donor, horiz_only, "A", unit, "coagulation tendency" )
         if(history_aerosol)then
            call add_default( fieldname_receiver, 1, ' ' )
            call add_default( fieldname_donor   , 1, ' ')
         end if
      end if

   end do

end subroutine initializeCoagulationOutput

subroutine initializeCoagulationReceivers()
   implicit none

   !These are the lifecycle-species receiving coagulate
   lifeCycleReceiver(:) = -99
   lifeCycleReceiver(chemistryIndex(l_bc_ax))  = chemistryIndex(l_bc_ac)
   lifeCycleReceiver(chemistryIndex(l_so4_na)) = chemistryIndex(l_so4_ac)  !create so4 coagulate from so4 in mode 1
   lifeCycleReceiver(chemistryIndex(l_bc_a))   = chemistryIndex(l_bc_ac)   !create bc coagulate  from bc in mode 2
   lifeCycleReceiver(chemistryIndex(l_bc_ai))  = chemistryIndex(l_bc_ac)   !create bc coagulate from bc in mode 4
   lifeCycleReceiver(chemistryIndex(l_om_ai))  = chemistryIndex(l_om_ac)   !create om coagulate from om in mode 4
   lifeCycleReceiver(chemistryIndex(l_bc_n))  = chemistryIndex(l_bc_ac)    !create bc coagulate from bc in mode 12
   lifeCycleReceiver(chemistryIndex(l_bc_ni)) = chemistryIndex(l_bc_ac)    !create bc coagulate from om in mode 14
   lifeCycleReceiver(chemistryIndex(l_om_ni)) = chemistryIndex(l_om_ac)    !create om coagulate from om in mode 14
   lifeCycleReceiver(chemistryIndex(l_so4_a1)) = chemistryIndex(l_so4_ac)  !Create so4 coagulate from so4 condensate
   lifeCycleReceiver(chemistryINdex(l_soa_na)) = chemistryIndex(l_soa_a1)

  !These are the lifecycle-species receiving coagulate
   CloudAerReceiver(:) = -99
   CloudAerReceiver(chemistryIndex(l_bc_ax))  = chemistryIndex(l_bc_ac)
   CloudAerReceiver(chemistryIndex(l_so4_na)) = chemistryIndex(l_so4_a2)  !create so4 coagulate from so4 in mode 1
   CloudAerReceiver(chemistryIndex(l_bc_a))   = chemistryIndex(l_bc_ac)   !create bc coagulate  from bc in mode 2
   CloudAerReceiver(chemistryIndex(l_bc_ai))  = chemistryIndex(l_bc_ac)   !create bc coagulate from bc in mode 4
   CloudAerReceiver(chemistryIndex(l_om_ai))  = chemistryIndex(l_om_ac)   !create om coagulate from om in mode 4
   CloudAerReceiver(chemistryIndex(l_bc_n))  = chemistryIndex(l_bc_ac)    !create bc coagulate from bc in mode 12
   CloudAerReceiver(chemistryIndex(l_bc_ni)) = chemistryIndex(l_bc_ac)    !create bc coagulate from om in mode 14
   CloudAerReceiver(chemistryIndex(l_om_ni)) = chemistryIndex(l_om_ac)    !create om coagulate from om in mode 14
   CloudAerReceiver(chemistryIndex(l_so4_a1)) = chemistryIndex(l_so4_a2)  !Create so4 coagulate from so4 condensate
   cloudAerReceiver(chemistryIndex(l_soa_na)) = chemistryIndex(l_soa_a1)


end subroutine initializeCoagulationReceivers

subroutine initializeCoagulationCoefficients(rhob,rk)

      use mo_constants,  only:  pi
      use const, only: normnk

      implicit none

      real(r8), intent(in) :: rk(0:nmodes)   ![unit] radius of background (receiver) mode
      real(r8), intent(in) :: rhob(0:nmodes) !density of background mode
   
      real(r8), dimension(numberOfCoagulationReceivers, numberOfCoagulatingModes, nBinsTab) :: K12 = 0.0_r8  !Coagulation coefficient (m3/s)

!nuctst3+
!      real(r8), dimension(nBinsTab) :: CoagCoeffMode1 = 0.0_r8  !Coagulation coefficient mode 1 with 1 (m3/s)
!nuctst3-
!ak+
      real(r8), dimension(numberOfAddCoagReceivers,nBinsTab) :: CoagCoeffModeAdd = 0.0_r8  !Coagulation coefficient mode 1 (m3/s)
!ak-

      real(r8), dimension(numberOfCoagulatingModes,nBinsTab) :: K12Cl = 0.0_r8  !Coagulation coefficient (m3/s)

      real(r8), dimension(nBinsTab) :: coagulationCoefficient
      integer              :: aMode
      integer              :: modeIndex
      integer              :: modeIndexCoagulator  !Index of coagulating mode
      integer              :: modeIndexReceiver    !Index of receiving mode
      integer              :: iCoagulatingMode     !Counter for coagulating mode
      integer              :: iReceiverMode        !Counter for receiver modes
      integer              :: nsiz                 !counter for look up table sizes

      do iReceiverMode = 1, numberOfCoagulationReceivers
         do iCoagulatingMode = 1,numberOfCoagulatingModes

            !Index of the coagulating mode (0-14), see list above
            modeIndexCoagulator = coagulatingMode(iCoagulatingMode) 

            !Index of receiver mode (0-14), see list above
            modeIndexReceiver = receiverMode(iReceiverMode) 
            
            !Pre-calculate coagulation coefficients for this coagulator..
            !Note: Not using actual density of coagulator here 
            !Since this is not known at init-time
            call calculateCoagulationCoefficient(CoagulationCoefficient    & !O [m3/s] coagulation coefficient 
                                 , rk(modeIndexCoagulator)                 & !I [m] radius of coagulator
                                 , rhob(modeIndexCoagulator)               & !I [kg/m3] density of coagulator
                                 , rhob(modeIndexReceiver) )                 !I [kg/m3] density of receiver

            !Save values
            K12(iReceiverMode,iCoagulatingMode,:) = CoagulationCoefficient(:)

         enddo
      end do !receiver modes

!nuctst3+
!            call calculateCoagulationCoefficient(CoagulationCoefficient    & !O [m3/s] coagulation coefficient 
!                                 , rk(1)                                   & !I [m] radius of coagulator
!                                 , rhob(1)                                 & !I [kg/m3] density of coagulator
!                                 , rhob(1) )                                 !I [kg/m3] density of receiver
!            CoagCoeffMode1(:) = CoagulationCoefficient(:)
!nuctst3-
!ak+
      do iReceiverMode = 1, numberOfAddCoagReceivers
            iCoagulatingMode = 1

            !Index of the coagulating mode (0-14), see list above
            modeIndexCoagulator = coagulatingMode(iCoagulatingMode) 

            !Index of receiver mode (0-14), see list above
            modeIndexReceiver = addReceiverMode(iReceiverMode) 
            
            !Pre-calculate coagulation coefficients for this coagulator..
            !Note: Not using actual density of coagulator here 
            !Since this is not known at init-time
            call calculateCoagulationCoefficient(CoagulationCoefficient    & !O [m3/s] coagulation coefficient 
                                 , rk(modeIndexCoagulator)                 & !I [m] radius of coagulator
                                 , rhob(modeIndexCoagulator)               & !I [kg/m3] density of coagulator
                                 , rhob(modeIndexReceiver) )                 !I [kg/m3] density of receiver

            !Save values
            CoagCoeffModeAdd(iReceiverMode,:) = CoagulationCoefficient(:)

      end do !receiver modes
!ak-

! Onl one receivermode for cloud coagulation (water)
      do iCoagulatingMode = 1,numberOfCoagulatingModes

            !Index of the coagulating mode (0-14), see list above
         modeIndexCoagulator = coagulatingMode(iCoagulatingMode) 
            
            !Pre-calculate coagulation coefficients for this coagulator..
            !Note: Not using actual density of coagulator here 
            !Since this is not known at init-time
         call calculateCoagulationCoefficient(CoagulationCoefficient    & !O [m3/s] coagulation coefficient 
                                 , rk(modeIndexCoagulator)                 & !I [m] radius of coagulator
                                 , rhob(modeIndexCoagulator)               & !I [kg/m3] density of coagulator
                                 , rhoh2o )                 !I [kg/m3] density of receiver

            !Save values
            K12Cl(iCoagulatingMode,:) = CoagulationCoefficient(:)

      enddo



      !We don't need to remember K12 for all lookuptable sizes!!
      !We only need to rember for 1 [#/m3] of each receiver mode
      !and then later scale by number concentration in receiver modes
      normalizedCoagulationSink(:,:) = 0.0_r8

      do iCoagulatingMode = 1, numberOfCoagulatingModes

         !Sum the loss for all possible receivers
         do iReceiverMode = 1, numberOfCoagulationReceivers

            modeIndexCoagulator = coagulatingMode(iCoagulatingMode) !Index of the coagulating mode

            modeIndexReceiver = receiverMode(iReceiverMode) !Index of receiver mode
            
            do nsiz=1,nBinsTab  !aerotab bin sizes   
      
               !Sum up coagulation sink for this coagulating species (for all receiving modes)
               normalizedCoagulationSink(modeIndexReceiver, modeIndexCoagulator)      =   &   ![m3/#/s]
                     normalizedCoagulationSink(modeIndexReceiver, modeIndexCoagulator)    &   ![m3/#/s] Previous value
                   + normnk(modeIndexReceiver, nsiz)          &   !Normalized size distribution for receiver mode
                   * K12(iReceiverMode, iCoagulatingMode, nsiz)                  !Koagulation coefficient (m3/#/s)
            end do !Look up table size
         end do    !receiver modes
      end do       !coagulator
 

!nuctst3+
!            !Add simple self coagulation sink for mode 1 (with 1) in such a way that it
!            !affects coagulationSink but not the lifecycling (directly) otherwise
!            normCoagSinkMode1 = 0.0_r8
!            do nsiz=1,nBinsTab  !aerotab bin sizes   
!                   normCoagSinkMode1 = normCoagSinkMode1 + normnk(1,nsiz) * CoagCoeffMode1(nsiz)
!            end do !Look up table size
!nuctst3-
!ak+
            !Calculate additional coagulation sink for mode 1 in such a way that it
            !affects coagulationSink but not the lifecycling (directly) otherwise

         !Sum the loss for all possible receivers
         normCoagSinkAdd(:) = 0.0_r8
         iCoagulatingMode = 1
         do iReceiverMode = 1, numberOfAddCoagReceivers

            modeIndexReceiver = addReceiverMode(iReceiverMode) !Index of additional receiver mode
            
            do nsiz=1,nBinsTab  !aerotab bin sizes   
      
               !Sum up coagulation sink for this coagulating species (for all receiving modes)
               normCoagSinkAdd(iReceiverMode)      =   &   ![m3/#/s]
                     normCoagSinkAdd(iReceiverMode)    &   ![m3/#/s] Previous value
                   + normnk(modeIndexReceiver, nsiz)          &   !Normalized size distribution for receiver mode
                   * CoagCoeffModeAdd(iReceiverMode, nsiz)                  !Koagulation coefficient (m3/#/s)
            end do !Look up table size
         end do    !receiver modes
!ak-
 
      nsiz=1
      do while (rBinMidPoint(nsiz).lt.rcoagdroplet.and.nsiz.lt.nBinsTab)
         nsiz=nsiz+1
      end do

      if (abs(rBinMidPoint(nsiz-1)-rcoagdroplet).lt.abs(rBinMidPoint(nsiz)-rcoagdroplet)) then      
         tableindexcloud=nsiz-1
      else
         tableindexcloud=nsiz
      end if         
      write(iulog,*) 'Assumed droplet size and table bin number for cloud  &
         coagulation ',rcoagdroplet, ' nbin ',tableindexcloud,'binmid',rBinMidPoint(tableindexcloud)

      do iCoagulatingMode = 1, numberOfCoagulatingModes
         modeIndexCoagulator = coagulatingMode(iCoagulatingMode) !Index of the coagulating mode

         NCloudCoagulationSink(modeIndexCoagulator)      =   &   ![m3/#/s]
         K12Cl(iCoagulatingMode, tableindexcloud)                  !Koagulation coefficient (m3/#/s)
              
      end do
         
end subroutine initializeCoagulationCoefficients

!Calculates coagulation coefficient for a coagulator mode
!with a given radius with all look-up table modes
subroutine calculateCoagulationCoefficient(CoagulationCoefficient, modeRadius, modeDensity, receiverDensity)

      implicit none

      real(r8), intent(in)  :: modeRadius      ! [m] (?)
      real(r8), intent(in)  :: modeDensity     ! [kg/m3] densityi
      real(r8), intent(in)  :: receiverDensity ! [kg/m3] density of receiver
      real(r8), intent(out), dimension(:)  :: coagulationCoefficient ![m3/s]

      integer               :: i             !Counter for look-up tables

      real(r8) :: diff1 ![m2/s] diffusivity
      real(r8) :: diff2 ![m2/s] diffusivity
      real(r8) :: g12   ![-] factor
      real(r8) :: g1    ![-] factor
      real(r8) :: g2    ![-] factor
      real(r8) :: c12   ![m/s] average particle thermal velocity
      real(r8) :: c1    ![m/s] particle thermal velocity 
      real(r8) :: c2    ![m/s] particle thermal velocity
      real(r8) :: mfv1  ![m] mean free path particle
      real(r8) :: mfv2  ![m] mean free path particle

!     coagulation coefficient for SO4 (Brownian, Fuchs form)
      !Loop through indexes in look-up table
      do i=1,nBinsTab
        c1=calculateThermalVelocity(rBinMidPoint(i), receiverDensity)     !receiving size
        c2=calculateThermalVelocity(modeRadius, modeDensity)    !coagulating aerosol
        c12=sqrt(c1**2+c2**2)

        diff1 = calculateParticleDiffusivity(rBinMidPoint(i))             !receiving particle
        diff2 = calculateParticleDiffusivity(modeRadius)        !coagulating particle

        mfv1=calculateMeanFreePath(diff1,c1)  !receiving particle
        mfv2=calculateMeanFreePath(diff2,c2)  !coagulating particle

        g1 = calculateGFactor(rBinMidPoint(i), mfv1)
        g2 = calculateGFactor(modeRadius, mfv2)

        g12=sqrt(g1**2+g2**2)

        !Coagulation coefficient of receiver size "i" with the coagulating
        !mode "kcomp"
        CoagulationCoefficient(i) =  &
                  4.0_r8*pi*(rBinMidPoint(i)+modeRadius)*(diff1+diff2)          &
                  /((rBinMidPoint(i)+modeRadius)/(rBinMidPoint(i)+modeRadius+g12)         &
                  +(4.0_r8/c12)*(diff1+diff2)/(modeRadius+rBinMidPoint(i)))

      enddo ! loop on imax

   return

end subroutine calculateCoagulationCoefficient


!Time step routine for coagulation
!Called from chemistry

subroutine coagtend(  q, pmid, pdel, temperature, delt_inverse, ncol , lchnk) 

! Calculate the coagulation of small aerosols with larger particles and 
! cloud droplets. Only particles smaller that dry radius of 
! 40 nm is assumed to have an efficient coagulation with other particles.

use shr_kind_mod, only: r8 => shr_kind_r8
use ppgrid,           only : pcols, pver
use cam_history,  only: outfld
use aerosoldef
use const
use physics_buffer, only : physics_buffer_desc
use modal_aero_data, only : qqcw_get_field
implicit none
 

!  input arguments
   integer, intent(in)     :: ncol                        ! number of horizontal grid cells (columns)
   real(r8), intent(inout) :: q(pcols,pver,gas_pcnst)     ! TMR [kg/kg] including moisture
   real(r8), intent(in) :: pmid(pcols,pver)               ! [Pa] midpoint pressure
   real(r8), intent(in) :: pdel(pcols,pver)
   real(r8), intent(in) :: temperature(pcols,pver)        ! [K] temperature
   real(r8), intent(in) :: delt_inverse                   ! [1/s] inverse time step
   integer, intent(in)  :: lchnk                          ! [] chnk id needed for output
! local
   integer :: k ! level counter
   integer :: i ! horizontal counter
   integer :: m ! Species counter
   integer :: iCoagulator !counter for species coagulating
   integer :: iReceiver !counter for species receiving coagulate
   integer :: iSpecie   !counter for species in mode
   integer :: nsiz      !loop up table size
   integer :: l_index_receiver
   integer :: l_index_donor
   integer              :: modeIndexCoagulator !Index of coagulating mode
   integer              :: modeIndexReceiver   !Index of receiving mode
   real(r8)                                         :: rhoAir              ![kg/m3] air density
   real(r8)                                         :: coagulationSink     ![1/s] loss for coagulating specie   
   real(r8), dimension(numberOfCoagulationReceivers):: numberConcentration ![#/m3] number concentration
   real(r8)                                         :: totalLoss(pcols,pver,gas_pcnst) ![kg/kg] tracer lost
   character(128)                 :: long_name                              ![-] needed for diagnostics
   real(r8), pointer :: fldcw(:,:)
   real(r8), dimension(pcols, gas_pcnst)            :: coltend
   real(r8), dimension(pcols)                       :: tracer_coltend
   logical                                          :: history_aerosol


   totalLoss(:,:,:)=0.0_r8


   call phys_getopts(history_aerosol_out = history_aerosol)

   do k=1,pver
      do i=1,ncol  
        
         !Air density
         rhoAir = pmid(i,k)/rair/temperature(i,k)
   
         !Initialize number concentration for all receivers
         numberConcentration(:) = 0.0_r8

         !Go though all modes receiving coagulation
         do ireceiver = 1,numberOfCoagulationReceivers

            !Go through all core species in that mode
            do iSpecie = 1,getNumberOfTracersInMode(receiverMode(ireceiver))

               !Find the lifecycle-specie receiving the coagulation
               l_index_receiver = getTracerIndex(receiverMode(ireceiver) , iSpecie , .true.)

               long_name = solsym(l_index_receiver) !For testing


               if(.NOT. is_process_mode(l_index_receiver,.true.)) then
                  !Add up the number concentration of the receiving mode
                   numberConcentration(iReceiver) = numberConcentration(iReceiver)     &  !previous value
                                             + q(i,k,l_index_receiver)                 &  !kg/kg
                                             / rhopart(physicsIndex(l_index_receiver))    &  !*[m3/kg] ==> m3/kg
                                             * volumeToNumber(receiverMode(ireceiver)) &  ![#/m3] ==> #/kg 
                                             * rhoAir                                 !#/kg ==> #/m3
               end if
            end do !Lifecycle "core" species in this mode
         enddo


         !Go through all coagulating modes
         do iCoagulator = 1, numberOfCoagulatingModes

            !Initialize loss (for a coagulator) summed over all receivers
            coagulationSink = 0.0_r8

            modeIndexCoagulator = coagulatingMode(iCoagulator)

            !Sum the loss for all possible receivers
            do iReceiver = 1, numberOfCoagulationReceivers
     
               modeIndexReceiver = receiverMode(iReceiver)

               !Sum up coagulation sink for this coagulating species (for all receiving modes)
               coagulationSink =   &                                                    ![1/s]
                  coagulationSink + &                                                   ![1/] previous value
                  normalizedCoagulationSink(modeIndexReceiver, modeIndexCoagulator) &   ![m3/#/s] 
                                 * numberConcentration(ireceiver)                       !numberConcentration (#/m3)
            end do    !receiver modes
           
            !SOME LIFECYCLE SPECIES CHANGE "HOST MODE" WHEN THEY PARTICIPATE
            !IN COAGULATION (THEY GO FROM EXTERNALLY MIXED TO INTERNALLY MIXED MODES)

            !Each coagulating mode can contain several species
            do ispecie = 1, getNumberOfTracersInMode(modeIndexCoagulator)

               !Get the lifecycle specie which is lost
               l_index_donor = getTracerIndex(modeIndexCoagulator , ispecie,.true. )

               !Move lifecycle species to new lifecycle species due to coagulation

               !process modes don't change mode except so4 condensate which becomes coagulate instead
               !assumed to have same sink as MODE_IDX_OMBC_INTMIX_AIT
               if( .NOT. is_process_mode(l_index_donor,.true.)   &   
                  .OR. ( (l_index_donor.eq.chemistryIndex(l_so4_a1))  .AND. modeIndexCoagulator .eq. MODE_IDX_OMBC_INTMIX_COAT_AIT) ) then       

                  !Done summing total loss of this coagulating specie
                  totalLoss(i,k,l_index_donor) = coagulationSink         &   !loss rate for a mode in [1/s] summed over all receivers
                              * q(i,k,l_index_donor)                     &   !* mixing ratio ==> MMR/s
                              / delt_inverse                                 ! seconds ==> MMR

                  !Can not loose more than we have
                  totalLoss(i,k,l_index_donor) = min(totalLoss(i,k,l_index_donor) , q(i,k,l_index_donor))


               end if !check on process modes
            end do    !species in mode

         end do !coagulator mode
      end do ! i
   end do    ! k


   !UPDATE THE TRACERS AND DO DIAGNOSTICS
   do iCoagulator = 1, numberOfCoagulatingModes
      do ispecie = 1, getNumberOfTracersInMode(coagulatingMode(iCoagulator))

         l_index_donor = getTracerIndex(coagulatingMode(iCoagulator) , ispecie ,.true.)

         !so4_a1 is a process mode (condensate), but is still lost in coagulation
         if( .NOT. is_process_mode(l_index_donor, .true.)          &
            .OR. ( (l_index_donor.eq.chemistryIndex(l_so4_a1))  .AND. coagulatingMode(iCoagulator) .eq. MODE_IDX_OMBC_INTMIX_COAT_AIT) ) then       

            l_index_donor = getTracerIndex(coagulatingMode(iCoagulator) , ispecie,.true. )

            !index of mode gaining mass (l_so4_ac, l_om_ac, l_bc_ac), coagulate
            l_index_receiver = lifeCycleReceiver(l_index_donor)          

            do k=1,pver
               !Loose mass from tracer in donor mode
               q(:ncol,k,l_index_donor)    = q(:ncol,k,l_index_donor) - totalLoss(:ncol,k,l_index_donor)

               !Give mass to tracer in receiver mode
               q(:ncol,k,l_index_receiver) = q(:ncol,k,l_index_receiver) + totalLoss(:ncol,k,l_index_donor)
            end do !k
         endif
     end do
   end do

   !Output for diagnostics
   if(history_aerosol)then
      coltend(:ncol,:) = 0.0_r8
      do i=1,gas_pcnst 
         !Check if species contributes to coagulation
         if(lifeCycleReceiver(i) .gt. 0)then
           !Loss from the donor specie
           tracer_coltend(:ncol) = sum(totalLoss(:ncol, :,i)*pdel(:ncol,:),2)/gravit*delt_inverse
           coltend(:ncol,i) = coltend(:ncol,i) - tracer_coltend(:ncol) !negative, loss for donor
           coltend(:ncol,lifeCycleReceiver(i)) = coltend(:ncol,lifeCycleReceiver(i)) + tracer_coltend(:ncol) 
         endif
      end do
      do i=1,gas_pcnst
         if(lifeCycleReceiver(i) .gt. 0)then
            long_name= trim(solsym(i))//"coagTend"
            call outfld(long_name, coltend(:ncol,i), pcols, lchnk)
            long_name= trim(solsym(lifeCycleReceiver(i)))//"coagTend"
            call outfld(long_name, coltend(:ncol,lifeCycleReceiver(i)),pcols,lchnk)
         end if
      end do
   endif

end subroutine coagtend

subroutine clcoag(  q, pmid, pdel, temperature, cldnum, cldfrc, delt_inverse, ncol , lchnk, im, pbuf) 

! Calculate the coagulation of small aerosols with larger particles and 
! cloud droplets. Only particles smaller that dry radius of 
! 40 nm is assumed to have an efficient coagulation with other particles.

use shr_kind_mod, only: r8 => shr_kind_r8
use ppgrid,           only : pcols, pver
use cam_history,  only: outfld
use aerosoldef
use const
use physics_buffer, only : physics_buffer_desc
use modal_aero_data, only : qqcw_get_field
implicit none
 

!  input arguments
   integer, intent(in)     :: ncol                        ! number of horizontal grid cells (columns)
   real(r8), intent(inout) :: q(pcols,pver,gas_pcnst)     ! TMR [kg/kg]  including moisture
   real(r8), intent(in) :: pmid(pcols,pver)               ! [Pa] midpoint pressure
   real(r8), intent(in) :: pdel(pcols,pver)
   real(r8), intent(in) :: temperature(pcols,pver)        ! [K] temperature

   real(r8), dimension(ncol,pver),intent(in)     :: cldnum ! Droplet concentration #/kg  
   real(r8), dimension(ncol,pver),intent(in)     :: cldfrc ! Cloud volume fraction

   real(r8), intent(in) :: delt_inverse                   ! [1/s] inverse time step
   integer, intent(in)  :: lchnk                          ! [] chnk id needed for output
  integer,   intent(in)    :: im

   type(physics_buffer_desc), pointer :: pbuf(:)


! local
   integer :: k ! level counter
   integer :: i ! horizontal counter
   integer :: m ! Species counter
   integer :: iCoagulator !counter for species coagulating
   integer :: iReceiver !counter for species receiving coagulate
   integer :: iSpecie   !counter for species in mode
   integer :: nsiz      !loop up table size
   integer :: l_index_receiver
   integer :: l_index_donor
   integer              :: modeIndexCoagulator !Index of coagulating mode
   integer              :: modeIndexReceiver   !Index of receiving mode
   real(r8)                                         :: coagulationSink     ![1/s] loss for coagulating specie   
   real(r8), dimension(numberOfCoagulationReceivers):: numberConcentration ![#/m3] number concentration
   real(r8)                                         :: cloudLoss(pcols,pver,gas_pcnst) ![kg/kg] tracer lost
   character(128)                 :: long_name                              ![-] needed for diagnostics
   real(r8)                                         :: rhoAir              ![kg/m3] air density
   real(r8), pointer :: fldcw(:,:)
   real(r8), dimension(pcols, gas_pcnst)            :: coltend
   real(r8), dimension(pcols)                       :: tracer_coltend
   logical                                          :: history_aerosol


   call phys_getopts(history_aerosol_out = history_aerosol)

   cloudLoss(:,:,:)=0.0_r8


  do k=1,pver
     do i=1,ncol  
        if (cldfrc(i,k).gt.1.e-2) then
         rhoAir = pmid(i,k)/rair/temperature(i,k)
          !Go through all coagulating modes
           do iCoagulator = 1, numberOfCoagulatingModes

            !Initialize loss (for a coagulator) summed over all receivers
              coagulationSink = 0.0_r8

              modeIndexCoagulator = coagulatingMode(iCoagulator)

            !Receiver for cloud coagulation is water droplets so do not need
            !go through the coagulation receivers.

               !Sum up coagulation sink for this coagulating species (for all receiving modes)
              coagulationSink =   &         ![1/s]
                NCloudCoagulationSink(modeIndexCoagulator) &   ![m3/#/s] 
                        * (rhoair*cldnum(i,k)/cldfrc(i,k))     ![kg/m3*#/kg
           
            !Each coagulating mode can contain several species
                do ispecie = 1, getNumberOfTracersInMode(modeIndexCoagulator)

               !Get the lifecycle specie which is lost
                l_index_donor = getTracerIndex(modeIndexCoagulator , ispecie,.true. )

               !Move lifecycle species to new lifecycle species due to coagulation

               !process modes don't change mode except so4 condensate which becomes coagulate instead
               !assumed to have same sink as MODE_IDX_OMBC_INTMIX_AIT
               if( .NOT. is_process_mode(l_index_donor,.true.)   &   
                  .OR. ( (l_index_donor.eq.chemistryIndex(l_so4_a1))  .AND. modeIndexCoagulator .eq. MODE_IDX_OMBC_INTMIX_COAT_AIT) ) then       

                  !Done summing total loss of this coagulating specie
                  cloudLoss(i,k,l_index_donor) = coagulationSink         &   !loss rate for a mode in [1/s] summed over all receivers
                              * cldfrc(i,k)*q(i,k,l_index_donor)                     &   !* mixing ratio ==> MMR/s
                              / delt_inverse                                 ! seconds ==> MMR

                  !Can not loose more than we have 
                  ! At present day assumed lost within the cloud
                  cloudLoss(i,k,l_index_donor) = min(cloudLoss(i,k,l_index_donor) , cldfrc(i,k)*q(i,k,l_index_donor))

                  
                  end if !check on process modes
               end do    !species in mode
  
            end do !coagulator mode
         end if ! cldfrc .gt. 0.01 
      end do ! i
   end do    ! k

!UPDATE THE TRACERS AND DO DIAGNOSTICS
   do iCoagulator = 1, numberOfCoagulatingModes
      do ispecie = 1, getNumberOfTracersInMode(coagulatingMode(iCoagulator))

         l_index_donor = getTracerIndex(coagulatingMode(iCoagulator) , ispecie ,.true.)

         !so4_a1 is a process mode (condensate), but is still lost in coagulation
         if( .NOT. is_process_mode(l_index_donor, .true.)          &
            .OR. ( (l_index_donor.eq.chemistryIndex(l_so4_a1))  .AND. coagulatingMode(iCoagulator) .eq. MODE_IDX_OMBC_INTMIX_COAT_AIT) ) then       

            l_index_donor = getTracerIndex(coagulatingMode(iCoagulator) , ispecie,.true. )

            !index of mode gaining mass (l_so4_a2, l_om_ac, l_bc_ac), coagulate
            l_index_receiver = CloudAerReceiver(l_index_donor)          
      fldcw => qqcw_get_field(pbuf, CloudAerReceiver(l_index_donor)+im,lchnk,errorhandle=.true.)
            do k=1,pver
               !Loose mass from tracer in donor mode
               q(:ncol,k,l_index_donor)    = q(:ncol,k,l_index_donor) - cloudLoss(:ncol,k,l_index_donor)
               !Give mass to tracer in receiver mode
               if(associated(fldcw)) then
                 fldcw(:ncol,k) = fldcw(:ncol,k) + cloudLoss(:ncol,k,l_index_donor)
               end if 
            end do !k
         endif
     end do
  end do   


   !Output for diagnostics
   if(history_aerosol)then
      coltend(:ncol,:) = 0.0_r8
      do i=1,gas_pcnst 
          !Check if species contributes to coagulation
         if(CloudAerReceiver(i) .gt. 0)then
           !Loss from the donor specie
           tracer_coltend(:ncol) = sum(cloudLoss(:ncol, :,i)*pdel(:ncol,:),2)/gravit*delt_inverse

           coltend(:ncol,i) = coltend(:ncol,i) - tracer_coltend(:ncol) !negative, loss for donor
           coltend(:ncol,CloudAerReceiver(i)) = coltend(:ncol,CloudAerReceiver(i)) + tracer_coltend(:ncol) 
         endif
      end do
      do i=1,gas_pcnst
         if(CloudAerReceiver(i) .gt. 0)then
            long_name= trim(solsym(i))//"clcoagTend"
            call outfld(long_name, coltend(:ncol,i), pcols, lchnk)
            long_name= trim(solsym(CloudAerReceiver(i)))//"_OCWclcoagTend"
            call outfld(long_name, coltend(:ncol,CloudAerReceiver(i)),pcols,lchnk)
         end if
      end do
   endif




end subroutine clcoag

function calculateThermalVelocity(radius, density) result(thermalVelocity)
   implicit none
   real(r8), intent(in) :: radius          ![m]
   real(r8), intent(in) :: density         ![kg/m3]
   real(r8)             :: thermalVelocity ![m/s]

   !Formula for "c1" in Seinfeld & Pandis, table 12.1
   thermalVelocity = sqrt(8.0_r8*kboltzmann*temperatureLookupTables/pi/pi/((4.0_r8/3.0_r8)*density*radius**3))
end function calculateThermalVelocity




function calculateParticleDiffusivity(radius) result (diffusivity)
   implicit none
   real(r8), intent(in) :: radius        ![m] particle radius
   real(r8)             :: knudsenNumber ![-] knudsen number
   real(r8)             :: diffusivity   ![m2/s] diffusivity

   real(r8)             :: factor
   real(r8)             :: numerator, nominator


   !Solve eqn for diffusivity in Seinfeld/Pandis, table 12.1

   knudsenNumber = mfpAir/radius

   factor = (kboltzmann*temperatureLookupTables/3.0_r8/pi/viscosityAir/2.0_r8/radius) 
   numerator = 5.0_r8 + 4.0_r8*knudsenNumber + 6.0_r8*knudsenNumber**2 + 18.0_r8*knudsenNumber**3
   nominator = 5.0_r8 - knudsenNumber + (8.0_r8 + pi)*knudsenNumber**2 

   diffusivity = factor*numerator/nominator
end function calculateParticleDiffusivity




function calculateMeanFreePath(diffusivity,thermalVelocity) result(MeanFreePath)
   implicit none
   real(r8) :: diffusivity     ![m2/s]
   real(r8) :: thermalVelocity ![m/s]
   real(r8) :: meanFreePath    ![m]

   meanFreePath = 8.0_r8*diffusivity/(pi*thermalVelocity)
end function calculateMeanFreePath


function calculateGFactor(radius, meanFreePath) result(g)
   implicit none
   real(r8) :: radius       ![m]
   real(r8) :: meanFreePath ![m]
   real(r8) :: g

   g = ((2.0_r8*radius+meanFreePath)**3     &
         -(4.0_r8*radius**2+meanFreePath**2)**1.5_r8) &
         /(6.0_r8*radius*meanFreePath)                &
         -2.0_r8*radius

end function calculateGFactor

end module koagsub
