module parmix_progncdnc

   use const, only : volumeToNumber,smallNumber
   use modalapp2d
   use physconst, only: density_water =>rhoh2o, molecularWeightWater=>mwh2o
   use ppgrid, only : pcols, pver
   use shr_kind_mod, only: r8 => shr_kind_r8
   use commondefinitions
   use aerosoldef
   use physconst, only: pi
   use constituents, only: pcnst, cnst_name
   use intlog1to3, only: intlog1to3_sub
   use intlog4, only: intlog4_sub
   use intlog5to10, only: intlog5to10_sub
   use constituents, only: cnst_name

   implicit none
   public
   save

   !Size of molecule-layer which defines when particles are coated
   real(r8), parameter :: coatingLimit = 2.e-9_r8  ![m]
   !The fraction of soluble material required in a components before it
   !will add to any coating
   real(r8), parameter     :: solubleMassFractionCoatingLimit=0.50_r8

   real(r8), parameter :: aThird       = 1.0_r8/3.0_r8 
   real(r8), parameter :: ln10         = log(10.0_r8)

contains

   !Calculate concentrations of aerosol modes based on lifecycle species
   !Create an array of "mode_definition_t" which holds the aerosol concentrations
   subroutine parmix_progncdnc_sub(  &
                  ncol               &        !I [nbr] number of columns used
                  ,mmr               &        !I [kg/kg] mass mixing ratio of tracers
                  ,rho_air           &        !I [kg/m3] air density
                  ,CProcessModes     &
                  ,f_c               &
                  ,f_bc              &
                  ,f_aq              &
                  ,f_so4_cond        &
                  ,f_soa             &
                  ,cam               &
                  ,f_acm             &        !O [frc] carbon fraction in mode
                  ,f_bcm             &        !O [frc] fraction of c being bc
                  ,f_aqm             &        !O [frc] fraction of sulfate being aquous
                  ,f_so4_condm       &        !O [frc] fraction of non-aquous SO4 being condensate
                  ,f_soam            &
                  ,numberConcentration &      !O [#/m3] number concentration
                  ,volumeConcentration &      !O [m3/m3] volume concentration
                  ,hygroscopicity    &        !O [mol/mol]
                  ,lnsigma           &        !O [-] log sigma 
                  ,hasAerosol        &        !I [t/f] do we have this type of aerosol here?
!++ MH_2015/04/10
		  ,volumeCore        &
		  ,volumeCoat        &
!-- MH_2015/04/10
                  )

      implicit none

      !input 
      integer, intent(in)   :: ncol                  !Number of columns used in chunk
      real(r8), intent(in)  :: mmr(pcols,pver,pcnst)
      real(r8), intent(in)  :: rho_air(pcols,pver)

      !output 
      logical, intent(out)  :: hasAerosol(pcols, pver, nmodes)
      real(r8), intent(out) :: f_acm(pcols,pver, nbmodes)
      real(r8), intent(out) :: f_bcm(pcols,pver, nbmodes)
      real(r8), intent(out) :: f_aqm(pcols, pver, nbmodes)
      real(r8), intent(out) :: f_so4_condm(pcols, pver, nbmodes)            !Needed in "get component fraction"
      real(r8), intent(out) :: f_soam(pcols, pver, nbmodes)            !Needed in "get component fraction"
      real(r8), intent(out) :: numberConcentration(pcols,pver,0:nmodes) ![#/m3] number concentraiton
      real(r8), intent(out) :: volumeConcentration(pcols,pver,nmodes)   ![m3/m3] volume concentration
      real(r8), intent(out) :: hygroscopicity(pcols,pver,nmodes)        ![mol_{aer}/mol_{water}] hygroscopicity
      real(r8), intent(out) :: lnsigma(pcols,pver,nmodes)              ![-] log(base e) sigma
      real(r8),intent(out)  :: CProcessModes(pcols,pver)
      real(r8),intent(out)  :: cam(pcols,pver,nbmodes)
      real(r8),intent(out)  :: f_c(pcols, pver)
      real(r8),intent(out)  :: f_aq(pcols,pver)
      real(r8),intent(out)  :: f_bc(pcols,pver)
      real(r8),intent(out)  :: f_so4_cond(pcols,pver)
      real(r8),intent(out)  :: f_soa(pcols,pver)
!++ MH_2015/04/10
      real(r8), intent(out) :: volumeCore(pcols,pver,nmodes)
      real(r8), intent(out) :: volumeCoat(pcols,pver,nmodes)
!-- MH_2015/04/10

      real(r8)              :: f_aitbc(pcols,pver) ! [-] bc fraction in the coated bc-oc mode 
      real(r8)              :: f_nbc(pcols,pver)   ! [-] mass fraction of bc in uncoated bc/oc mode
      real(r8)              :: f_soana(pcols,pver) ! [-] 

      !Get mass, number concentration and the total add-ons (previous convaer)
      call calculateBulkProperties(               &
                             ncol                 &  !I 
                           , mmr                  &  !I
                           , rho_air              &  !I
                           , numberConcentration  &  !O
                           , CProcessModes        &  !O
                           , f_c                  &  !O
                           , f_bc                 &  !O
                           , f_aq                 &  !O
                           , f_so4_cond           &  !O
                           , f_soa                &  !O 
                           , f_aitbc              &  !O
                           , f_nbc                &  !O
                           , f_soana              &  !O
                           )
   
      !Find the points where we have aerosol (number concentration)
      call getAerosolMask(ncol, numberConcentration, hasAerosol)

      !Findn out how much is added per size-mode (modalapp)
      call partitionMass( ncol                &
                           ,numberConcentration &
                           ,CProcessModes  &  !I [kg/m3] total added mass
                           ,f_c            &  !I [frc] fraction of added mass being c
                           ,f_bc           &  !I [frc] fraction of c being bc
                           ,f_aq           &  !I [frc] fraction of SO4 being aq
                           ,f_so4_cond     &  !I [frc] fraction of SO4 coag+cond being cond
                           ,f_soa          &  !I [frc] fraction of OM being SOA
                           ,cam            &  !O [kg/m3] added mass distributed to modes
                           ,f_acm          &  !O [frc] as f_c per mode
                           ,f_bcm          &  !O [frc] as f_bc per mode
                           ,f_aqm          &  !O [frc] as f_aq per mode
                           ,f_so4_condm    &  !O [frc] as f_so4_cond per mode
                           ,f_soam         &  !O [frc]
                           )

      !Calculate they hygroscopicity (previously in cldwat_par.F90)
      call calculateHygroscopicity(  ncol                &
                                    ,mmr                 &
                                    ,numberConcentration &
                                    ,rho_air             &
                                    ,Cam                 &
                                    ,f_acm               &
                                    ,f_bcm               &
                                    ,f_aqm               &
                                    ,hasAerosol          &
                                    ,hygroscopicity      & 
                                    ,volumeConcentration &
!++ MH_2015/04/10
				    ,volumeCore          &
				    ,volumeCoat          &
!-- MH_2015/04/10
                                    )

      !Do the interpolation to new modes
      call doLognormalInterpolation(ncol    &
                                    ,numberConcentration &
                                    ,hasAerosol          &
                                    ,cam                 &
                                    ,volumeConcentration &
                                    ,f_c                 &
                                    ,f_acm               &
                                    ,f_bcm               &
                                    ,f_aqm               &
                                    ,f_aitbc             & !I [frc] bc fraction in int mix bc/oc mode
                                       ,lnSigma             &
                                       )
									   
   end subroutine parmix_progncdnc_sub

   !******************************************************************
   !purpose: Create bulk properties (dependent on tracers, not size modes)
   subroutine calculateBulkProperties(  &
                ncol                   &
               ,qm                     & !I [kg/kg] transported tracers
               ,rho_air                & !I [kg/m3] air density
               ,numberConcentration    & !O [#/m3] aerosol number concentration
               ,CProcessModes          & !O [kg/m3] total added material
               ,f_c                    & !O [-] fraction of aerosol which is carbon 
               ,f_bc                   & !O [-] fraction of carbon which is bc
               ,f_aq                   & !O [-] fraction of sulfate which is aq.
               ,f_so4_cond             & !O [-] fraction of non-aq so4 which is condensate
               ,f_soa                  & !O [-] fraction of OM which is SOA
               ,f_aitbc                & !O [-] fraction of bc in the background tracer mode
               ,f_nbc                  & !O [-] fraction of bc in the background tracer mode 14
               ,f_soana                & !O [-] fraction of soa in background int-mix mode (1)
               )

      use shr_kind_mod, only: r8 => shr_kind_r8
      use aerosoldef
      use oslo_utils, only : calculateNumberConcentration
      use const, only      : smallNumber

      implicit none

      integer, intent(in)  :: ncol                            ! [nbr] number of columns used
		real(r8), intent(in) :: rho_air(pcols,pver)             ! [kg/m3] air density
		real(r8), intent(in) :: qm(pcols,pver,pcnst)            ! [kg/kg] mmr for transported tracers

      real(r8), intent(out) :: numberConcentration(pcols,pver,0:nmodes)     ! [#/m3]
      
      real(r8), intent(out) :: f_c(pcols,pver)        ![-] mass fraction of process mode being c
      real(r8), intent(out) :: f_bc(pcols,pver)       ![-] mass fraction of c being bc
      real(r8), intent(out) :: f_aq(pcols,pver)       ![-] mass fraction of s being aq phase
      real(r8), intent(out) :: f_so4_cond(pcols,pver) ![-] mass fraction of non-aq s being condensate
      real(r8), intent(out) :: f_soa(pcols,pver)      ![-] mass fraction of OM being SOA
      real(r8), intent(out) :: f_aitbc(pcols,pver)    ![-] mass fraction of bc in bc/oc mixed, coated mode
      real(r8), intent(out) :: f_nbc(pcols,pver)      ![-] mass fraction of bc in bc/oc mixed, un-coated mode
      real(r8), intent(out) :: f_soana(pcols,pver)    ![-] mass fraction of soa in background in int mix ait mode (1)
      !Local variables
		real(r8) :: totalProcessModes(pcols,pver)    ! [kg/kg] Int. mixed (cond./coag./aq.) SO4+BC+OC concentration
		real(r8) :: CProcessModes(pcols,pver)        ! [kg/m3] Int. mixed (cond./coag./aq.) SO4+BC+OC concentration

      integer  :: k  !counter for layers

      !Total number concentration per mode
      call calculateNumberConcentration(ncol, qm, rho_air, numberConcentration)

      do k=1,pver

         !Total coagulated bc and oc and SO4 (condensate, wet phase and coagulated) (kg/kg) 
         !internally mixed with background modes
         totalProcessModes(:ncol,k)  = qm(:ncol,k,l_bc_ac) + qm(:ncol,k,l_om_ac) &
               +  qm(:ncol,k,l_so4_a1) + qm(:ncol,k,l_so4_a2) + qm(:ncol,k,l_so4_ac) + qm(:ncol,k,l_soa_a1)

         CProcessModes(:ncol,k) = rho_air(:ncol,k)*totalProcessModes(:ncol,k)  !==> kg/m3

         !fraction of process-mode being carbonaceous
         f_c(:ncol,k)   = min((qm(:ncol,k,l_bc_ac)+qm(:ncol,k,l_om_ac)+qm(:ncol,k,l_soa_a1) )&
                 /(totalProcessModes(:ncol,k)+smallNumber), 1.0_r8)

         !fraction of "c" being bc (total is oc and bc)
         f_bc(:ncol,k)  = min(qm(:ncol,k,l_bc_ac)/(qm(:ncol,k,l_bc_ac)+qm(:ncol,k,l_om_ac)+qm(:ncol,k,l_soa_a1)+smallNumber), 1.0_r8)

         !fraction of non-aqeous phase sulphate being condensate
         f_so4_cond(:ncol,k) = min(qm(:ncol,k,l_so4_a1)/(qm(:ncol,k,l_so4_a1)+qm(:ncol,k,l_so4_ac)+smallNumber), 1.0_r8)

         !fraction of sulphate being aquous phase (total is condensate + aqeous phase + coagulate)
         f_aq(:ncol,k)  = min(qm(:ncol,k,l_so4_a2) &
                        /(qm(:ncol,k,l_so4_a1)+qm(:ncol,k,l_so4_a2)+qm(:ncol,k,l_so4_ac)+smallNumber),1.0_r8)

         !fraction of bc in the sulfate-coated bc/oc mode (total background is bc and oc)
         f_aitbc(:ncol,k) = min(qm(:ncol,k,l_bc_ai) / (qm(:ncol,k,l_bc_ai) + qm(:ncol,k,l_om_ai) + smallNumber), 1.0_r8)

         !fraction of bc in the un-coated bc/oc (total is bc and oc)
         f_nbc(:ncol,k) = min(qm(:ncol,k,l_bc_ni) / (qm(:ncol,k,l_bc_ni) + qm(:ncol,k,l_om_ni) + smallNumber),1.0_r8)

         !fraction of OM process-mode which is SOA
         f_soa(:ncol,k) = min(qm(:ncol,k,l_soa_a1) / (qm(:ncol,k,l_om_ac) + qm(:ncol,k,l_soa_a1) + smallNumber), 1.0_r8)

         !fraction of "background" int-mix (mode 1) which is SOA
         f_soana(:ncol,k) = min(qm(:ncol,k,l_soa_na) / (qm(:ncol,k,l_soa_na) + qm(:ncol,k,l_so4_na) + smallNumber), 1.0_r8 )

     end do !k

   return
   end subroutine calculateBulkProperties

   !********************************************************************************
   subroutine partitionMass(  ncol            &  !I [nbr] number of columns used
                              ,Nnatk          &  !I [#/m3] number concentration
                              ,CProcessModes  &  !I [kg/m3] total added mass
                              ,f_c            &  !I [frc] fraction of added mass being c
                              ,f_bc           &  !I [frc] fraction of c being bc
                              ,f_aq           &  !I [frc] fraction of SO4 being aq
                              ,f_so4_cond     &  !I [frc] fraction of SO4 coag+cond being cond
                              ,f_soa          &  !I [frc] fraction of OM being SOA
                              ,cam            &  !O [kg/m3] added mass distributed to modes
                              ,f_acm          &  !O [frc] as f_c per mode
                              ,f_bcm          &  !O [frc] as f_bc per mode
                              ,f_aqm          &  !O [frc] as f_aq per mode
                              ,f_so4_condm    &  !O [frc] fraction of non aq sulfate being coagulate
                              ,f_soam         &  !O [frc] fraction of OC being SOA
                              )

      implicit none

      integer, intent(in)                     :: ncol
      real(r8), intent(in)                    :: Nnatk(pcols,pver,0:nmodes)
      real(r8), intent(in)                    :: CProcessModes(pcols,pver)
      real(r8), intent(in)                    :: f_c(pcols,pver)
      real(r8), intent(in)                    :: f_bc(pcols,pver)
      real(r8), intent(in)                    :: f_aq(pcols,pver)
      real(r8), intent(in)                    :: f_so4_cond(pcols,pver)
      real(r8), intent(in)                    :: f_soa(pcols,pver)
      real(r8), intent(out)                   :: f_aqm(pcols,pver,nbmodes)
      real(r8), intent(out)                   :: f_acm(pcols,pver,nbmodes)
      real(r8), intent(out)                   :: f_bcm(pcols,pver,nbmodes)
      real(r8), intent(out)                   :: f_so4_condm(pcols,pver,nbmodes)
      real(r8), intent(out)                   :: f_soam(pcols,pver,nbmodes)
      real(r8), intent(out)                   :: cam(pcols, pver, nbmodes)

      !Budget of condensate SO4
      integer                      :: i

      !++test
      integer                      :: k,l,lptr,m,m1,kcomp
      real(r8)                     :: total
      real(r8)                     :: fraction(pcols,pver,pcnst)      !ak: oversized, but only for test use
      !--test
#undef EXTRATESTS


      call modalapp2d_sub(ncol         &
                ,Nnatk(1,1,1)          &  !I [#/m3]  Total number concentration (skip mode 0)
                ,CProcessModes         &  !I [kg/m3] Total process mode mass concentration
                ,f_c                   &  !I [frc] fraction of process mode mass being oc or bc
                ,f_bc                  &  !I [frc] fraction of coagulate mass being bc
                ,f_aq                  &  !I [frc] fraction of process mode sulfate mass being aq
                ,f_so4_cond            &
                ,f_soa                 &
                ,cam                   &  !O [kg/m3] Process mode mass distributed to each mode
                ,f_acm                 &  !O [frc] as f_c, for each mode
                ,f_bcm                 &  !O [frc] as f_bc, for each mode
                ,f_aqm                 &  !O [frc] as f_aq, for each mode
                ,f_so4_condm           &  !O [frc] 
                ,f_soam                &
                )

#ifdef EXTRATESTS
      !++testing
      fraction(:,:,:)=0.0_r8
      do m=1,nbmodes
         do l = 1, getNumberOfTracersInMode(m)
            lptr = getTracerIndex(m,l,.false.)
            do k=1,pver
               do i=1,ncol
                  fraction(i,k,lptr) = fraction(i,k,lptr) &
                           + getConstituentFraction(CProcessModes(i,k), f_c(i,k), f_bc(i,k), f_aq(i,k), f_so4_cond(i,k), f_soa(i,k) &
                           ,Cam(i,k,m), f_acm(i,k,m), f_bcm(i,k,m), f_aqm(i,k,m), f_so4_condm(i,k,m),f_soam(i,k,m), lptr  )
               end do
             end do
         enddo
      enddo

         !testing that the mass fractions summed over all modes and species = 1 (or 0 if not present).
         do m1=1,pcnst
            do k=1, pver
               do i=1,ncol
                  !Check if "fraction" differs from one (accept 0.01 error), only check for concentrations > 1.e-30 kg/m3
                  if((abs(fraction(i,k,m1)-1.0_r8) .gt. 1.e-2) .and. (fraction(i,k,m1).gt.0.0_r8) .and. (CProcessModes(i,k) .gt. 1.e-30_r8) )then
                     if( ( m1 .eq. l_so4_a1 .and. (1.0_r8-f_c(i,k))*(1.0_r8-f_aq(i,k))*f_so4_cond(i,k) .gt. 1.0e-4_r8).or. &
                         ( m1 .eq. l_so4_a2 .and. (1.0_r8-f_c(i,k))*f_aq(i,k) .gt. 1.0e-4_r8).or. &
                         ( m1 .eq. l_so4_ac .and. (1.0_r8-f_c(i,k))*(1.0_r8-f_aq(i,k))*(1.0_r8-f_so4_cond(i,k)) .gt. 1.0e-4_r8).or. &
                         ( m1 .eq. l_bc_ac .and.  f_c(i,k)*f_bc(i,k) .gt. 1.0e-4_r8).or. &
                         ( m1 .eq. l_om_ac .and.  f_c(i,k)*(1.0_r8-f_bc(i,k))*(1.0_r8 - f_soa(i,k)) .gt. 1.0e-4_r8) .or. & 
                         ( m1 .eq. l_soa_a1 .and. f_c(i,k)*(1.0_r8-f_bc(i,k))*f_soa(i,k) .gt. 1.0e-4_r8) &
                         )then

               print*,"  "
               print*,"fraction error ", m1, fraction(i,k,m1), cnst_name(m1)
               print*, "Cprocessmodes", CProcessModes(i,k), f_c(i,k), f_bc(i,k), f_aq(i,k), f_so4_cond(i,k), f_soa(i,k)
               do l=1,nbmodes
                  print*, "mode, cam", l, cam(i,k,l),nnatk(i,k,l)
               enddo
               print*,"ca, sum(cam)", CProcessModes(i,k), sum(cam(i,k,:))
               print*,"sulfate fraction", (1.0_r8 - f_c(i,k))
               print*,"carbon fraction", f_c(i,k)
               print*,"non aq sulf fraction", (1.0_r8 - f_aq(i,k))*(1.0_r8 - f_c(i,k))
               !There is something wrong with tracer lptr
               do m=1,nmodes
                  do l =1,getNumberOfTracersInMode(m)
                     lptr = getTracerIndex(m,l,.false.)
                     if(lptr .eq. m1)then !This is the tracer with problems
                        print*, "lptr, fraction ", m,l,lptr, &
                            getConstituentFraction(CProcessModes(i,k), f_c(i,k), f_bc(i,k), f_aq(i,k), f_so4_cond(i,k), f_soa(i,k) &
                             ,Cam(i,k,m), f_acm(i,k,m), f_bcm(i,k,m), f_aqm(i,k,m), f_so4_condm(i,k,m) , f_soam(i,k,m), lptr,.TRUE.  ) &
                            , NNatk(i,k,m),cam(i,k,m),numberFractionAvailableAqChem(m)

                     endif
                  enddo
               enddo
               do m=1,nbmodes
                  print*,"sulfate / c, aq ", m, (1.0_r8-f_acm(i,k,m)),  f_acm(i,k,m)& 
                        ,f_aqm(i,k,m), f_so4_condm(i,k,m), f_so4_condm(i,k,m), f_soam(i,k,m)
               enddo

               stop !stop on error
               endif !if tracer has error
            endif    !if budget is wrong
               enddo      
            enddo 
         enddo


         !Check total carbon
         do k=1,pver
            do i=1,ncol
               total=0.0_r8
               do kcomp=1,nbmodes
                  total = total + cam(i,k,kcomp)*f_acm(i,k,kcomp)
               enddo
               if( ABS(total - CProcessModes(i,k)*f_c(i,k)) .gt. 1.e-2_r8*CProcessModes(i,k) )then
                  if(abs(total) > 1.e-25)then
                     print*,"CProcessModes", CProcessModes(i,k), total, abs(total - CProcessModes(i,k)*f_c(i,k))
                     do kcomp=1,nbmodes
                        print*,"fcm,cam,fc,ctot", f_acm(i,k,kcomp), cam(i,k,kcomp), f_c(i,k), CProcessModes(i,k)
                     enddo
                     stop
                  endif
               endif
             end do
          end do

   !--testing
#endif
      !EXTRATESTS

   end subroutine partitionMass

   !*************************************************************
   !Find out where we have aerosols
   subroutine getAerosolMask(ncol,numberConcentration, hasAerosol)
      implicit none

      integer, intent(in)   :: ncol         !number of columns used
      real(r8), intent(in)  :: numberConcentration(pcols, pver, 0:nmodes)
      logical, intent(out)  :: hasAerosol(pcols, pver, nmodes)
      integer               :: k !counter for levels
      integer               :: m !counter for modes

      do m=1,nmodes
         do k=1,pver
            where(numberConcentration(:ncol,k,m) .gt. smallNumber)
               hasAerosol(:ncol,k,m)= .true.
            elsewhere
               hasAerosol(:ncol,k,m) = .false.
            end where
         end do  !levels
      end do     !modes
   end subroutine
   !*************************************************************


   !**************************************************************
   subroutine calculateHygroscopicity(  ncol                &
                                       ,mmr                 &
                                       ,numberConcentration &
                                       ,rho_air             &
                                       ,Cam                 &
                                       ,f_acm               &
                                       ,f_bcm               &
                                       ,f_aqm               &
                                       ,hasAerosol          &
                                       ,hygroscopicity      &
                                       ,volumeConcentration &
!++ MH_2015/04/10
				       ,volumeCore          &
				       ,volumeCoat          &
!-- MH_2015/04/10
                                       )

      !All theory in this subroutine is from 
      !Abdul-Razzak and S. Ghan: 
      !A parameterization of aerosol activation 2. Multiple aerosol types, JGR, vol 105, noD5, pp 6837
      !http://onlinelibrary.wiley.com/doi/10.1029/1999JD901161/abstract
      implicit none

      !INPUT
      integer, intent(in)     :: ncol
      real(r8), intent(in)    :: mmr(pcols,pver,pcnst)                   !I [kg/kg] mass mixing ratios
      real(r8), intent(in)    :: numberConcentration(pcols,pver,0:nmodes)!I [#/m3] number concentrations
      real(r8), intent(in)    :: rho_air(pcols,pver)                     !I [kg/m3] air density
      real(r8), intent(in)    :: Cam(pcols, pver, nbmodes)               !I [kg/m3] total added mass during microphysics
      real(r8), intent(in)    :: f_acm(pcols,pver,nbmodes)               !I [-] fraction of added mass which is carbon
      real(r8), intent(in)    :: f_aqm(pcols,pver,nbmodes)               !I [-] fraction of sulfate which is aq. phase
      real(r8), intent(in)    :: f_bcm(pcols,pver,nbmodes)               !I [-] fraction of C which is bc
      logical, intent(in)     :: hasAerosol(pcols,pver,nmodes)          !I [t/f] do we have aerosols

      !OUTPUT
      real(r8), intent(out)   :: hygroscopicity(pcols,pver,nmodes)
      real(r8), intent(out)   :: volumeConcentration(pcols,pver,nmodes)

      !Local variables
      real(r8)                :: hygroscopicityAvg(pcols,pver)
      real(r8)                :: hygroscopicityCoat(pcols,pver)
      real(r8)                :: massConcentrationTracerInMode(pcols,pver)
	  !++ MH_2015/04/10
      real(r8), intent(out)   :: volumeCore(pcols,pver,nmodes)      ![m3]
      real(r8), intent(out)   :: volumeCoat(pcols,pver,nmodes)      ![m3]
	  !-- MH_2015/04/10
      real(r8)                :: averageRadiusCore(pcols,pver)      ![m]
      real(r8)                :: averageRadiusTotal(pcols,pver)     ![m]
      integer                 :: kcomp !counter for modes
      integer                 :: l     !counter for components
      integer                 :: tracerIndex

      integer                 :: k !counter for levels

      integer                 :: i


      !initialize
      hygroscopicity(:,:,:) = 0.0_r8
      volumeConcentration(:,:,:)=0.0_r8

      do kcomp=1,nmodes

         !Don't do anything if no tracers in mode
         if(getNumberOfBackgroundTracersInMode(kcomp) .lt. 1)then
            volumeCore(:,:,kcomp)=smallNumber
            volumeCoat(:,:,kcomp)=smallNumber
            volumeConcentration(:,:,kcomp)=smallNumber
            hygroscopicity(:,:,kcomp) = smallNumber
            cycle
         end if

         hygroscopicityAvg(:,:) = 0.0_r8
         hygroscopicityCoat(:,:) = 0.0_r8
         volumeCore(:,:,kcomp) = 0.0_r8
         volumeCoat(:,:,kcomp) = 0.0_r8

         !Loop over tracers in mode
         do l=1,getNumberOfBackgroundTracersInMode(kcomp)
      
            tracerIndex = getTracerIndex(kcomp,l,.false.) !get index in physcis space

            do k=1,pver
               massConcentrationTracerInMode(:ncol,k) = mmr(:ncol,k,tracerIndex)*rho_air(:ncol,k)
            end do

            call addModeHygroscopicity(   ncol                          &
                                        , hasAerosol(:,:,kcomp)         &  !true if any concentration in this point
                                        , massConcentrationTracerInMode &
                                        , volumeCore(:,:,kcomp)         &
                                        , volumeCoat(:,:,kcomp)         &
                                        , hygroscopicityAvg             &
                                        , hygroscopicityCoat            &
                                        , tracerIndex                   &
                                         ) 
         end do !background tracers in mode (l)

         !The background modes can have tracer mass added to them
         if(kcomp .le. nbmodes)then

            !added aquous sulfate
            if(isTracerInMode(kcomp,l_so4_a2))then
            
               do k=1,pver
                  massConcentrationTracerInMode(:ncol,k) = Cam(:ncol,k,kcomp)*(1.0_r8 - f_acm(:ncol,k,kcomp))*f_aqm(:ncol,k,kcomp)
               end do

               call addModeHygroscopicity(   ncol                          &
                                           , hasAerosol(:,:,kcomp)         &  !true if any concentration in this point
                                           , massConcentrationTracerInMode &
                                           , volumeCore(:,:,kcomp)         &
                                           , volumeCoat(:,:,kcomp)         &
                                           , hygroscopicityAvg             &
                                           , hygroscopicityCoat            &
                                           , l_so4_a2                      &
                                            ) 
            endif

            !added condensate/coagulate
            !All modes which have coagulate have also condensate, so it is 
            !ok to check for condensate and add the combined mass..
            if(isTracerInMode(kcomp,l_so4_a1))then

               do k=1,pver
                  massConcentrationTracerInMode(:ncol,k) = Cam(:ncol,k,kcomp)*(1.0_r8 - f_acm(:ncol,k,kcomp))*(1.0_r8 - f_aqm(:ncol,k,kcomp))
               end do

               call addModeHygroscopicity(   ncol                          &
                                           , hasAerosol(:,:,kcomp)         &  !true if any concentration in this point
                                           , massConcentrationTracerInMode &
                                           , volumeCore(:,:,kcomp)         &
                                           , volumeCoat(:,:,kcomp)         &
                                           , hygroscopicityAvg             &
                                           , hygroscopicityCoat            &
                                           , l_so4_a1                      &
                                            ) 
            endif
            !Added bc
            if(isTracerInMode(kcomp,l_bc_ac))then

               do k=1,pver
                  massConcentrationTracerInMode(:ncol,k) = Cam(:ncol,k,kcomp)*f_acm(:ncol,k,kcomp)*f_bcm(:ncol,k,kcomp)
               end do

               call addModeHygroscopicity( ncol                            &
                                           , hasAerosol(:,:,kcomp)         &  !true if any concentration in this point
                                           , massConcentrationTracerInMode &
                                           , volumeCore(:,:,kcomp)         &
                                           , volumeCoat(:,:,kcomp)         &
                                           , hygroscopicityAvg             &
                                           , hygroscopicityCoat            &
                                           , l_bc_ac                       &
                                            ) 
            endif

            !Added oc (both POM and SOA), then both have the same
            !properties, so add combined mass here.
            !All modes which have condensate also has coagulate, so OK to check
            !for condensate and distribute the sum..
            if(isTracerInMode(kcomp,l_soa_a1))then

               do k=1,pver
                  massConcentrationTracerInMode(:ncol,k) = Cam(:ncol,k,kcomp)*f_acm(:ncol,k,kcomp)*(1.0_r8 -f_bcm(:ncol,k,kcomp))
               end do

               call addModeHygroscopicity( ncol                            &
                                           , hasAerosol(:,:,kcomp)         &  !true if any concentration in this point
                                           , massConcentrationTracerInMode &
                                           , volumeCore(:,:,kcomp)         &
                                           , volumeCoat(:,:,kcomp)         &
                                           , hygroscopicityAvg             &
                                           , hygroscopicityCoat            &
                                           , l_om_ac                       &
                                            ) 
               endif
         end if

         !Note: NCAR definitions of molecular weights are kg/kmol. This is used
         !inside "addModeHygroscopicity" and here as in molecularWeightWater. SI units are kg/mol, but
         !the error cancels out since eqn 4 has Mw_water/Mw_tracer

         do k=1,pver

            !Finally, when the sums are calculated, Apply finally eqn 4 here!!

            where (hasAerosol(:ncol,k,kcomp))
               where(VolumeCoat(:ncol,k,kcomp) .gt. 1.e-30_r8)
                  !If there is enough soluble material, a coating will be formed: In that case, the 
                  !volume of the aerosol in question is only the volume of the coating!
                  hygroscopicityCoat(:ncol,k) = molecularWeightWater*hygroscopicityCoat(:ncol,k) &
                                                & /( density_water * volumeCoat(:ncol,k,kcomp)) !Note use of volume Coating here
               elsewhere
                  hygroscopicityCoat(:ncol,k) = 1.e-30_r8
               endwhere 
               !mode total volume:
               volumeConcentration(:ncol,k,kcomp) = volumeCore(:ncol,k,kcomp) + volumeCoat(:ncol,k,kcomp)

               !hygroscopicity of mixture (Note use of total volume to get average hygroscopicity)
               hygroscopicityAvg(:ncol,k) = molecularWeightWater*hygroscopicityAvg(:ncol,k) &
                                             & /(density_water * volumeConcentration(:ncol,k,kcomp))


               !Average size of insoluble core (average radius) 
               averageRadiusCore(:ncol,k) = 0.5_r8*( (volumeCore(:ncol,k,kcomp)) / numberConcentration(:ncol,k,kcomp) * (6.0_r8/pi))**athird 
    
               !Average size of total aerosol (average radius)
               averageRadiusTotal(:ncol,k) = 0.5_r8*((volumeConcentration(:ncol,k,kcomp)) / numberConcentration(:ncol,k,kcomp)*(6.0_r8/pi))**athird

               !do i=1,ncol
               !   if(numberConcentration(i,k,kcomp) .gt. 1.e6 .and. kcomp.eq.6 )then
               !      print*, "hygro_check",kcomp,numberConcentration(i,k,kcomp), averageRadiusTotal(i,k)*1.e6, averageRadiusCore(i,k)*1.e6 & 
               !               , hygroscopicityCoat(i,k), hygroscopicityAvg(i,k), (averageRadiusTotal(i,k)-averageRadiusCore(i,k))*1.e9
               !   endif
               !end do

               !use one or the other hygroscopicity based on coating
               where ( averageRadiusTotal(:ncol,k) - averageRadiusCore(:ncol,k)  .gt. coatingLimit )
                  hygroscopicity(:ncol,k,kcomp) = hygroscopicityCoat(:ncol,k)
               elsewhere
                  hygroscopicity(:ncol,k,kcomp) = hygroscopicityAvg(:ncol,k)
               endwhere
            elsewhere ! No aerosol
               hygroscopicity(:ncol,k,kcomp) = 1.e-10_r8
            end where

         end do !levels 

      end do !kcomp /modes

   end subroutine calculateHygroscopicity

   !**************************************************************************************
   subroutine addModeHygroscopicity ( ncol                            & ![nbr] number of columns used
                                      , hasAerosol                    & ![bool] do we have any aerosol here?
                                      , massConcentrationTracerInMode & ![kg/m3] mass concentration of aerosol in a mode
                                      , volumeCore                    & ![m3/m3] volume concentration we are adding
                                      , volumeCoat                    & ![m3/m3] volume concentration we are adding
                                      , hygroscopicityAvg             & ![mol_{aerosol}/mol_{tracer} hygroscopicity
                                      , hygroscopicityCoat            & ![mol_{aerosol}/mol_{tracer} hygroscopicity coating
                                      , tracerIndex                   & ![idx] which tracer are we talking about (physics space)
                                      )
   
      implicit none

      integer, intent(in)     :: ncol
      real(r8), intent(in)    :: massConcentrationTracerInMode(pcols,pver) ![kg/m3] mass concentration in
      logical, intent(in)     :: hasAerosol(pcols,pver)                    ![bool] true if we have any aerosol here
      integer, intent(in)     :: tracerIndex                               !in physics space

      real(r8), intent(inout) :: volumeCore(pcols, pver)                  !O [m3/m3] volume of insoluble core
      real(r8), intent(inout) :: volumeCoat(pcols, pver)                  !O [m3/m3] volume of total aerosol
      real(r8), intent(inout) :: hygroscopicityAvg(pcols, pver)           !O [-] average hygroscopicity 
      real(r8), intent(inout) :: hygroscopicityCoat(pcols, pver)          !O [-] average hygroscopicity 

      real(r8)                :: massFractionInCoating

      integer                 :: k                                        !counter for levels

      !Only tracers more soluble than 20% can add to the coating volume
      if(solubleMassFraction(tracerIndex) .gt. solubleMassFractionCoatingLimit)then
         massFractionInCoating = 1.0_r8 !all volume goes to coating
      else
         massFractionInCoating = 0.0_r8 !zero volume goes to coating
      endif

      do k=1,pver

         where(hasAerosol(:ncol,k) .eqv. .true.)

            volumeCore(:ncol,k) = volumeCore(:ncol,k) &
                     + massConcentrationTracerInMode(:ncol,k)/rhopart(tracerIndex)*(1.0_r8 - massFractionInCoating)

            volumeCoat(:ncol,k) = volumeCoat(:ncol,k) + massConcentrationTracerInMode(:ncol,k)/rhopart(tracerIndex)*massFractionInCoating

            !sum up numerator in eqn 4 in Abdul-Razzak et al (average hygrocopicity)
            !Note that molecular weight is that of the AEROSOL TYPE
            !This is because of some conflict with mozart which needs molecular weight of OC tracers to be 12 when reading emissions
            !So molecular weight is duplicated, and the molecular weight of the TYPE is used here!
            hygroscopicityAvg(:ncol,k) = hygroscopicityAvg(:ncol,k) +  &
               massConcentrationTracerInMode(:ncol,k)*numberOfIons(tracerIndex)*osmoticCoefficient(tracerIndex) &
            *solubleMassFraction(tracerIndex)/aerosol_type_molecular_weight(aerosolType(tracerIndex)) 

            !Contribution to hygroscopicity of coating (only if goes to coating)
            !sum up numerator in eqn 4 in Abdul-Razzak et al (average hygrocopicity)
            !Note that molecular weight is that of the AEROSOL TYPE
            !This is because of some conflict with mozart which needs molecular weight of OC tracers to be 12 when reading emissions
            !So molecular weight is duplicated, and the molecular weight of the TYPE is used here!
            hygroscopicityCoat(:ncol,k) = hygroscopicityCoat(:ncol,k) +  &
               massConcentrationTracerInMode(:ncol,k)*numberOfIons(tracerIndex)*osmoticCoefficient(tracerIndex) &
            *solubleMassFraction(tracerIndex)/aerosol_type_molecular_weight(aerosolType(tracerIndex))           &
            *massFractionInCoating !Only add to this if mass goes to coating

         elsewhere
            hygroscopicityAvg(:ncol,k) = 1.0e-10_r8
            hygroscopicityCoat(:ncol,k)= 1.0e-10_r8
         end where
   
      end do

   end subroutine addModeHygroscopicity

   !****************************************************************

   subroutine doLognormalInterpolation(ncol                 &
                                       ,numberConcentration  &
                                       ,hasAerosol          &
                                       ,cam                 &
                                       ,volumeConcentration &
                                       ,f_c                 &
                                       ,f_acm             &
                                       ,f_bcm             &
                                       ,f_aqm             &
                                       ,f_aitbc           &
                                       ,lnSigma           &
                                       )
                                                                  
      implicit none

      !input
      integer, intent(in)     :: ncol
      real(r8), intent(in)    :: volumeConcentration(pcols,pver,nmodes)
      logical, intent(in)     :: hasAerosol(pcols,pver,nmodes)
      real(r8), intent(in)    :: cam(pcols,pver,nbmodes)   ![kg/m3] total added mass per mode
      real(r8), intent(in)    :: f_c(pcols,pver)           ![frc] fraction of carbon in total add-on
      real(r8), intent(in)    :: f_acm(pcols,pver,nbmodes) ![frc] fraction of carbon per mode (in add-on)
      real(r8), intent(in)    :: f_bcm(pcols,pver,nbmodes) ![frc] fraction of bc in carbon per mode
      real(r8), intent(in)    :: f_aqm(pcols,pver,nbmodes) ![frc] fraction of aq in sulfate added
      real(r8), intent(in)    :: f_aitbc(pcols,pver)       ![frc] fraction of bc in coated bc/oc mode

      !output
      real(r8), intent(inout) :: numberConcentration(pcols,pver,0:nmodes) ![#/m3] number concentration
      real(r8), intent(out)   :: lnsigma(pcols,pver,nmodes)    ![-] log (base e) of std. dev

      
      !work arrays
      real(r8)                :: nconccm3(pcols,pver)
      real(r8)                :: camUg(pcols,pver)
      real(r8)                :: log10sig(pcols,pver)        ![-] logarithm (base 10) of look up tables
      real(r8), dimension(pcols,pver,nbmodes)  :: cxs        ![ug/m3] NOTE NON-SI UNITS non-allocated mass
      !real(r8), dimension(pcols,pver)         :: cxstot     ![kg/m3] non allocated mass
      integer, dimension(pcols)                :: ind        ![idx] index in mapping (not really used)
      real(r8), dimension(pcols,pver)          :: radius_tmp ![m] radius in look up tables
      real(r8) :: f_ocm(pcols,pver,4) ! [-] fraction of added mass which is either SOA condensate or OC coagulate
      integer                               :: iloop
      integer                               :: kcomp
      integer                               :: i
      integer                               :: k
 

      !total mass not allocated to any mode
      !this is non-zero if the look-up table can not cope with all the add-on mass
      !cxstot(:,:) = 0.0_r8
  
      !Remove this later!
      do i=1,ncol
         ind(i)=i
      end do

!    calculate fraction of added mass which is either SOA condensate or OC coagulate,
!    which in AeroTab are both treated as condensate for kcomp=1-4
      do kcomp=1,4
        do k=1,pver
          do i=1,ncol
            f_ocm(i,k,kcomp) = f_acm(i,k,kcomp)*(1.0_r8-f_bcm(i,k,kcomp))
          enddo
        enddo
      enddo

 do iloop=1,1   !  loop over i>1 for testing CPU use in intlog*  

      !Go through all "background" size-modes (kcomp=1-10)
      do kcomp=1,nbmodes

         camUg(:,:) = cam(:,:,kcomp)*1.e9_r8
         nConccm3(:,:) = 1e-6_r8*numberConcentration(:,:,kcomp)

         !Calculate growth from knowing added process specific internally mixed mass to each background mode
         !(level sent but not needed, and kcomp not needed for intlog4_sub)

         if( kcomp .ge. MODE_IDX_SO4SOA_AIT .and. kcomp .le. MODE_IDX_BC_AIT)then       ! kcomp=1,2

            do k=1,pver
               call intlog1to3_sub(                            &
                                   ncol                        &                 !I number of points
                                 , ind                         &                 !I [idx] mappoing of points to use 
                                 , kcomp                       &                 !I [idx] mode index
                                 , camUg(:,k)                  &                 !I [ug/m3] mass concentration
                                 , nConccm3(:,k)               &     !I [#/cm3] number concentration
                                 , f_ocm(:,k,kcomp)            &                 !I [frc] mass fraction which is SOA cond. or OC coag.
                                 , cxs(:,k,kcomp)              &                 !O [ug/m3] mass which did not fit the table
                                 , log10sig(:,k)               &                 !O [-]sigma, is later thrown away begause of volume balance
                                 , radius_tmp(:,k)             &                 !O [m] Number median radius                  
                              )

            end do  !loop on levels

         else if(kcomp .eq. MODE_IDX_OMBC_INTMIX_COAT_AIT)then                       ! kcomp=4

            do k=1,pver
               call intlog4_sub(                               &
                                ncol                           &                 !I number of points
                              , ind                            &                 !I [idx] mappoing of points to use
                              , kcomp                          &                 !I [idx] mode index
                              , camUg(:,k)                     &                 !I [ug/m3] mass concentration
                              , nConccm3(:,k)                  &        !I [#/cm3] number concentration
                              , f_ocm(:,k,kcomp)               &                 !I [frc] mass fraction which is SOA cond. or OC coag.
                              , f_aqm(:,k,kcomp)               &                 !I [frc] fraction of sulfate which is aquous
                              , cxs(:,k,kcomp)                 &                 !O [ug/m3] mass which did not fit the table
                              , log10sig(:,k)                  &                 !O [-]sigma, is later thrown away begause of volume balance
                              , radius_tmp(:,k)                &                 !O [m] Number median radius 
                             )
            end do

         else if (kcomp .ge. MODE_IDX_SO4_AC .and. kcomp .le. MODE_IDX_SS_A3)then    ! kcomp=5-10

            do k=1,pver
               call intlog5to10_sub(                           &
                                    ncol                       &                 !I [nbr] number of points used
                                  , ind                        &                 !I [mapping] (not used)
                                  , kcomp                      &                 !I [mode index]
                                  , camUg(:,k)                 &                 !I [ug/m3] mass concentration
                                  , nConccm3(:,k)              &    !I [#/cm3] number concentration
                                  , f_acm(:,k,kcomp)           &                 !I [frc] fraction of aerosol which is carbon
                                  , f_bcm(:,k,kcomp)           &                 !I [frc] fraction of carbon which is bc  
                                  , f_aqm(:,k,kcomp)           &                 !I [frc] fraction of sulfate which is aquous 
                                  , cxs(:,k,kcomp)             &                 !O [ug/m3] mass which did not fit the table (not given to any mode)
                                  , log10sig(:,k)              &                 !O logarithm (base 10) sigma, is later thrown away begause of volume balance
                                  , radius_tmp(:,k)            &                 !O [m] Number median radius 
                                )
           end do ! k

         endif

         !initialize
         lnsigma(:,:,kcomp) = log(2.0_r8)

         !The whole point of the interpolation routines is to get the new sigma ==> so trust the sigma

         !This means that in order to conserve the volume (which is known), we have to throw away 
         !the number concentration. Should create a diagnostic or a warning if number concenration is very different
         !from the original number concentration since in principal, the number concentration is 
         !also conserved!
         do k=1,pver
            !Don't change number concentration unless "hasAerosol" is true
            where(hasAerosol(:ncol,k,kcomp))

               lnsigma(:ncol,k,kcomp) = ln10*log10sig(:ncol,k)

               numberConcentration(:ncol,k,kcomp) = volumeConcentration(:ncol,k,kcomp)*6.0_r8/pi      &
                                             /(2.0_r8*radius_tmp(:ncol,k))**3  &
                                             *DEXP(-4.5_r8*lnsigma(:ncol,k,kcomp)*lnsigma(:ncol,k,kcomp))

               !==> Now we have a set of n, vol, sigma which is consistent and gives back whatever the 
               !lookup tables told us! If the look up tables were conserving volume we didn't have to do 
               !the step just above!!

               !Sum up all mass which was not added to any mode (mass exceeding the max limit in the look-up tables)
               !cxstot(:ncol,k) = cxstot(:ncol,k) + cxs(:ncol,k,kcomp)*1.e-9_r8 ! ug/m3 ==> kg/m3
     
            end where 
         end do

      end do !kcomp

      !The modes which do not have any added aerosol:
      do kcomp=nbmodes+1,nmodes
         do k=1,pver
            lnsigma(:ncol,k,kcomp) = log(originalSigma(kcomp))
         end do
      end do

      !AK (fxm): "unactivated" code below...
      !Excessive internally mixed process mass added to the background modes (exceeding the max limit in the look-up tables) 
                !is instead added to / lumped with the externally mixed non-background modes (kcomp=11,12,14)
      !numberConcentration(:,:,MODE_IDX_SO4_NUC) = numberConcentration(:,:,MODE_IDX_SO4_NUC) &
      !                                    + (volumeToNumber(MODE_IDX_SO4_NUC) &          !excess sulfate mass is moved to this mode  
      !                                     *RESHAPE(cxstot,(/pcols,pver/)) &
      !                                     *(1.0_r8-f_c(:,:))/rhopart(l_so4_n))
      
      !numberConcentration(:,:,MODE_IDX_BC_NUC) = numberConcentration(:,:,MODE_IDX_BC_NUC) &
      !                                    + (volumeToNumber(MODE_IDX_BC_NUC)  &          !excess carbon mass is moved to this mode  
      !                                    * RESHAPE(cxstot,(/pcols,pver/)) &
      !                                    * f_c(:,:)/rhopart(l_bc_n))
      
      !SKIP LUMPING OF OC-MODE TO MODE MODE_IDX_LUMPED ORGANICS SINCE THIS WILL MESS UP THE HASAEROSOL-MASK!
      !   modedefs(i)%Nnatk(MODE_IDX_LUMPED_ORGANICS) = efact_omn &   !excess OM mass is moved to this mode (originally kcomp=13) 
      !            * (modedefs(i)%Nnatk(MODE_IDX_LUMPED_ORGANICS) + cxstot(i)*modedefs(i)%f_c*(1.0_r8-modedefs(i)%f_bc))


 enddo ! iloop


   end subroutine doLognormalInterpolation

end module parmix_progncdnc
