module modalapp2d
   public
   save
contains

   subroutine modalapp2d_sub(ncol,Nnatkbg,Ca,f_c,f_bc,f_aq,f_so4_cond,f_soa,Cam,fcm,fbcm,faqm,fso4condm,fsoam)

!     Calculation of the apportionment of internally mixed SO4, BC and OC 
!     mass between the various background mineral and sea-salt modes. Separated 
!     from pmxsub into a independent subroutine by Alf Kirkevåg on September 
!     12'th, 2005, and converted to 2D for use in parmix on September 15'th.
!     Modified for new aerosol schemes by Alf Kirkevaag in January 2006: Now
!     also Aitken-modes are subject to condensation of H2SO4, and both n and
!     Aitken modes may coagulate onto the mineral/sea-salt background aerosol.
!SOA  
!     May 2013: The SO4(Ait) mode now takes into account condensed SOA in addition 
!     to H2SO4, but as long as SOA is not allowed to condense on more than one
!     mode, no changes are necessary here. NB: to allow SOA to condense also on 
!     the BC(Ait) and/or other modes, change this code accordingly! Without any
!     changes, Cam(pcols,1) = condensed SO4 onto the SO4(ait) mode still.  
!SOA
!     Alf Grini, february 2014 : Added info about units, 
!     used values calculated at initialization. 
!     changed in-out variables to components of derived data types (modedefs) 
!     defined in microphysics_oslo.F90, and corrected for mass balance error 
!     for SO4 due to lumping of coagulate and condensate. 


      use ppgrid, only : pcols, pver
      use shr_kind_mod, only: r8 => shr_kind_r8

      use commondefinitions
      use aerosoldef
      use const, only: smallNumber
      use koagsub, only: normalizedCoagulationSink
      use condtend, only: normalizedCondensationSink, COND_VAP_H2SO4, COND_VAP_ORG_SV

      implicit none
!
! Input arguments
!
      integer , intent(in) :: ncol                               ! number of columns used
      real(r8), intent(in) :: Nnatkbg(pcols,pver,nbmodes)        ! aerosol background mode number concentration     #/m3 
      real(r8), intent(in) :: Ca(pcols,pver)                     ! internally mixed mass, tot=SO4+OC+BC   
      real(r8), intent(in) :: f_c(pcols,pver)                    ! mass fraction (OC+BC)/tot
      real(r8), intent(in) :: f_bc(pcols,pver)                   ! mass fraction BC/(OC+BC)
      real(r8), intent(in) :: f_aq(pcols,pver)                   ! mass fraction SO4(aq)/SO4
      real(r8), intent(in) :: f_soa(pcols,pver)                  ! mass fraction SOA/(POM+SOA)
      real(r8), intent(in) :: f_so4_cond(pcols,pver)             ! mass fraction SO4_COND/(COND+COAG)
      !
      ! Output arguments
      !
      real(r8), intent(out) :: Cam(pcols,pver,nbmodes)  ! modal internal mass, tot=SO4+BC+OC 
      real(r8), intent(out) :: fcm(pcols,pver,nbmodes)  ! modal mass fraction (OC+BC)/tot
      real(r8), intent(out) :: fbcm(pcols,pver,nbmodes) ! modal mass fraction BC/(OC+BC)
      real(r8), intent(out) :: faqm(pcols,pver,nbmodes) ! modal mass fraction SO4(aq)/SO4
      real(r8), intent(out) :: fso4condm(pcols,pver,nbmodes) !modal mass fraction (SO4(cond)/SO4(cond+coag))
      real(r8), intent(out) :: fsoam(pcols,pver,nbmodes)! modal mass fraction SOA / (POM+SOA)

      !
      ! Local variables
      real(r8) condensationSinkSO4(pcols,pver,nbmodes)     ![1/s] loss rate of cond. vap on any mode
      real(r8) condensationSinkOA(pcols,pver,nbmodes)        ![1/s] loss rate of cond. vap on any mode
      real(r8) coagulationSink(pcols,pver,nbmodes)      ![1/s] loss rate of BC through coagulation on any mode
      real(r8) aquousPhaseSink(pcols,pver,nbmodes)      ![-] fraction of particles available for aq. phase in any mode

      real(r8) sumCondensationSinkSO4(pcols,pver)          ![1/s] sum condensation sink to all modes
      real(r8) sumCondensationSinkOA(pcols,pver)          ![1/s] sum condensation sink to all modes
      real(r8) sumCoagulationSink(pcols,pver)           ![1/s] sum coagulation sink to all modes
      real(r8) sumAquousPhaseSink(pcols,pver)           ![1/s] sum aquous phase sink to all modes

      real(r8) fcondkSO4(pcols,pver,nbmodes)
      real(r8) fcondkOA(pcols,pver,nbmodes)
      real(r8) fcoagk(pcols,pver,nbmodes)
      real(r8) faqk(pcols,pver,nbmodes) 

      real(r8) cabck(pcols,pver,nbmodes)                ![kg/m3] bc distributed to each mode
      real(r8) caock(pcols,pver,nbmodes)                ![kg/m3] pom coagulate distributed to each mode
      real(r8) csoacondsk(pcols,pver,nbmodes)
      real(r8) caqsk(pcols,pver,nbmodes)                ![kg/m3] aq phase sulfate distributed to each mode
      real(r8) cso4condsk(pcols,pver,nbmodes)           ![kg/m3] non-aq sulfate condensate distributed to each mode
      real(r8) cso4coagsk(pcols,pver,nbmodes)           ![kg/m3] non-aq sulfate coagulate distributed to each mode 
      real(r8) cso4condcoagsk(pcols,pver,nbmodes)      ![kg/m3] non-aq sulfate condensate distributed to each mode
      real(r8) coccondcoagsk(pcols,pver,nbmodes)       ![kg/m3] non-aq sulfate coagulate distributed to each mode 

      integer :: i !counter for modes
      integer :: k !counter for levels

      !Find the sink on any mode (0 is omitted here, WHY??, it does receive matter in koagsub/condtend!!))
      !Should either remove it from there or add something to it here!
      do i=1,nbmodes
         do k=1,pver
            condensationSinkSO4(:ncol,k,i) = normalizedCondensationSink(i,COND_VAP_H2SO4)*Nnatkbg(:ncol,k,i)
            condensationSinkOA(:ncol,k,i) = normalizedCondensationSink(i,COND_VAP_ORG_SV)*Nnatkbg(:ncol,k,i)
            coagulationSink(:ncol,k,i)  = normalizedCoagulationSink(i,MODE_IDX_BC_NUC)*Nnatkbg(:ncol,k,i) !use a typical coagulator (BC_NUC)
            aquousPhaseSink(:ncol,k,i)  = numberFractionAvailableAqChem(i)*Nnatkbg(:ncol,k,i)             !aq phase sink to this mode
         end do
      enddo

      !Sum the sinks
      sumCondensationSinkSO4(:,:) = 0.0_r8
      sumCondensationSinkOA(:,:) = 0.0_r8
      sumCoagulationSink(:,:) = 0.0_r8
      sumAquousPhaseSink(:,:) = 0.0_r8
      do i=1,nbmodes
         do k=1,pver
            sumCondensationSinkSO4(:ncol,k) = sumCondensationSinkSO4(:ncol,k) + condensationSinkSO4(:ncol,k,i)
            sumCondensationSinkOA(:ncol,k) = sumCondensationSinkOA(:ncol,k) + condensationSinkOA(:ncol,k,i)
            sumCoagulationSink(:ncol,k) = sumCoagulationSink(:ncol,k) + coagulationSink(:ncol,k,i)  
            sumAquousPhaseSink(:ncol,k) = sumAquousPhaseSink(:ncol,k) + aquousPhaseSink(:ncol,k,i)   
         end do
      end do

      ! And finally the contribution from each mode relative to the totals are calculated,
      ! assuming that the apportionment of mass for the first iteration (in time) is representative
      ! for the whole apportionment process (which is ok for small and moderate masses added): 
      do i=1,nbmodes
         do k=1,pver
            !Get the fraction of contribution per process per mode
            fcondkSO4(:ncol,k,i)=condensationSinkSO4(:ncol,k,i)/(sumCondensationSinkSO4(:ncol,k)+1.e-100_r8)  !fraction of condensation sink in this mode
            fcondkOA(:ncol,k,i)=condensationSinkOA(:ncol,k,i)/(sumCondensationSinkOA(:ncol,k)+1.e-100_r8)  !fraction of condensation sink in this mode
            fcoagk(:ncol,k,i)=coagulationSink(:ncol,k,i)/(sumCoagulationSink(:ncol,k)+1.e-100_r8)    !fraction of coagulation sink in this mode
            faqk(:ncol,k,i)=aquousPhaseSink(:ncol,k,i)/(sumAquousPhaseSink(:ncol,k)+1.e-100_r8)      !fraction of aquous phase sink in this mode

            !BC coagulate to this mode [kg/m3]
            cabck(:ncol,k,i)=fcoagk(:ncol,k,i)*f_c(:ncol,k)*f_bc(:ncol,k)*Ca(:ncol,k) 

            !OC coagulate to this mode [kg/m3]
            caock(:ncol,k,i)=fcoagk(:ncol,k,i)*f_c(:ncol,k)*(1.0_r8-f_bc(:ncol,k))*(1.0_r8-f_soa(:ncol,k))*Ca(:ncol,k)

            !SOA condensate to this mode [kg/m3]
            csoacondsk(:ncol,k,i) = fcondkOA(:ncol,k,i)*f_c(:ncol,k)*(1.0_r8-f_bc(:ncol,k))*f_soa(:ncol,k)*Ca(:ncol,k)

            !Aquous phase SO4 to this mode [kg/m3]
            caqsk(:ncol,k,i)=faqk(:ncol,k,i)*f_aq(:ncol,k)*(1.0_r8-f_c(:ncol,k))*Ca(:ncol,k)

            !so4 condensate 
            cso4condsk(:ncol,k,i)=fcondkSO4(:ncol,k,i)*(1.0_r8-f_aq(:ncol,k))*f_so4_cond(:ncol,k)*(1.0_r8-f_c(:ncol,k))*Ca(:ncol,k)

            !soa coagulate 
            cso4coagsk(:ncol,k,i) = fcoagk(:ncol,k,i)*(1.0_r8-f_aq(:ncol,k))*(1.0_r8-f_so4_cond(:ncol,k))*(1.0_r8-f_c(:ncol,k))*Ca(:ncol,k) ![kg/m3] so4 coagulate
         end do
      enddo      
   
      !The tables take as input the combined coagulate and condensate (both POM and SOA)
      !The activation needs them separately for mass balance!
      cso4condcoagsk(:ncol,:,:) = cso4condsk(:ncol,:,:) + cso4coagsk(:ncol,:,:)
      coccondcoagsk(:ncol,:,:) =  caock(:ncol,:,:) + csoacondsk(:ncol,:,:) 

      do i=1,nbmodes
         do k=1,pver
            Cam(:ncol,k,i)=  cabck(:ncol,k,i)           &                               !BC
                           + coccondcoagsk(:ncol,k,i)   &                               !OM
                           + caqsk(:ncol,k,i) + cso4condcoagsk(:ncol,k,i)  + smallNumber!SO4 ==>   !total process mode mass to mode i

            fcm(:ncol,k,i)=(cabck(:ncol,k,i)+coccondcoagsk(:ncol,k,i))/(Cam(:ncol,k,i)+smallNumber)       !fraction of mass being carbon (oc or bc)
            fbcm(:ncol,k,i)=cabck(:ncol,k,i)/(cabck(:ncol,k,i)+coccondcoagsk(:ncol,k,i)+smallNumber)      !fraction of carbon mass being bc
            faqm(:ncol,k,i)=caqsk(:ncol,k,i)/(caqsk(:ncol,k,i)+cso4condcoagsk(:ncol,k,i)+smallNumber)     !fraction of sulfate being aq phase

            !Not  needed for tables, but for mass balances in activation
            fso4condm(:ncol,k,i) = cso4condsk(:ncol,k,i)/(cso4condcoagsk(:ncol,k,i) + smallNumber) !fraction of cond+coag which is coag
            fsoam(:ncol,k,i) = csoacondsk(:ncol,k,i)/(coccondcoagsk(:ncol,k,i) + smallNumber) !fraction of OC which is SOA
         end do
      enddo  

      return
end subroutine modalapp2d_sub

end module modalapp2d
