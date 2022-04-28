module pmxsub_mod

#include <preprocessorDefinitions.h>

!===============================================================================
contains
!===============================================================================

subroutine pmxsub(lchnk, ncol, pint, pmid, coszrs, state, t, cld, qm1, Nnatk, &
                  per_tau, per_tau_w, per_tau_w_g, per_tau_w_f, per_lw_abs, & 
                  volc_ext_sun, volc_omega_sun, volc_g_sun, &
                  volc_ext_earth, volc_omega_earth, & 
#ifdef AEROCOM
                  aodvis, absvis, dod440, dod550, dod870, abs550, abs550alt)
#else
                  aodvis, absvis)
#endif

! Optical parameters for a composite aerosol is calculated by interpolation  
! from the tables kcomp1.out-kcomp14.out.
! Optimized June 2002 byrild Burud/NoSerC 
! Optimized July 2002 by Egil Storen/NoSerC (ces)
! Revised for inclusion of OC and modified aerosol backgeound aerosol 
! by Alf Kirkevaag in 2003, and finally rewritten for CAM3 February 2005.
! Modified for new aerosol schemes by Alf Kirkevaag in January 2006.
! Updated by Alf Kirkevåg, May 2013: The SO4(Ait) mode now takes into
! account condensed SOA in addition to H2SO4.
! Updated for CAM5-Oslo with RRTMG by Alf Kirkevåg, 2014-2015, for new
! SOA treatment August/September 2015, and for cleanig up and optimizing
! the code around interpolations in November 2016.

   use ppgrid
   use shr_kind_mod, only: r8 => shr_kind_r8
   use cam_history,  only: outfld
   use constituents, only: pcnst
   use physconst,    only: rair,pi
   use opttab
   use oslo_utils, only: calculateNumberConcentration
   use parmix_progncdnc, only: calculateBulkProperties, partitionMass
   use opttab_lw
   use const
   use aerosoldef
   use commondefinitions
   use optinterpol,   only: interpol0,interpol1,interpol2to3,interpol4,interpol5to10
   use physics_types, only: physics_state
   use wv_saturation, only: qsat_water

   implicit none

!
! Input arguments

   integer, intent(in) :: lchnk                   ! chunk identifier
   integer, intent(in) :: ncol                    ! number of atmospheric columns
   real(r8), intent(in) :: coszrs(pcols)          ! Cosine solar zenith angle
   real(r8), intent(in) :: pint(pcols,pverp)      ! Model interface pressures (10*Pa)
   real(r8), intent(in) :: pmid(pcols,pver)       ! Model level pressures (Pa)
   real(r8), intent(in) :: t(pcols,pver)          ! Model level temperatures (K)
   real(r8), intent(in) :: cld(pcols,pver)        ! cloud fraction
   real(r8), intent(in) :: qm1(pcols,pver,pcnst)  ! Specific humidity and tracers (kg/kg)
   real(r8), intent(in) :: volc_ext_sun(pcols,pver,nbands) ! volcanic aerosol extinction for solar bands, CMIP6
   real(r8), intent(in) :: volc_omega_sun(pcols,pver,nbands) ! volcanic aerosol SSA for solar bands, CMIP6
   real(r8), intent(in) :: volc_g_sun(pcols,pver,nbands) ! volcanic aerosol g for solar bands, CMIP6
   real(r8), intent(in) :: volc_ext_earth(pcols,pver,nlwbands) ! volcanic aerosol extinction for terrestrial bands, CMIP6
   real(r8), intent(in) :: volc_omega_earth(pcols,pver,nlwbands) ! volcanic aerosol SSA for terrestrial bands, CMIP6
!  real(r8) batotsw13(pcols,pver), batotlw01(pcols,pver)  ! for testing bare
!
! Input-output arguments

   real(r8), intent(inout) :: Nnatk(pcols,pver,0:nmodes)! aerosol mode number concentration  

! Output arguments
!
   real(r8), intent(out) :: per_tau    (pcols,0:pver,nbands) ! aerosol extinction optical depth
   real(r8), intent(out) :: per_tau_w  (pcols,0:pver,nbands) ! aerosol single scattering albedo * tau
   real(r8), intent(out) :: per_tau_w_g(pcols,0:pver,nbands) ! aerosol assymetry parameter * w * tau
   real(r8), intent(out) :: per_tau_w_f(pcols,0:pver,nbands) ! aerosol forward scattered fraction * w * tau
   real(r8), intent(out) :: per_lw_abs (pcols,pver,nlwbands) ! aerosol absorption optical depth (LW)
!  AOD and absorptive AOD for visible wavelength closest to 0.55 um (0.442-0.625)
!  Note that aodvis and absvis output should be devided by dayfoc to give physical (A)AOD values  
   real(r8), intent(out) :: aodvis(pcols)             ! AOD vis
   real(r8), intent(out) :: absvis(pcols)             ! AAOD vis

!
!---------------------------Local variables-----------------------------
!
   integer  i, k, ib, icol, mplus10
   integer iloop
   logical  daylight(pcols)        ! SW calculations also at (polar) night in interpol* if daylight=.true. 

   real(r8) aodvisvolc(pcols)      ! AOD vis for CMIP6 volcanic aerosol
   real(r8) absvisvolc(pcols)      ! AAOD vis for CMIP6 volcanic aerosol
!akc6+
   real(r8) bevisvolc(pcols,pver)  ! Extinction in vis wavelength band for CMIP6 volcanic aerosol
!akc6-
   real(r8) rhum(pcols,pver)       ! (trimmed) relative humidity for the aerosol calculations
!tst
!   real(r8) aodvis3d(pcols,pver)  ! 3D AOD in VIS
!tst

   real(r8) deltah_km(pcols,pver)  ! Layer thickness, unit km

!akc6   real(r8) deltah, airmass(pcols,pver) 
   real(r8) deltah, airmassl(pcols,pver), airmass(pcols) !akc6
   real(r8) Ca(pcols,pver), f_c(pcols,pver), f_bc(pcols,pver), f_aq(pcols,pver)
   real(r8) fnbc(pcols,pver), faitbc(pcols,pver), f_so4_cond(pcols,pver), &
            f_soa(pcols,pver),f_soana(pcols,pver), vnbc, vaitbc
   real(r8) v_soana(pcols,pver), vnbcarr(pcols,pver), vaitbcarr(pcols,pver)
   real(r8) dCtot(pcols,pver), Ctot(pcols,pver)
   real(r8) Cam(pcols,pver,nbmodes), fbcm(pcols,pver,nbmodes), fcm(pcols,pver,nbmodes), &
            faqm(pcols,pver,nbmodes), f_condm(pcols,pver,nbmodes), &
            f_soam(pcols, pver,nbmodes), faqm4(pcols,pver) 
   real(r8) xrh(pcols,pver), xrhnull(pcols,pver)
   integer  irh1(pcols,pver), irh2(pcols,pver), irh1null(pcols,pver), irh2null(pcols,pver)
   real(r8) focm(pcols,pver,4)
!   real(r8) akso4c(pcols), akbcc(pcols), akocc(pcols)
   real(r8) ssa(pcols,pver,0:nmodes,nbands), asym(pcols,pver,0:nmodes,nbands), & 
            be(pcols,pver,0:nmodes,nbands), ke(pcols,pver,0:nmodes,nbands),    &
            betotvis(pcols,pver), batotvis(pcols,pver)
   real(r8) ssatot(pcols,pver,nbands)     ! spectral aerosol single scattering albedo
   real(r8) asymtot(pcols,pver,nbands)    ! spectral aerosol asymmetry factor
   real(r8) betot(pcols,pver,nbands)      ! spectral aerosol extinction coefficient
   real(r8) batotlw(pcols,pver,nlwbands)  ! spectral aerosol absportion extinction in LW
   real(r8) kalw(pcols,pver,0:nmodes,nlwbands)
   real(r8) balw(pcols,pver,0:nmodes,nlwbands)
   logical  lw_on   ! LW calculations are performed in interpol* if true
   real(r8) volc_balw(pcols,0:pver,nlwbands) ! volcanic aerosol absorption coefficient for terrestrial bands, CMIP6

#ifdef COLTST4INTCONS 
!-3   real(r8) bekc1(pcols,pver), bekc2(pcols,pver), bekc3(pcols,pver), bekc4(pcols,pver), & 
   real(r8) bekc1(pcols,pver), bekc2(pcols,pver), bekc4(pcols,pver), & 
            bekc5(pcols,pver), bekc6(pcols,pver), bekc7(pcols,pver), bekc8(pcols,pver), &
!-11            bekc9(pcols,pver), bekc10(pcols,pver), bekc11(pcols,pver), &
            bekc9(pcols,pver), bekc10(pcols,pver), &
!-13            bekc12(pcols,pver), bekc13(pcols,pver), bekc14(pcols,pver), bekc0(pcols,pver)   
            bekc12(pcols,pver), bekc14(pcols,pver), bekc0(pcols,pver)   
   real(r8) taukc1(pcols), taukc2(pcols), taukc3(pcols), taukc4(pcols), taukc5(pcols),  & 
            taukc6(pcols), taukc7(pcols), taukc8(pcols), taukc9(pcols), taukc10(pcols), & 
            taukc11(pcols), taukc12(pcols), taukc13(pcols), taukc14(pcols), taukc0(pcols)   
   real(r8) kekc1(pcols,pver), kekc2(pcols,pver), kekc4(pcols,pver), & 
            kekc5(pcols,pver), kekc6(pcols,pver), kekc7(pcols,pver), kekc8(pcols,pver), &
            kekc9(pcols,pver), kekc10(pcols,pver), &
            kekc12(pcols,pver), kekc14(pcols,pver), kekc0(pcols,pver)   
#ifdef AEROCOM
   real(r8) cmodedry(pcols,pver,0:nmodes), &
            cmdry0(pcols), cmdry1(pcols), cmdry2(pcols), cmdry4(pcols), &
            cmdry5(pcols), cmdry6(pcols), cmdry7(pcols), cmdry8(pcols), &
            cmdry9(pcols), cmdry10(pcols), cmdry12(pcols), cmdry14(pcols)
#endif
#endif
   real(r8) rh0(pcols,pver), rhoda(pcols,pver)
   real(r8) ssavis(pcols,pver), asymmvis(pcols,pver), extvis(pcols,pver), dayfoc(pcols,pver)
   real(r8) n_aerorig(pcols,pver), n_aer(pcols,pver)
   type(physics_state), intent(in), target :: state
   real(r8) :: es(pcols,pver)      ! saturation vapor pressure
   real(r8) :: qs(pcols,pver)      ! saturation specific humidity
   real(r8) :: rht(pcols,pver)     ! relative humidity (fraction) (rh is already used in opptab)
   real(r8) :: rh_temp(pcols,pver) ! relative humidity (fraction) for input to LUT
   real(r8) xfombg(pcols,pver)
   integer ifombg1(pcols,pver), ifombg2(pcols,pver)
   real(r8) xct(pcols,pver,nmodes)
   integer ict1(pcols,pver,nmodes)
   real(r8) xfac(pcols,pver,nbmodes)
   integer ifac1(pcols,pver,nbmodes)
   real(r8) xfbc(pcols,pver,nbmodes)
   integer ifbc1(pcols,pver,nbmodes)
   real(r8) xfaq(pcols,pver,nbmodes)
   integer ifaq1(pcols,pver,nbmodes)
   real(r8) xfbcbg(pcols,pver)
   integer ifbcbg1(pcols,pver)
   real(r8) xfbcbgn(pcols,pver)
   integer ifbcbgn1(pcols,pver)

#ifdef AEROCOM 
   real(r8) Ctotdry(pcols,pver), Cwater(pcols,pver), mmr_aerh2o(pcols,pver), &
            dod550dry(pcols), abs550dry(pcols)
   real(r8) daerh2o(pcols),  dload(pcols,0:nmodes), dload3d(pcols,pver,0:nmodes), &
            dload_mi(pcols), dload_ss(pcols), &
            dload_s4(pcols), dload_oc(pcols), dload_bc(pcols), &
            dload_s4_a(pcols), dload_s4_1(pcols), dload_s4_5(pcols)
   real(r8) dload_bc_0(pcols), dload_bc_ac(pcols), dload_oc_ac(pcols), &
            dload_bc_2(pcols), dload_bc_4(pcols), dload_bc_12(pcols), dload_bc_14(pcols), &    
            dload_oc_4(pcols), dload_oc_14(pcols)
   real(r8) cmin(pcols,pver), cseas(pcols,pver)
   real(r8) nnat_1(pcols,pver), nnat_2(pcols,pver), nnat_3(pcols,pver), &
            nnat_4(pcols,pver), nnat_5(pcols,pver), nnat_6(pcols,pver), &
            nnat_7(pcols,pver), nnat_8(pcols,pver), nnat_9(pcols,pver), &
            nnat_10(pcols,pver), nnat_12(pcols,pver), &
            nnat_14(pcols,pver), nnat_0(pcols,pver)
   real(r8) ck(pcols,pver,0:nmodes), cknorm(pcols,pver,0:nmodes), &
            cknlt05(pcols,pver,0:nmodes), ckngt125(pcols,pver,0:nmodes)
   real(r8) aaerosn(pcols,pver,nbmp1:nmodes), aaeroln(pcols,pver,nbmp1:nmodes), &
            vaerosn(pcols,pver,nbmp1:nmodes), vaeroln(pcols,pver,nbmp1:nmodes), &
            aaeros(pcols,pver,0:nbmodes), aaerol(pcols,pver,0:nbmodes), & 
            vaeros(pcols,pver,0:nbmodes), vaerol(pcols,pver,0:nbmodes) 
   real(r8) cintbg(pcols,pver,0:nbmodes), &
            cintbg05(pcols,pver,0:nbmodes), cintbg125(pcols,pver,0:nbmodes), &
            cintbc(pcols,pver,0:nbmodes), &
            cintbc05(pcols,pver,0:nbmodes), cintbc125(pcols,pver,0:nbmodes), &  
            cintoc(pcols,pver,0:nbmodes), &
            cintoc05(pcols,pver,0:nbmodes), cintoc125(pcols,pver,0:nbmodes), &
            cintsc(pcols,pver,0:nbmodes), &
            cintsc05(pcols,pver,0:nbmodes), cintsc125(pcols,pver,0:nbmodes), &        
            cintsa(pcols,pver,0:nbmodes), &
            cintsa05(pcols,pver,0:nbmodes), cintsa125(pcols,pver,0:nbmodes)
   real(r8) c_mi(pcols,pver), c_mi05(pcols,pver), c_mi125(pcols,pver), &
            c_ss(pcols,pver), c_ss05(pcols,pver), c_ss125(pcols,pver), &
            c_bc(pcols,pver), c_bc05(pcols,pver), c_bc125(pcols,pver), &
            c_oc(pcols,pver), c_oc05(pcols,pver), c_oc125(pcols,pver), &
            c_sa(pcols,pver), c_sa05(pcols,pver), c_sa125(pcols,pver), &
            c_sc(pcols,pver), c_sc05(pcols,pver), c_sc125(pcols,pver), &
            c_s4(pcols,pver), c_s405(pcols,pver), c_s4125(pcols,pver), &
            c_s4_a(pcols,pver), c_s4_1(pcols,pver), c_s4_5(pcols,pver)
   real(r8) c_bc_0(pcols,pver), c_bc_ac(pcols,pver), c_oc_ac(pcols,pver), &
            c_bc_2(pcols,pver), c_bc_4(pcols,pver), c_bc_12(pcols,pver), c_bc_14(pcols,pver), &  
            c_oc_4(pcols,pver), c_oc_14(pcols,pver)   
   real(r8) c_tots(pcols), c_tot125s(pcols), c_pm25s(pcols) ! = PM all sizes, PM>2.5um and PM<2.5um (PM2.5)
!akc6+
   real(r8) c_tot(pcols,pver), c_tot125(pcols,pver), c_pm25(pcols,pver), &
            mmr_pm25(pcols,pver), c_tot05(pcols,pver), c_pm1(pcols,pver), mmr_pm1(pcols,pver)  
!akc6-
   real(r8) aaeros_tot(pcols,pver), aaerol_tot(pcols,pver), vaeros_tot(pcols,pver), &
            vaerol_tot(pcols,pver), aaercols(pcols), aaercoll(pcols), vaercols(pcols), & 
            vaercoll(pcols), derlt05(pcols), dergt05(pcols), der(pcols), &
            erlt053d(pcols,pver), ergt053d(pcols,pver), er3d(pcols,pver)
   real(r8) bext440(pcols,pver,0:nbmodes), babs440(pcols,pver,0:nbmodes), &
            bext500(pcols,pver,0:nbmodes), babs500(pcols,pver,0:nbmodes), &
            bext550(pcols,pver,0:nbmodes), babs550(pcols,pver,0:nbmodes), &
            bext670(pcols,pver,0:nbmodes), babs670(pcols,pver,0:nbmodes), &
            bext870(pcols,pver,0:nbmodes), babs870(pcols,pver,0:nbmodes), &
            bebg440(pcols,pver,0:nbmodes), babg440(pcols,pver,0:nbmodes), &
            bebg500(pcols,pver,0:nbmodes), babg500(pcols,pver,0:nbmodes), &
            bebg550(pcols,pver,0:nbmodes), babg550(pcols,pver,0:nbmodes), &
            bebg670(pcols,pver,0:nbmodes), babg670(pcols,pver,0:nbmodes), &
            bebg870(pcols,pver,0:nbmodes), babg870(pcols,pver,0:nbmodes), &
            bebc440(pcols,pver,0:nbmodes), babc440(pcols,pver,0:nbmodes), &
            bebc500(pcols,pver,0:nbmodes), babc500(pcols,pver,0:nbmodes), &
            bebc550(pcols,pver,0:nbmodes), babc550(pcols,pver,0:nbmodes), &
            bebc670(pcols,pver,0:nbmodes), babc670(pcols,pver,0:nbmodes), &
            bebc870(pcols,pver,0:nbmodes), babc870(pcols,pver,0:nbmodes), &
            beoc440(pcols,pver,0:nbmodes), baoc440(pcols,pver,0:nbmodes), &
            beoc500(pcols,pver,0:nbmodes), baoc500(pcols,pver,0:nbmodes), &
            beoc550(pcols,pver,0:nbmodes), baoc550(pcols,pver,0:nbmodes), &
            beoc670(pcols,pver,0:nbmodes), baoc670(pcols,pver,0:nbmodes), &
            beoc870(pcols,pver,0:nbmodes), baoc870(pcols,pver,0:nbmodes), &
            besu440(pcols,pver,0:nbmodes), basu440(pcols,pver,0:nbmodes), &
            besu500(pcols,pver,0:nbmodes), basu500(pcols,pver,0:nbmodes), &
            besu550(pcols,pver,0:nbmodes), basu550(pcols,pver,0:nbmodes), &
            besu670(pcols,pver,0:nbmodes), basu670(pcols,pver,0:nbmodes), &
            besu870(pcols,pver,0:nbmodes), basu870(pcols,pver,0:nbmodes)
   real(r8) bebglt1(pcols,pver,0:nbmodes), bebggt1(pcols,pver,0:nbmodes), &
            bebclt1(pcols,pver,0:nbmodes), bebcgt1(pcols,pver,0:nbmodes), & 
            beoclt1(pcols,pver,0:nbmodes), beocgt1(pcols,pver,0:nbmodes), & 
            bes4lt1(pcols,pver,0:nbmodes), bes4gt1(pcols,pver,0:nbmodes), &
            backsc550(pcols,pver,0:nbmodes), backsc550x(pcols,pver,nbmp1:nmodes), &
            backsc550tot(pcols,pver), ec550_aer(pcols,pver), abs550_aer(pcols,pver), &
            bs550_aer(pcols,pver)
!           Additional AeroCom Phase III output:   
   real(r8) asydry_aer(pcols,pver)    ! dry asymtot in the visible band
!  
   real(r8) ec550_so4(pcols,pver),ec550_bc(pcols,pver), ec550_pom(pcols,pver), &
            ec550_ss(pcols,pver), ec550_du(pcols,pver)
   real(r8) bext440n(pcols,pver,0:nbmodes), babs440n(pcols,pver,0:nbmodes), &
            bext500n(pcols,pver,0:nbmodes), babs500n(pcols,pver,0:nbmodes), &
            bext550n(pcols,pver,0:nbmodes), babs550n(pcols,pver,0:nbmodes), &
            bext670n(pcols,pver,0:nbmodes), babs670n(pcols,pver,0:nbmodes), &
            bext870n(pcols,pver,0:nbmodes), babs870n(pcols,pver,0:nbmodes), &
            bebg440n(pcols,pver,0:nbmodes), babg440n(pcols,pver,0:nbmodes), &
            bebg500n(pcols,pver,0:nbmodes), babg500n(pcols,pver,0:nbmodes), &
            bebg550n(pcols,pver,0:nbmodes), babg550n(pcols,pver,0:nbmodes), &
            bebg670n(pcols,pver,0:nbmodes), babg670n(pcols,pver,0:nbmodes), &
            bebg870n(pcols,pver,0:nbmodes), babg870n(pcols,pver,0:nbmodes), &
            bebc440n(pcols,pver,0:nbmodes), babc440n(pcols,pver,0:nbmodes), &
            bebc500n(pcols,pver,0:nbmodes), babc500n(pcols,pver,0:nbmodes), &
            bebc550n(pcols,pver,0:nbmodes), babc550n(pcols,pver,0:nbmodes), &
            bebc670n(pcols,pver,0:nbmodes), babc670n(pcols,pver,0:nbmodes), &
            bebc870n(pcols,pver,0:nbmodes), babc870n(pcols,pver,0:nbmodes), &
            beoc440n(pcols,pver,0:nbmodes), baoc440n(pcols,pver,0:nbmodes), &
            beoc500n(pcols,pver,0:nbmodes), baoc500n(pcols,pver,0:nbmodes), &
            beoc550n(pcols,pver,0:nbmodes), baoc550n(pcols,pver,0:nbmodes), &
            beoc670n(pcols,pver,0:nbmodes), baoc670n(pcols,pver,0:nbmodes), &
            beoc870n(pcols,pver,0:nbmodes), baoc870n(pcols,pver,0:nbmodes), &
            besu440n(pcols,pver,0:nbmodes), basu440n(pcols,pver,0:nbmodes), &
            besu500n(pcols,pver,0:nbmodes), basu500n(pcols,pver,0:nbmodes), &
            besu550n(pcols,pver,0:nbmodes), basu550n(pcols,pver,0:nbmodes), &
            besu670n(pcols,pver,0:nbmodes), basu670n(pcols,pver,0:nbmodes), &
            besu870n(pcols,pver,0:nbmodes), basu870n(pcols,pver,0:nbmodes)
   real(r8) bebglt1n(pcols,pver,0:nbmodes), bebggt1n(pcols,pver,0:nbmodes), &
            bebclt1n(pcols,pver,0:nbmodes), bebcgt1n(pcols,pver,0:nbmodes), & 
            beoclt1n(pcols,pver,0:nbmodes), beocgt1n(pcols,pver,0:nbmodes), & 
            bes4lt1n(pcols,pver,0:nbmodes), bes4gt1n(pcols,pver,0:nbmodes), &
            backsc550n(pcols,pver,0:nbmodes) 
   real(r8) bext440tot(pcols,pver), babs440tot(pcols,pver), &
            bext500tot(pcols,pver), babs500tot(pcols,pver), &
            bext550tot(pcols,pver), babs550tot(pcols,pver), &
            bext670tot(pcols,pver), babs670tot(pcols,pver), &
            bext870tot(pcols,pver), babs870tot(pcols,pver), &
            bebg440tot(pcols,pver), babg440tot(pcols,pver), &
            bebg500tot(pcols,pver), babg500tot(pcols,pver), &
            bebg550tot(pcols,pver), babg550tot(pcols,pver), &
            bebg670tot(pcols,pver), babg670tot(pcols,pver), &
            bebg870tot(pcols,pver), babg870tot(pcols,pver), &
            bebc440tot(pcols,pver), babc440tot(pcols,pver), &
            bebc500tot(pcols,pver), babc500tot(pcols,pver), &
            bebc550tot(pcols,pver), babc550tot(pcols,pver), &
            bebc670tot(pcols,pver), babc670tot(pcols,pver), &
            bebc870tot(pcols,pver), babc870tot(pcols,pver), &
            beoc440tot(pcols,pver), baoc440tot(pcols,pver), &
            beoc500tot(pcols,pver), baoc500tot(pcols,pver), &
            beoc550tot(pcols,pver), baoc550tot(pcols,pver), &
            beoc670tot(pcols,pver), baoc670tot(pcols,pver), &
            beoc870tot(pcols,pver), baoc870tot(pcols,pver), &
            besu440tot(pcols,pver), basu440tot(pcols,pver), &
            besu500tot(pcols,pver), basu500tot(pcols,pver), &
            besu550tot(pcols,pver), basu550tot(pcols,pver), &
            besu670tot(pcols,pver), basu670tot(pcols,pver), &
            besu870tot(pcols,pver), basu870tot(pcols,pver)
   real(r8) bebglt1t(pcols,pver), bebggt1t(pcols,pver), bebclt1t(pcols,pver), & 
            bebcgt1t(pcols,pver), beoclt1t(pcols,pver), beocgt1t(pcols,pver), &
            bes4lt1t(pcols,pver), bes4gt1t(pcols,pver)
   real(r8) be440x(pcols,pver,nbmp1:nmodes), ba440x(pcols,pver,nbmp1:nmodes), &
            be500x(pcols,pver,nbmp1:nmodes), ba500x(pcols,pver,nbmp1:nmodes), &
            be550x(pcols,pver,nbmp1:nmodes), ba550x(pcols,pver,nbmp1:nmodes), &
            be670x(pcols,pver,nbmp1:nmodes), ba670x(pcols,pver,nbmp1:nmodes), &
            be870x(pcols,pver,nbmp1:nmodes), ba870x(pcols,pver,nbmp1:nmodes), &
            belt1x(pcols,pver,nbmp1:nmodes), begt1x(pcols,pver,nbmp1:nmodes)
   real(r8) bebc440xt(pcols,pver),babc440xt(pcols,pver), &
            bebc500xt(pcols,pver),babc500xt(pcols,pver), &
            bebc550xt(pcols,pver),babc550xt(pcols,pver), &
            bebc670xt(pcols,pver),babc670xt(pcols,pver), &
            bebc870xt(pcols,pver),babc870xt(pcols,pver), &
            beoc440xt(pcols,pver),baoc440xt(pcols,pver), &
            beoc500xt(pcols,pver),baoc500xt(pcols,pver), &
            beoc550xt(pcols,pver),baoc550xt(pcols,pver), &
            beoc670xt(pcols,pver),baoc670xt(pcols,pver), &
            beoc870xt(pcols,pver),baoc870xt(pcols,pver) 
   real(r8) bbclt1xt(pcols,pver), &
            bbcgt1xt(pcols,pver), boclt1xt(pcols,pver), bocgt1xt(pcols,pver)
   real(r8) bint440du(pcols,pver), bint500du(pcols,pver), bint550du(pcols,pver), &
            bint670du(pcols,pver), bint870du(pcols,pver), &
            bint440ss(pcols,pver), bint500ss(pcols,pver), bint550ss(pcols,pver), &
            bint670ss(pcols,pver), bint870ss(pcols,pver), &
            baint550du(pcols,pver), baint550ss(pcols,pver)
   real(r8) bedustlt1(pcols,pver), bedustgt1(pcols,pver), &
            besslt1(pcols,pver), bessgt1(pcols,pver)
   real(r8) dod4403d(pcols,pver), abs4403d(pcols,pver), &
            dod4403d_ss(pcols,pver),   & ! abs4403d_ss(pcols,pver), & 
            dod4403d_dust(pcols,pver), & ! abs4403d_dust(pcols,pver), &
            dod4403d_so4(pcols,pver),  & ! abs4403d_so4(pcols,pver), &
            dod4403d_bc(pcols,pver),   & ! abs4403d_bc(pcols,pver), &
            dod4403d_pom(pcols,pver),  & ! abs4403d_pom(pcols,pver), &
            dod5003d(pcols,pver), abs5003d(pcols,pver), &
            dod5003d_ss(pcols,pver),   & ! abs5003d_ss(pcols,pver), & 
            dod5003d_dust(pcols,pver), & ! abs5003d_dust(pcols,pver), &
            dod5003d_so4(pcols,pver),  & ! abs5003d_so4(pcols,pver), &
            dod5003d_bc(pcols,pver),   & ! abs5003d_bc(pcols,pver), &
            dod5003d_pom(pcols,pver),  & ! abs5003d_pom(pcols,pver), &
            dod5503d(pcols,pver), abs5503d(pcols,pver), abs5503dalt(pcols,pver), &
            dod5503d_ss(pcols,pver), abs5503d_ss(pcols,pver), & 
            dod5503d_dust(pcols,pver), abs5503d_dust(pcols,pver), &
            dod5503d_so4(pcols,pver), abs5503d_so4(pcols,pver), &
            dod5503d_bc(pcols,pver), abs5503d_bc(pcols,pver), &
            dod5503d_pom(pcols,pver), abs5503d_pom(pcols,pver), &
            dod6703d(pcols,pver), abs6703d(pcols,pver), &
            dod6703d_ss(pcols,pver),   & ! abs6703d_ss(pcols,pver), & 
            dod6703d_dust(pcols,pver), & ! abs6703d_dust(pcols,pver), &
            dod6703d_so4(pcols,pver),  & ! abs6703d_so4(pcols,pver), &
            dod6703d_bc(pcols,pver),   & ! abs6703d_bc(pcols,pver), &
            dod6703d_pom(pcols,pver),  & ! abs6703d_pom(pcols,pver), &
            dod8703d(pcols,pver), abs8703d(pcols,pver), &
            dod8703d_ss(pcols,pver),   & ! abs8703d_ss(pcols,pver), & 
            dod8703d_dust(pcols,pver), & ! abs8703d_dust(pcols,pver), &
            dod8703d_so4(pcols,pver),  & ! abs8703d_so4(pcols,pver), &
            dod8703d_bc(pcols,pver),   & ! abs8703d_bc(pcols,pver), &
            dod8703d_pom(pcols,pver) ! abs8703d_pom(pcols,pver)
   real(r8) dod5503dlt1_ss(pcols,pver), dod5503dgt1_ss(pcols,pver), &
            dod5503dlt1_dust(pcols,pver), dod5503dgt1_dust(pcols,pver), &
            dod5503dlt1_so4(pcols,pver), dod5503dgt1_so4(pcols,pver), &
            dod5503dlt1_bc(pcols,pver), dod5503dgt1_bc(pcols,pver), &
            dod5503dlt1_pom(pcols,pver), dod5503dgt1_pom(pcols,pver)
   real(r8) dod440(pcols), abs440(pcols), dod500(pcols), abs500(pcols),  &
            dod550(pcols), abs550(pcols), abs550alt(pcols), dod670(pcols),& 
            abs670(pcols), dod870(pcols), abs870(pcols),                 &
            dod440_ss(pcols), dod440_dust(pcols), dod440_so4(pcols),     &
            dod440_bc(pcols), dod440_pom(pcols),                         & 
            dod500_ss(pcols), dod500_dust(pcols), dod500_so4(pcols),     &
            dod500_bc(pcols), dod500_pom(pcols),                         &        
            dod550_ss(pcols), dod550_dust(pcols), dod550_so4(pcols),     &
            dod550_bc(pcols), dod550_pom(pcols),                         &        
            dod670_ss(pcols), dod670_dust(pcols), dod670_so4(pcols),     &
            dod670_bc(pcols), dod670_pom(pcols),                         &        
            dod870_ss(pcols), dod870_dust(pcols), dod870_so4(pcols),     &
            dod870_bc(pcols), dod870_pom(pcols),                         &        
            dod550lt1_ss(pcols), dod550gt1_ss(pcols), dod550lt1_dust(pcols), & 
            dod550gt1_dust(pcols), dod550lt1_so4(pcols), &
            dod550gt1_so4(pcols), dod550lt1_bc(pcols), dod550gt1_bc(pcols), &
            dod550lt1_pom(pcols), dod550gt1_pom(pcols)
   real(r8) abs550_ss(pcols), abs550_dust(pcols), &
            abs550_so4(pcols), abs550_bc(pcols), abs550_pom(pcols)
   real(r8) batotsw13(pcols,pver), batotlw01(pcols,pver)
#endif  ! AEROCOM
!+
#ifdef AEROCOM
      character(len=10) :: modeString
      character(len=20) :: varname
      integer irf,irfmax
      real(r8) Camrel(pcols,pver,nbmodes)
      real(r8) Camtot(pcols,nbmodes)
      real(r8) cxsmtot(pcols,nbmodes)
      real(r8) cxsmrel(pcols,nbmodes)
      real(r8) xctrel,camdiff,cxsm
      real(r8) cxs(pcols,pver), cxstot(pcols,pver), akcxs(pcols) 
#endif  
!-
      
!
!-------------------------------------------------------------------------
!

!test: hentet fra aer_rad_props, saa modifisert/rettet (!x)
   ! calculate relative humidity for table lookup into rh grid
!x   call qsat(state%t(1:ncol,1:pver), state%pmid(1:ncol,1:pver), &
   call qsat_water(state%t(1:ncol,1:pver), state%pmid(1:ncol,1:pver), &
        es(1:ncol,1:pver), qs(1:ncol,1:pver))
   rht(1:ncol,1:pver) = state%q(1:ncol,1:pver,1) / qs(1:ncol,1:pver)
   rh_temp(1:ncol,1:pver) = min(rht(1:ncol,1:pver),1._r8)


      do k=1,pver
         do icol=1,ncol
!         Set upper and lower relative humidity for the aerosol calculations
          rhum(icol,k) = min(0.995_r8, max(rh_temp(icol,k), 0.01_r8))
          rhoda(icol,k) = pmid(icol,k)/(rair*t(icol,k))      ! unit kg/m^3
!test          rhum(icol,k) = 0.01_r8
          if (cld(icol,k) .lt. 1.0_r8) then
             rhum(icol,k) = (rhum(icol,k) - cld(icol,k)) / (1.0_r8 - cld(icol,k))  ! clear portion
          end if
          rhum(icol,k) = min(0.995_r8, max(rhum(icol,k), 0.01_r8))
         end do
      end do

!     Layer thickness with unit km
      do icol=1,ncol
        do k=1,pver
          deltah_km(icol,k)=1.e-4_r8*(pint(icol,k+1)-pint(icol,k))/(rhoda(icol,k)*9.8_r8) 
        end do
      end do

!     interpol-calculations only when daylight or not:
#ifdef AEROCOM                   ! always calculate optics (also at (polar) night)
      do icol=1,ncol
        daylight(icol) = .true. 
      end do
#else                            ! calculate optics only in daytime
      do icol=1,ncol
        if (coszrs(icol) > 0.0_r8) then
          daylight(icol) = .true. 
        else
          daylight(icol) = .false.
        endif
      end do
#endif  ! AEROCOM

!     Set SO4, BC and OC concentrations:

!     initialize concentration fields
      do i=0,nmodes
        do k=1,pver
          do icol=1,ncol
            Nnatk(icol,k,i)  = 0.0_r8
          end do
        end do 
      end do 
      do k=1,pver
        do icol=1,ncol
          n_aerorig(icol,k) = 0.0_r8
          n_aer(icol,k)     = 0.0_r8
        end do
      end do
      kalw(:,:,:,:)=0._r8
        be(:,:,:,:)=0._r8
        ke(:,:,:,:)=0._r8
      asym(:,:,:,:)=0._r8
       ssa(:,:,:,:)=0._r8
!      Find process tagged bulk aerosol properies (from the life cycle module): 

          call calculateBulkProperties(ncol, qm1, rhoda, Nnatk, Ca, f_c, f_bc, &
                                       f_aq, f_so4_cond, f_soa, faitbc, fnbc, f_soana)

!      calculating vulume fractions from mass fractions: 
      do k=1,pver
        do icol=1,ncol
          v_soana(icol,k) = f_soana(icol,k)/(f_soana(icol,k) &
                           +(1.0_r8-f_soana(icol,k))*rhopart(l_soa_na)/rhopart(l_so4_na))
        end do
      end do 

!     Avoid very small numbers
      do k=1,pver
         do icol=1,ncol
          Ca(icol,k)        = max(eps,Ca(icol,k))
          f_c(icol,k)       = max(eps,f_c(icol,k))
          f_bc(icol,k)      = max(eps,f_bc(icol,k)) 
          f_aq(icol,k)      = max(eps,f_aq(icol,k)) 
          fnbc(icol,k)      = max(eps,fnbc(icol,k))
          faitbc(icol,k)    = max(eps,faitbc(icol,k)) 
         end do
       end do

!     Calculation of the apportionment of internally mixed SO4, BC and OC 
!     mass between the various background modes.

      !==> calls modalapp to partition the mass
      call partitionMass(ncol, nnatk, Ca, f_c, f_bc, f_aq, f_so4_cond, f_soa , &
                              cam, fcm, fbcm, faqm, f_condm, f_soam )

      !The following uses non-standard units, #/cm3 and ug/m3
      Nnatk(:ncol,:,:) = Nnatk(:ncol,:,:)*1.e-6_r8
      cam(:ncol,:,:)=cam(:ncol,:,:)*1.e9_r8

!     Calculate fraction of added mass which is either SOA condensate or OC coagulate,
!     which in AeroTab are both treated as condensate for kcomp=1-4.
      do i=1,4
        do k=1,pver
          do icol=1,ncol
            focm(icol,k,i) = fcm(icol,k,i)*(1.0_r8-fbcm(icol,k,i))
          enddo
        enddo
      enddo
      do k=1,pver
        do icol=1,ncol
          faqm4(icol,k) = faqm(icol,k,4)
        end do
      enddo  

!     find common input parameters for use in the interpolation routines

      call inputForInterpol (lchnk, ncol, rhum, xrh, irh1,   &
         f_soana, xfombg, ifombg1, faitbc, xfbcbg, ifbcbg1,  &
         fnbc, xfbcbgn, ifbcbgn1, Nnatk, Cam, xct, ict1,     &
         focm, fcm, xfac, ifac1, fbcm, xfbc, ifbc1, faqm, xfaq, ifaq1)

!     and define the respective RH input variables for dry aerosols     
      do k=1,pver
        do icol=1,ncol
          xrhnull(icol,k)=rh(1)
          irh1null(icol,k)=1
        end do
      enddo  


#ifdef AEROCOM

!     Initialize overshooting mass summed over all modes
      do k=1,pver
        do icol=1,ncol
          cxstot(icol,k)=0.0_r8
        enddo 
      enddo  
      do icol=1,ncol
        akcxs(icol)=0.0_r8
      enddo 

!     Initializing total and relative exessive (overshooting w.r.t. 
!     look-up table maxima) added mass column:
      do i=1,nbmodes
        do icol=1,ncol
           Camtot(icol,i)=0.0_r8
           cxsmtot(icol,i)=0.0_r8
           cxsmrel(icol,i)=0.0_r8
        enddo
      enddo
!     Calculating added internally mixed mass onto each mode 1-10, relative to
!     maximum mass which can be added w.r.t. the look-up tables (for level k),
!     as well as the relative exessive added mass column:
      do i=1,4
       do k=1,pver
         do icol=1,ncol
            Camrel(icol,k,i) = (Cam(icol,k,i)/(Nnatk(icol,k,i)+eps))/cate(i,16)
            xctrel=min(max(Camrel(icol,k,i),cate(i,1)/cate(i,16)),1.0_r8)
            camdiff=Cam(icol,k,i)-xctrel*cate(i,16)*(Nnatk(icol,k,i)+eps)
            cxsm=max(0.0_r8,camdiff)
            cxsmtot(icol,i)=cxsmtot(icol,i)+cxsm*deltah_km(icol,k)          
            Camtot(icol,i)=Camtot(icol,i)+Cam(icol,k,i)*deltah_km(icol,k)          
!t
            camdiff=Cam(icol,k,i)-xct(icol,k,i)*(Nnatk(icol,k,i)+eps)
            cxs(icol,k)=max(0.0_r8,camdiff)
            cxstot(icol,k)= cxstot(icol,k)+cxs(icol,k)
!t
         enddo 
       enddo  
      enddo  
      do i=5,nbmodes
       do k=1,pver
         do icol=1,ncol
            Camrel(icol,k,i) = (Cam(icol,k,i)/(Nnatk(icol,k,i)+eps))/cat(i,6)
            xctrel=min(max(Camrel(icol,k,i),cat(i,1)/cat(i,6)),1.0_r8)
            camdiff=Cam(icol,k,i)-xctrel*cat(i,6)*(Nnatk(icol,k,i)+eps)
            cxsm=max(0.0_r8,camdiff)
            cxsmtot(icol,i)=cxsmtot(icol,i)+cxsm*deltah_km(icol,k) 
            Camtot(icol,i)=Camtot(icol,i)+Cam(icol,k,i)*deltah_km(icol,k)          
!t
            camdiff=Cam(icol,k,i)-xct(icol,k,i)*(Nnatk(icol,k,i)+eps)
            cxs(icol,k)=max(0.0_r8,camdiff)
            cxstot(icol,k)= cxstot(icol,k)+cxs(icol,k)
!t
         enddo 
       enddo  
      enddo  

!     Total overshooting mass summed over all modes and all levels
      do icol=1,ncol
        do k=1,pver
          akcxs(icol) =akcxs(icol)+cxstot(icol,k)*deltah_km(icol,k)
        enddo
      enddo
      call outfld('AKCXS   ',akcxs ,pcols,lchnk)

      do i=1,nbmodes
        do icol=1,ncol
           cxsmrel(icol,i)=cxsmtot(icol,i)/(Camtot(icol,i)+eps)
        enddo
      enddo

      do i=1,nbmodes
          modeString="  "
          write(modeString,"(I2)"),i
          if(i.lt.10) modeString="0"//adjustl(modeString)
          varName = "Camrel"//trim(modeString)
          if(i.ne.3) call outfld(varName,Camrel(:,:,i),pcols,lchnk)
      enddo  

      do i=1,nbmodes
          modeString="  "
          write(modeString,"(I2)"),i
          if(i.lt.10) modeString="0"//adjustl(modeString)
          varName = "Cxsrel"//trim(modeString)
          if(i.ne.3) call outfld(varName,cxsmrel(:,i),pcols,lchnk)
      enddo  

#endif


!     AeroCom: Find dry aerosol asymmetry factor and mass for subsequent 
!     calculation of condensed water mass below...

#ifdef AEROCOM      
!ccccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5ccccccccc6ccccccccc7cc

      do k=1,pver
        do icol=1,ncol
          Ctotdry(icol,k)=0.0_r8
          rh0(icol,k)=0.0_r8
          asydry_aer(icol,k)=0.0_r8 
        end do
      enddo  

      lw_on = .false.  ! No LW optics needed for RH=0 (interpol returns 0-values)

 do iloop=1,1  ! loop over i>1 for testing CPU use in interpol*
!     BC(ax) mode (dry only):  
      call interpol0 (lchnk, ncol, daylight, Nnatk, ssa, asym, be, ke, lw_on, kalw)

      mplus10=0
!     SO4/SOA(Ait) mode:                                         
      call interpol1 (lchnk, ncol, daylight, xrhnull, irh1null, mplus10, &
                      Nnatk, xfombg, ifombg1, xct, ict1, xfac, ifac1, &
                      ssa, asym, be, ke, lw_on, kalw)

!     BC(Ait) and OC(Ait) modes:
      call interpol2to3 (lchnk, ncol, daylight, xrhnull, irh1null, mplus10, &
                         Nnatk, xct, ict1, xfac, ifac1, &
                         ssa, asym, be, ke, lw_on, kalw)

!     BC&OC(Ait) mode:   ------ fcm not valid here (=0). Use faitbc instead
      call interpol4 (lchnk, ncol, daylight, xrhnull, irh1null, mplus10, &
                      Nnatk, xfbcbg, ifbcbg1, xct, ict1, xfac, ifac1, &
                      xfaq, ifaq1, ssa, asym, be, ke, lw_on, kalw)

!     SO4(Ait75) (5), Mineral (6-7) and Sea-salt (8-10) modes:
      call interpol5to10 (lchnk, ncol, daylight, xrhnull, irh1null, &
                          Nnatk, xct, ict1, xfac, ifac1, &
                          xfbc, ifbc1, xfaq, ifaq1, &
                          ssa, asym, be, ke, lw_on, kalw)
 enddo ! iloop


 do iloop=1,1
      mplus10=1
!     BC(Ait) and OC(Ait) modes:
      call interpol2to3 (lchnk, ncol, daylight, xrhnull, irh1null, mplus10, &
                         Nnatk, xct, ict1, xfac, ifac1, &
                         ssa, asym, be, ke, lw_on, kalw)

!     BC&OC(n) mode:   ------ fcm not valid here (=0). Use fnbc instead
      call interpol4 (lchnk, ncol, daylight, xrhnull, irh1null, mplus10, &
                      Nnatk, xfbcbgn, ifbcbgn1, xct, ict1, &
                      xfac, ifac1, xfaq, ifaq1, &
                      ssa, asym, be, ke, lw_on, kalw)

enddo ! iloop

       do i=0,nmodes    ! mode 0 to 14 
         do k=1,pver
            do icol=1,ncol
               dCtot(icol,k)=1.e3_r8*be(icol,k,i,4)/(ke(icol,k,i,4)+eps)
               Ctotdry(icol,k)=Ctotdry(icol,k)+dCtot(icol,k)*Nnatk(icol,k,i) 
#ifdef COLTST4INTCONS 
               cmodedry(icol,k,i)=dCtot(icol,k)*Nnatk(icol,k,i)
#endif
           end do
         enddo  
       enddo

!!! AeroCom Phase III: adding asymmetry factor for dry aerosol, wavelength band 4 only
!!! (and with no CMIP6 volcnic contribution)
      ib=4
      do k=1,pver
       do icol=1,ncol
          betot(icol,k,ib)=0.0_r8
          ssatot(icol,k,ib)=0.0_r8
          asymtot(icol,k,ib)=0.0_r8
        end do
      enddo  
      do i=0,nmodes
        do k=1,pver
          do icol=1,ncol
           betot(icol,k,ib)=betot(icol,k,ib)+Nnatk(icol,k,i)*be(icol,k,i,ib)
           ssatot(icol,k,ib)=ssatot(icol,k,ib)+Nnatk(icol,k,i) &
                             *be(icol,k,i,ib)*ssa(icol,k,i,ib)           
           asymtot(icol,k,ib)=asymtot(icol,k,ib)+Nnatk(icol,k,i) &
                         *be(icol,k,i,ib)*ssa(icol,k,i,ib)*asym(icol,k,i,ib)
!       if(ib.eq.4) then
!         write(*,*) 'i, asym =', i, asym(icol,k,i,ib)
!         write(*,*) 'i, be =', i, be(icol,k,i,ib)
!         write(*,*) 'i, ssa =', i, ssa(icol,k,i,ib)
!       endif

          end do
        enddo
       enddo
       do k=1,pver
        do icol=1,ncol
          ssatot(icol,k,ib)=ssatot(icol,k,ib)/(betot(icol,k,ib)+eps)
          asymtot(icol,k,ib)=asymtot(icol,k,ib) &
                           /(betot(icol,k,ib)*ssatot(icol,k,ib)+eps)
          asydry_aer(icol,k)=asymtot(icol,k,ib)
        end do
       enddo
!
        call outfld('ASYMMDRY',asydry_aer,pcols,lchnk)
!

!ccccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5ccccccccc6ccccccccc7cc
#endif  ! AEROCOM

!     (Wet) Optical properties for each of the aerosol modes:

      lw_on = .true.  ! No LW optics needed for RH=0 (interpol returns 0-values)

 do iloop=1,1
!     BC(ax) mode (dry only):
      call interpol0 (lchnk, ncol, daylight, Nnatk, ssa, asym, be, ke, lw_on, kalw)

      mplus10=0
!     SO4/SOA(Ait) mode:                                         
      call interpol1 (lchnk, ncol, daylight, xrh, irh1, mplus10, &
                      Nnatk, xfombg, ifombg1, xct, ict1,    &
                      xfac, ifac1, ssa, asym, be, ke, lw_on, kalw)

!     BC(Ait) and OC(Ait) modes:
      call interpol2to3 (lchnk, ncol, daylight, xrh, irh1, mplus10, &
                         Nnatk, xct, ict1, xfac, ifac1, &
                         ssa, asym, be, ke, lw_on, kalw)

!     BC&OC(Ait) mode:   ------ fcm invalid here (=0). Using faitbc instead
      call interpol4 (lchnk, ncol, daylight, xrh, irh1, mplus10, &
                      Nnatk, xfbcbg, ifbcbg1, xct, ict1,    &
                      xfac, ifac1, xfaq, ifaq1, ssa, asym, be, ke, lw_on, kalw)

!     SO4(Ait75) (5), Mineral (6-7) and Sea-salt (8-10) modes:
      call interpol5to10 (lchnk, ncol, daylight, xrh, irh1, &
                          Nnatk, xct, ict1, xfac, ifac1, &
                          xfbc, ifbc1, xfaq, ifaq1, ssa, asym, be, ke, lw_on, kalw)
 enddo ! iloop


!       total aerosol number concentrations
        do i=0,nmodes    ! mode 0 to 14 
         do k=1,pver
          do icol=1,ncol
            n_aer(icol,k)=n_aer(icol,k)+Nnatk(icol,k,i)
          end do
         enddo  
        enddo
        call outfld('N_AER   ',n_aer ,pcols,lchnk)

 do iloop=1,1
      mplus10=1
!     SO4/SOA(Ait) mode: 
      !does no longer exist as an externally mixed mode

!     BC(Ait) and OC(Ait) modes:
      call interpol2to3 (lchnk, ncol, daylight, xrh, irh1, mplus10, &
                         Nnatk, xct, ict1, xfac, ifac1, &
                         ssa, asym, be, ke, lw_on, kalw)

!     BC&OC(n) mode:    ------ fcm not valid here (=0). Use fnbc instead
      call interpol4 (lchnk, ncol, daylight, xrh, irh1, mplus10, &
                      Nnatk, xfbcbgn, ifbcbgn1, xct, ict1,  &
                      xfac, ifac1, xfaq, ifaq1, ssa, asym, be, ke, lw_on, kalw)
 enddo ! iloop

!ccccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5ccccccccc6ccccccccc7cc
      do k=1,pver
        do icol=1,ncol
          Ctot(icol,k)=0.0_r8
        end do
      enddo  

      do i=0,nmodes    ! mode 0 to 14 
        do k=1,pver
          do icol=1,ncol
              dCtot(icol,k)=1.e3_r8*be(icol,k,i,4)/(ke(icol,k,i,4)+eps) 
              Ctot(icol,k)=Ctot(icol,k)+dCtot(icol,k)*Nnatk(icol,k,i) 
          end do
        enddo  
      enddo

#ifdef AEROCOM      
#ifdef COLTST4INTCONS
!     initializing modal mass column burdens 
      do icol=1,ncol
          cmdry0(icol)=0.0_r8
          cmdry1(icol)=0.0_r8
          cmdry2(icol)=0.0_r8
          cmdry4(icol)=0.0_r8
          cmdry5(icol)=0.0_r8
          cmdry6(icol)=0.0_r8
          cmdry7(icol)=0.0_r8
          cmdry8(icol)=0.0_r8
          cmdry9(icol)=0.0_r8
          cmdry10(icol)=0.0_r8
          cmdry12(icol)=0.0_r8
          cmdry14(icol)=0.0_r8
      enddo
#endif
!     Mass concentration (ug/m3) and mmr (kg/kg) of aerosol condensed water
      do k=1,pver
        do icol=1,ncol
          Cwater(icol,k)=Ctot(icol,k)-Ctotdry(icol,k)
          mmr_aerh2o(icol,k)=1.e-9_r8*Cwater(icol,k)/rhoda(icol,k)
#ifdef COLTST4INTCONS 
!         and dry mass column burdens for each mode/mixture
          deltah=deltah_km(icol,k)
          cmdry0(icol)=cmdry0(icol)+cmodedry(icol,k,0)*deltah
          cmdry1(icol)=cmdry1(icol)+cmodedry(icol,k,1)*deltah
          cmdry2(icol)=cmdry2(icol)+cmodedry(icol,k,2)*deltah
          cmdry4(icol)=cmdry4(icol)+cmodedry(icol,k,4)*deltah
          cmdry5(icol)=cmdry5(icol)+cmodedry(icol,k,5)*deltah
          cmdry6(icol)=cmdry6(icol)+cmodedry(icol,k,6)*deltah
          cmdry7(icol)=cmdry7(icol)+cmodedry(icol,k,7)*deltah
          cmdry8(icol)=cmdry8(icol)+cmodedry(icol,k,8)*deltah
          cmdry9(icol)=cmdry9(icol)+cmodedry(icol,k,9)*deltah
          cmdry10(icol)=cmdry10(icol)+cmodedry(icol,k,10)*deltah
          cmdry12(icol)=cmdry12(icol)+cmodedry(icol,k,12)*deltah
          cmdry14(icol)=cmdry14(icol)+cmodedry(icol,k,14)*deltah
#endif
        end do
      enddo  
#endif
!ccccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5ccccccccc6ccccccccc7cc
!     SW Optical properties of total aerosol:
      do ib=1,nbands
        do k=1,pver
         do icol=1,ncol
            betot(icol,k,ib)=0.0_r8
            ssatot(icol,k,ib)=0.0_r8
            asymtot(icol,k,ib)=0.0_r8
         end do
        enddo  
      enddo
      do ib=1,nbands
       do i=0,nmodes
        do k=1,pver
          do icol=1,ncol
           betot(icol,k,ib)=betot(icol,k,ib)+Nnatk(icol,k,i)*be(icol,k,i,ib)
           ssatot(icol,k,ib)=ssatot(icol,k,ib)+Nnatk(icol,k,i) &
                             *be(icol,k,i,ib)*ssa(icol,k,i,ib)           
           asymtot(icol,k,ib)=asymtot(icol,k,ib)+Nnatk(icol,k,i) &
                         *be(icol,k,i,ib)*ssa(icol,k,i,ib)*asym(icol,k,i,ib)
          end do
        enddo
       enddo
      enddo
!      Adding also the volcanic contribution (CMIP6), which is using a CMIP6
!      band numbering identical to the AeroTab numbering (unlike CAM) both
!      for SW and LW. I.e., no remapping is required here.
!   Info from CMIP_CAM6_radiation_v3.nc
! wl1_sun = 0.2, 0.263158, 0.344828, 0.441501, 0.625, 0.77821, 1.24224, 
!    1.2987, 1.62602, 1.94175, 2.15054, 2.5, 3.07692, 3.84615 ;
! wl2_sun = 0.263158, 0.344828, 0.441501, 0.625, 0.77821, 1.24224, 1.2987, 
!    1.62602, 1.94175, 2.15054, 2.5, 3.07692, 3.84615, 12.1951 ;
! wl1_earth = 3.07692, 3.84615, 4.20168, 4.44444, 4.80769, 5.55556, 6.75676, 
!    7.19424, 8.47458, 9.25926, 10.2041, 12.1951, 14.2857, 15.873, 20, 28.5714 ;
! wl2_earth = 3.84615, 4.20168, 4.44444, 4.80769, 5.55556, 6.75676, 7.19424, 
!    8.47458, 9.25926, 10.2041, 12.1951, 14.2857, 15.873, 20, 28.5714, 1000 ;
      do ib=1,nbands
         betot(1:ncol,1:pver,ib) = betot(1:ncol,1:pver,ib) &
             + volc_ext_sun(1:ncol,1:pver,ib)
         ssatot(1:ncol,1:pver,ib) = ssatot(1:ncol,1:pver,ib) &
             + volc_ext_sun(1:ncol,1:pver,ib)*volc_omega_sun(1:ncol,1:pver,ib)
         asymtot(1:ncol,1:pver,ib) = asymtot(1:ncol,1:pver,ib) &
             + volc_ext_sun(1:ncol,1:pver,ib)*volc_omega_sun(1:ncol,1:pver,ib) &
              *volc_g_sun(1:ncol,1:pver,ib)
      enddo
!akc6+
      bevisvolc(1:ncol,1:pver) = volc_ext_sun(1:ncol,1:pver,4)        
!akc6-
!     and then calculate the total bulk optical parameters
      do ib=1,nbands
       do k=1,pver
        do icol=1,ncol
          ssatot(icol,k,ib)=ssatot(icol,k,ib)/(betot(icol,k,ib)+eps)
          asymtot(icol,k,ib)=asymtot(icol,k,ib) &
                           /(betot(icol,k,ib)*ssatot(icol,k,ib)+eps)
        end do
       enddo
     enddo
         
!------------------------------------------------------------------------------------------------
! Replace CAM5 standard aerosol optics with CAM5-Oslo optics (except top layer: no aerosol)
! Remapping from AeroTab to CAM5 SW bands, see p. 167 in the CAM5.0 description:
!  CAM5 bands         AeroTab bands   
! 14 3.846 12.195        14 
! 1 3.077 3.846          13 
! 2 2.500 3.077          12
! 3 2.150 2.500          11
! 4 1.942 2.150          10
! 5 1.626 1.942           9
! 6 1.299 1.626           8
! 7 1.242 1.299           7
! 8 0.778 1.242           6
! 9 0.625 0.778           5
! 10 0.442 0.625          4
! 11 0.345 0.442          3
! 12 0.263 0.345          2
! 13 0.200 0.263          1
      
       do i=1,ncol  ! zero aerosol in the top layer
        do ib=1,14 ! 1-nbands
            per_tau(i,0,ib)= 0._r8
            per_tau_w(i,0,ib)= 0.999_r8
            per_tau_w_g(i,0,ib)= 0.5_r8
            per_tau_w_f(i,0,ib)= 0.25_r8
        end do
        do ib=1,14  ! initialize also for the other layers
          do k=1,pver
            per_tau(i,k,ib)= 0._r8
            per_tau_w(i,k,ib)= 0.999_r8
            per_tau_w_g(i,k,ib)= 0.5_r8
            per_tau_w_f(i,k,ib)= 0.25_r8
          end do
        end do
       end do
!      Remapping of SW wavelength bands from AeroTab to CAM5
       do i=1,ncol
        do ib=1,13
          do k=1,pver
            per_tau(i,k,ib)=deltah_km(i,k)*betot(i,k,14-ib)
            per_tau_w(i,k,ib)=per_tau(i,k,ib)*max(min(ssatot(i,k,14-ib),0.999999_r8),1.e-6_r8)
            per_tau_w_g(i,k,ib)=per_tau_w(i,k,ib)*asymtot(i,k,14-ib)
            per_tau_w_f(i,k,ib)=per_tau_w_g(i,k,ib)*asymtot(i,k,14-ib)
!tst
!       if(ib.eq.4.and.k.eq.pver.and.i.eq.1) then
!         write(*,*) 'per_tau =', per_tau(i,k,ib)
!         write(*,*) 'per_tau_w =', per_tau_w(i,k,ib)
!         write(*,*) 'per_tau_w_g =', per_tau_w_g(i,k,ib)
!       endif
!tst
          end do
        end do
          ib=14
          do k=1,pver
            per_tau(i,k,ib)=deltah_km(i,k)*betot(i,k,ib)
            per_tau_w(i,k,ib)=per_tau(i,k,ib)*max(min(ssatot(i,k,ib),0.999999_r8),1.e-6_r8)
            per_tau_w_g(i,k,ib)=per_tau_w(i,k,ib)*asymtot(i,k,ib)
            per_tau_w_f(i,k,ib)=per_tau_w_g(i,k,ib)*asymtot(i,k,ib)
          end do
      end do  ! ncol                  
!------------------------------------------------------------------------------------------------

!     LW Optical properties of total aerosol:
      do ib=1,nlwbands
        do k=1,pver
         do icol=1,ncol
            batotlw(icol,k,ib)=0.0_r8
          end do
        enddo  
      enddo
      do ib=1,nlwbands
       do i=0,nmodes
        do k=1,pver
          do icol=1,ncol
           balw(icol,k,i,ib)=kalw(icol,k,i,ib)*(be(icol,k,i,4)/(ke(icol,k,i,4)+eps))
           batotlw(icol,k,ib)=batotlw(icol,k,ib)+Nnatk(icol,k,i)*balw(icol,k,i,ib)
          end do
        enddo
       enddo
      enddo

!     Adding also the volcanic contribution (CMIP6), which is also using
!     AeroTab band numbering, so that a remapping is required here 
      do ib=1,nlwbands
        volc_balw(1:ncol,1:pver,ib) = volc_ext_earth(:ncol,1:pver,ib) &
                              *(1.0_r8-volc_omega_earth(:ncol,1:pver,ib))
        batotlw(1:ncol,1:pver,ib)=batotlw(1:ncol,1:pver,ib)+volc_balw(1:ncol,1:pver,ib)
      enddo     

!     Remapping of LW wavelength bands from AeroTab to CAM5 
      do ib=1,nlwbands
       do i=1,ncol
        do k=1,pver
         per_lw_abs(i,k,ib)=deltah_km(i,k)*batotlw(i,k,17-ib) 
!       if(ib.eq.1.and.k.eq.pver.and.i.eq.1) then
!         write(*,*) 'per_lw_abs =', per_lw_abs(i,k,ib)
!       endif
        end do
      end do
     end do

#ifdef AEROCOM
       do i=1,ncol 
        do k=1,pver 
          batotsw13(i,k)=betot(i,k,13)*(1.0_r8-ssatot(i,k,13))
          batotlw01(i,k)=batotlw(i,k,1)
        end do
      end do
!    These two fields should be close to equal, both representing absorption 
!    in the 3.077-3.846 um wavelenght band (i.e., a check of LUT for LW vs. SW).
     call outfld('BATSW13 ',batotsw13,pcols,lchnk)
     call outfld('BATLW01 ',batotlw01,pcols,lchnk)
#endif

#ifdef COLTST4INTCONS
!     initialize modal optical extinctions
      do k=1,pver
        do icol=1,ncol
          bekc0(icol,k)=0.0_r8
          bekc1(icol,k)=0.0_r8
          bekc2(icol,k)=0.0_r8
          bekc4(icol,k)=0.0_r8
          bekc5(icol,k)=0.0_r8
          bekc6(icol,k)=0.0_r8
          bekc7(icol,k)=0.0_r8
          bekc8(icol,k)=0.0_r8
          bekc9(icol,k)=0.0_r8
          bekc10(icol,k)=0.0_r8
          bekc12(icol,k)=0.0_r8
          bekc14(icol,k)=0.0_r8
!
          kekc0(icol,k)=0.0_r8
          kekc1(icol,k)=0.0_r8
          kekc2(icol,k)=0.0_r8
          kekc4(icol,k)=0.0_r8
          kekc5(icol,k)=0.0_r8
          kekc6(icol,k)=0.0_r8
          kekc7(icol,k)=0.0_r8
          kekc8(icol,k)=0.0_r8
          kekc9(icol,k)=0.0_r8
          kekc10(icol,k)=0.0_r8
          kekc12(icol,k)=0.0_r8
          kekc14(icol,k)=0.0_r8
        end do
      enddo  
!     optical depth (in band 4 = vis.) for each of the modes
      do k=1,pver
        do icol=1,ncol
          bekc0(icol,k) =Nnatk(icol,k,0) *be(icol,k,0,4)
          bekc1(icol,k) =Nnatk(icol,k,1) *be(icol,k,1,4)
          bekc2(icol,k) =Nnatk(icol,k,2) *be(icol,k,2,4)
          bekc4(icol,k) =Nnatk(icol,k,4) *be(icol,k,4,4)
          bekc5(icol,k) =Nnatk(icol,k,5) *be(icol,k,5,4)
          bekc6(icol,k) =Nnatk(icol,k,6) *be(icol,k,6,4)
          bekc7(icol,k) =Nnatk(icol,k,7) *be(icol,k,7,4)
          bekc8(icol,k) =Nnatk(icol,k,8) *be(icol,k,8,4)
          bekc9(icol,k) =Nnatk(icol,k,9) *be(icol,k,9,4)
          bekc10(icol,k)=Nnatk(icol,k,10)*be(icol,k,10,4)
          bekc12(icol,k)=Nnatk(icol,k,12)*be(icol,k,12,4)
          bekc14(icol,k)=Nnatk(icol,k,14)*be(icol,k,14,4)
!
          kekc0(icol,k) =ke(icol,k,0,4)
          kekc1(icol,k) =ke(icol,k,1,4)
          kekc2(icol,k) =ke(icol,k,2,4)
          kekc4(icol,k) =ke(icol,k,4,4)
          kekc5(icol,k) =ke(icol,k,5,4)
          kekc6(icol,k) =ke(icol,k,6,4)
          kekc7(icol,k) =ke(icol,k,7,4)
          kekc8(icol,k) =ke(icol,k,8,4)
          kekc9(icol,k) =ke(icol,k,9,4)
          kekc10(icol,k)=ke(icol,k,10,4)
          kekc12(icol,k)=ke(icol,k,12,4)
          kekc14(icol,k)=ke(icol,k,14,4)
        end do
      enddo
#endif


!ccccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5ccccccccc6ccccccccc7cc
!     APPROXIMATE aerosol extinction and absorption at 550nm (0.442-0.625 um)
!     (in the visible wavelength band)
      do k=1,pver
        do icol=1,ncol
          betotvis(icol,k)=betot(icol,k,4)
          batotvis(icol,k)=betotvis(icol,k)*(1.0-ssatot(icol,k,4))
        end do
      enddo  
!ccccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5ccccccccc6ccccccccc7cc

      do k=1,pver
        do icol=1,ncol
          ssavis(icol,k) = 0.0_r8
          asymmvis(icol,k) = 0.0_r8
          extvis(icol,k) = 0.0_r8
          dayfoc(icol,k) = 0.0_r8 
        enddo
      end do

      do k=1,pver
        do icol=1,ncol
!           dayfoc < 1 when looping only over gridcells with daylight 
          if(daylight(icol)) then
            dayfoc(icol,k) = 1.0_r8
!     with the new bands in CAM5, band 4 is now at ca 0.5 um (0.442-0.625)
            ssavis(icol,k) = ssatot(icol,k,4)
            asymmvis(icol,k) = asymtot(icol,k,4)
            extvis(icol,k) = betot(icol,k,4)
          endif 
        enddo
      end do

!       optical parameters in visible light (0.442-0.625um)
        call outfld('SSAVIS  ',ssavis,pcols,lchnk)
        call outfld('ASYMMVIS',asymmvis,pcols,lchnk)
        call outfld('EXTVIS  ',extvis,pcols,lchnk)
        call outfld('DAYFOC  ',dayfoc,pcols,lchnk)

!       Initialize fields
        do icol=1,ncol
!          akso4c(icol)=0.0_r8
!          akbcc(icol)=0.0_r8
!          akocc(icol)=0.0_r8
          aodvis(icol)=0.0_r8
          absvis(icol)=0.0_r8
          aodvisvolc(icol)=0.0_r8
          absvisvolc(icol)=0.0_r8
          airmass(icol)=0.0_r8  !akc6
#ifdef COLTST4INTCONS 
          taukc0(icol)=0.0_r8
          taukc1(icol)=0.0_r8
          taukc2(icol)=0.0_r8
!          taukc3(icol)=0.0_r8
          taukc4(icol)=0.0_r8
          taukc5(icol)=0.0_r8
          taukc6(icol)=0.0_r8
          taukc7(icol)=0.0_r8
          taukc8(icol)=0.0_r8
          taukc9(icol)=0.0_r8
          taukc10(icol)=0.0_r8
!          taukc11(icol)=0.0_r8
          taukc12(icol)=0.0_r8
!          taukc13(icol)=0.0_r8
          taukc14(icol)=0.0_r8
#endif
       enddo
       
        do icol=1,ncol
         if(daylight(icol)) then
         do k=1,pver
!         Layer thickness, unit km, and layer airmass, unit kg/m2
          deltah=deltah_km(icol,k)
!akc6          airmass(icol,k)=1.e3_r8*deltah*rhoda(icol,k)
          airmassl(icol,k)=1.e3_r8*deltah*rhoda(icol,k)
          airmass(icol)=airmass(icol)+airmassl(icol,k)  !akc6
!          Optical depths at ca. 550 nm (0.442-0.625um) all aerosols
!tst
!          aodvis3d(icol,k)=betotvis(icol,k)*deltah
!tst
          aodvis(icol)=aodvis(icol)+betotvis(icol,k)*deltah
          absvis(icol)=absvis(icol)+batotvis(icol,k)*deltah
!          Optical depths at ca. 550 nm (0.442-0.625um) CMIP6 volcanic aerosol
          aodvisvolc(icol)=aodvisvolc(icol)+volc_ext_sun(icol,k,4)*deltah
          absvisvolc(icol)=absvisvolc(icol)+volc_ext_sun(icol,k,4) &
                                *(1.0_r8-volc_omega_sun(icol,k,4))*deltah
#ifdef COLTST4INTCONS 
!         To check internal consistency of these AOD calculations, make
!         sure that sum_i(taukc_i)=aodvis (tested to be ok on 7/1-2016).
!         Note that this will not be the case when CMIP6 volcanic forcing
!         as optical properties are included, since this comes "on top of"
!         the mixtures 0-14 below.          
          taukc0(icol) =taukc0(icol) +bekc0(icol,k)*deltah
          taukc1(icol) =taukc1(icol) +bekc1(icol,k)*deltah
          taukc2(icol) =taukc2(icol) +bekc2(icol,k)*deltah
          taukc4(icol) =taukc4(icol) +bekc4(icol,k)*deltah
          taukc5(icol) =taukc5(icol) +bekc5(icol,k)*deltah
          taukc6(icol) =taukc6(icol) +bekc6(icol,k)*deltah
          taukc7(icol) =taukc7(icol) +bekc7(icol,k)*deltah
          taukc8(icol) =taukc8(icol) +bekc8(icol,k)*deltah
          taukc9(icol) =taukc9(icol) +bekc9(icol,k)*deltah
          taukc10(icol)=taukc10(icol)+bekc10(icol,k)*deltah
          taukc12(icol)=taukc12(icol)+bekc12(icol,k)*deltah
          taukc14(icol)=taukc14(icol)+bekc14(icol,k)*deltah
#endif
         end do  ! k
         endif   ! daylight         
        end do   ! icol
      
!       Extinction and absorption for 0.55 um for the total aerosol, and AODs 
#ifdef AEROCOM
        call outfld('BETOTVIS',betotvis,pcols,lchnk)
        call outfld('BATOTVIS',batotvis,pcols,lchnk)
#endif
!        call outfld('AODVIS  ',aodvis ,pcols,lchnk)
        call outfld('AOD_VIS ',aodvis ,pcols,lchnk)
        call outfld('ABSVIS  ',absvis ,pcols,lchnk)
        call outfld('AODVVOLC',aodvisvolc ,pcols,lchnk)
        call outfld('ABSVVOLC',absvisvolc ,pcols,lchnk)
!akc6+
        call outfld('BVISVOLC',bevisvolc ,pcols,lchnk)
!akc6-
!tst
!        call outfld('AODVIS3D',aodvis3d,pcols,lchnk)
!tst
#ifdef COLTST4INTCONS 
        call outfld('TAUKC0  ',taukc0 ,pcols,lchnk)
        call outfld('TAUKC1  ',taukc1 ,pcols,lchnk)
        call outfld('TAUKC2  ',taukc2 ,pcols,lchnk)
        call outfld('TAUKC4  ',taukc4 ,pcols,lchnk)
        call outfld('TAUKC5  ',taukc5 ,pcols,lchnk)
        call outfld('TAUKC6  ',taukc6 ,pcols,lchnk)
        call outfld('TAUKC7  ',taukc7 ,pcols,lchnk)
        call outfld('TAUKC8  ',taukc8 ,pcols,lchnk)
        call outfld('TAUKC9  ',taukc9 ,pcols,lchnk)
        call outfld('TAUKC10 ',taukc10,pcols,lchnk)
        call outfld('TAUKC12 ',taukc12,pcols,lchnk)
        call outfld('TAUKC14 ',taukc14,pcols,lchnk)
!
        call outfld('MECKC0  ',kekc0  ,pcols,lchnk)
        call outfld('MECKC1  ',kekc1  ,pcols,lchnk)
        call outfld('MECKC2  ',kekc2  ,pcols,lchnk)
        call outfld('MECKC4  ',kekc4  ,pcols,lchnk)
        call outfld('MECKC5  ',kekc5  ,pcols,lchnk)
        call outfld('MECKC6  ',kekc6  ,pcols,lchnk)
        call outfld('MECKC7  ',kekc7  ,pcols,lchnk)
        call outfld('MECKC8  ',kekc8  ,pcols,lchnk)
        call outfld('MECKC9  ',kekc9  ,pcols,lchnk)
        call outfld('MECKC10 ',kekc10 ,pcols,lchnk)
        call outfld('MECKC12 ',kekc12 ,pcols,lchnk)
        call outfld('MECKC14 ',kekc14 ,pcols,lchnk)
#endif

#ifdef AEROCOM  ! AEROCOM***********AEROCOM**************AEROCOM***************below   

!        call outfld('BEKC4   ',bekc4   ,pcols,lchnk)

!       Initialize fields
        do icol=1,ncol
          daerh2o(icol)=0.0_r8
          vaercols(icol)=0.0_r8
          vaercoll(icol)=0.0_r8
          aaercols(icol)=0.0_r8
          aaercoll(icol)=0.0_r8
          do i=0,nmodes
            dload(icol,i)=0.0_r8
          enddo
        enddo
     bext550n(:,:,:) = 0._r8
     babs550n(:,:,:) = 0._r8
     bext440n(:,:,:) = 0._r8
     babs440n(:,:,:) = 0._r8
     bext870n(:,:,:) = 0._r8
     babs870n(:,:,:) = 0._r8
     babs500n(:,:,:) = 0._r8
     babs670n(:,:,:) = 0._r8
        vnbcarr(:,:) =0.0_r8
        vaitbcarr(:,:) =0.0_r8
        cknorm(:,:,:) =0.0_r8
!000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000

!     AeroCom diagnostics requiring table look-ups with ambient RH. 

      do irf=0,0
        call opticsAtConstRh(lchnk, ncol, pint, rhoda, Nnatk, xrh, irh1, irf, &
           xct, ict1, xfaq, ifaq1, xfbcbg, ifbcbg1,           &
           xfbcbgn, ifbcbgn1, xfac, ifac1, xfbc, ifbc1,       &
           xfombg, ifombg1, vnbcarr, vaitbcarr, v_soana,      &
           bext440, bext500, bext550, bext670, bext870,       &
           bebg440, bebg500, bebg550, bebg670, bebg870,       &
           bebc440, bebc500, bebc550, bebc670, bebc870,       &
           beoc440, beoc500, beoc550, beoc670, beoc870,       &
           besu440, besu500, besu550, besu670, besu870,       &
           babs440, babs500, babs550, babs670, babs870,       &
           bebglt1, bebggt1, bebclt1, bebcgt1,                &
           beoclt1, beocgt1, bes4lt1, bes4gt1,                &
           backsc550, babg550, babc550, baoc550, basu550,     & 
           bext440n, bext500n, bext550n, bext670n, bext870n,  &
           bebg440n, bebg500n, bebg550n, bebg670n, bebg870n,  &
           bebc440n, bebc500n, bebc550n, bebc670n, bebc870n,  &
           beoc440n, beoc500n, beoc550n, beoc670n, beoc870n,  &
           besu440n, besu500n, besu550n, besu670n, besu870n,  &
           babs440n, babs500n, babs550n, babs670n, babs870n,  &
           bebglt1n, bebggt1n, bebclt1n, bebcgt1n,            &
           beoclt1n, beocgt1n, bes4lt1n, bes4gt1n,            &
           backsc550n, babg550n, babc550n, baoc550n, basu550n)
      end do ! irf

!000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000

        do k=1,pver
         do icol=1,ncol

         bebglt1t(icol,k)=0.0_r8
         bebggt1t(icol,k)=0.0_r8
         bebclt1t(icol,k)=0.0_r8
         bebcgt1t(icol,k)=0.0_r8
         beoclt1t(icol,k)=0.0_r8
         beocgt1t(icol,k)=0.0_r8
         bes4lt1t(icol,k)=0.0_r8
         bes4gt1t(icol,k)=0.0_r8 
         bedustlt1(icol,k)=0.0_r8
         bedustgt1(icol,k)=0.0_r8
         besslt1(icol,k)=0.0_r8
         bessgt1(icol,k)=0.0_r8

         bext440tot(icol,k)=0.0_r8 
         babs440tot(icol,k)=0.0_r8 
         bext500tot(icol,k)=0.0_r8 
         babs500tot(icol,k)=0.0_r8 
         bext550tot(icol,k)=0.0_r8 
         babs550tot(icol,k)=0.0_r8 
         bext670tot(icol,k)=0.0_r8 
         babs670tot(icol,k)=0.0_r8 
         bext870tot(icol,k)=0.0_r8 
         babs870tot(icol,k)=0.0_r8 

         backsc550tot(icol,k)=0.0_r8 
 
         bebg440tot(icol,k)=0.0_r8 
!         babg440tot(icol,k)=0.0_r8 
         bebg500tot(icol,k)=0.0_r8 
!         babg500tot(icol,k)=0.0_r8 
         bebg550tot(icol,k)=0.0_r8 
         babg550tot(icol,k)=0.0_r8 
         bebg670tot(icol,k)=0.0_r8 
!         babg670tot(icol,k)=0.0_r8 
         bebg870tot(icol,k)=0.0_r8 
!         babg870tot(icol,k)=0.0_r8 

         bebc440tot(icol,k)=0.0_r8 
!         babc440tot(icol,k)=0.0_r8 
         bebc500tot(icol,k)=0.0_r8 
!         babc500tot(icol,k)=0.0_r8 
         bebc550tot(icol,k)=0.0_r8 
         babc550tot(icol,k)=0.0_r8 
         bebc670tot(icol,k)=0.0_r8 
!         babc670tot(icol,k)=0.0_r8 
         bebc870tot(icol,k)=0.0_r8 
!         babc870tot(icol,k)=0.0_r8 

         beoc440tot(icol,k)=0.0_r8 
!         baoc440tot(icol,k)=0.0_r8 
         beoc500tot(icol,k)=0.0_r8 
!         baoc500tot(icol,k)=0.0_r8 
         beoc550tot(icol,k)=0.0_r8 
         baoc550tot(icol,k)=0.0_r8 
         beoc670tot(icol,k)=0.0_r8 
!         baoc670tot(icol,k)=0.0_r8 
         beoc870tot(icol,k)=0.0_r8 
!         baoc870tot(icol,k)=0.0_r8 

         besu440tot(icol,k)=0.0_r8 
!         basu440tot(icol,k)=0.0_r8 
         besu500tot(icol,k)=0.0_r8 
!         basu500tot(icol,k)=0.0_r8 
         besu550tot(icol,k)=0.0_r8 
         basu550tot(icol,k)=0.0_r8 
         besu670tot(icol,k)=0.0_r8 
!         basu670tot(icol,k)=0.0_r8 
         besu870tot(icol,k)=0.0_r8 
!         basu870tot(icol,k)=0.0_r8 

         enddo
        enddo

        do i=0,nbmodes
         do k=1,pver
          do icol=1,ncol
!      total internal extinction and absorption for 0.44, 0.50, 0.55, 0.68 and 0.87 um
       bext440tot(icol,k)=bext440tot(icol,k)+Nnatk(icol,k,i)*bext440(icol,k,i)
       babs440tot(icol,k)=babs440tot(icol,k)+Nnatk(icol,k,i)*babs440(icol,k,i)
       bext500tot(icol,k)=bext500tot(icol,k)+Nnatk(icol,k,i)*bext500(icol,k,i)
       babs500tot(icol,k)=babs500tot(icol,k)+Nnatk(icol,k,i)*babs500(icol,k,i)
       bext550tot(icol,k)=bext550tot(icol,k)+Nnatk(icol,k,i)*bext550(icol,k,i)
       babs550tot(icol,k)=babs550tot(icol,k)+Nnatk(icol,k,i)*babs550(icol,k,i)
       bext670tot(icol,k)=bext670tot(icol,k)+Nnatk(icol,k,i)*bext670(icol,k,i)
       babs670tot(icol,k)=babs670tot(icol,k)+Nnatk(icol,k,i)*babs670(icol,k,i)
       bext870tot(icol,k)=bext870tot(icol,k)+Nnatk(icol,k,i)*bext870(icol,k,i)
       babs870tot(icol,k)=babs870tot(icol,k)+Nnatk(icol,k,i)*babs870(icol,k,i)
       backsc550tot(icol,k)=backsc550tot(icol,k)+Nnatk(icol,k,i)*backsc550(icol,k,i)
!      extinction and absorption for 0.44, 0.50, 0.55 (no abs), 0.68 and 0.87 um
!      for the whole background aerosol (icluding SO4,BC, and OC for modes 0-5)
       bebg440tot(icol,k)=bebg440tot(icol,k)+Nnatk(icol,k,i)*bebg440(icol,k,i)
!       babg440tot(icol,k)=babg440tot(icol,k)+Nnatk(icol,k,i)*babg440(icol,k,i)
       bebg500tot(icol,k)=bebg500tot(icol,k)+Nnatk(icol,k,i)*bebg500(icol,k,i)
!       babg500tot(icol,k)=babg500tot(icol,k)+Nnatk(icol,k,i)*babg500(icol,k,i)
       bebg550tot(icol,k)=bebg550tot(icol,k)+Nnatk(icol,k,i)*bebg550(icol,k,i)
       babg550tot(icol,k)=babg550tot(icol,k)+Nnatk(icol,k,i)*babg550(icol,k,i)
       bebg670tot(icol,k)=bebg670tot(icol,k)+Nnatk(icol,k,i)*bebg670(icol,k,i)
!       babg670tot(icol,k)=babg670tot(icol,k)+Nnatk(icol,k,i)*babg670(icol,k,i)
       bebg870tot(icol,k)=bebg870tot(icol,k)+Nnatk(icol,k,i)*bebg870(icol,k,i)
!       babg870tot(icol,k)=babg870tot(icol,k)+Nnatk(icol,k,i)*babg870(icol,k,i)
!      extinction and absorption for 0.44, 0.50, 0.55 (no abs), 0.68 and 0.87 um
!      for each added (internally mixed through Aq./cond./coag.) component (SO4,BC,OC).
!      Condensed/coagulated SO4 on all modes 1-10, and wet-phase SO4 on modes 4-10:
       besu440tot(icol,k)=besu440tot(icol,k)+Nnatk(icol,k,i)*besu440(icol,k,i)
!       basu440tot(icol,k)=basu440tot(icol,k)+Nnatk(icol,k,i)*basu440(icol,k,i)
       besu500tot(icol,k)=besu500tot(icol,k)+Nnatk(icol,k,i)*besu500(icol,k,i)
!       basu500tot(icol,k)=basu500tot(icol,k)+Nnatk(icol,k,i)*basu500(icol,k,i)
       besu550tot(icol,k)=besu550tot(icol,k)+Nnatk(icol,k,i)*besu550(icol,k,i)
       basu550tot(icol,k)=basu550tot(icol,k)+Nnatk(icol,k,i)*basu550(icol,k,i)
       besu670tot(icol,k)=besu670tot(icol,k)+Nnatk(icol,k,i)*besu670(icol,k,i)
!       basu670tot(icol,k)=basu670tot(icol,k)+Nnatk(icol,k,i)*basu670(icol,k,i)
       besu870tot(icol,k)=besu870tot(icol,k)+Nnatk(icol,k,i)*besu870(icol,k,i)
!       basu870tot(icol,k)=basu870tot(icol,k)+Nnatk(icol,k,i)*basu870(icol,k,i)
!
!      Condensed OC on modes 1-4 and coagulated BC and OC on modes 5-10:
       if(i>=1) then
       bebc440tot(icol,k)=bebc440tot(icol,k)+Nnatk(icol,k,i)*bebc440(icol,k,i)
!       babc440tot(icol,k)=babc440tot(icol,k)+Nnatk(icol,k,i)*babc440(icol,k,i)
       bebc500tot(icol,k)=bebc500tot(icol,k)+Nnatk(icol,k,i)*bebc500(icol,k,i)
!       babc500tot(icol,k)=babc500tot(icol,k)+Nnatk(icol,k,i)*babc500(icol,k,i)
       bebc550tot(icol,k)=bebc550tot(icol,k)+Nnatk(icol,k,i)*bebc550(icol,k,i)
       babc550tot(icol,k)=babc550tot(icol,k)+Nnatk(icol,k,i)*babc550(icol,k,i)
       bebc670tot(icol,k)=bebc670tot(icol,k)+Nnatk(icol,k,i)*bebc670(icol,k,i)
!       babc670tot(icol,k)=babc670tot(icol,k)+Nnatk(icol,k,i)*babc670(icol,k,i)
       bebc870tot(icol,k)=bebc870tot(icol,k)+Nnatk(icol,k,i)*bebc870(icol,k,i)
!       babc870tot(icol,k)=babc870tot(icol,k)+Nnatk(icol,k,i)*babc870(icol,k,i)
       beoc440tot(icol,k)=beoc440tot(icol,k)+Nnatk(icol,k,i)*beoc440(icol,k,i)
!       baoc440tot(icol,k)=baoc440tot(icol,k)+Nnatk(icol,k,i)*baoc440(icol,k,i)
       beoc500tot(icol,k)=beoc500tot(icol,k)+Nnatk(icol,k,i)*beoc500(icol,k,i)
!       baoc500tot(icol,k)=baoc500tot(icol,k)+Nnatk(icol,k,i)*baoc500(icol,k,i)
       beoc550tot(icol,k)=beoc550tot(icol,k)+Nnatk(icol,k,i)*beoc550(icol,k,i)
       baoc550tot(icol,k)=baoc550tot(icol,k)+Nnatk(icol,k,i)*baoc550(icol,k,i)
       beoc670tot(icol,k)=beoc670tot(icol,k)+Nnatk(icol,k,i)*beoc670(icol,k,i)
!       baoc670tot(icol,k)=baoc670tot(icol,k)+Nnatk(icol,k,i)*baoc670(icol,k,i)
       beoc870tot(icol,k)=beoc870tot(icol,k)+Nnatk(icol,k,i)*beoc870(icol,k,i)
!       baoc870tot(icol,k)=baoc870tot(icol,k)+Nnatk(icol,k,i)*baoc870(icol,k,i)
       endif  ! i>=1
       if(i==6.or.i==7) then
         bedustlt1(icol,k)=bedustlt1(icol,k) &
                        +Nnatk(icol,k,i)*bebglt1(icol,k,i)
         bedustgt1(icol,k)=bedustgt1(icol,k) &
                        +Nnatk(icol,k,i)*bebggt1(icol,k,i)
       elseif(i>=8.and.i<=10) then
         besslt1(icol,k)=besslt1(icol,k) &
                        +Nnatk(icol,k,i)*bebglt1(icol,k,i)
         bessgt1(icol,k)=bessgt1(icol,k) &
                        +Nnatk(icol,k,i)*bebggt1(icol,k,i)
       endif
!      Condensed/coagulated SO4 on all modes 1-10, and wet-phase SO4 on modes 4-10:
         bes4lt1t(icol,k)=bes4lt1t(icol,k) &
                        +Nnatk(icol,k,i)*bes4lt1(icol,k,i)
         bes4gt1t(icol,k)=bes4gt1t(icol,k) &
                        +Nnatk(icol,k,i)*bes4gt1(icol,k,i)
!      Condensed OC on mode 1 and coagulated BC and OC on modes 5-10:
       if(i>=1) then
         bebclt1t(icol,k)=bebclt1t(icol,k) &
                        +Nnatk(icol,k,i)*bebclt1(icol,k,i)
         bebcgt1t(icol,k)=bebcgt1t(icol,k) &
                        +Nnatk(icol,k,i)*bebcgt1(icol,k,i)
         beoclt1t(icol,k)=beoclt1t(icol,k) &
                        +Nnatk(icol,k,i)*beoclt1(icol,k,i)
         beocgt1t(icol,k)=beocgt1t(icol,k) &
                        +Nnatk(icol,k,i)*beocgt1(icol,k,i)
       endif   ! i>=1
          end do   ! icol
         enddo     ! k
        enddo      ! i

!      extinction/absorptions (km-1) for each background component 
!      in the internal mixture are
        do k=1,pver
          do icol=1,ncol
            bint440du(icol,k)=Nnatk(icol,k,6)*bebg440(icol,k,6) &
                             +Nnatk(icol,k,7)*bebg440(icol,k,7)
            bint500du(icol,k)=Nnatk(icol,k,6)*bebg500(icol,k,6) &
                             +Nnatk(icol,k,7)*bebg500(icol,k,7)
            bint550du(icol,k)=Nnatk(icol,k,6)*bebg550(icol,k,6) &
                             +Nnatk(icol,k,7)*bebg550(icol,k,7)
            bint670du(icol,k)=Nnatk(icol,k,6)*bebg670(icol,k,6) &
                             +Nnatk(icol,k,7)*bebg670(icol,k,7)
            bint870du(icol,k)=Nnatk(icol,k,6)*bebg870(icol,k,6) &
                             +Nnatk(icol,k,7)*bebg870(icol,k,7)
            bint440ss(icol,k)=Nnatk(icol,k,8)*bebg440(icol,k,8) &
                             +Nnatk(icol,k,9)*bebg440(icol,k,9) &
                            +Nnatk(icol,k,10)*bebg440(icol,k,10)
            bint500ss(icol,k)=Nnatk(icol,k,8)*bebg500(icol,k,8) &
                             +Nnatk(icol,k,9)*bebg500(icol,k,9) &
                            +Nnatk(icol,k,10)*bebg500(icol,k,10)
            bint550ss(icol,k)=Nnatk(icol,k,8)*bebg550(icol,k,8) &
                             +Nnatk(icol,k,9)*bebg550(icol,k,9) &
                            +Nnatk(icol,k,10)*bebg550(icol,k,10)
            bint670ss(icol,k)=Nnatk(icol,k,8)*bebg670(icol,k,8) &
                             +Nnatk(icol,k,9)*bebg670(icol,k,9) &
                            +Nnatk(icol,k,10)*bebg670(icol,k,10)
            bint870ss(icol,k)=Nnatk(icol,k,8)*bebg870(icol,k,8) &
                             +Nnatk(icol,k,9)*bebg870(icol,k,9) &
                            +Nnatk(icol,k,10)*bebg870(icol,k,10)
            baint550du(icol,k)=Nnatk(icol,k,6)*babg550(icol,k,6) &
                             +Nnatk(icol,k,7)*babg550(icol,k,7)
            baint550ss(icol,k)=Nnatk(icol,k,8)*babg550(icol,k,8) &
                             +Nnatk(icol,k,9)*babg550(icol,k,9) &
                            +Nnatk(icol,k,10)*babg550(icol,k,10)
          end do
        enddo

      do i=11,14
       do k=1,pver
        do icol=1,ncol
          be440x(icol,k,i)=bext440n(icol,k,i-10)
          ba440x(icol,k,i)=babs440n(icol,k,i-10)
          be500x(icol,k,i)=bext500n(icol,k,i-10)
          ba500x(icol,k,i)=babs500n(icol,k,i-10)
          be550x(icol,k,i)=bext550n(icol,k,i-10)
          ba550x(icol,k,i)=babs550n(icol,k,i-10)
          be670x(icol,k,i)=bext670n(icol,k,i-10)
          ba670x(icol,k,i)=babs670n(icol,k,i-10)
          be870x(icol,k,i)=bext870n(icol,k,i-10)
          ba870x(icol,k,i)=babs870n(icol,k,i-10)
          belt1x(icol,k,i)=bebglt1n(icol,k,i-10)
          begt1x(icol,k,i)=bebggt1n(icol,k,i-10)
          backsc550x(icol,k,i)=backsc550n(icol,k,i-10)
        end do
       enddo
      enddo

!     The externally modes' contribution to extinction and absorption:
         do k=1,pver
          do icol=1,ncol

!BC
            vnbcarr(icol,k) = fnbc(icol,k)/(fnbc(icol,k) &
                           +(1.0_r8-fnbc(icol,k))*rhopart(l_bc_ni)/rhopart(l_om_ni))
            vnbc = vnbcarr(icol,k)            
            bebc440xt(icol,k) =Nnatk(icol,k,12)*be440x(icol,k,12)  &
                         +vnbc*Nnatk(icol,k,14)*be440x(icol,k,14)
            babc440xt(icol,k) =Nnatk(icol,k,12)*ba440x(icol,k,12)  &
                         +vnbc*Nnatk(icol,k,14)*ba440x(icol,k,14)
            bebc500xt(icol,k) =Nnatk(icol,k,12)*be500x(icol,k,12)  &
                         +vnbc*Nnatk(icol,k,14)*be500x(icol,k,14)
            babc500xt(icol,k) =Nnatk(icol,k,12)*ba500x(icol,k,12)  &
                         +vnbc*Nnatk(icol,k,14)*ba500x(icol,k,14)
            bebc550xt(icol,k) =Nnatk(icol,k,12)*be550x(icol,k,12)  &
                         +vnbc*Nnatk(icol,k,14)*be550x(icol,k,14)
            babc550xt(icol,k) =Nnatk(icol,k,12)*ba550x(icol,k,12)  &
                         +vnbc*Nnatk(icol,k,14)*ba550x(icol,k,14)
            bebc670xt(icol,k) =Nnatk(icol,k,12)*be670x(icol,k,12)  &
                         +vnbc*Nnatk(icol,k,14)*be670x(icol,k,14)
            babc670xt(icol,k) =Nnatk(icol,k,12)*ba670x(icol,k,12)  &
                         +vnbc*Nnatk(icol,k,14)*ba670x(icol,k,14)
            bebc870xt(icol,k) =Nnatk(icol,k,12)*be870x(icol,k,12)  &
                         +vnbc*Nnatk(icol,k,14)*be870x(icol,k,14)
            babc870xt(icol,k) =Nnatk(icol,k,12)*ba870x(icol,k,12)  &
                         +vnbc*Nnatk(icol,k,14)*ba870x(icol,k,14)
            bbclt1xt(icol,k)=Nnatk(icol,k,12)*belt1x(icol,k,12) &
                       +vnbc*Nnatk(icol,k,14)*belt1x(icol,k,14)
            bbcgt1xt(icol,k)=Nnatk(icol,k,12)*begt1x(icol,k,12) &
                       +vnbc*Nnatk(icol,k,14)*begt1x(icol,k,14)
!OC
            beoc440xt(icol,k) = &
                  +(1.0_r8-vnbc)*Nnatk(icol,k,14)*be440x(icol,k,14) 
            baoc440xt(icol,k) = &
                  +(1.0_r8-vnbc)*Nnatk(icol,k,14)*ba440x(icol,k,14) 
            beoc500xt(icol,k) = &
                  +(1.0_r8-vnbc)*Nnatk(icol,k,14)*be500x(icol,k,14) 
            baoc500xt(icol,k) = &
                  +(1.0_r8-vnbc)*Nnatk(icol,k,14)*ba500x(icol,k,14) 
            beoc550xt(icol,k) = &
                  +(1.0_r8-vnbc)*Nnatk(icol,k,14)*be550x(icol,k,14) 
            baoc550xt(icol,k) = &
                  +(1.0_r8-vnbc)*Nnatk(icol,k,14)*ba550x(icol,k,14) 
            beoc670xt(icol,k) = &
                  +(1.0_r8-vnbc)*Nnatk(icol,k,14)*be670x(icol,k,14) 
            baoc670xt(icol,k) = &
                  +(1.0_r8-vnbc)*Nnatk(icol,k,14)*ba670x(icol,k,14) 
            beoc870xt(icol,k) = &
                  +(1.0_r8-vnbc)*Nnatk(icol,k,14)*be870x(icol,k,14) 
            baoc870xt(icol,k) = &
                  +(1.0_r8-vnbc)*Nnatk(icol,k,14)*ba870x(icol,k,14) 
            boclt1xt(icol,k) =  &
                 +(1.0_r8-vnbc)*Nnatk(icol,k,14)*belt1x(icol,k,14) 
            bocgt1xt(icol,k) = &
                 +(1.0_r8-vnbc)*Nnatk(icol,k,14)*begt1x(icol,k,14) 
!     Total (for all modes) absorption optical depth and backscattering
            abs550_aer(icol,k)=babs550tot(icol,k)  &
              +Nnatk(icol,k,12)*ba550x(icol,k,12) &
              +Nnatk(icol,k,14)*ba550x(icol,k,14)
            abs550_aer(icol,k)=1.e-3_r8*abs550_aer(icol,k)
            bs550_aer(icol,k)= backsc550tot(icol,k)   &
              +Nnatk(icol,k,12)*backsc550x(icol,k,12) &
              +Nnatk(icol,k,14)*backsc550x(icol,k,14)
            bs550_aer(icol,k)=1.e-3_r8*bs550_aer(icol,k)
!
          end do
         enddo
!ccccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5ccccccccc6ccccccccc7cc
!       collect AeroCom-fields for optical depth/absorption of each comp,
!       3D and 2D, at 440, 500, 550, 670 and 870 nm, for all d, d<1um and d>1um  
!        initialize 2d-fields
         do icol=1,ncol
           dod440(icol) = 0.0_r8
           abs440(icol) = 0.0_r8
           dod500(icol) = 0.0_r8
           abs500(icol) = 0.0_r8
           dod550(icol) = 0.0_r8
           abs550(icol) = 0.0_r8
           abs550alt(icol) = 0.0_r8
           dod670(icol) = 0.0_r8
           abs670(icol) = 0.0_r8
           dod870(icol) = 0.0_r8
           abs870(icol) = 0.0_r8
!
           abs550_ss(icol) = 0.0_r8
           abs550_dust(icol) = 0.0_r8
           abs550_so4(icol) = 0.0_r8
           abs550_bc(icol) = 0.0_r8
           abs550_pom(icol) = 0.0_r8
!
           dod440_ss(icol) = 0.0_r8
           dod440_dust(icol) = 0.0_r8
           dod440_so4(icol) = 0.0_r8
           dod440_bc(icol) = 0.0_r8
           dod440_pom(icol) = 0.0_r8
           dod500_ss(icol) = 0.0_r8
           dod500_dust(icol) = 0.0_r8
           dod500_so4(icol) = 0.0_r8
           dod500_bc(icol) = 0.0_r8
           dod500_pom(icol) = 0.0_r8
           dod550_ss(icol) = 0.0_r8
           dod550_dust(icol) = 0.0_r8
           dod550_so4(icol) = 0.0_r8
           dod550_bc(icol) = 0.0_r8
           dod550_pom(icol) = 0.0_r8
           dod670_ss(icol) = 0.0_r8
           dod670_dust(icol) = 0.0_r8
           dod670_so4(icol) = 0.0_r8
           dod670_bc(icol) = 0.0_r8
           dod670_pom(icol) = 0.0_r8
           dod870_ss(icol) = 0.0_r8
           dod870_dust(icol) = 0.0_r8
           dod870_so4(icol) = 0.0_r8
           dod870_bc(icol) = 0.0_r8
           dod870_pom(icol) = 0.0_r8
           dod550lt1_ss(icol) = 0.0_r8
           dod550gt1_ss(icol) = 0.0_r8
           dod550lt1_dust(icol) = 0.0_r8
           dod550gt1_dust(icol) = 0.0_r8
           dod550lt1_so4(icol) = 0.0_r8
           dod550gt1_so4(icol) = 0.0_r8
           dod550lt1_bc(icol) = 0.0_r8
           dod550gt1_bc(icol) = 0.0_r8
           dod550lt1_pom(icol) = 0.0_r8
           dod550gt1_pom(icol) = 0.0_r8
          do k=1,pver
             abs4403d(icol,k) = 0.0_r8
             abs5003d(icol,k) = 0.0_r8
             abs5503d(icol,k) = 0.0_r8
             abs6703d(icol,k) = 0.0_r8
             abs8703d(icol,k) = 0.0_r8
             abs5503dalt(icol,k) = 0.0_r8
          enddo
         enddo

         do icol=1,ncol
          do k=1,pver
!          Layer thickness, unit km
           deltah=deltah_km(icol,k)
!          if(k==pver) write(*,*) 'icol, deltah(pmxsub)=', icol, deltah
!          3D optical depths for monthly averages
!SS
           dod4403d_ss(icol,k) = bint440ss(icol,k)*deltah
           dod5003d_ss(icol,k) = bint500ss(icol,k)*deltah
           dod5503d_ss(icol,k) = bint550ss(icol,k)*deltah
           abs5503d_ss(icol,k) = baint550ss(icol,k)*deltah
           dod6703d_ss(icol,k) = bint670ss(icol,k)*deltah
           dod8703d_ss(icol,k) = bint870ss(icol,k)*deltah
!DUST
           dod4403d_dust(icol,k) = bint440du(icol,k)*deltah
           dod5003d_dust(icol,k) = bint500du(icol,k)*deltah
           dod5503d_dust(icol,k) = bint550du(icol,k)*deltah
           abs5503d_dust(icol,k) = baint550du(icol,k)*deltah
           dod6703d_dust(icol,k) = bint670du(icol,k)*deltah
           dod8703d_dust(icol,k) = bint870du(icol,k)*deltah
!SO4
!soa: *(1-v_soana) for the sulfate volume fraction of mode 1
           dod4403d_so4(icol,k) = (besu440tot(icol,k)                 &       ! condensate )
          +(1.0_r8-v_soana(icol,k))*Nnatk(icol,k,1)*bebg440(icol,k,1) &       ! background, SO4(Ait) mode (1)
                                  + Nnatk(icol,k,5)*bebg440(icol,k,5))*deltah ! background, SO4(Ait75) mode (5)
           dod5003d_so4(icol,k) = (besu500tot(icol,k)                 &       ! condensate 
          +(1.0_r8-v_soana(icol,k))*Nnatk(icol,k,1)*bebg500(icol,k,1) &       ! background, SO4(Ait) mode (1)
                                  + Nnatk(icol,k,5)*bebg500(icol,k,5))*deltah ! background, SO4(Ait75) mode (5)
           dod5503d_so4(icol,k) = (besu550tot(icol,k)                   &     ! condensate 
          +(1.0_r8-v_soana(icol,k))*Nnatk(icol,k,1)*bebg550(icol,k,1) &       ! background, SO4(Ait) mode (1)
                                  + Nnatk(icol,k,5)*bebg550(icol,k,5))*deltah ! background, SO4(Ait75) mode (5)
           abs5503d_so4(icol,k) = (basu550tot(icol,k)                 &       ! condensate )
          +(1.0_r8-v_soana(icol,k))*Nnatk(icol,k,1)*babg550(icol,k,1) &       ! background, SO4(Ait) mode (1)
                                  + Nnatk(icol,k,5)*babg550(icol,k,5))*deltah ! background, SO4(Ait75) mode (5)
           dod6703d_so4(icol,k) = (besu670tot(icol,k)                   &     ! condensate
          +(1.0_r8-v_soana(icol,k))*Nnatk(icol,k,1)*bebg670(icol,k,1) &       ! background, SO4(Ait) mode (1)
                                  + Nnatk(icol,k,5)*bebg670(icol,k,5))*deltah ! background, SO4(Ait75) mode (5)
           dod8703d_so4(icol,k) = (besu870tot(icol,k)                   &     ! condensate 
          +(1.0_r8-v_soana(icol,k))*Nnatk(icol,k,1)*bebg870(icol,k,1) &       ! background, SO4(Ait) mode (1)
                                  + Nnatk(icol,k,5)*bebg870(icol,k,5))*deltah ! background, SO4(Ait75) mode (5)
!BC
           vaitbcarr(icol,k) = faitbc(icol,k)/(faitbc(icol,k) &
                           +(1.0_r8-faitbc(icol,k))*rhopart(l_bc_ni)/rhopart(l_om_ni))
           vaitbc = vaitbcarr(icol,k)
           dod4403d_bc(icol,k) = (bebc440tot(icol,k)+bebc440xt(icol,k)  &     ! coagulated + n-mode BC (12)
                                  + Nnatk(icol,k,2)*bebg440(icol,k,2) &       ! background, BC(Ait) mode (2)
                           + vaitbc*Nnatk(icol,k,4)*bebg440(icol,k,4) &       ! background in OC&BC(Ait) mode (4)
                                  + Nnatk(icol,k,0)*bebg440(icol,k,0))*deltah ! background, BC(ax) mode (0)
           dod5003d_bc(icol,k) = (bebc500tot(icol,k)+bebc500xt(icol,k)  &     ! coagulated + n-mode BC (12)
                                  + Nnatk(icol,k,2)*bebg500(icol,k,2) &       ! background, BC(Ait) mode (2)
                           + vaitbc*Nnatk(icol,k,4)*bebg500(icol,k,4) &       ! background in OC&BC(Ait) mode (4)
                                  + Nnatk(icol,k,0)*bebg500(icol,k,0))*deltah ! background, BC(ax) mode (0)
           dod5503d_bc(icol,k) = (bebc550tot(icol,k)+bebc550xt(icol,k)  &     ! coagulated + n-mode BC (12)
                                  + Nnatk(icol,k,2)*bebg550(icol,k,2) &       ! background, BC(Ait) mode (2)
                           + vaitbc*Nnatk(icol,k,4)*bebg550(icol,k,4) &       ! background in OC&BC(Ait) mode (4)
                                  + Nnatk(icol,k,0)*bebg550(icol,k,0))*deltah ! background, BC(ax) mode (0)
           abs5503d_bc(icol,k) = (babc550tot(icol,k)+babc550xt(icol,k)  &     ! coagulated + n-mode BC (12)
                                  + Nnatk(icol,k,2)*babg550(icol,k,2) &       ! background, BC(Ait) mode (2)
                           + vaitbc*Nnatk(icol,k,4)*babg550(icol,k,4) &       ! background in OC&BC(Ait) mode (4)
                                  + Nnatk(icol,k,0)*babg550(icol,k,0))*deltah ! background, BC(ax) mode (0)
           dod6703d_bc(icol,k) = (bebc670tot(icol,k)+bebc670xt(icol,k)  &     ! coagulated + n-mode BC (12)
                                  + Nnatk(icol,k,2)*bebg670(icol,k,2) &       ! background, BC(Ait) mode (2)
                           + vaitbc*Nnatk(icol,k,4)*bebg670(icol,k,4) &       ! background in OC&BC(Ait) mode (4)
                                  + Nnatk(icol,k,0)*bebg670(icol,k,0))*deltah ! background, BC(ax) mode (0)
           dod8703d_bc(icol,k) = (bebc870tot(icol,k)+bebc870xt(icol,k)  &     ! coagulated + n-mode BC (12)
                                  + Nnatk(icol,k,2)*bebg870(icol,k,2) &       ! background, BC(Ait) mode (2)
                           + vaitbc*Nnatk(icol,k,4)*bebg870(icol,k,4) &       ! background in OC&BC(Ait) mode (4)
                                  + Nnatk(icol,k,0)*bebg870(icol,k,0))*deltah ! background, BC(ax) mode (0)
!OC        
!soa + v_soana part of mode 11 for the OC volume fraction of that mode
! v_soana(icol,k)
           dod4403d_pom(icol,k) = (beoc440tot(icol,k)+beoc440xt(icol,k) &     ! coagulated + n-mode OC&BC (14)
                                  + Nnatk(icol,k,1)*bebg440(icol,k,1)*v_soana(icol,k) & ! SOA fraction of mode 1
!-3                                  + Nnatk(icol,k,3)*bebg440(icol,k,3) &       ! background, OC(Ait) mode (3)
                     + (1.0_r8-vaitbc)*Nnatk(icol,k,4)*bebg440(icol,k,4))*deltah ! background in OC&BC(Ait) mode (4)
           dod5003d_pom(icol,k) = (beoc500tot(icol,k)+beoc500xt(icol,k) &     ! coagulated + n-mode OC&BC (14)
                                  + Nnatk(icol,k,1)*bebg500(icol,k,1)*v_soana(icol,k) & ! SOA fraction of mode 1
!-3                                  + Nnatk(icol,k,3)*bebg500(icol,k,3) &       ! background, OC(Ait) mode (3)
                     + (1.0_r8-vaitbc)*Nnatk(icol,k,4)*bebg500(icol,k,4))*deltah ! background in OC&BC(Ait) mode (4)
           dod5503d_pom(icol,k) = (beoc550tot(icol,k)+beoc550xt(icol,k) &     ! coagulated + n-mode OC&BC (14)
                                  + Nnatk(icol,k,1)*bebg550(icol,k,1)*v_soana(icol,k) & ! SOA fraction of mode 1
!-3                                  + Nnatk(icol,k,3)*bebg550(icol,k,3) &       ! background, OC(Ait) mode (3)
                     + (1.0_r8-vaitbc)*Nnatk(icol,k,4)*bebg550(icol,k,4))*deltah ! background in OC&BC(Ait) mode (4)
           abs5503d_pom(icol,k) = (baoc550tot(icol,k)+baoc550xt(icol,k) &     ! coagulated + n-mode OC&BC (14)
                                  + Nnatk(icol,k,1)*babg550(icol,k,1)*v_soana(icol,k) & ! SOA fraction of mode 1
!-3                                  + Nnatk(icol,k,3)*babg550(icol,k,3) &       ! background, OC(Ait) mode (3)
                     + (1.0_r8-vaitbc)*Nnatk(icol,k,4)*babg550(icol,k,4))*deltah ! background in OC&BC(Ait) mode (4)
           dod6703d_pom(icol,k) = (beoc670tot(icol,k)+beoc670xt(icol,k) &     ! coagulated + n-mode OC&BC (14)
                                  + Nnatk(icol,k,1)*bebg670(icol,k,1)*v_soana(icol,k) & ! SOA fraction of mode 1
!-3                                  + Nnatk(icol,k,3)*bebg670(icol,k,3) &       ! background, OC(Ait) mode (3)
                     + (1.0_r8-vaitbc)*Nnatk(icol,k,4)*bebg670(icol,k,4))*deltah ! background in OC&BC(Ait) mode (4)
           dod8703d_pom(icol,k) = (beoc870tot(icol,k)+beoc870xt(icol,k) &     ! coagulated + n-mode OC&BC (14)
                                  + Nnatk(icol,k,1)*bebg870(icol,k,1)*v_soana(icol,k) & ! SOA fraction of mode 1
!-3                                  + Nnatk(icol,k,3)*bebg870(icol,k,3) &       ! background, OC(Ait) mode (3)
                     + (1.0_r8-vaitbc)*Nnatk(icol,k,4)*bebg870(icol,k,4))*deltah ! background in OC&BC(Ait) mode (4)

           ec550_so4(icol,k) = 1.e-3*dod5503d_so4(icol,k)/deltah
           ec550_bc(icol,k)  = 1.e-3*dod5503d_bc(icol,k)/deltah
           ec550_pom(icol,k) = 1.e-3*dod5503d_pom(icol,k)/deltah
           ec550_ss(icol,k)  = 1.e-3*dod5503d_ss(icol,k)/deltah
           ec550_du(icol,k)  = 1.e-3*dod5503d_dust(icol,k)/deltah
           ec550_aer(icol,k) = ec550_so4(icol,k)+ec550_bc(icol,k)+ec550_pom(icol,k) &
                             + ec550_ss(icol,k)+ec550_du(icol,k)

!ccccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5ccccccccc6ccccccccc7cc
!          Total 3D optical depths/abs. for column integrations
           dod4403d(icol,k) = dod4403d_ss(icol,k)+dod4403d_dust(icol,k) &
                            +dod4403d_so4(icol,k)+dod4403d_bc(icol,k)    &
                            +dod4403d_pom(icol,k)                     
           dod5003d(icol,k) = dod5003d_ss(icol,k)+dod5003d_dust(icol,k) &
                            +dod5003d_so4(icol,k)+dod5003d_bc(icol,k)    &
                            +dod5003d_pom(icol,k)                     
           dod5503d(icol,k) = dod5503d_ss(icol,k)+dod5503d_dust(icol,k) &
                           +dod5503d_so4(icol,k)+dod5503d_bc(icol,k)    &
                           +dod5503d_pom(icol,k)                     
           dod6703d(icol,k) = dod6703d_ss(icol,k)+dod6703d_dust(icol,k) &
                            +dod6703d_so4(icol,k)+dod6703d_bc(icol,k)    &
                            +dod6703d_pom(icol,k)                     
           dod8703d(icol,k) = dod8703d_ss(icol,k)+dod8703d_dust(icol,k) &
                            +dod8703d_so4(icol,k)+dod8703d_bc(icol,k)    &
                            +dod8703d_pom(icol,k)                     
           abs5503d(icol,k) = abs5503d_ss(icol,k)+abs5503d_dust(icol,k) &
                           +abs5503d_so4(icol,k)+abs5503d_bc(icol,k)    &
                           +abs5503d_pom(icol,k)                     
!   (Note: Local abs550alt is up to 6% larger (annually averaged) in typical b.b.
!   regions, compared to abs550. This is most likely most correct, but should be checked!)
           do i=0,10
             abs4403d(icol,k) = abs4403d(icol,k)+Nnatk(icol,k,i)*babs440(icol,k,i)*deltah
             abs5003d(icol,k) = abs5003d(icol,k)+Nnatk(icol,k,i)*babs500(icol,k,i)*deltah
             abs6703d(icol,k) = abs6703d(icol,k)+Nnatk(icol,k,i)*babs670(icol,k,i)*deltah
             abs8703d(icol,k) = abs8703d(icol,k)+Nnatk(icol,k,i)*babs870(icol,k,i)*deltah
             abs5503dalt(icol,k) = abs5503dalt(icol,k)+Nnatk(icol,k,i)*babs550(icol,k,i)*deltah
           enddo
           do i=11,14
             abs4403d(icol,k) = abs4403d(icol,k)+Nnatk(icol,k,i)*babs440n(icol,k,i-10)*deltah
             abs5003d(icol,k) = abs5003d(icol,k)+Nnatk(icol,k,i)*babs500n(icol,k,i-10)*deltah
             abs6703d(icol,k) = abs6703d(icol,k)+Nnatk(icol,k,i)*babs670n(icol,k,i-10)*deltah
             abs8703d(icol,k) = abs8703d(icol,k)+Nnatk(icol,k,i)*babs870n(icol,k,i-10)*deltah
             abs5503dalt(icol,k) = abs5503dalt(icol,k)+Nnatk(icol,k,i)*babs550n(icol,k,i-10)*deltah
           enddo
!ccccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5ccccccccc6ccccccccc7cc
!            optical depths for d<1um and d>1um (r<0.5um and r>0.5um)
!SS
           dod5503dlt1_ss(icol,k) = besslt1(icol,k)*deltah
           dod5503dgt1_ss(icol,k) = bessgt1(icol,k)*deltah
!DUST
           dod5503dlt1_dust(icol,k) = bedustlt1(icol,k)*deltah
           dod5503dgt1_dust(icol,k) = bedustgt1(icol,k)*deltah

!soa: *(1-v_soana) for the sulfate volume fraction of mode 1
           dod5503dlt1_so4(icol,k) = (bes4lt1t(icol,k)                  &   ! condensate
                    + Nnatk(icol,k,1)*bebglt1(icol,k,1)*(1.0_r8-v_soana(icol,k)) &   ! background, SO4(Ait) mode (1)
                    + Nnatk(icol,k,5)*bebglt1(icol,k,5))*deltah             ! background, SO4(Ait75) mode (5)
           dod5503dgt1_so4(icol,k) = (bes4gt1t(icol,k)                  &   ! condensate + n-mode (11)
                    + Nnatk(icol,k,1)*bebggt1(icol,k,1)*(1.0_r8-v_soana(icol,k)) &   ! background, SO4(Ait) mode (1)
                    + Nnatk(icol,k,5)*bebggt1(icol,k,5))*deltah             ! background, SO4(Ait75) mode (5)
!BC
           dod5503dlt1_bc(icol,k) =  (bebclt1t(icol,k)+bbclt1xt(icol,k) &   ! coagulated + n-mode BC (12)
                    + Nnatk(icol,k,2)*bebglt1(icol,k,2)                 &   ! background, BC(Ait) mode (2)
             + vaitbc*Nnatk(icol,k,4)*bebglt1(icol,k,4)                 &   ! background in OC&BC(Ait) mode (4)
                    + Nnatk(icol,k,0)*bebglt1(icol,k,0))*deltah             ! background, BC(ax) mode (0)
           dod5503dgt1_bc(icol,k) =  (bebcgt1t(icol,k)+bbcgt1xt(icol,k) &   ! coagulated + n-mode BC (12)
                    + Nnatk(icol,k,2)*bebggt1(icol,k,2)                 &   ! background, BC(Ait) mode (2)
             + vaitbc*Nnatk(icol,k,4)*bebggt1(icol,k,4)                 &   ! background in OC&BC(Ait) mode (4)
                    + Nnatk(icol,k,0)*bebggt1(icol,k,0))*deltah             ! background, BC(ax) mode (0)
!OC
!soa + v_soana part of mode 11 for the OC volume fraction of that mode
           dod5503dlt1_pom(icol,k) = (beoclt1t(icol,k)+boclt1xt(icol,k) &   ! coagulated + n-mode OC&BC (14)
                    + Nnatk(icol,k,1)*bebglt1(icol,k,1)*v_soana(icol,k) &   ! SOA fraction of mode 1
!-3                    + Nnatk(icol,k,3)*bebglt1(icol,k,3)                 &   ! background, OC(Ait) mode (3)
       + (1.0_r8-vaitbc)*Nnatk(icol,k,4)*bebglt1(icol,k,4))*deltah          ! background in OC&BC(Ait) mode (4)
           dod5503dgt1_pom(icol,k) = (beocgt1t(icol,k)+bocgt1xt(icol,k) &   ! coagulated + n-mode OC&OC (14)
                    + Nnatk(icol,k,1)*bebggt1(icol,k,1)*v_soana(icol,k) &   ! SOA fraction of mode 1
!-3                    + Nnatk(icol,k,3)*bebggt1(icol,k,3)                 &   ! background, OC(Ait) mode (3)
       + (1.0_r8-vaitbc)*Nnatk(icol,k,4)*bebggt1(icol,k,4))*deltah          ! background in OC&BC(Ait) mode (4)
!ccccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5ccccccccc6ccccccccc7cc
!          Column integrated optical depths/abs., total and for each constituent
           dod440(icol)         = dod440(icol)+dod4403d(icol,k)
           abs440(icol)         = abs440(icol)+abs4403d(icol,k)
           dod500(icol)         = dod500(icol)+dod5003d(icol,k)
           abs500(icol)         = abs500(icol)+abs5003d(icol,k)
           dod550(icol)         = dod550(icol)+dod5503d(icol,k)
           abs550(icol)         = abs550(icol)+abs5503d(icol,k)
           abs550alt(icol)      = abs550alt(icol)+abs5503dalt(icol,k)
           dod670(icol)         = dod670(icol)+dod6703d(icol,k)
           abs670(icol)         = abs670(icol)+abs6703d(icol,k)
           dod870(icol)         = dod870(icol)+dod8703d(icol,k)
           abs870(icol)         = abs870(icol)+abs8703d(icol,k)
! Added abs components
           abs550_ss(icol)      = abs550_ss(icol)+abs5503d_ss(icol,k)
           abs550_dust(icol)    = abs550_dust(icol)+abs5503d_dust(icol,k)
           abs550_so4(icol)     = abs550_so4(icol)+abs5503d_so4(icol,k)
           abs550_bc(icol)      = abs550_bc(icol)+abs5503d_bc(icol,k)
           abs550_pom(icol)     = abs550_pom(icol)+abs5503d_pom(icol,k)
!
           dod440_ss(icol)      = dod440_ss(icol)+dod4403d_ss(icol,k)
           dod440_dust(icol)    = dod440_dust(icol)+dod4403d_dust(icol,k)
           dod440_so4(icol)     = dod440_so4(icol)+dod4403d_so4(icol,k)
           dod440_bc(icol)      = dod440_bc(icol)+dod4403d_bc(icol,k)
           dod440_pom(icol)     = dod440_pom(icol)+dod4403d_pom(icol,k)
           dod500_ss(icol)      = dod500_ss(icol)+dod5003d_ss(icol,k)
           dod500_dust(icol)    = dod500_dust(icol)+dod5003d_dust(icol,k)
           dod500_so4(icol)     = dod500_so4(icol)+dod5003d_so4(icol,k)
           dod500_bc(icol)      = dod500_bc(icol)+dod5003d_bc(icol,k)
           dod500_pom(icol)     = dod500_pom(icol)+dod5003d_pom(icol,k)
           dod550_ss(icol)      = dod550_ss(icol)+dod5503d_ss(icol,k)
           dod550_dust(icol)    = dod550_dust(icol)+dod5503d_dust(icol,k)
           dod550_so4(icol)     = dod550_so4(icol)+dod5503d_so4(icol,k)
           dod550_bc(icol)      = dod550_bc(icol)+dod5503d_bc(icol,k)
           dod550_pom(icol)     = dod550_pom(icol)+dod5503d_pom(icol,k)
           dod670_ss(icol)      = dod670_ss(icol)+dod6703d_ss(icol,k)
           dod670_dust(icol)    = dod670_dust(icol)+dod6703d_dust(icol,k)
           dod670_so4(icol)     = dod670_so4(icol)+dod6703d_so4(icol,k)
           dod670_bc(icol)      = dod670_bc(icol)+dod6703d_bc(icol,k)
           dod670_pom(icol)     = dod670_pom(icol)+dod6703d_pom(icol,k)
           dod870_ss(icol)      = dod870_ss(icol)+dod8703d_ss(icol,k)
           dod870_dust(icol)    = dod870_dust(icol)+dod8703d_dust(icol,k)
           dod870_so4(icol)     = dod870_so4(icol)+dod8703d_so4(icol,k)
           dod870_bc(icol)      = dod870_bc(icol)+dod8703d_bc(icol,k)
           dod870_pom(icol)     = dod870_pom(icol)+dod8703d_pom(icol,k)
           dod550lt1_ss(icol)   = dod550lt1_ss(icol)+dod5503dlt1_ss(icol,k)
           dod550gt1_ss(icol)   = dod550gt1_ss(icol)+dod5503dgt1_ss(icol,k)
           dod550lt1_dust(icol) = dod550lt1_dust(icol)+dod5503dlt1_dust(icol,k)
           dod550gt1_dust(icol) = dod550gt1_dust(icol)+dod5503dgt1_dust(icol,k)
           dod550lt1_so4(icol)  = dod550lt1_so4(icol)+dod5503dlt1_so4(icol,k) 
           dod550gt1_so4(icol)  = dod550gt1_so4(icol)+dod5503dgt1_so4(icol,k) 
           dod550lt1_bc(icol)   = dod550lt1_bc(icol)+dod5503dlt1_bc(icol,k)  
           dod550gt1_bc(icol)   = dod550gt1_bc(icol)+dod5503dgt1_bc(icol,k)  
           dod550lt1_pom(icol)  = dod550lt1_pom(icol)+dod5503dlt1_pom(icol,k) 
           dod550gt1_pom(icol)  = dod550gt1_pom(icol)+dod5503dgt1_pom(icol,k)
!ccccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5ccccccccc6ccccccccc7cc
          enddo ! k

         enddo  ! icol

!       extinction, absorption (m-1) and backscatter coefficients (m-1 sr-1)
        call outfld('EC550AER',ec550_aer,pcols,lchnk)
        call outfld('ABS550_A',abs550_aer,pcols,lchnk)
        call outfld('BS550AER',bs550_aer,pcols,lchnk)
!
!       speciated extinction coefficients (m-1)
        call outfld('EC550SO4',ec550_so4,pcols,lchnk)
        call outfld('EC550BC ',ec550_bc ,pcols,lchnk)
        call outfld('EC550POM',ec550_pom,pcols,lchnk)
        call outfld('EC550SS ',ec550_ss ,pcols,lchnk)
        call outfld('EC550DU ',ec550_du ,pcols,lchnk)
!
!       optical depths and absorption as requested by AeroCom
!       notation: 3=3D, D=DOD, A=ABS, LT=d<1um, GT=d>1um
        call outfld('DOD440  ',dod440  ,pcols,lchnk)
        call outfld('ABS440  ',abs440  ,pcols,lchnk)
        call outfld('DOD500  ',dod500  ,pcols,lchnk)
        call outfld('ABS500  ',abs500  ,pcols,lchnk)
        call outfld('DOD550  ',dod550  ,pcols,lchnk)
        call outfld('ABS550  ',abs550  ,pcols,lchnk)
        call outfld('ABS550AL',abs550alt,pcols,lchnk)
        call outfld('DOD670  ',dod670  ,pcols,lchnk)
        call outfld('ABS670  ',abs670  ,pcols,lchnk)
        call outfld('DOD870  ',dod870  ,pcols,lchnk)
        call outfld('ABS870  ',abs870  ,pcols,lchnk)
        call outfld('A550_SS ',abs550_ss  ,pcols,lchnk)
        call outfld('A550_DU ',abs550_dust,pcols,lchnk)
        call outfld('A550_SO4',abs550_so4 ,pcols,lchnk)
        call outfld('A550_BC ',abs550_bc  ,pcols,lchnk)
        call outfld('A550_POM',abs550_pom ,pcols,lchnk)
!
        call outfld('D440_SS ',dod440_ss  ,pcols,lchnk)
        call outfld('D440_DU ',dod440_dust,pcols,lchnk)
        call outfld('D440_SO4',dod440_so4 ,pcols,lchnk)
        call outfld('D440_BC ',dod440_bc  ,pcols,lchnk)
        call outfld('D440_POM',dod440_pom ,pcols,lchnk)
        call outfld('D500_SS ',dod500_ss  ,pcols,lchnk)
        call outfld('D500_DU ',dod500_dust,pcols,lchnk)
        call outfld('D500_SO4',dod500_so4 ,pcols,lchnk)
        call outfld('D500_BC ',dod500_bc  ,pcols,lchnk)
        call outfld('D500_POM',dod500_pom ,pcols,lchnk)
        call outfld('D550_SS ',dod550_ss  ,pcols,lchnk)
        call outfld('D550_DU ',dod550_dust,pcols,lchnk)
        call outfld('D550_SO4',dod550_so4 ,pcols,lchnk)
        call outfld('D550_BC ',dod550_bc  ,pcols,lchnk)
        call outfld('D550_POM',dod550_pom ,pcols,lchnk)
        call outfld('D670_SS ',dod670_ss  ,pcols,lchnk)
        call outfld('D670_DU ',dod670_dust,pcols,lchnk)
        call outfld('D670_SO4',dod670_so4 ,pcols,lchnk)
        call outfld('D670_BC ',dod670_bc  ,pcols,lchnk)
        call outfld('D670_POM',dod670_pom ,pcols,lchnk)
        call outfld('D870_SS ',dod870_ss  ,pcols,lchnk)
        call outfld('D870_DU ',dod870_dust,pcols,lchnk)
        call outfld('D870_SO4',dod870_so4 ,pcols,lchnk)
        call outfld('D870_BC ',dod870_bc  ,pcols,lchnk)
        call outfld('D870_POM',dod870_pom ,pcols,lchnk)
        call outfld('DLT_SS  ',dod550lt1_ss,pcols,lchnk)
        call outfld('DGT_SS  ',dod550gt1_ss,pcols,lchnk)
        call outfld('DLT_DUST',dod550lt1_dust,pcols,lchnk)
        call outfld('DGT_DUST',dod550gt1_dust,pcols,lchnk)
        call outfld('DLT_SO4 ',dod550lt1_so4,pcols,lchnk)
        call outfld('DGT_SO4 ',dod550gt1_so4,pcols,lchnk)
        call outfld('DLT_BC  ',dod550lt1_bc,pcols,lchnk)
        call outfld('DGT_BC  ',dod550gt1_bc,pcols,lchnk)
        call outfld('DLT_POM ',dod550lt1_pom,pcols,lchnk)
        call outfld('DGT_POM ',dod550gt1_pom,pcols,lchnk)
!tst
!        call outfld('DOD5503D',dod5503d,pcols,lchnk)
!tst
!-        call outfld('ABS5503D',abs5503d,pcols,lchnk)
!-        call outfld('D443_SS ',dod4403d_ss  ,pcols,lchnk)
!-        call outfld('D443_DU ',dod4403d_dust,pcols,lchnk)
!-        call outfld('D443_SO4',dod4403d_so4 ,pcols,lchnk)
!-        call outfld('D443_BC ',dod4403d_bc  ,pcols,lchnk)
!-        call outfld('D443_POM',dod4403d_pom ,pcols,lchnk)
!-        call outfld('D503_SS ',dod5003d_ss  ,pcols,lchnk)
!-        call outfld('D503_DU ',dod5003d_dust,pcols,lchnk)
!-        call outfld('D503_SO4',dod5003d_so4 ,pcols,lchnk)
!-        call outfld('D503_BC ',dod5003d_bc  ,pcols,lchnk)
!-        call outfld('D503_POM',dod5003d_pom ,pcols,lchnk)
!-        call outfld('D553_SS ',dod5503d_ss  ,pcols,lchnk)
!-        call outfld('D553_DU ',dod5503d_dust,pcols,lchnk)
!-        call outfld('D553_SO4',dod5503d_so4 ,pcols,lchnk)
!-        call outfld('D553_BC ',dod5503d_bc  ,pcols,lchnk)
!-        call outfld('D553_POM',dod5503d_pom ,pcols,lchnk)
!-        call outfld('D673_SS ',dod6703d_ss  ,pcols,lchnk)
!-        call outfld('D673_DU ',dod6703d_dust,pcols,lchnk)
!-        call outfld('D673_SO4',dod6703d_so4 ,pcols,lchnk)
!-        call outfld('D673_BC ',dod6703d_bc  ,pcols,lchnk)
!-        call outfld('D673_POM',dod6703d_pom ,pcols,lchnk)
!-        call outfld('D873_SS ',dod8703d_ss  ,pcols,lchnk)
!-        call outfld('D873_DU ',dod8703d_dust,pcols,lchnk)
!-        call outfld('D873_SO4',dod8703d_so4 ,pcols,lchnk)
!-        call outfld('D873_BC ',dod8703d_bc  ,pcols,lchnk)
!-        call outfld('D873_POM',dod8703d_pom ,pcols,lchnk)


!000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000

!       Dry parameters of each aerosol component 
!       BC(ax) mode
        call intdrypar0(lchnk, ncol, Nnatk,                          & 
           cintbg, cintbg05, cintbg125, cintbc, cintbc05, cintbc125, & 
           cintoc, cintoc05, cintoc125, cintsc, cintsc05, cintsc125, &
           cintsa, cintsa05, cintsa125, aaeros, aaerol, vaeros, vaerol,&
           cknorm,cknlt05,ckngt125)
!       SO4&SOA(Ait,n) mode
        call intdrypar1(lchnk, ncol, Nnatk, xfombg, ifombg1,         &
           xct, ict1, xfac, ifac1,                                   & 
           cintbg, cintbg05, cintbg125, cintbc, cintbc05, cintbc125, & 
           cintoc, cintoc05, cintoc125, cintsc, cintsc05, cintsc125, &
           cintsa, cintsa05, cintsa125, aaeros, aaerol, vaeros, vaerol,&
           aaerosn,aaeroln,vaerosn,vaeroln,cknorm,cknlt05,ckngt125)
!       BC(Ait,n) and OC(Ait,n) modes
         call intdrypar2to3(lchnk, ncol, Nnatk, xct, ict1, xfac, ifac1, & 
           cintbg, cintbg05, cintbg125, cintbc, cintbc05, cintbc125,   & 
           cintoc, cintoc05, cintoc125, cintsc, cintsc05, cintsc125,   &
           cintsa, cintsa05, cintsa125, aaeros, aaerol, vaeros, vaerol,&
           aaerosn,aaeroln,vaerosn,vaeroln,cknorm,cknlt05,ckngt125)
!       BC&OC(Ait,n) mode   ------ fcm not valid here (=0). Use faitbc or fnbc instead
        call intdrypar4(lchnk, ncol, Nnatk,                          &
           xfbcbg, ifbcbg1, xfbcbgn, ifbcbgn1,                       &
           xct, ict1, xfac, ifac1, xfaq, ifaq1,                      &
           cintbg, cintbg05, cintbg125, cintbc, cintbc05, cintbc125, &
           cintoc, cintoc05, cintoc125, cintsc, cintsc05, cintsc125, &
           cintsa, cintsa05, cintsa125, aaeros, aaerol, vaeros, vaerol, &
           aaerosn,aaeroln,vaerosn,vaeroln,cknorm,cknlt05,ckngt125)
!       SO4(Ait75) (5), mineral (6-7) and Sea-salt (8-10) modes:
        call intdrypar5to10(lchnk, ncol, Nnatk,                      &
           xct, ict1, xfac, ifac1, xfbc, ifbc1, xfaq, ifaq1,         & 
           cintbg, cintbg05, cintbg125, cintbc, cintbc05, cintbc125, & 
           cintoc, cintoc05, cintoc125, cintsc, cintsc05, cintsc125, &
           cintsa, cintsa05, cintsa125, aaeros, aaerol, vaeros, vaerol,&
           cknorm,cknlt05,ckngt125)

        do k=1,pver
           do icol=1,ncol
            c_ss(icol,k)=0.0_r8
            c_mi(icol,k)=0.0_r8
           enddo
        enddo

        do k=1,pver
           do icol=1,ncol
!           mineral and sea-salt background concentrations, internally mixed
            c_mi(icol,k)    = Nnatk(icol,k,6)*cintbg(icol,k,6)   &
                             +Nnatk(icol,k,7)*cintbg(icol,k,7)
            c_mi05(icol,k)  = Nnatk(icol,k,6)*cintbg05(icol,k,6) &
                             +Nnatk(icol,k,7)*cintbg05(icol,k,7)
            c_mi125(icol,k) = Nnatk(icol,k,6)*cintbg125(icol,k,6)& 
                             +Nnatk(icol,k,7)*cintbg125(icol,k,7) 
            c_ss(icol,k)    = Nnatk(icol,k,8)*cintbg(icol,k,8)   &
                             +Nnatk(icol,k,9)*cintbg(icol,k,9)    &
                             +Nnatk(icol,k,10)*cintbg(icol,k,10)
            c_ss05(icol,k)  = Nnatk(icol,k,8)*cintbg05(icol,k,8) &
                             +Nnatk(icol,k,9)*cintbg05(icol,k,9)  &
                             +Nnatk(icol,k,10)*cintbg05(icol,k,10)
            c_ss125(icol,k) = Nnatk(icol,k,8)*cintbg125(icol,k,8)&
                             +Nnatk(icol,k,9)*cintbg125(icol,k,9) &
                             +Nnatk(icol,k,10)*cintbg125(icol,k,10)
!           internally mixed bc and oc (from coagulation) and so4 concentrations 
!           (sa=so4(aq) and sc=so4(cond+coag), separated because of different density: 
!           necessary for calculation of volume fractions!), and total aerosol surface 
!           areas and volumes. 
            c_bc(icol,k)=0.0_r8
            c_bc05(icol,k)=0.0_r8
            c_bc125(icol,k)=0.0_r8
            c_oc(icol,k)=0.0_r8
            c_oc05(icol,k)=0.0_r8
            c_oc125(icol,k)=0.0_r8
            c_s4(icol,k)=0.0_r8
            c_s4_a(icol,k)=0.0_r8
            c_s4_1(icol,k)=0.0_r8
            c_s4_5(icol,k)=0.0_r8
            c_sa(icol,k)=0.0_r8
            c_sa05(icol,k)=0.0_r8
            c_sa125(icol,k)=0.0_r8
            c_sc(icol,k)=0.0_r8
            c_sc05(icol,k)=0.0_r8
            c_sc125(icol,k)=0.0_r8
            aaeros_tot(icol,k)=0.0_r8
            aaerol_tot(icol,k)=0.0_r8
            vaeros_tot(icol,k)=0.0_r8
            vaerol_tot(icol,k)=0.0_r8
            c_bc_0(icol,k)=0.0_r8
            c_bc_2(icol,k)=0.0_r8
            c_bc_4(icol,k)=0.0_r8
            c_bc_12(icol,k)=0.0_r8
            c_bc_14(icol,k)=0.0_r8
            c_oc_4(icol,k)=0.0_r8
            c_oc_14(icol,k)=0.0_r8
!akc6+
            c_tot(icol,k)=0.0_r8
            c_tot125(icol,k)=0.0_r8
            c_tot05(icol,k)=0.0_r8
            c_pm25(icol,k)=0.0_r8
            c_pm1(icol,k)=0.0_r8
            mmr_pm25(icol,k)=0.0_r8
            mmr_pm1(icol,k)=0.0_r8
!akc6-

            do i=0,nbmodes
        if(i.ne.3) then
             c_bc(icol,k)    = c_bc(icol,k) &
                            +Nnatk(icol,k,i)*cintbc(icol,k,i)
             c_bc05(icol,k)  = c_bc05(icol,k) &
                            +Nnatk(icol,k,i)*cintbc05(icol,k,i)
             c_bc125(icol,k) = c_bc125(icol,k) &
                            +Nnatk(icol,k,i)*cintbc125(icol,k,i)
             c_oc(icol,k)    = c_oc(icol,k) &
                            +Nnatk(icol,k,i)*cintoc(icol,k,i)
             c_oc05(icol,k)  = c_oc05(icol,k) &
                            +Nnatk(icol,k,i)*cintoc05(icol,k,i)
             c_oc125(icol,k) = c_oc125(icol,k) &
                            +Nnatk(icol,k,i)*cintoc125(icol,k,i)
             c_sa(icol,k)    = c_sa(icol,k) &
                            +Nnatk(icol,k,i)*cintsa(icol,k,i)
             c_sa05(icol,k)  = c_sa05(icol,k) &
                            +Nnatk(icol,k,i)*cintsa05(icol,k,i)
             c_sa125(icol,k) = c_sa125(icol,k) &
                            +Nnatk(icol,k,i)*cintsa125(icol,k,i)
             c_sc(icol,k)    = c_sc(icol,k) &
                            +Nnatk(icol,k,i)*cintsc(icol,k,i)
             c_sc05(icol,k)  = c_sc05(icol,k) &
                            +Nnatk(icol,k,i)*cintsc05(icol,k,i)
             c_sc125(icol,k) = c_sc125(icol,k) &
                            +Nnatk(icol,k,i)*cintsc125(icol,k,i)
             aaeros_tot(icol,k) = aaeros_tot(icol,k) &
                            +Nnatk(icol,k,i)*aaeros(icol,k,i)
             aaerol_tot(icol,k) = aaerol_tot(icol,k) &
                            +Nnatk(icol,k,i)*aaerol(icol,k,i)
             vaeros_tot(icol,k) =vaeros_tot(icol,k) &
                            +Nnatk(icol,k,i)*vaeros(icol,k,i)
             vaerol_tot(icol,k) = vaerol_tot(icol,k) &
                            +Nnatk(icol,k,i)*vaerol(icol,k,i)
        endif
            enddo
!           add dry aerosol area and volume of externally mixed modes
            do i=nbmp1,nmodes
             aaeros_tot(icol,k) = aaeros_tot(icol,k) &
                            +Nnatk(icol,k,i)*aaerosn(icol,k,i)
             aaerol_tot(icol,k) = aaerol_tot(icol,k) &
                            +Nnatk(icol,k,i)*aaeroln(icol,k,i)
             vaeros_tot(icol,k) =vaeros_tot(icol,k) &
                            +Nnatk(icol,k,i)*vaerosn(icol,k,i)
             vaerol_tot(icol,k) = vaerol_tot(icol,k) &
                            +Nnatk(icol,k,i)*vaeroln(icol,k,i)
            end do
!c_er3d           
!           Effective radii for particles smaller and greater than 0.5um, 
!           and for all radii, in each layer (er=3*V/A):
            erlt053d(icol,k)=3.0_r8*vaeros_tot(icol,k) &
                             /(aaeros_tot(icol,k)+eps)
            ergt053d(icol,k)=3.0_r8*vaerol_tot(icol,k) &
                             /(aaerol_tot(icol,k)+eps)
            er3d(icol,k)=3.0_r8*(vaeros_tot(icol,k)+vaerol_tot(icol,k)) &
                          /(aaeros_tot(icol,k)+aaerol_tot(icol,k)+eps)
!c_er3d
!           column integrated dry aerosol surface areas and volumes
!           for r<0.5um and r>0.5um (s and l, respectively).
            aaercols(icol)=aaercols(icol)+aaeros_tot(icol,k)
            aaercoll(icol)=aaercoll(icol)+aaerol_tot(icol,k)
            vaercols(icol)=vaercols(icol)+vaeros_tot(icol,k)
            vaercoll(icol)=vaercoll(icol)+vaerol_tot(icol,k)
!           then add background and externally mixed BC, OC and SO4 to mass concentrations
             c_bc_ac(icol,k)= c_bc(icol,k)
             c_bc_0(icol,k) = Nnatk(icol,k,0)*cintbg(icol,k,0)
             c_bc_2(icol,k) = Nnatk(icol,k,2)*cintbg(icol,k,2)
             c_bc_4(icol,k) = Nnatk(icol,k,4)*cintbg(icol,k,4)*faitbc(icol,k)
             c_bc_12(icol,k)= Nnatk(icol,k,12)*cknorm(icol,k,12)
             c_bc_14(icol,k)= Nnatk(icol,k,14)*cknorm(icol,k,14)*fnbc(icol,k)
             c_bc(icol,k)   = c_bc(icol,k)                                      &
                            +Nnatk(icol,k,2)*cintbg(icol,k,2)                   &
                            +Nnatk(icol,k,4)*cintbg(icol,k,4)*faitbc(icol,k) &
                            +Nnatk(icol,k,0)*cintbg(icol,k,0)                   &
                            +Nnatk(icol,k,12)*cknorm(icol,k,12)                 &
                            +Nnatk(icol,k,14)*cknorm(icol,k,14)*fnbc(icol,k)
             c_bc05(icol,k)  = c_bc05(icol,k)                                   &
                            +Nnatk(icol,k,2)*cintbg05(icol,k,2)                 &
                            +Nnatk(icol,k,4)*cintbg05(icol,k,4)*faitbc(icol,k) &
                            +Nnatk(icol,k,0)*cintbg05(icol,k,0)                 &
                            +Nnatk(icol,k,12)*cknlt05(icol,k,12)                &
                            +Nnatk(icol,k,14)*cknlt05(icol,k,14)*fnbc(icol,k)
             c_bc125(icol,k) = c_bc125(icol,k)                                  &
                            +Nnatk(icol,k,2)*cintbg125(icol,k,2)                &
                            +Nnatk(icol,k,4)*cintbg125(icol,k,4)*faitbc(icol,k) &
                            +Nnatk(icol,k,0)*cintbg125(icol,k,0)                &
                            +Nnatk(icol,k,12)*ckngt125(icol,k,12)               &
                            +Nnatk(icol,k,14)*ckngt125(icol,k,14)*fnbc(icol,k)
             c_oc_ac(icol,k)= c_oc(icol,k)
             c_oc_4(icol,k)  = Nnatk(icol,k,4)*cintbg(icol,k,4)*(1.0_r8-faitbc(icol,k))
             c_oc_14(icol,k) = Nnatk(icol,k,14)*cknorm(icol,k,14)*(1.0_r8-fnbc(icol,k))
             c_oc(icol,k)    = c_oc(icol,k)                                           &
                            +Nnatk(icol,k,1)*cintbg(icol,k,1)*f_soana(icol,k)         &
!-3                            +Nnatk(icol,k,3)*cintbg(icol,k,3)                         &
                            +Nnatk(icol,k,4)*cintbg(icol,k,4)*(1.0_r8-faitbc(icol,k))   &
                            +Nnatk(icol,k,14)*cknorm(icol,k,14)*(1.0_r8-fnbc(icol,k))
             c_oc05(icol,k)  = c_oc05(icol,k)                                         &
                            +Nnatk(icol,k,1)*cintbg05(icol,k,1)*f_soana(icol,k)       &
!-3                            +Nnatk(icol,k,3)*cintbg05(icol,k,3)                       &
                            +Nnatk(icol,k,4)*cintbg05(icol,k,4)*(1.0_r8-faitbc(icol,k))  &
                            +Nnatk(icol,k,14)*cknlt05(icol,k,14)*(1.0_r8-fnbc(icol,k))
             c_oc125(icol,k) = c_oc125(icol,k)                                        &
                            +Nnatk(icol,k,1)*cintbg125(icol,k,1)*f_soana(icol,k)      &
!-3                            +Nnatk(icol,k,3)*cintbg125(icol,k,3)                      &
                            +Nnatk(icol,k,4)*cintbg125(icol,k,4)*(1.0_r8-faitbc(icol,k)) &
                            +Nnatk(icol,k,14)*ckngt125(icol,k,14)*(1.0_r8-fnbc(icol,k))
             c_s4(icol,k)    = c_sa(icol,k)+c_sc(icol,k)          &
                            +Nnatk(icol,k,1)*cintbg(icol,k,1)*(1.0_r8-f_soana(icol,k))   &
                            +Nnatk(icol,k,5)*cintbg(icol,k,5)     
             c_s405(icol,k)  = c_sa05(icol,k)+c_sc05(icol,k)      &
                            +Nnatk(icol,k,1)*cintbg05(icol,k,1)*(1.0_r8-f_soana(icol,k)) &
                            +Nnatk(icol,k,5)*cintbg05(icol,k,5)    
             c_s4125(icol,k) = c_sa125(icol,k)+c_sc125(icol,k)    &
                            +Nnatk(icol,k,1)*cintbg125(icol,k,1)*(1.0_r8-f_soana(icol,k)) &
                            +Nnatk(icol,k,5)*cintbg125(icol,k,5)  

!akc6+
             c_tot(icol,k)    = c_s4(icol,k) + c_oc(icol,k) + c_bc(icol,k) &
                            + c_mi(icol,k) + c_ss(icol,k)
             c_tot125(icol,k) = c_s4125(icol,k) + c_oc125(icol,k) + c_bc125(icol,k) &
                            + c_mi125(icol,k) + c_ss125(icol,k)
             c_tot05(icol,k) = c_s405(icol,k) + c_oc05(icol,k) + c_bc05(icol,k) &
                            + c_mi05(icol,k) + c_ss05(icol,k)
             c_pm25(icol,k)   = c_tot(icol,k) - c_tot125(icol,k)
             c_pm1(icol,k)    = c_tot05(icol,k)
!            mass mixing ratio:
             mmr_pm25(icol,k) = 1.e-9*c_pm25(icol,k)/rhoda(icol,k)   
             mmr_pm1(icol,k)  = 1.e-9*c_pm1(icol,k)/rhoda(icol,k)   
!akc6-

!            converting from S to SO4 concentrations is no longer necessary, since 
!             sc=H2SO4 and sa=(NH4)2SO4 now, not SO4 as in CAM4-Oslo     
!             c_s4(icol,k)=c_s4(icol,k)/3._r8
!             c_s405(icol,k)=c_s405(icol,k)/3._r8
!             c_s4125(icol,k)=c_s4125(icol,k)/3._r8

             c_s4_a(icol,k) = c_sa(icol,k)+c_sc(icol,k) 
             c_s4_1(icol,k) = Nnatk(icol,k,1)*cintbg(icol,k,1)*(1.0_r8-f_soana(icol,k))
             c_s4_5(icol,k) = Nnatk(icol,k,5)*cintbg05(icol,k,5) 

           end do ! icol
        enddo     ! k

!       Total PM and PM2.5 (dry r>1.25um), surface values (ug/m3)
        do icol=1,ncol
!          c_tots(icol) = c_s4(icol,pver) + c_oc(icol,pver) + c_bc(icol,pver) &
!                          + c_mi(icol,pver) + c_ss(icol,pver)
!          c_tot125s(icol) = c_s4125(icol,pver) + c_oc125(icol,pver) + c_bc125(icol,pver) &
!                          + c_mi125(icol,pver) + c_ss125(icol,pver)
!          c_pm25s(icol) = c_tots(icol) - c_tot125s(icol)
!akc6+
          c_tots(icol) = c_tot(icol,pver)
          c_tot125s(icol) = c_tot125(icol,pver)
          c_pm25s(icol) = c_pm25(icol,pver)
!akc6-
        enddo

!       Effective, column integrated, radii for particles
!       smaller and greater than 0.5um, and for all radii
        do icol=1,ncol
            derlt05(icol)=3.0_r8*vaercols(icol)/(aaercols(icol)+eps)
            dergt05(icol)=3.0_r8*vaercoll(icol)/(aaercoll(icol)+eps)
            der(icol)=3.0_r8*(vaercols(icol)+vaercoll(icol)) &
                       /(aaercols(icol)+aaercoll(icol)+eps)
        enddo

        do icol=1,ncol
          dload_s4(icol)=0.0_r8
          dload_s4_a(icol)=0.0_r8
          dload_s4_1(icol)=0.0_r8
          dload_s4_5(icol)=0.0_r8
          dload_oc(icol)=0.0_r8
          dload_bc(icol)=0.0_r8
          dload_bc_ac(icol)=0.0_r8
          dload_bc_0(icol)=0.0_r8
          dload_bc_2(icol)=0.0_r8
          dload_bc_4(icol)=0.0_r8
          dload_bc_12(icol)=0.0_r8
          dload_bc_14(icol)=0.0_r8
          dload_oc_ac(icol)=0.0_r8
          dload_oc_4(icol)=0.0_r8
          dload_oc_14(icol)=0.0_r8
         do k=1,pver
!         Layer thickness, unit km
!-          deltah=1.e-4_r8*(pint(icol,k+1)-pint(icol,k))/(rhoda(icol,k)*9.8_r8)
          deltah=deltah_km(icol,k)
!         Modal and total mass concentrations for clean and dry aerosol, 
!         i.e. not including coag./cond./Aq. BC,OC,SO4 or condensed water. 
!         Units: ug/m3 for concentrations and mg/m2 (--> kg/m2 later) for mass loading.  
          do i=0,nmodes
            ck(icol,k,i)=cknorm(icol,k,i)*Nnatk(icol,k,i)
            dload3d(icol,k,i)=ck(icol,k,i)*deltah
            dload(icol,i)=dload(icol,i)+dload3d(icol,k,i)
          enddo
          nnat_0(icol,k) =Nnatk(icol,k,0)
          nnat_1(icol,k) =Nnatk(icol,k,1)
          nnat_2(icol,k) =Nnatk(icol,k,2)
          nnat_4(icol,k) =Nnatk(icol,k,4)
          nnat_5(icol,k) =Nnatk(icol,k,5)
          nnat_6(icol,k) =Nnatk(icol,k,6)
          nnat_7(icol,k) =Nnatk(icol,k,7)
          nnat_8(icol,k) =Nnatk(icol,k,8)
          nnat_9(icol,k) =Nnatk(icol,k,9)
          nnat_10(icol,k)=Nnatk(icol,k,10)
          nnat_12(icol,k)=Nnatk(icol,k,12)
          nnat_14(icol,k)=Nnatk(icol,k,14)
!         mineral and sea-salt mass concentrations
          cmin(icol,k)=ck(icol,k,6)+ck(icol,k,7)               
          cseas(icol,k)=ck(icol,k,8)+ck(icol,k,9)+ck(icol,k,10)
!          Aerocom: Condensed water loading (mg_m2)
          daerh2o(icol)=daerh2o(icol)+Cwater(icol,k)*deltah
!         just for checking purposes:
          dload_s4(icol)=dload_s4(icol)+c_s4(icol,k)*deltah
          dload_s4_a(icol)=dload_s4_a(icol)+c_s4_a(icol,k)*deltah
          dload_s4_1(icol)=dload_s4_1(icol)+c_s4_1(icol,k)*deltah
          dload_s4_5(icol)=dload_s4_5(icol)+c_s4_5(icol,k)*deltah
          dload_oc(icol)=dload_oc(icol)+c_oc(icol,k)*deltah
          dload_bc(icol)=dload_bc(icol)+c_bc(icol,k)*deltah
!
          dload_bc_ac(icol)=dload_bc_ac(icol)+c_bc_ac(icol,k)*deltah
          dload_bc_0(icol)=dload_bc_0(icol)+c_bc_0(icol,k)*deltah
          dload_bc_2(icol)=dload_bc_2(icol)+c_bc_2(icol,k)*deltah
          dload_bc_4(icol)=dload_bc_4(icol)+c_bc_4(icol,k)*deltah
          dload_bc_12(icol)=dload_bc_12(icol)+c_bc_12(icol,k)*deltah
          dload_bc_14(icol)=dload_bc_14(icol)+c_bc_14(icol,k)*deltah
          dload_oc_ac(icol)=dload_oc_ac(icol)+c_oc_ac(icol,k)*deltah
          dload_oc_4(icol)=dload_oc_4(icol)+c_oc_4(icol,k)*deltah
          dload_oc_14(icol)=dload_oc_14(icol)+c_oc_14(icol,k)*deltah
!
         end do  ! k
         dload_mi(icol)=dload(icol,6)+dload(icol,7)
         dload_ss(icol)=dload(icol,8)+dload(icol,9)+dload(icol,10)
        end do   ! icol

#ifdef  COLTST4INTCONS
!      Testing column burdens for internal consistency between intdrypar* 
!      (use of aerodryk*.out look-up tables) and calculations directly 
!      from the qm1 array. Will only work with #define AEROCOM.
!   
        call coltst4intcons (lchnk, ncol, qm1, deltah_km, rhoda, fnbc, &
            dload_mi, dload_ss, dload_s4, dload_oc, dload_bc, &
            dload_bc_0, dload_bc_2, dload_bc_4, dload_bc_12, dload_bc_14, dload_bc_ac, &
            dload_oc_4, dload_oc_14, dload_oc_ac, dload_s4_a, dload_s4_1, dload_s4_5)
!
#ifdef AEROCOM
        call outfld('CMDRY0  ',cmdry0  ,pcols,lchnk)
        call outfld('CMDRY1  ',cmdry1  ,pcols,lchnk)
        call outfld('CMDRY2  ',cmdry2  ,pcols,lchnk)
        call outfld('CMDRY4  ',cmdry4  ,pcols,lchnk)
        call outfld('CMDRY5  ',cmdry5  ,pcols,lchnk)
        call outfld('CMDRY6  ',cmdry6  ,pcols,lchnk)
        call outfld('CMDRY7  ',cmdry7  ,pcols,lchnk)
        call outfld('CMDRY8  ',cmdry8  ,pcols,lchnk)
        call outfld('CMDRY9  ',cmdry9  ,pcols,lchnk)
        call outfld('CMDRY10 ',cmdry10 ,pcols,lchnk)
        call outfld('CMDRY12 ',cmdry12 ,pcols,lchnk)
        call outfld('CMDRY14 ',cmdry14 ,pcols,lchnk)
#endif
#endif  ! COLTST4INTCONS

!       Internally and externally mixed dry concentrations (ug/m3) of  
!       SO4, BC and OC, for all r, r<0.5um and r>1.25um...
!        call outfld('C_BCPM  ',c_bc   ,pcols,lchnk)
!        call outfld('C_BC05  ',c_bc05 ,pcols,lchnk)
!        call outfld('C_BC125 ',c_bc125,pcols,lchnk)
!        call outfld('C_OCPM  ',c_oc   ,pcols,lchnk)
!        call outfld('C_OC05  ',c_oc05 ,pcols,lchnk)
!        call outfld('C_OC125 ',c_oc125,pcols,lchnk)
!        call outfld('C_S4PM  ',c_s4   ,pcols,lchnk)
!        call outfld('C_S405  ',c_s405 ,pcols,lchnk)
!        call outfld('C_S4125 ',c_s4125,pcols,lchnk)
!       ... and of background components for all r, r<0.5um and r>1.25um
!        call outfld('C_MIPM  ',c_mi   ,pcols,lchnk)
!        call outfld('C_MI05  ',c_mi05 ,pcols,lchnk)
!        call outfld('C_MI125 ',c_mi125,pcols,lchnk)
!        call outfld('C_SSPM  ',c_ss   ,pcols,lchnk)
!        call outfld('C_SS05  ',c_ss05 ,pcols,lchnk)
!        call outfld('C_SS125 ',c_ss125,pcols,lchnk)
        call outfld('PMTOT  ',c_tots   ,pcols,lchnk)
        call outfld('PM25    ',c_pm25s ,pcols,lchnk)
!akc6+
        call outfld('PM2P5   ',c_pm25  ,pcols,lchnk)
        call outfld('MMRPM2P5',mmr_pm25,pcols,lchnk)
        call outfld('MMRPM1  ',mmr_pm1 ,pcols,lchnk)
        call outfld('MMRPM2P5_SRF',mmr_pm25(:pcols,pver),pcols,lchnk) 
!akc6-
!       total (all r) dry concentrations (ug/m3) and loadings (mg/m2)
        call outfld('DLOAD_MI',dload_mi,pcols,lchnk)
        call outfld('DLOAD_SS',dload_ss,pcols,lchnk)
        call outfld('DLOAD_S4',dload_s4,pcols,lchnk)
        call outfld('DLOAD_OC',dload_oc,pcols,lchnk)
        call outfld('DLOAD_BC',dload_bc,pcols,lchnk)

        call outfld('LOADBCAC',dload_bc_ac,pcols,lchnk)
        call outfld('LOADBC0 ',dload_bc_0,pcols,lchnk)
        call outfld('LOADBC2 ',dload_bc_2,pcols,lchnk)
        call outfld('LOADBC4 ',dload_bc_4,pcols,lchnk)
        call outfld('LOADBC12',dload_bc_12,pcols,lchnk)
        call outfld('LOADBC14',dload_bc_14,pcols,lchnk)
        call outfld('LOADOCAC',dload_oc_ac,pcols,lchnk)
        call outfld('LOADOC4 ',dload_oc_4,pcols,lchnk)
        call outfld('LOADOC14',dload_oc_14,pcols,lchnk)
!       condensed water mmr (kg/kg)
        call outfld('MMR_AH2O',mmr_aerh2o,pcols,lchnk)
!       condensed water loading (mg/m2)
        call outfld('DAERH2O ',daerh2o ,pcols,lchnk)
!       number concentrations (1/cm3)
        call outfld('NNAT_0  ',nnat_0 ,pcols,lchnk)
        call outfld('NNAT_1  ',nnat_1 ,pcols,lchnk)
        call outfld('NNAT_2  ',nnat_2 ,pcols,lchnk)
!=0        call outfld('NNAT_3  ',nnat_3 ,pcols,lchnk)
        call outfld('NNAT_4  ',nnat_4 ,pcols,lchnk)
        call outfld('NNAT_5  ',nnat_5 ,pcols,lchnk)
        call outfld('NNAT_6  ',nnat_6 ,pcols,lchnk)
        call outfld('NNAT_7  ',nnat_7 ,pcols,lchnk)
        call outfld('NNAT_8  ',nnat_8 ,pcols,lchnk)
        call outfld('NNAT_9  ',nnat_9 ,pcols,lchnk)
        call outfld('NNAT_10 ',nnat_10,pcols,lchnk)
!=0        call outfld('NNAT_11 ',nnat_11,pcols,lchnk)
        call outfld('NNAT_12 ',nnat_12,pcols,lchnk)
!=0        call outfld('NNAT_13 ',nnat_13,pcols,lchnk)
        call outfld('NNAT_14 ',nnat_14,pcols,lchnk)
!akc6        call outfld('AIRMASSL',airmassl,pcols,lchnk)
        call outfld('AIRMASSL',airmassl,pcols,lchnk)
        call outfld('AIRMASS ',airmass,pcols,lchnk)  !akc6

!c_er3d 
!       effective dry radii (um) in each layer
!        call outfld('ERLT053D',erlt053d,pcols,lchnk)
!        call outfld('ERGT053D',ergt053d,pcols,lchnk)
!        call outfld('ER3D    ',er3d    ,pcols,lchnk)
!c_er3d           
!       column integrated effective dry radii (um)
        call outfld('DERLT05 ',derlt05,pcols,lchnk)
        call outfld('DERGT05 ',dergt05,pcols,lchnk)
        call outfld('DER     ',der    ,pcols,lchnk)
!

!000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000

!     Extra AeroCom diagnostics requiring table look-ups with RH = constant 

#ifdef AEROCOM_INSITU
      irfmax=6
#else
      irfmax=1
#endif  ! AEROCOM_INSITU

!     Note: using xrhnull etc as proxy for constant RH input values (see opttab.F90)
      do irf=1,irfmax
       do k=1,pver
        do icol=1,ncol
          xrhnull(icol,k)=xrhrf(irf)
          irh1null(icol,k)=irhrf1(irf)
        end do
       enddo  
        call opticsAtConstRh(lchnk, ncol, pint, rhoda, Nnatk, xrhnull, irh1null, irf, &
           xct, ict1, xfaq, ifaq1, xfbcbg, ifbcbg1,           &
           xfbcbgn, ifbcbgn1, xfac, ifac1, xfbc, ifbc1,       &
           xfombg, ifombg1, vnbcarr, vaitbcarr, v_soana,      &
           bext440, bext500, bext550, bext670, bext870,       &
           bebg440, bebg500, bebg550, bebg670, bebg870,       &
           bebc440, bebc500, bebc550, bebc670, bebc870,       &
           beoc440, beoc500, beoc550, beoc670, beoc870,       &
           besu440, besu500, besu550, besu670, besu870,       &
           babs440, babs500, babs550, babs670, babs870,       &
           bebglt1, bebggt1, bebclt1, bebcgt1,                &
           beoclt1, beocgt1, bes4lt1, bes4gt1,                &
           backsc550, babg550, babc550, baoc550, basu550,     & 
           bext440n, bext500n, bext550n, bext670n, bext870n,  &
           bebg440n, bebg500n, bebg550n, bebg670n, bebg870n,  &
           bebc440n, bebc500n, bebc550n, bebc670n, bebc870n,  &
           beoc440n, beoc500n, beoc550n, beoc670n, beoc870n,  &
           besu440n, besu500n, besu550n, besu670n, besu870n,  &
           babs440n, babs500n, babs550n, babs670n, babs870n,  &
           bebglt1n, bebggt1n, bebclt1n, bebcgt1n,            &
           beoclt1n, beocgt1n, bes4lt1n, bes4gt1n,            &
           backsc550n, babg550n, babc550n, baoc550n, basu550n)
      end do ! irf

!000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000


#endif  ! ***********AEROCOM***********AEROCOM**************AEROCOM***************above


      return
end subroutine pmxsub
 
end module pmxsub_mod
