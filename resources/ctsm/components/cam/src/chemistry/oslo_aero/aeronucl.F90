subroutine aeronucl(lchnk, ncol, t, pmid, h2ommr, h2so4pc, oxidorg, coagnuc, nuclso4, nuclorg, zm, pblht)

! Subroutine to calculate nucleation (formation) rates of new particles
! At the moment, the final nucleation rate consists of
!  (1) Binary sulphuric acid-water nucleation in whole atmosphere (Vehkamaki et al., 2002, JGR)
!      JGR, vol 107, No D22, http://onlinelibrary.wiley.com/doi/10.1029/2002JD002184/abstract   
!  (2) Boundary-layer nucleation
!      Paasonen et al (2010), ACP, vol 10, pp 11223: http://www.atmos-chem-phys.net/10/11223/2010/acp-10-11223-2010.html
!  (3) First version published ACP (Risto Makkonen)
!      ACP, vol 14, no 10, pp 5127 http://www.atmos-chem-phys.net/14/5127/2014/acp-14-5127-2014.html
! Modified Spring 2015, cka

    use shr_kind_mod,   only: r8 => shr_kind_r8
    use wv_saturation,  only: qsat_water
    use physconst,      only: avogad, rair 
    use ppgrid,         only: pcols, pver, pverp
    use aerosoldef, only : MODE_IDX_SO4SOA_AIT, rhopart, l_so4_a1, l_soa_lv, l_so4_na, l_soa_na
    use commondefinitions, only: originalNumberMedianRadius
    use cam_history,    only: outfld
    use phys_control,   only: phys_getopts
    use chem_mods,      only: adv_mass  
    use m_spc_id,       only : id_H2SO4, id_soa_lv
    use const,          only : volumeToNumber

    implicit none

    !-- Arguments
    integer,  intent(in)  :: lchnk                    ! chunk identifier
    integer,  intent(in)  :: ncol                     ! number of atmospheric column
    real(r8), intent(in)  :: pmid(pcols,pver)         ! layer pressure (Pa)
    real(r8), intent(in)  :: h2ommr(pcols,pver)       ! layer specific humidity
    real(r8), intent(in)  :: t(pcols,pver)            ! Temperature (K)
    real(r8), intent(in)  :: h2so4pc(pcols,pver)      ! Sulphuric acid concentration (kg kg-1)
    real(r8), intent(in)  :: oxidorg(pcols,pver)      ! Organic vapour concentration (kg kg-1) 
    real(r8), intent(in)  :: coagnuc(pcols,pver)      ! Coagulation sink for nucleating particles [1/s]
    real(r8), intent(out) :: nuclorg(pcols,pver)      ! Nucleated mass (ORG)
    real(r8), intent(out) :: nuclso4(pcols,pver)      ! Nucleated mass (H2SO4)
    real(r8), intent(in)  :: zm(pcols,pver)           ! Height at layer midpoints (m)
    real(r8), intent(in)  :: pblht(pcols)             ! Planetary boundary layer height (m)

    !-- Local variables
    
    real(r8), parameter   :: pi=3.141592654_r8
    !cka+
    real(r8), parameter   :: gasconst_R=8.314472_r8    ! universal gas constant [J mol-1 K-1]
    real(r8), parameter   :: h2so4_dens=1841._r8       ! h2so4 density [kg m-3]
    real(r8), parameter   :: org_dens=2000._r8         ! density of organics [kg m-3], based on RM assumptions
    !cka -

    integer               :: i,k
    real(r8)              :: qs(pcols,pver)            ! Saturation specific humidity
    real(r8)              :: relhum(pcols,pver)        ! Relative humidity
    real(r8)              :: h2so4(pcols,pver)         ! Sulphuric acid concentration [#/cm3]
    real(r8)              :: nuclvolume(pcols,pver)    ! [m3/m3/s] Nucleated mass (SO4+ORG)
    real(r8)              :: rhoair(pcols,pver)        ! density of air [kg/m3] !cka
    real(r8)              :: pblht_lim(pcols)          ! Planetary boundary layer height (m) (500m<pblht_lim<7000m) (cka)

    real(r8)              :: nuclrate_bin(pcols,pver) ! Binary nucleation rate (# cm-3 s-1)
    real(r8)              :: formrate_bin(pcols,pver) ! Binary formation rate (12 nm) (# cm-3 s-1)
    real(r8)              :: nuclsize_bin(pcols,pver) ! Binary nucleation critical cluster size (m)
    real(r8)              :: nuclrate_pbl(pcols,pver) ! Boundary layer nucleation rate (# cm-3 s-1)
    real(r8)              :: formrate_pbl(pcols,pver) ! Boundary layer formation rate (12 nm) (# cm-3 s-1)
    real(r8)              :: nuclsize_pbl(pcols,pver) ! Boundary layer nucleation formation size (m)

    real(r8)              :: orgforgrowth(pcols,pver) ! Organic vapour mass available for growth

    real(r8)              :: d_form                   ! Particle size at calculated formation rate [m]
    real(r8)              :: gr(pcols,pver), grh2so4(pcols,pver), grorg(pcols,pver) !growth rates
    real(r8)              :: vmolh2so4, vmolorg       ! [m/s] molecular speed of condenseable gases
    real(r8)              :: frach2so4
    real(r8)              :: dummy                    

    integer               :: atm_nucleation           ! Nucleation parameterization for the whole atmosphere
    integer               :: pbl_nucleation           ! Nucleation parameterization for the boundary layer
    real(r8)              :: molmass_h2so4            ! molecular mass of h2so4 [g/mol] 
    real(r8)              :: molmass_soa              ! molecular mass of soa [g/mol]

   ! Variables for binary nucleation parameterization
   real(r8)              :: zrhoa, zrh, zt, zt2, zt3, zlogrh, zlogrh2, zlogrh3, zlogrhoa, zlogrhoa2, zlogrhoa3, x, zxmole, zix
   real(r8)              :: zjnuc, zntot, zrc, zrxc

!cka: OBS    call phys_getopts(pbl_nucleation_out=pbl_nucleation, atm_nucleation_out=atm_nucleation)
    !cka: testing by setting these flags: 
    pbl_nucleation = 2
    atm_nucleation = 1

    nuclso4(:,:)=0._r8
    nuclorg(:,:)=0._r8
    nuclrate_bin(:,:)=0._r8
    nuclrate_pbl(:,:)=0._r8
    formrate_bin(:,:)=0._r8
    formrate_pbl(:,:)=0._r8 
    !-- The highest level in planetary boundary layer
    do i=1,ncol
        pblht_lim(i)=MIN(MAX(pblht(i),500._r8),7000._r8)
    end do

    !-- Get molecular mass of h2so4 and soa_lv (cka)
    molmass_h2so4=adv_mass(id_H2SO4)
    molmass_soa=adv_mass(id_SOA_LV)

    !-- Formation diameters (m). Nucleated particles are inserted to SO4(n), same size used for soa  (cka)
    d_form=2._r8*originalNumberMedianRadius(MODE_IDX_SO4SOA_AIT) 

    !-- Conversion of H2SO4 from kg/kg to #/cm3 
    !-- and calculation of relative humidity (needed by binary nucleation parameterization)
    do k=1,pver
        do i=1,ncol
            rhoair(i,k)=pmid(i,k)/(t(i,k)*rair)
            !avogad*1.e-3_r8 to get molec/mol instead of molec/kmol
            h2so4(i,k)=(1.e-6_r8*h2so4pc(i,k)*avogad*1.e-3_r8*rhoair(i,k))/(molmass_h2so4*1.E-3_r8)
            orgforgrowth(i,k)=(1.e-6_r8*oxidorg(i,k)*avogad*1.e-3_r8*rhoair(i,k))/(molmass_soa*1.E-3_r8)
            orgforgrowth(i,k)=MAX(MIN(orgforgrowth(i,k),1.E10_r8),0._r8)    
 
            call qsat_water(t(i,k),pmid(i,k),dummy,qs(i,k))

            relhum(i,k) = h2ommr(i,k)/qs(i,k)
            relhum(i,k) = max(relhum(i,k),0.0_r8)
            relhum(i,k) = min(relhum(i,k),1.0_r8)
        end do !ncol  
    end do     !layers  
    
    !-- Binary sulphuric acid-water nucleation rate	  
    if(atm_nucleation .EQ. 1) then
        do k=1,pver
            do i=1,ncol

                ! Calculate nucleation only for valid thermodynamic conditions:
                zrhoa = max(h2so4(i,k),1.E+4_r8)
                zrhoa = min(zrhoa,1.E11_r8)
                     
                zrh   = max(relhum(i,k),1.E-4_r8)
                zrh   = min(zrh,1.0_r8)
                     
                zt    = max(t(i,k),190.15_r8)
                zt    = min(zt,300.15_r8)

                zt2 = zt*zt
                zt3 = zt2*zt

                ! Equation (11) - molefraction of H2SO4 in the critical cluster

                zlogrh  = LOG(zrh)
                zlogrh2 = zlogrh*zlogrh
                zlogrh3 = zlogrh2*zlogrh

                zlogrhoa  = LOG(zrhoa)
                zlogrhoa2 = zlogrhoa*zlogrhoa
                zlogrhoa3 = zlogrhoa2*zlogrhoa

                x=0.7409967177282139_r8 - 0.002663785665140117_r8*zt   &
                + 0.002010478847383187_r8*zlogrh    &
                - 0.0001832894131464668_r8*zt*zlogrh    &
                + 0.001574072538464286_r8*zlogrh2        &
                - 0.00001790589121766952_r8*zt*zlogrh2    &
                + 0.0001844027436573778_r8*zlogrh3     &
                -  1.503452308794887e-6_r8*zt*zlogrh3    &
                - 0.003499978417957668_r8*zlogrhoa   &
                + 0.0000504021689382576_r8*zt*zlogrhoa

                zxmole=x

                zix = 1.0_r8/x

                ! Equation (12) - nucleation rate in 1/cm3s

                zjnuc=0.1430901615568665_r8 + 2.219563673425199_r8*zt -   &
                  0.02739106114964264_r8*zt2 +     &
                  0.00007228107239317088_r8*zt3 + 5.91822263375044_r8*zix +     &
                  0.1174886643003278_r8*zlogrh + 0.4625315047693772_r8*zt*zlogrh -     &
                  0.01180591129059253_r8*zt2*zlogrh +     &
                  0.0000404196487152575_r8*zt3*zlogrh +    &
                  (15.79628615047088_r8*zlogrh)*zix -     &
                  0.215553951893509_r8*zlogrh2 -    &
                  0.0810269192332194_r8*zt*zlogrh2 +     &
                  0.001435808434184642_r8*zt2*zlogrh2 -    &
                  4.775796947178588e-6_r8*zt3*zlogrh2 -     &
                  (2.912974063702185_r8*zlogrh2)*zix -   &
                  3.588557942822751_r8*zlogrh3 +     &
                  0.04950795302831703_r8*zt*zlogrh3 -     &
                  0.0002138195118737068_r8*zt2*zlogrh3 +    &
                  3.108005107949533e-7_r8*zt3*zlogrh3 -     &
                  (0.02933332747098296_r8*zlogrh3)*zix +     &
                  1.145983818561277_r8*zlogrhoa -    &
                  0.6007956227856778_r8*zt*zlogrhoa +    &
                  0.00864244733283759_r8*zt2*zlogrhoa -    &
                  0.00002289467254710888_r8*zt3*zlogrhoa -    &
                  (8.44984513869014_r8*zlogrhoa)*zix +    &
                  2.158548369286559_r8*zlogrh*zlogrhoa +   &
                  0.0808121412840917_r8*zt*zlogrh*zlogrhoa -    &
                  0.0004073815255395214_r8*zt2*zlogrh*zlogrhoa -   &
                  4.019572560156515e-7_r8*zt3*zlogrh*zlogrhoa +    &
                  (0.7213255852557236_r8*zlogrh*zlogrhoa)*zix +    &
                  1.62409850488771_r8*zlogrh2*zlogrhoa -    &
                  0.01601062035325362_r8*zt*zlogrh2*zlogrhoa +   &
                  0.00003771238979714162_r8*zt2*zlogrh2*zlogrhoa +    &
                  3.217942606371182e-8_r8*zt3*zlogrh2*zlogrhoa -    &
                  (0.01132550810022116_r8*zlogrh2*zlogrhoa)*zix +    &
                  9.71681713056504_r8*zlogrhoa2 -    &
                  0.1150478558347306_r8*zt*zlogrhoa2 +    &
                  0.0001570982486038294_r8*zt2*zlogrhoa2 +    &
                  4.009144680125015e-7_r8*zt3*zlogrhoa2 +    &
                  (0.7118597859976135_r8*zlogrhoa2)*zix -    &
                  1.056105824379897_r8*zlogrh*zlogrhoa2 +    &
                  0.00903377584628419_r8*zt*zlogrh*zlogrhoa2 -    &
                  0.00001984167387090606_r8*zt2*zlogrh*zlogrhoa2 +    &
                  2.460478196482179e-8_r8*zt3*zlogrh*zlogrhoa2 -    &
                  (0.05790872906645181_r8*zlogrh*zlogrhoa2)*zix -    &
                  0.1487119673397459_r8*zlogrhoa3 +    &
                  0.002835082097822667_r8*zt*zlogrhoa3 -    &
                  9.24618825471694e-6_r8*zt2*zlogrhoa3 +    &
                  5.004267665960894e-9_r8*zt3*zlogrhoa3 -    &
                  (0.01270805101481648_r8*zlogrhoa3)*zix

                zjnuc=EXP(zjnuc)      !   add. Eq. (12) [1/(cm^3s)]      

                ! Equation (13) - total number of molecules in the critical cluster

                zntot=-0.002954125078716302_r8 - 0.0976834264241286_r8*zt +   &
                  0.001024847927067835_r8*zt2 - 2.186459697726116e-6_r8*zt3 -    &
                  0.1017165718716887_r8*zix - 0.002050640345231486_r8*zlogrh -   &
                  0.007585041382707174_r8*zt*zlogrh +    &
                  0.0001926539658089536_r8*zt2*zlogrh -   &
                  6.70429719683894e-7_r8*zt3*zlogrh -    &
                  (0.2557744774673163_r8*zlogrh)*zix +   &
                  0.003223076552477191_r8*zlogrh2 +   &
                  0.000852636632240633_r8*zt*zlogrh2 -    &
                  0.00001547571354871789_r8*zt2*zlogrh2 +   &
                  5.666608424980593e-8_r8*zt3*zlogrh2 +    &
                  (0.03384437400744206_r8*zlogrh2)*zix +   &
                  0.04743226764572505_r8*zlogrh3 -    &
                  0.0006251042204583412_r8*zt*zlogrh3 +   &
                  2.650663328519478e-6_r8*zt2*zlogrh3 -    &
                  3.674710848763778e-9_r8*zt3*zlogrh3 -   &
                  (0.0002672510825259393_r8*zlogrh3)*zix -    &
                  0.01252108546759328_r8*zlogrhoa +   &
                  0.005806550506277202_r8*zt*zlogrhoa -    &
                  0.0001016735312443444_r8*zt2*zlogrhoa +   &
                  2.881946187214505e-7_r8*zt3*zlogrhoa +    &
                  (0.0942243379396279_r8*zlogrhoa)*zix -   &
                  0.0385459592773097_r8*zlogrh*zlogrhoa -   &
                  0.0006723156277391984_r8*zt*zlogrh*zlogrhoa +   &
                  2.602884877659698e-6_r8*zt2*zlogrh*zlogrhoa +    &
                  1.194163699688297e-8_r8*zt3*zlogrh*zlogrhoa -   &
                  (0.00851515345806281_r8*zlogrh*zlogrhoa)*zix -    &
                  0.01837488495738111_r8*zlogrh2*zlogrhoa +   &
                  0.0001720723574407498_r8*zt*zlogrh2*zlogrhoa -   &
                  3.717657974086814e-7_r8*zt2*zlogrh2*zlogrhoa -    &
                  5.148746022615196e-10_r8*zt3*zlogrh2*zlogrhoa +    &
                  (0.0002686602132926594_r8*zlogrh2*zlogrhoa)*zix -   &
                  0.06199739728812199_r8*zlogrhoa2 +    &
                  0.000906958053583576_r8*zt*zlogrhoa2 -   &
                  9.11727926129757e-7_r8*zt2*zlogrhoa2 -    &
                  5.367963396508457e-9_r8*zt3*zlogrhoa2 -   &
                  (0.007742343393937707_r8*zlogrhoa2)*zix +    &
                  0.0121827103101659_r8*zlogrh*zlogrhoa2 -   &
                  0.0001066499571188091_r8*zt*zlogrh*zlogrhoa2 +    &
                  2.534598655067518e-7_r8*zt2*zlogrh*zlogrhoa2 -    &
                  3.635186504599571e-10_r8*zt3*zlogrh*zlogrhoa2 +    &
                  (0.0006100650851863252_r8*zlogrh*zlogrhoa2)*zix +   &
                  0.0003201836700403512_r8*zlogrhoa3 -    &
                  0.0000174761713262546_r8*zt*zlogrhoa3 +   &
                  6.065037668052182e-8_r8*zt2*zlogrhoa3 -    &
                  1.421771723004557e-11_r8*zt3*zlogrhoa3 +   &
                  (0.0001357509859501723_r8*zlogrhoa3)*zix

                zntot=EXP(zntot)  !  add. Eq. (13)
      
                ! Equation (14) - radius of the critical cluster in nm

                zrc=EXP(-1.6524245_r8+0.42316402_r8*x+0.33466487_r8*LOG(zntot))    ! [nm]

                !----1.2) Limiter

                IF(zjnuc<1.e-7_r8 .OR. zntot<4.0_r8) zjnuc=0.0_r8

                ! limitation to 1E+10 [1/cm3s]

                nuclrate_bin(i,k)=MAX(MIN(zjnuc,1.E10_r8),0._r8)
                nuclsize_bin(i,k)=MAX(MIN(zrc,1.E2_r8),0.01_r8)

            end do
        end do
    else   !No atmospheric nucleation
        nuclrate_bin(:,:)=0._r8
        nuclsize_bin(:,:)=1._r8
    end if

    !-- Boundary layer nucleation	
    do k=1,pver
        do i=1,ncol
         
            !-- Nucleation rate #/cm3/s
            if(pblht_lim(i)>zm(i,k) .AND. pbl_nucleation>0) then
            
                if(pbl_nucleation .EQ. 1) then

                    !-- Paasonen et al. (2010), eqn 10, Table 4
                    nuclrate_pbl(i,k)=(1.7E-6_r8)*h2so4(i,k)

                else if(pbl_nucleation .EQ. 2) then

                    !-- Paasonen et al. (2010)
                    !values from Table 3 in Paasonen et al (2010), modified version of eqn 14
                    nuclrate_pbl(i,k)=(6.1E-7_r8)*h2so4(i,k)+(0.39E-7_r8)*orgforgrowth(i,k)

                end if

                nuclrate_pbl(i,k)=MAX(MIN(nuclrate_pbl(i,k),1.E10_r8),0._r8)

            else !Not using PBL-nucleation
                nuclrate_pbl(i,k)=0._r8
            end if
            !Size [nm] of particles in PBL
            nuclsize_pbl(i,k)=2._r8

        end do !horizontal points
    end do     !levels

    !-- Calculate total nucleated mass
    do k=1,pver
        do i=1,ncol

            !   Molecular speed and growth rate: H2SO4. Eq. 21 in Kerminen and Kulmala 2002
            vmolh2so4=SQRT(8._r8*gasconst_R*t(i,k)/(pi*molmass_h2so4*1.E-3_r8))
            grh2so4(i,k)=(3.E-9_r8/h2so4_dens)*(vmolh2so4*molmass_h2so4*h2so4(i,k))
            grh2so4(i,k)=MAX(MIN(grh2so4(i,k),10000._r8),1.E-10_r8)

            !   Molecular speed and growth rate: ORG. Eq. 21 in Kerminen and Kulmala 2002
            vmolorg=SQRT(8._r8*gasconst_R*t(i,k)/(pi*molmass_soa*1.E-3_r8))
            grorg(i,k)=(3.E-9_r8/org_dens)*(vmolorg*molmass_soa*orgforgrowth(i,k))
            grorg(i,k)=MAX(MIN(grorg(i,k),10000._r8),1.E-10_r8)

            ! Combined growth rate (cka)
            gr(i,k)=grh2so4(i,k)+grorg(i,k)
            
            !-- Lehtinen 2007 parameterization for apparent formation rate
            !   diameters in nm, growth rate in nm h-1, coagulation in s-1

            call appformrate(nuclsize_bin(i,k), d_form*1.E9_r8, nuclrate_bin(i,k), formrate_bin(i,k), coagnuc(i,k), gr(i,k))
            call appformrate(nuclsize_pbl(i,k), d_form*1.E9_r8, nuclrate_pbl(i,k), formrate_pbl(i,k), coagnuc(i,k), gr(i,k))

            formrate_bin(i,k)=MAX(MIN(formrate_bin(i,k),1.E3_r8),0._r8)
            formrate_pbl(i,k)=MAX(MIN(formrate_pbl(i,k),1.E3_r8),0._r8)

            !   Number of mol nucleated per g air per second.
            nuclvolume(i,k) = (formrate_bin(i,k) + formrate_pbl(i,k)) & ![particles/cm3]
                            *1.0e6_r8                                 & !==> [particles / m3 /]
                            /volumeToNumber(MODE_IDX_SO4SOA_AIT)   & !==> [m3_{aer} / m3_{air} / sec]
                            / rhoair(i,k)                            !==> m3_{aer} / kg_{air} /sec

            !Estimate how much is organic based on growth-rate
            if(gr(i,k)>1.E-10_r8) then
              frach2so4=grh2so4(i,k)/gr(i,k)
            else
              frach2so4=1._r8
            end if            

            ! Nucleated so4 and soa mass mixing ratio per second [kg kg-1 s-1]
            ! used density of particle phase, not of condensing gas 
            nuclso4(i,k)=rhopart(l_so4_na)*nuclvolume(i,k)*frach2so4  
            nuclorg(i,k)=rhopart(l_soa_na)*nuclvolume(i,k)*(1.0_r8-frach2so4)

        end do
    end do

    !-- Diagnostic output
    call outfld('NUCLRATE', nuclrate_bin+nuclrate_pbl, pcols   ,lchnk) 
    call outfld('FORMRATE', formrate_bin+formrate_pbl, pcols   ,lchnk) 
    call outfld('COAGNUCL', coagnuc, pcols   ,lchnk) 
    call outfld('GRH2SO4', grh2so4, pcols   ,lchnk) 
    call outfld('GRSOA', grorg, pcols   ,lchnk) 
    call outfld('GR', gr, pcols   ,lchnk) 

    return
end

