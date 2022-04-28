subroutine appformrate(d1, dx, j1, jx, CoagS_dx, gr)
    !-- appformrate calculates the formation rate jx of dx sized particles from the nucleation rate j1 (d1 sized particles)
    !-- Formation rate is parameterized according to Lehtinen et al. (2007), JAS 38:988-994
    !-- Parameterization takes into account the loss of particles due to coagulation
    !-- Growth by self-coagulation is not accounted for
    !-- Typically, 1% of 1 nm nuclei make it to 12 nm
    !-- Written by Risto Makkonen
    
    use shr_kind_mod, only: r8 => shr_kind_r8

    implicit none

	!-- Arguments

	real(r8), intent(in)  :: d1                  ! Size of nucleation-sized particles (nm)
	real(r8), intent(in)  :: dx                  ! Size of calculated apparent formation rate (nm)
	real(r8), intent(in)  :: j1                  ! Nucleation rate of d1 sized particles (# cm-3 s-1)
	real(r8), intent(out) :: jx                  ! Formation rate of dx sized particles (# cm-3 s-1)
	real(r8), intent(in)  :: CoagS_dx            ! Coagulation term for nucleating particles (s-1)
	real(r8), intent(in)  :: gr                  ! Particle growth rate (nm h-1)

        !-- Local variables
    
        real(r8)              :: m
        real(r8)              :: gamma
        real(r8)              :: CoagS_d1            ! Coagulation term for nucleating particles, calculated from CoagS_dx

	! In Hyytiala, typically 80% of the nuclei are scavenged onto larger background particles while they grow from 1 to 3 nm

	!-- (Eq. 6) Exponent m, depends on background distribution
	! m=log(CoagS_dx/CoagS_d1)/log(dx/d1)
	! Or, if we dont want to calculate CoagS_d1, lets assume a typical value for m (-1.5 -- -1.9) and calculate CoagS_d1 from Eq.5
	m=-1.6_r8
	CoagS_d1=CoagS_dx*(d1/dx)**m
        CoagS_d1=MAX(MIN(CoagS_d1,1.E2_r8),1.E-10_r8)

	gamma=(1._r8/(m+1._r8))*((dx/d1)**(m+1._r8)-1._r8)
	gamma=MAX(MIN(gamma,1.E2_r8),1.E-10_r8)
	
	!gr=MAX(MIN(gr,1.E3_r8),1.E-5_r8)
	
	!-- (Eq. 7) CoagS_d1 is multiplied with 3600 to get units h-1
	!WRITE(*,*) 'gammaym:',gamma,exp(-gamma*d1*CoagS_d1*3600/gr)
	jx=j1*exp(-gamma*d1*CoagS_d1*3600._r8/gr)

    return
	
end

    ! First estimate: 99% of particles are lost during growth from 1 nm to 12 nm

	! Koagtendista:
	! Siis lasketaan siella koagulaatio SO4_N -moodille. 
	! Condtend lasketaan ennen coagtendia, eli naita ei ole saatavilla!! Voisiko vaihtaa jarjestysta
	! Nama on constants.F90:ssa  
	!		 rhob(0)  = rhopart(l_bc_ax)       ! mostly not in use (rhorbc in stead)
	!		rk(1)    = effsize(l_so4_n)*1.e6_r8
	! Pitaisko siis koagsubissa laskea oma Kp12s4 nukleaatiokoon hiukkasille, vai olettaako samaksi kuin 10nm, vai onko joku kaava
