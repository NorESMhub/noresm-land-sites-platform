      subroutine constsize(d, imax, imaxi, r, rp, r0, rbcn,
     $ logs0, rhobc, rhooc, rhos, rhosv, rhoc2,
     $ rhow, bcint, fracdim, diff, th, mfv, diffsoa, thsoa, mfvsoa) 

c **********************************************************************************
c     Created by Alf Kirkevåg.
c **********************************************************************************

c     Define constants and parameters for calculations of size distributions.

      use commondefinitions

      implicit none

      INTEGER i, imax, imaxi
      REAL d, r0, rbcn, logs0 
      REAL diff, th, mfv, rhobc, rhooc, rhos, rhosv, rhoc2, rhorbc 
      REAL diffsoa, thsoa, mfvsoa
      REAL rhobcax, rhow, bcint, vbcint, xlastval
      REAL r(0:100), rp(0:100), fracdim(0:100) 
      REAL pi, e
      REAL aunit, boltz, Mair, Msv, Msoa, Mdual, rair, rmol, t0, p0,
     $ Vad, Vadair, Vadsoa 

      PARAMETER  (pi=3.141592654, e=2.718281828)


c   character(len=8),public,  dimension(N_AEROSOL_TYPES)  :: aerosol_type_name = &
c                                 (/"SULFATE ", "BC      ","OM      ", "DUST    ", "SALT    " /)
c     mass densities (kg/m^3)
      rhow=1.0e3         ! mass density of water
      rhosv=1841.0       ! mass density of sulfuric acid, H2SO4
c     the rest are already defined in CAM5-Oslo
c      rhos=1769.0        ! mass density of ammonium sulfate, (NH4)2SO4
      rhos=aerosol_type_density(1)
c      rhobc=2000.0       ! mass density of nucleation mode BC
      rhobc=aerosol_type_density(2)
      rhoc2=rhobc        ! mass density used in c2 (in K12 coeffiecient in koagsub.f)
c      rhooc=1.5e3        ! mass density of OC
      rhooc=aerosol_type_density(3)

c     modal radius and standard deviation for externally mixed 
c     nucleation modes of sulfate (mode 11), BC (mode 12) and OC (mode 14)
cbt      r11=0.0695      ! for comparison with Box & Trautman
ckb      r11=0.05        ! for comparison with Kiehl & Briegleb
cbt      logs11=0.307    ! for comparison with Box & Trautman      
c      r11=0.0118          ! base case
c      logs11=0.2553       ! base case  ! AEROCOM & Stier et al., 2005. 
c      r12=0.04          ! cak_jacobson
c      r12=0.0118         ! base case
c      logs12=0.2553      ! base case  ! Aerocom & Stier et al., 2005.
c      r14=0.04           ! base caes   ! Aerocom
c      logs14=0.2553      ! base case   ! Aerocom
      rbcn=originalNumberMedianRadius(12)*1.e6
c     modal radius and standard deviation for externally mixed 
c     fractal soot accumulation mode (mode 13)
ctest      r13=0.2         ! old base case
c      r13=0.1         ! cak_jacobson/Strom
c      logs13=0.2041   ! base case
      r0=originalNumberMedianRadius(0)*1.e6
      logs0=log10(originalSigma(0))

c     bin-size in log(r) space, d=log(r(i+1))-log(r(i))
      d=0.1
c     value of imax, which with defined d gives minimum and maximum 
c     modelled aerosol radius, rmin=0.001 and rmax=20 (or 100) micron: 
c      imax=1+nint(5.0/d) ! rmax=100
      imax=1+nint(4.3/d) ! rmax=20
c     index for largest radius in the Mie calculations
      imaxi=imax

c     define discrete radii in log(r) space, r(i) and rp(i)=r(i+1/2), and 
c     mass density (rhorbc) for fractal mode 0 (based on Strom et al, 1992).
      bcint=0.0
      vbcint=0.0
      do i=0,imax
        r(i)=10.0**(d*(i-1.0)-3.0)
        rp(i)=r(i)*10.0**(d/2.0)
        fracdim(i)=2.5  ! fractal dimension for aged soot aggregates
ctest        fracdim(i)=1.8  
ctest        fracdim(i)=3.0
        if(r(i).le.rbcn) then
          fracdim(i)=3.0
          rhorbc=rhobc
        else
          rhorbc=rhobc*(rbcn/r(i))**(3.0-fracdim(i))
ctest          rhorbc=rhobc
        endif
c        write(51,*) r(i), rhorbc*1.e-3
        bcint=bcint+d*rhorbc*r(i)**3.0
     $              *exp(-0.5*(log10(r(i)/r0)/logs0)**2.0)
      enddo
      do i=0,imax
        vbcint=vbcint+d*r(i)**3.0
     $              *exp(-0.5*(log10(r(i)/r0)/logs0)**2.0)  
      enddo
      rhobcax=bcint/vbcint
c      write(*,*) 'rhobcax = ', rhobcax


c     key parameters for sulfuric acid, H2SO4 (for standard atmosphere),
c     updated values May 2013, based on Seinfeld & Pandis and Poling, 
c     Prausnitz and O'Connell (5'th edition of The Properties of Gases 
c     and Fluids), using the method by Fuller et al. for diffusion,
c     assuming p = 1 atm and T=273 K (see notes, folder XXVI).
c      diff=9.5e6         ! diffusion coeffisient (um^2/s)
c      th=2.43e8          ! thermal velocity      (um/s)                  
c      mfv=1.65e-2        ! mean free path        (um) 
c     adding SOA-values for the same variables, based on the same as above,
c     assuming molar mass of SOA to be 144 (or 150) (jfr. notater s. 336 i perm XXVI).
c      (rhosoa=rhooc)    for M=144                         (for M=150)
c      diffsoa=6.1e6     ! diffusion coeffisient (um^2/s)    (5.9e6)
c      thsoa=2.0e8       ! thermal velocity      (um/s)      (2.0e8)              
c      mfvsoa=1.2e-2     ! mean free path        (um)        (1.1e-2)

c     From july 2015: diff(soa), th(soa) and mfv(soa) is instead calculated in the code:
c     physical constants and properties for ambient dry air at standard temperature and pressure 
c     (mainly taken from Seinfeld & Pandis (1998), Appendix A):
      boltz=1.381e-23   ! Boltzmann constant (J/K)
      aunit=1.6606e-27  ! Atomic mass unit (kg)
      t0=273.15         ! Standard temperature (K)
      p0=101325.0       ! Standard pressure (Pa)
      rair=1.73e-10     ! Typical air molecule (collision) radius (m)
      Mair=28.97        ! Molecular weight for dry air (atomic units)
      Msv=98.08         ! Molecular weight of sulfuric acid (H2SO4)
      Msoa=aerosol_type_molecular_weight(3) ! ! Molecular weight of SOA
c     calculating microphysical parameters from equations in Ch. 8 of Seinfeld & Pandis (1998): 
      th=1.e6*sqrt(8.0*boltz*t0/(pi*Msv*aunit))         ! thermal velocity for H2SO4 in air (um/s)     
      thsoa=1.e6*sqrt(8.0*boltz*t0/(pi*Msoa*aunit))     ! thermal velocity for SOA in air (um/s)     
      rmol=(3*Msv*aunit/(4*pi*rhosv))**(1.0/3.0)        ! molecule radius for H2SO4 (m)  
      mfv=1.0e6/(pi*sqrt(1.0+Msv/Mair)*(rair+rmol)**2*p0/(boltz*t0))  ! mean free path for H2SO4 in air (um)  
      rmol=(3*Msoa*aunit/(4*pi*rhooc))**(1.0/3.0)       ! molecule radius for H2SO4 (m)  
      mfvsoa=1.0e6/(pi*sqrt(1.0+Msoa/Mair)*(rair+rmol)**2*p0/(boltz*t0))  ! mean free path for SOA in air (um)  
c     formula for collisions between "hard sphere" molecules, for comparison with the Fuller treatment below:
c      diff=(3*pi/32.0)*(1.0+Msv/Mair)*th*mfv            ! diffusion coeffisient for H2SO4 in air (um^2/s)
c      diffsoa=(3*pi/32.0)*(1.0+Msoa/Mair)*thsoa*mfvsoa  ! diffusion coeffisient for SOA in air (um^2/s)   
c     semi-empirically based formula from Fuller et al (1965, 1966, 1969), see "The Properties of Gases and 
c     Liquids" by Poling et al., 5'th edition (DOI:10.1036/0070116822), Eq. 11-4.4:  
      Vadair=19.7   ! atomic diffusion volume for air (Table 11-1)
      Vad=51.96     ! atomic diffusion volume for H2SO4 (estimated from atomic values in Table 11-1) 
      Vadsoa=208.18 ! atomic diffusion volume for SOA as C10H16O2 (estimated from atomic values in Table 11-1)  ! Does not follow from Msoa!!!
      Mdual=2.0/(1.0/Mair+1.0/Msv)
      diff=1.e8*0.00143*t0**1.75
     $ /((p0/1.0e5)*sqrt(Mdual)
     $ *(((Vad)**(1.0/3.0)+(Vadair)**(1.0/3.0))**2))     ! diffusion coeffisient for H2SO4 in air (um^2/s)
      Mdual=2.0/(1.0/Mair+1.0/Msoa)
      diffsoa=1.e8*0.00143*t0**1.75
     $ /((p0/1.0e5)*sqrt(Mdual)
     $ *(((Vadsoa)**(1.0/3.0)+(Vadair)**(1.0/3.0))**2))  ! diffusion coeffisient for SOA in air (um^2/s)
c      write(*,*) ' th = ', th
c      write(*,*) ' thsoa = ', thsoa
c      write(*,*) ' mfv = ', mfv
c      write(*,*) ' mfvsoa = ', mfvsoa
c      write(*,*) ' diff = ', diff
c      write(*,*) ' diffsoa = ', diffsoa

      return
      end 
