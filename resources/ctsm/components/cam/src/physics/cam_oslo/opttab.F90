module opttab

!Purpose: To read in SW look-up tables for calculation of aerosol optical properties, 
!   and to define the grid for discrete input-values in these look-up tables. 

!   Modified for new wavelength bands and look-up tables - Alf Kirkevaag Dec. 2013.
!   Updated for reading input files with extra header info - Alf Kirkevaag, May 2015.
!   Extended for new SOA treatment - Alf Kirkevaag, August 2015.
!   Added output (ASCII) Jabuary 2016: #ifdef COLTST4INTCONS -> extinction 
!   koefficients (wrt. all added mass including condensed water vapour) are 
!   written out for checking against the look-up tables (using xmgrace), e.g. 
!   as function of RH (to be changed to whatever parameter the user is interested in)
!   Modified for optimized added masses and mass fractions for concentrations from 
!   condensation, coagulation or cloud-processing - Alf Kirkevaag, May 2016. 
!   Modified cate values for kcomp=2 (as  in AeroTab) - Alf Kirkevaag October 2016.  

#include <preprocessorDefinitions.h>

  use shr_kind_mod, only: r8 => shr_kind_r8
  use cam_logfile,  only: iulog
  implicit none

  private 
  save


  ! Interfaces
  public initopt


!  integer, public, parameter :: nbands=12    ! number of aerosol spectral bands in CAM4-Oslo
  integer, public, parameter :: nbands=14    ! number of aerosol spectral bands in SW
  integer, public, parameter :: nbmp1=11     ! number of first non-background mode

  real(r8), public, dimension(10)     :: rh
  real(r8), public, dimension(6)      :: fombg, fbcbg, fac, fbc, faq
  real(r8), public, dimension(4,16)   :: cate
  real(r8), public, dimension(5:10,6) :: cat
   
  real(r8), public :: om1(nbands,10,6,16,6)
  real(r8), public :: g1 (nbands,10,6,16,6)
  real(r8), public :: be1(nbands,10,6,16,6)
  real(r8), public :: ke1(nbands,10,6,16,6)

  real(r8), public :: om2to3(nbands,10,16,6,2:3)
  real(r8), public :: g2to3 (nbands,10,16,6,2:3)
  real(r8), public :: be2to3(nbands,10,16,6,2:3)
  real(r8), public :: ke2to3(nbands,10,16,6,2:3)

  real(r8), public :: om4(nbands,10,6,16,6,6)
  real(r8), public :: g4 (nbands,10,6,16,6,6)
  real(r8), public :: be4(nbands,10,6,16,6,6)
  real(r8), public :: ke4(nbands,10,6,16,6,6)

  real(r8), public :: om0(nbands)
  real(r8), public :: g0(nbands)
  real(r8), public :: be0(nbands)
  real(r8), public :: ke0(nbands)

  real(r8), public :: om5to10(nbands,10,6,6,6,6,5:10)
  real(r8), public :: g5to10(nbands,10,6,6,6,6,5:10)
  real(r8), public :: be5to10(nbands,10,6,6,6,6,5:10)
  real(r8), public :: ke5to10(nbands,10,6,6,6,6,5:10)

! relative humidity (RH, as integer for output variable names) for use in AeroCom code 
  integer, public, dimension(6) :: RF = (/0, 40, 55, 65, 75, 85 /)

! AeroCom specific RH input variables for use in opticsAtConstRh.F90     
  integer, public :: irhrf1(6)
  real(r8), public :: xrhrf(6)

  real(r8), public :: e, eps
  parameter (e=2.718281828_r8, eps=1.0e-30_r8)
  

 contains

subroutine initopt

!---------------------------------------------------------------
!   Modified by Egil Storen/NoSerC July 2002.
!   The sequence of the indices in arrays om1, g1, be1 and ke1
!   (common block /tab1/) has been rearranged to avoid cache
!   problems while running subroutine interpol1. Files also 
!   involved by this modification: interpol1.F and opttab.h.
!   Modified for new aerosol schemes by Alf Kirkevaag in January 
!   2006. Modified for new wavelength bands and look-up tables 
!   by Alf Kirkevaag in December 2013, and for SOA in August 2015.
!---------------------------------------------------------------

      use oslo_control, only : oslo_getopts, dir_string_length

      implicit none

      integer kcomp, iwl, irelh, ictot, ifac, ifbc, ifaq, i, irf
      integer ifombg, ifbcbg
      integer ik, ic, ifil, lin, linmax
      real(r8) catot, relh, frac, fabc, fraq, frombg, frbcbg
      real(r8) ssa, ass, ext, spext
      real(r8) :: eps2 = 1.e-2_r8
      real(r8) :: eps3 = 1.e-3_r8
      real(r8) :: eps4 = 1.e-4_r8
      real(r8) :: eps6 = 1.e-6_r8
      character(len=dir_string_length) :: aerotab_table_dir

!     Defining array bounds for tabulated optical parameters (and r and sigma) 
!     relative humidity (only 0 value used for r and sigma tables):
      rh = (/ 0.0_r8, 0.37_r8, 0.47_r8, 0.65_r8, 0.75_r8, 0.8_r8, 0.85_r8, 0.9_r8, 0.95_r8, 0.995_r8 /)

!     AeroCom specific RH input variables for use in opticsAtConstRh.F90     
      do irf=1,6
         xrhrf(irf)  = real(RF(irf))*0.01_r8
      enddo  
      do irelh=1,9
        do irf=1,6
           if(xrhrf(irf)>=rh(irelh).and.xrhrf(irf)<=rh(irelh+1)) then
             irhrf1(irf)=irelh  
           endif
         end do
      end do

!     mass fractions internal mixtures in background (fombg and fbcbg) and mass added to the 
!     background modes (fac, faq, faq)
      fombg = (/ 0.0_r8, 0.2_r8,  0.4_r8, 0.6_r8, 0.8_r8, 1.0_r8  /)
      fac =   (/ 0.0_r8, 0.2_r8,  0.4_r8, 0.6_r8, 0.8_r8, 1.0_r8  /)
      faq =   (/ 0.0_r8, 0.2_r8,  0.4_r8, 0.6_r8, 0.8_r8, 1.0_r8  /)

!     with more weight on low fractions (thus a logaritmic f axis) for BC, 
!     which is less ambundant than sulfate and OC, and the first value 
!     corresponding to a clean background mode:
      fbcbg(1)=1.e-10_r8
      fbc(1)=1.e-10_r8
      do i=2,6
        fbcbg(i)=10**((i-1)/4.0_r8-1.25_r8)
        fbc(i)=fbcbg(i)
      end do
!     and most weight on small concentrations for added mass onto the background: 
      do kcomp=1,4
        cate(kcomp,1)=1.e-10_r8
        do i=2,16
          if(kcomp.eq.1.or.kcomp.eq.2) then
            cate(kcomp,i)=10.0_r8**((i-1)/3.0_r8-6.222_r8)
          elseif(kcomp.eq.3) then
            cate(kcomp,i)=1.0e-10_r8  ! not used
          else
            cate(kcomp,i)=10.0_r8**((i-1)/3.0_r8-4.301_r8)
          endif
        end do
      end do
      do kcomp=5,10
        cat(kcomp,1) =1.e-10_r8
        do i=2,6
          if(kcomp.eq.5) then
            cat(kcomp,i)=10.0_r8**((i-1)-3.824_r8)
          elseif(kcomp.eq.6) then
            cat(kcomp,i)=10.0_r8**((i-1)-3.523_r8)
          elseif(kcomp.eq.7) then
            cat(kcomp,i)=10.0_r8**((i-1)-3.699_r8)
          elseif(kcomp.eq.8) then
            cat(kcomp,i)=10.0_r8**((i-1)-4.921_r8)
          elseif(kcomp.eq.9) then
            cat(kcomp,i)=10.0_r8**((i-1)-3.301_r8)
          else
            cat(kcomp,i)=10.0_r8**((i-1)-3.699_r8)
          endif
        end do
      end do

      call oslo_getopts(aerotab_table_dir_out= aerotab_table_dir)

!     Opening the 'kcomp'-files:

!ccccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5ccccccccc6ccccccccc7cc

      open(40,file=trim(aerotab_table_dir)//'/kcomp1.out' &
             ,form='formatted',status='old')
      open(41,file=trim(aerotab_table_dir)//'/kcomp2.out' &
             ,form='formatted',status='old')
      open(42,file=trim(aerotab_table_dir)//'/kcomp3.out' &
             ,form='formatted',status='old')
      open(43,file=trim(aerotab_table_dir)//'/kcomp4.out' &
             ,form='formatted',status='old')
      open(44,file=trim(aerotab_table_dir)//'/kcomp5.out' &
             ,form='formatted',status='old')
      open(45,file=trim(aerotab_table_dir)//'/kcomp6.out' &
             ,form='formatted',status='old')
      open(46,file=trim(aerotab_table_dir)//'/kcomp7.out' &
             ,form='formatted',status='old')
      open(47,file=trim(aerotab_table_dir)//'/kcomp8.out' &
             ,form='formatted',status='old')
      open(48,file=trim(aerotab_table_dir)//'/kcomp9.out' &
             ,form='formatted',status='old')
      open(49,file=trim(aerotab_table_dir)//'/kcomp10.out'& 
             ,form='formatted',status='old')
      open(50,file=trim(aerotab_table_dir)//'/kcomp0.out'& 
             ,form='formatted',status='old')
 
!     Skipping the header-text in all input files (Later: use it to check AeroTab - CAM5-Oslo consistency!)
      do ifil = 40,50
        call checkTableHeader (ifil)
      enddo

!     Then reading in the look-up table entries for each file (kcomp*.out)
     
!ccccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5ccccccccc6ccccccccc7cc
!       Mode 0, BC(ax)
!ccccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5ccccccccc6ccccccccc7cc

        ifil = 11
        linmax=nbands
        do lin = 1,linmax

          read(39+ifil,996) kcomp, iwl, relh, &
                          ssa, ass, ext, spext
          om0(iwl)=ssa    
          g0 (iwl)=ass
          be0(iwl)=ext    ! unit km^-1
          ke0(iwl)=spext  ! unit m^2/g

!      write(iulog,*) 'kcomp, om =', kcomp, om0(iwl)
!      write(iulog,*) 'kcomp, g  =', kcomp, g0(iwl)
!      write(iulog,*) 'kcomp, be =', kcomp, be0(iwl)
!      write(iulog,*) 'kcomp, ke =', kcomp, ke0(iwl)

        end do

    do iwl=1,nbands
     if(be0(iwl)<=0.0_r8) then
      write(iulog,*) 'be0 =', iwl, be0(iwl)
      write(iulog,*) 'Error in initialization of be0'
      stop
     endif
    enddo

        write(iulog,*)'mode 0 ok' 


!ccccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5ccccccccc6ccccccccc7cc
!       Mode 1 (H2SO4 and SOA + condesate from H2SO4 and SOA)
!ccccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5ccccccccc6ccccccccc7cc

#ifdef COLTST4INTCONS
!      open(101, file='check-kcomp1.out')
#endif

      linmax = nbands*10*6*16*6   ! 14*10*6*16*6
      do ifil = 1,1
        do lin = 1,linmax 

          read(39+ifil,995) kcomp, iwl, relh, frombg, catot, frac, &
                          ssa, ass, ext, spext

       	  do ic=1,10
	   if(abs(relh-rh(ic))<eps4) then
	    irelh=ic
	    goto 61
	   endif
	  end do
   61     continue

 	  do ic=1,16
	   if(abs((catot-cate(kcomp,ic))/cate(kcomp,ic))<eps2) then
	    ictot=ic
	    goto 71
	   endif
	  end do
   71     continue

 	  do ic=1,6
	   if(abs(frombg-fombg(ic))<eps4) then
	    ifombg=ic
	    goto 72
	   endif
	  end do
   72     continue

 	  do ic=1,6
	   if(abs(frac-fac(ic))<eps4) then
	    ifac=ic
	    goto 73
	   endif
	  end do
   73     continue

          om1(iwl,irelh,ifombg,ictot,ifac)=ssa    
          g1 (iwl,irelh,ifombg,ictot,ifac)=ass
          be1(iwl,irelh,ifombg,ictot,ifac)=ext    ! unit km^-1
          ke1(iwl,irelh,ifombg,ictot,ifac)=spext  ! unit m^2/g

!      write(iulog,*) 'kcomp, om =', kcomp, om1(iwl,irelh,ifombg,ictot,ifac)
!      write(iulog,*) 'kcomp, g  =', kcomp, g1(iwl,irelh,ifombg,ictot,ifac)
!      write(iulog,*) 'kcomp, be =', kcomp, be1(iwl,irelh,ifombg,ictot,ifac)
!      write(iulog,*) 'kcomp, ke =', kcomp, ke1(iwl,irelh,ifombg,ictot,ifac)

#ifdef COLTST4INTCONS
!     write to file parameters and dependencies for check against AeroTab and 
!     general model results wrt. RRTMG optics (for the vis. band 0.442-0.625um)
!      if(iwl.eq.4) then
!        write(101,*) rh(irelh), ke1(iwl,irelh,ifombg,ictot,ifac)
!      endif
#endif

        end do  ! lin
      end do    ! ifil

    do kcomp=1,1
    do iwl=1,nbands
    do irelh=1,10
    do ifombg=1,6
    do ictot=1,16
    do ifac=1,6
     if(be1(iwl,irelh,ifombg,ictot,ifac)<=0.0_r8) then
      write(iulog,*) 'be1 =', iwl, irelh, ifombg, ictot, be1(iwl,irelh,ifombg,ictot,ifac)
      write(iulog,*) 'Error in initialization of be1'
      stop
     endif
    enddo
    enddo
    enddo
    enddo
    enddo
    enddo

#ifdef COLTST4INTCONS
!      close(101)
#endif

        write(iulog,*)'mode 1 ok' 

!ccccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5ccccccccc6ccccccccc7cc
!       Modes 2 to 3 (BC/OC + condensate from H2SO4 and SOA)
!ccccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5ccccccccc6ccccccccc7cc

#ifdef COLTST4INTCONS
!      open(102, file='check-kcomp2.out')
!      open(103, file='check-kcomp3.out')
#endif

      linmax=nbands*10*16*6
!      do ifil = 2,3
      do ifil = 2,2
        do lin = 1,linmax

          read(39+ifil,997) kcomp, iwl, relh, catot, frac, &
                          ssa, ass, ext, spext

       	  do ic=1,10
	   if(abs(relh-rh(ic))<eps4) then
	    irelh=ic
	    goto 121
	   endif
	  end do
  121     continue

 	  do ic=1,16
	   if(abs((catot-cate(kcomp,ic))/cate(kcomp,ic))<eps2) then
	    ictot=ic
	    goto 131
	   endif
	  end do
  131     continue

 	  do ic=1,6
	   if(abs(frac-fac(ic))<eps4) then
	    ifac=ic
	    goto 141
	   endif
	  end do
  141     continue

          om2to3(iwl,irelh,ictot,ifac,kcomp)=ssa    
          g2to3 (iwl,irelh,ictot,ifac,kcomp)=ass
          be2to3(iwl,irelh,ictot,ifac,kcomp)=ext    ! unit km^-1
          ke2to3(iwl,irelh,ictot,ifac,kcomp)=spext  ! unit m^2/g

!      write(iulog,*) 'kcomp, om =', kcomp, om2to3(iwl,irelh,ictot,ifac,kcomp)
!      write(iulog,*) 'kcomp, g  =', kcomp, g2to3(iwl,irelh,ictot,ifac,kcomp)
!      write(iulog,*) 'kcomp, be =', kcomp, be2to3(iwl,irelh,ictot,ifac,kcomp)
!      write(iulog,*) 'kcomp, ke =', kcomp, ke2to3(iwl,irelh,ictot,ifac,kcomp)

#ifdef COLTST4INTCONS
!     write to file parameters and dependencies for check against AeroTab and 
!     general model results wrt. RRTMG optics (for the vis. band 0.442-0.625um)
!      if(iwl.eq.4) then
!        write(100+ifil,*) rh(irelh), ke2to3(iwl,irelh,ictot,ifac,ifil)
!      endif
#endif

        end do  ! lin
      enddo     ! ifil

!   Prescribed dummy values for kcomp=3
    kcomp=3
    do iwl=1,nbands
    do irelh=1,10
    do ictot=1,16
    do ifac=1,6
          om2to3(iwl,irelh,ictot,ifac,kcomp)=0.999_r8    
          g2to3 (iwl,irelh,ictot,ifac,kcomp)=0.5_r8
          be2to3(iwl,irelh,ictot,ifac,kcomp)=0.0001_r8    ! unit km^-1
          ke2to3(iwl,irelh,ictot,ifac,kcomp)=1.0_r8       ! unit m^2/g
    enddo
    enddo
    enddo
    enddo

    do kcomp=2,3
    do iwl=1,nbands
    do irelh=1,10
    do ictot=1,16
    do ifac=1,6
     if(be2to3(iwl,irelh,ictot,ifac,kcomp)<=0.0_r8) then
      write(iulog,*) 'be2to3 =', iwl, irelh, ictot, ifac, be2to3(iwl,irelh,ictot,ifac,kcomp)
      write(iulog,*) 'Error in initialization of be2to3'
      stop
     endif
    enddo
    enddo
    enddo
    enddo
    enddo

#ifdef COLTST4INTCONS
!    do ifil=2,3
!      close(100+ifil)
!    end do
#endif

        write(iulog,*)'modes 2-3 ok' 

!ccccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5ccccccccc6ccccccccc7cc
!       Mode 4 (BC&OC + condensate from H2SO4 and SOA + wet phase (NH4)2SO4)
!ccccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5ccccccccc6ccccccccc7cc

#ifdef COLTST4INTCONS
!      open(104, file='check-kcomp4.out')
#endif

        ifil = 4
        linmax = nbands*10*6*16*6*6 
        do lin = 1,linmax

          read(39+ifil,993) kcomp, iwl, relh, frbcbg, catot, frac, fraq, &
                          ssa, ass, ext, spext

       	  do ic=1,10
	   if(abs(relh-rh(ic))<eps4) then
	    irelh=ic
	    goto 81
	   endif
	  end do
   81     continue

 	  do ic=1,16
	   if(abs((catot-cate(kcomp,ic))/cate(kcomp,ic))<eps2) then
	    ictot=ic
	    goto 91
	   endif
	  end do
   91     continue

 	  do ic=1,6
	   if(abs(frac-fac(ic))<eps4) then
	    ifac=ic
	    goto 101
	   endif
	  end do
  101     continue

	  do ic=1,6
	   if(abs(fraq-faq(ic))<eps4) then
	    ifaq=ic
	    goto 111
	   endif
	  end do
  111     continue

 	  do ic=1,6
!	   if(abs(frbcbg-fbcbg(ic))<eps4) then
!	   if(abs(frbcbg-fbcbg(ic))<eps3) then
	   if(abs((frbcbg-fbcbg(ic))/fbcbg(ic))<eps2) then
	    ifbcbg=ic
	    goto 112
	   endif
	  end do
  112     continue

          om4(iwl,irelh,ifbcbg,ictot,ifac,ifaq)=ssa    
          g4 (iwl,irelh,ifbcbg,ictot,ifac,ifaq)=ass
          be4(iwl,irelh,ifbcbg,ictot,ifac,ifaq)=ext    ! unit km^-1
          ke4(iwl,irelh,ifbcbg,ictot,ifac,ifaq)=spext  ! unit m^2/g

!      write(iulog,*) 'kcomp, om =', kcomp, om4(iwl,irelh,ifbcbg,ictot,ifac,ifaq)
!      write(iulog,*) 'kcomp, g  =', kcomp, g4(iwl,irelh,ifbcbg,ictot,ifac,ifaq)
!      write(iulog,*) 'kcomp, be =', kcomp, be4(iwl,irelh,ifbcbg,ictot,ifac,ifaq)
!      write(iulog,*) 'kcomp, ke =', kcomp, ke4(iwl,irelh,ifbcbg,ictot,ifac,ifaq)

#ifdef COLTST4INTCONS
!     write to file parameters and dependencies for check against AeroTab and 
!     general model results wrt. RRTMG optics (for the vis. band 0.442-0.625um)
!      if(iwl.eq.4) then
!        write(104,*) rh(irelh), ke4(iwl,irelh,ifbcbg,ictot,ifac,ifaq)
!      endif
#endif
        end do

    do iwl=1,nbands
    do irelh=1,10
    do ifbcbg=1,6
    do ictot=1,16
    do ifac=1,6
    do ifaq=1,6
     if(be4(iwl,irelh,ifbcbg,ictot,ifac,ifaq)<=0.0_r8) then
      write(iulog,*) 'be4 =', iwl, irelh, ifbcbg, ictot, ifac, ifaq, be4(iwl,irelh,ifbcbg,ictot,ifac,ifaq)
      write(iulog,*) 'Error in initialization of be4'
      stop
     endif
    enddo
    enddo
    enddo
    enddo
    enddo
    enddo

#ifdef COLTST4INTCONS
!      close(104)
#endif

        write(iulog,*)'mode 4 ok' 


!ccccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5ccccccccc6ccccccccc7cc
!       Modes 5 to 10 (SO4(Ait75) and mineral and seasalt-modes + cond./coag./aq.)
!ccccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5ccccccccc6ccccccccc7cc

#ifdef COLTST4INTCONS
!      open(105, file='check-kcomp5.out')
!      open(106, file='check-kcomp6.out')
!      open(107, file='check-kcomp7.out')
!      open(108, file='check-kcomp8.out')
!      open(109, file='check-kcomp9.out')
!      open(110, file='check-kcomp10.out')
#endif

      linmax = nbands*10*6*6*6*6     ! 14*10*6*6*6*6
      do ifil = 5,10
        do lin = 1,linmax   

          read(39+ifil,993) kcomp, iwl, relh, catot, frac, fabc, fraq, &
                          ssa, ass, ext, spext

       	  do ic=1,10
	   if(abs(relh-rh(ic))<eps4) then
	    irelh=ic
	    goto 11
	   endif
	  end do
   11     continue

 	  do ic=1,6
!	   if(abs(catot-cat(kcomp,ic))<eps6) then
	   if(abs((catot-cat(kcomp,ic))/cat(kcomp,ic))<eps2) then
	    ictot=ic
	    goto 21
	   endif
	  end do
   21     continue

 	  do ic=1,6
	   if(abs(frac-fac(ic))<eps4) then
	    ifac=ic
	    goto 31
	   endif
	  end do
   31     continue

 	  do ic=1,6
!	   if(abs(fabc-fbc(ic))<eps4) then
	   if(abs((fabc-fbc(ic))/fbc(ic))<eps2) then
	    ifbc=ic
	    goto 41
	   endif
	  end do
   41     continue

	  do ic=1,6
	   if(abs(fraq-faq(ic))<eps4) then
	    ifaq=ic
	    goto 51
	   endif
	  end do
   51     continue

          om5to10(iwl,irelh,ictot,ifac,ifbc,ifaq,kcomp)=ssa    
          g5to10 (iwl,irelh,ictot,ifac,ifbc,ifaq,kcomp)=ass
          be5to10(iwl,irelh,ictot,ifac,ifbc,ifaq,kcomp)=ext    ! unit km^-1
          ke5to10(iwl,irelh,ictot,ifac,ifbc,ifaq,kcomp)=spext  ! unit m^2/g

!      write(iulog,*) 'kcomp, om =', kcomp, om5to10(iwl,irelh,ictot,ifac,ifbc,ifaq,kcomp) 
!      write(iulog,*) 'kcomp, g  =', kcomp, g5to10(iwl,irelh,ictot,ifac,ifbc,ifaq,kcomp) 
!      write(iulog,*) 'kcomp, be =', kcomp, be5to10(iwl,irelh,ictot,ifac,ifbc,ifaq,kcomp) 
!      write(iulog,*) 'kcomp, ke =', kcomp, ke5to10(iwl,irelh,ictot,ifac,ifbc,ifaq,kcomp) 

#ifdef COLTST4INTCONS
!     write to file parameters and dependencies for check against AeroTab and 
!     general model results wrt. RRTMG optics (for the vis. band 0.442-0.625um)
!      if(iwl.eq.4) then
!        write(100+ifil,*) rh(irelh), ke5to10(iwl,irelh,ictot,ifac,ifbc,ifaq,ifil)
!      endif
#endif

        end do  ! ifil
      end do    ! lin


    do kcomp=5,10
    do iwl=1,nbands
    do irelh=1,10
    do ictot=1,6
    do ifac=1,6
    do ifaq=1,6
     if(be5to10(iwl,irelh,ictot,ifac,ifbc,ifaq,kcomp)<=0.0_r8) then
      write(iulog,*) 'be5to10 =', iwl, irelh, ictot, ifac, ifbc, ifaq, be5to10(iwl,irelh,ictot,ifac,ifbc,ifaq,kcomp)
      write(iulog,*) 'Error in initialization of be5to10'
      write(iulog,*) 'kcomp, abs((fabc-fbc)/fbc) =', kcomp, abs((fabc-fbc(ic))/fbc(ic))
      stop
     endif
    enddo
    enddo
    enddo
    enddo
    enddo
    enddo

#ifdef COLTST4INTCONS
!    do ifil=5,10
!      close(100+ifil)
!    end do
#endif

        write(iulog,*)'modes 5-10 ok' 


!ccccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5ccccccccc6ccccccccc7cc


  993 format(2I3,f8.3,3(x,e10.3),f7.2,4(x,e12.5))
  995 format(2I3,f8.3,3(x,e10.3),4(x,e12.5))
  996 format(2I3,f8.3,4(x,e12.5))
  997 format(2I3,f8.3,2(x,e10.3),4(x,e12.5))


      do ifil=40,50
        close (ifil)
      end do 
      return
end subroutine initopt


end module opttab

