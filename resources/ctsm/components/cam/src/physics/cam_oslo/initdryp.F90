subroutine initdryp

   use shr_kind_mod, only: r8 => shr_kind_r8
   use oslo_control, only: oslo_getopts, dir_string_length
   use commondefinitions, only: nmodes, nbmodes
   use opttab,       only: cate, cat, fac, faq, fbc, fombg, fbcbg
   use cam_logfile,  only: iulog

   implicit none

!Purpose: To read in the AeroCom look-up tables for calculation of dry
!     aerosol size and mass distribution properties. The grid for discrete 
!     input-values in the look-up tables is defined in opptab. 

!     Tabulating the 'aerodryk'-files to save computing time. Routine
!     originally made by  Alf Kirkevaag, and modified for new aerosol 
!     schemes in January 2006.
!     Updated for new kcomp1.out including condensed SOA - Alf Kirkevåg, 
!     May 2013, and extended for new SOA treatment October 2015.
!     Modified for optimized added masses and mass fractions for 
!     concentrations from condensation, coagulation or cloud-processing 
!     - Alf Kirkevaag, May 2016. 
!     Modified for optimized added masses and mass fractions for concentrations from 
!     condensation, coagulation or cloud-processing - Alf Kirkevaag, May 2016. 


#include <aerodry.h>

      integer iv, kcomp, ifombg, ifbcbg, ictot, ifac, ifbc, ifaq
      integer ic, ifil, lin
      real(r8) frombg, frbcbg, catot, frac, fabc, fraq, &
          cintbg, cintbg05, cintbg125, cintbc, cintbc05, cintbc125,   &  
          cintoc, cintoc05, cintoc125, cintsc, cintsc05, cintsc125,   &      
          cintsa, cintsa05, cintsa125, aaeros, aaerol, vaeros, vaerol 
      real(r8) :: eps2 = 1.e-2_r8
      real(r8) :: eps4 = 1.e-4_r8
      real(r8) :: eps6 = 1.e-6_r8
      real(r8) :: eps7 = 1.e-7_r8
      character(len=dir_string_length) :: aerotab_table_dir

      call oslo_getopts(aerotab_table_dir_out = aerotab_table_dir)
!ccccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5ccccccccc6ccccccccc7cc

      open(11,file=trim(aerotab_table_dir)//'/aerodryk2.out' &
             ,form='formatted',status='old')
      open(12,file=trim(aerotab_table_dir)//'/aerodryk3.out' &
            ,form='formatted',status='old')
      open(13,file=trim(aerotab_table_dir)//'/aerodryk4.out' &
             ,form='formatted',status='old')
      open(14,file=trim(aerotab_table_dir)//'/aerodryk5.out' &
             ,form='formatted',status='old')
      open(15,file=trim(aerotab_table_dir)//'/aerodryk6.out' &
             ,form='formatted',status='old')
      open(16,file=trim(aerotab_table_dir)//'/aerodryk7.out' &
             ,form='formatted',status='old')
      open(17,file=trim(aerotab_table_dir)//'/aerodryk8.out' &
             ,form='formatted',status='old')
      open(18,file=trim(aerotab_table_dir)//'/aerodryk9.out' &
             ,form='formatted',status='old')
      open(19,file=trim(aerotab_table_dir)//'/aerodryk10.out' & 
             ,form='formatted',status='old')
      open(20,file=trim(aerotab_table_dir)//'/aerodryk0.out' & 
             ,form='formatted',status='old')
      open(21,file=trim(aerotab_table_dir)//'/aerodryk1.out' & 
             ,form='formatted',status='old')

!     Skipping the header-text in all input files (Later: use it to check AeroTab - CAM5-Oslo consistency!)
      do ifil = 11,21
        call checkTableHeader (ifil)
      enddo


!ccccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5ccccccccc6ccccccccc7cc
!       Mode 0, BC(ax)
!ccccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5ccccccccc6ccccccccc7cc

        ifil = 11
       
          read(9+ifil,996) kcomp, cintbg, cintbg05, cintbg125, &
           aaeros, aaerol, vaeros, vaerol

!         no ictot-, ifac-, ifbc- or ifaq-dependency for this mode,
!         since BC(ax) is purely externally mixed 

           a0cintbg=cintbg
           a0cintbg05=cintbg05
           a0cintbg125=cintbg125

           a0aaeros=aaeros
           a0aaerol=aaerol
           a0vaeros=vaeros
           a0vaerol=vaerol

        write(iulog,*)'mode 0 ok'



!ccccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5ccccccccc6ccccccccc7cc
!       Mode 1 (H2SO4 and SOA + condensate from H2SO4 and SOA)
!ccccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5ccccccccc6ccccccccc7cc

        ifil = 1
        do lin = 1,576     ! 6x16x6

          read(20+ifil,997) kcomp, frombg, catot, frac,               &
           cintbg, cintbg05, cintbg125, cintbc, cintbc05, cintbc125,  &
           cintoc, cintoc05, cintoc125, cintsc, cintsc05, cintsc125,  &      
           cintsa, cintsa05, cintsa125, aaeros, aaerol, vaeros, vaerol

 	  do ic=1,6
	   if(abs(frombg-fombg(ic))<eps4) then
	    ifombg=ic
	    goto 50
	   endif
	  end do
   50     continue

 	  do ic=1,16
!	   if(abs(catot-cate(kcomp,ic))<eps7) then
	   if(abs((catot-cate(kcomp,ic))/cate(kcomp,ic))<eps2) then
	    ictot=ic
	    goto 52
	   endif
	  end do
   52     continue

 	  do ic=1,6
	   if(abs(frac-fac(ic))<eps4) then
	    ifac=ic
	    goto 53
	   endif
	  end do
  53     continue

!         no ifombg-dependency for this mode, since all catot 
!         comes from condensate or from wet-phase sulfate 

            a1var(1,ifombg,ictot,ifac)=cintbg
            a1var(2,ifombg,ictot,ifac)=cintbg05
            a1var(3,ifombg,ictot,ifac)=cintbg125
            a1var(4,ifombg,ictot,ifac)=cintbc
            a1var(5,ifombg,ictot,ifac)=cintbc05
            a1var(6,ifombg,ictot,ifac)=cintbc125
            a1var(7,ifombg,ictot,ifac)=cintoc
            a1var(8,ifombg,ictot,ifac)=cintoc05
            a1var(9,ifombg,ictot,ifac)=cintoc125
            a1var(10,ifombg,ictot,ifac)=cintsc
            a1var(11,ifombg,ictot,ifac)=cintsc05
            a1var(12,ifombg,ictot,ifac)=cintsc125
            a1var(13,ifombg,ictot,ifac)=cintsa
            a1var(14,ifombg,ictot,ifac)=cintsa05
            a1var(15,ifombg,ictot,ifac)=cintsa125
            a1var(16,ifombg,ictot,ifac)=aaeros
            a1var(17,ifombg,ictot,ifac)=aaerol
            a1var(18,ifombg,ictot,ifac)=vaeros
            a1var(19,ifombg,ictot,ifac)=vaerol          

           if(cintsa<cintsa05) &
           write(*,*) 'cintsatot =', ictot, ifac, ifaq, cintsa, cintsa05, cintsa125

        end do  ! lin

    do iv=1,19
    do ifombg=1,6
    do ictot=1,16
    do ifac=1,6
     if(a1var(iv,ifombg,ictot,ifac)<=0.0_r8) then
      write(*,*) 'a1var =', iv, ifombg, ictot, ifac, a1var(iv,ifombg,ictot,ifac)
      write(*,*) 'Error in initialization of a1var'
      stop
     endif
    enddo
    enddo
    enddo
    enddo

        write(iulog,*)'new aerodry mode 1 ok'


!ccccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5ccccccccc6ccccccccc7cc
!       Modes 2 to 3 (BC/OC + condesate from H2SO4 and SOA)
!ccccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5ccccccccc6ccccccccc7cc

!      do ifil = 2,3
      do ifil = 2,2
        do lin = 1,96     ! 16x6

          read(9+ifil,994) kcomp, catot, frac,                        &
           cintbg, cintbg05, cintbg125, cintbc, cintbc05, cintbc125,  &
           cintoc, cintoc05, cintoc125, cintsc, cintsc05, cintsc125,  &      
           cintsa, cintsa05, cintsa125, aaeros, aaerol, vaeros, vaerol

 	  do ic=1,16
!	   if(abs(catot-cate(kcomp,ic))<eps7) then
	   if(abs((catot-cate(kcomp,ic))/cate(kcomp,ic))<eps2) then
	    ictot=ic
	    goto 61
	   endif
	  end do
   61     continue

 	  do ic=1,6
	   if(abs(frac-fac(ic))<eps4) then
	    ifac=ic
	    goto 62
	   endif
	  end do
   62     continue

!         no ifbc- or ifaq-dependency for these modes,
!         since all catot comes from condensed SOA and H2SO4

            a2to3var(1,ictot,ifac,kcomp)=cintbg
            a2to3var(2,ictot,ifac,kcomp)=cintbg05
            a2to3var(3,ictot,ifac,kcomp)=cintbg125
            a2to3var(4,ictot,ifac,kcomp)=cintbc
            a2to3var(5,ictot,ifac,kcomp)=cintbc05
            a2to3var(6,ictot,ifac,kcomp)=cintbc125
            a2to3var(7,ictot,ifac,kcomp)=cintoc
            a2to3var(8,ictot,ifac,kcomp)=cintoc05
            a2to3var(9,ictot,ifac,kcomp)=cintoc125
            a2to3var(10,ictot,ifac,kcomp)=cintsc
            a2to3var(11,ictot,ifac,kcomp)=cintsc05
            a2to3var(12,ictot,ifac,kcomp)=cintsc125
            a2to3var(13,ictot,ifac,kcomp)=cintsa
            a2to3var(14,ictot,ifac,kcomp)=cintsa05
            a2to3var(15,ictot,ifac,kcomp)=cintsa125
            a2to3var(16,ictot,ifac,kcomp)=aaeros
            a2to3var(17,ictot,ifac,kcomp)=aaerol
            a2to3var(18,ictot,ifac,kcomp)=vaeros
            a2to3var(19,ictot,ifac,kcomp)=vaerol          

        end do  ! lin
      end do    ! ifil


!   Prescribed dummy values for unused kcomp=3
    kcomp=3
    do ictot=1,16
    do ifac=1,6
    do iv=1,19
        a2to3var(iv,ictot,ifac,kcomp)=1.0_r8
    enddo
    enddo
    enddo


    do iv=1,19
    do kcomp=2,3
    do ictot=1,16
    do ifac=1,6
    do ifaq=1,6
     if(a2to3var(iv,ictot,ifac,kcomp)<=0.0_r8) then
      write(*,*) 'a2to3var =', iv, kcomp, ictot, a2to3var(iv,ictot,ifac,kcomp)
      write(*,*) 'Error in initialization of a2to3var'
      stop
     endif
    enddo
    enddo
    enddo
    enddo
    enddo

        write(iulog,*)'aerodry mode 2-3 ok'


!ccccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5ccccccccc6ccccccccc7cc
!       Mode 4 (BC&OC + condensate from H2SO4 and SOA + wetphase (NH4)2SO4)
!ccccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5ccccccccc6ccccccccc7cc

        ifil = 4
        do lin = 1,3456     ! 16x6x6x6

          read(9+ifil,995) kcomp, frbcbg, catot, frac, fraq,          &
           cintbg, cintbg05, cintbg125, cintbc, cintbc05, cintbc125,  &
           cintoc, cintoc05, cintoc125, cintsc, cintsc05, cintsc125,  &      
           cintsa, cintsa05, cintsa125, aaeros, aaerol, vaeros, vaerol

 	  do ic=1,6
!	   if(abs(frbcbg-fbcbg(ic))<eps4) then
	   if(abs((frbcbg-fbcbg(ic))/fbcbg(ic))<eps2) then
	    ifbcbg=ic
	    goto 70
	   endif
	  end do
   70     continue

 	  do ic=1,16
	   if(abs((catot-cate(kcomp,ic))/cate(kcomp,ic))<eps2) then
	    ictot=ic
	    goto 71
	   endif
	  end do
   71     continue

 	  do ic=1,6
	   if(abs(frac-fac(ic))<eps4) then
	    ifac=ic
	    goto 81
	   endif
	  end do
   81     continue

	  do ic=1,6
	   if(abs(fraq-faq(ic))<eps4) then
	    ifaq=ic
	    goto 91
	   endif
	  end do
   91     continue

!         no ifbc-dependency for this mode, since all catot 
!         comes from condensed or wet-phase sulfate 

            a4var(1,ifbcbg,ictot,ifac,ifaq)=cintbg
            a4var(2,ifbcbg,ictot,ifac,ifaq)=cintbg05
            a4var(3,ifbcbg,ictot,ifac,ifaq)=cintbg125
            a4var(4,ifbcbg,ictot,ifac,ifaq)=cintbc
            a4var(5,ifbcbg,ictot,ifac,ifaq)=cintbc05
            a4var(6,ifbcbg,ictot,ifac,ifaq)=cintbc125
            a4var(7,ifbcbg,ictot,ifac,ifaq)=cintoc
            a4var(8,ifbcbg,ictot,ifac,ifaq)=cintoc05
            a4var(9,ifbcbg,ictot,ifac,ifaq)=cintoc125
            a4var(10,ifbcbg,ictot,ifac,ifaq)=cintsc
            a4var(11,ifbcbg,ictot,ifac,ifaq)=cintsc05
            a4var(12,ifbcbg,ictot,ifac,ifaq)=cintsc125
            a4var(13,ifbcbg,ictot,ifac,ifaq)=cintsa
            a4var(14,ifbcbg,ictot,ifac,ifaq)=cintsa05
            a4var(15,ifbcbg,ictot,ifac,ifaq)=cintsa125
            a4var(16,ifbcbg,ictot,ifac,ifaq)=aaeros
            a4var(17,ifbcbg,ictot,ifac,ifaq)=aaerol
            a4var(18,ifbcbg,ictot,ifac,ifaq)=vaeros
            a4var(19,ifbcbg,ictot,ifac,ifaq)=vaerol

           if(cintsa<cintsa05) &
           write(*,*) 'cintsatot =', ictot, ifac, ifaq, cintsa, cintsa05, cintsa125

        end do  ! lin

    do iv=1,19
    do ifbcbg=1,6
    do ictot=1,16
    do ifac=1,6
    do ifaq=1,6
     if(a4var(iv,ifbcbg,ictot,ifac,ifaq)<=0.0_r8) then
      write(*,*) 'a4var =', iv, ifbcbg, ictot, ifac, ifaq, a4var(iv,ifbcbg,ictot,ifac,ifaq)
      write(*,*) 'Error in initialization of a4var'
      stop
     endif
    enddo
    enddo
    enddo
    enddo
    enddo

        write(iulog,*)'aerodry mode 4 ok'

!ccccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5ccccccccc6ccccccccc7cc
!       Modes 5 to 10 (mineral and seasalt-modes + cond./coag./aq.)
!ccccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5ccccccccc6ccccccccc7cc

      do ifil = 5,10
        do lin = 1,1296     ! 6x6x6x6

          read(9+ifil,993) kcomp, catot, frac, fabc, fraq,            &
           cintbg, cintbg05, cintbg125, cintbc, cintbc05, cintbc125,  &
           cintoc, cintoc05, cintoc125, cintsc, cintsc05, cintsc125,  &      
           cintsa, cintsa05, cintsa125, aaeros, aaerol, vaeros, vaerol

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

            a5to10var(1,ictot,ifac,ifbc,ifaq,kcomp)=cintbg
            a5to10var(2,ictot,ifac,ifbc,ifaq,kcomp)=cintbg05
            a5to10var(3,ictot,ifac,ifbc,ifaq,kcomp)=cintbg125
            a5to10var(4,ictot,ifac,ifbc,ifaq,kcomp)=cintbc
            a5to10var(5,ictot,ifac,ifbc,ifaq,kcomp)=cintbc05
            a5to10var(6,ictot,ifac,ifbc,ifaq,kcomp)=cintbc125
            a5to10var(7,ictot,ifac,ifbc,ifaq,kcomp)=cintoc
            a5to10var(8,ictot,ifac,ifbc,ifaq,kcomp)=cintoc05
            a5to10var(9,ictot,ifac,ifbc,ifaq,kcomp)=cintoc125
            a5to10var(10,ictot,ifac,ifbc,ifaq,kcomp)=cintsc
            a5to10var(11,ictot,ifac,ifbc,ifaq,kcomp)=cintsc05
            a5to10var(12,ictot,ifac,ifbc,ifaq,kcomp)=cintsc125
            a5to10var(13,ictot,ifac,ifbc,ifaq,kcomp)=cintsa
            a5to10var(14,ictot,ifac,ifbc,ifaq,kcomp)=cintsa05
            a5to10var(15,ictot,ifac,ifbc,ifaq,kcomp)=cintsa125
            a5to10var(16,ictot,ifac,ifbc,ifaq,kcomp)=aaeros
            a5to10var(17,ictot,ifac,ifbc,ifaq,kcomp)=aaerol
            a5to10var(18,ictot,ifac,ifbc,ifaq,kcomp)=vaeros
            a5to10var(19,ictot,ifac,ifbc,ifaq,kcomp)=vaerol

        end do  ! lin
      end do    ! ifil

    do iv=1,19
    do kcomp=5,10
    do ictot=1,6
    do ifac=1,6
    do ifaq=1,6
     if(a5to10var(iv,ictot,ifac,ifbc,ifaq,kcomp)<=0.0_r8) then
      write(*,*) 'a5to10var =', iv, kcomp, ictot, ifac, ifbc, ifaq, a5to10var(iv,ictot,ifac,ifbc,ifaq,kcomp)
      write(*,*) 'Error in initialization of a5to10var'
      stop
     endif
    enddo
    enddo
    enddo
    enddo
    enddo

        write(iulog,*)'aerodry mode 5-10 ok'

!ccccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5ccccccccc6ccccccccc7cc


  993 format(I2,23e10.3)
  994 format(I2,21e10.3)
  995 format(I2,23e10.3)
  996 format(I2,7e11.4)
  997 format(I2,22e10.3)

      do ifil=10,21
        close (ifil)
      end do 

      return
end subroutine initdryp

