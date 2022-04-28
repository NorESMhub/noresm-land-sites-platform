subroutine initaeropt

!Purpose: To read in the AeroCom look-up tables for aerosol optical properties. 
!     The grid for discrete input-values in the look-up tables is defined in opptab. 

!     Tabulating the 'aerocomk'-files to save computing time.
!     Updated for new kcomp1.out including condensed SOA - Alf Kirkevåg, May 2013
!     Extended for new SOA treatment - Alf Kirkevaag, September 2015.
!     Modified for optimized added masses and mass fractions for 
!     concentrations from condensation, coagulation or cloud-processing 
!     - Alf Kirkevaag, May 2016. 
!     Modified for optimized added masses and mass fractions for concentrations from 
!     condensation, coagulation or cloud-processing - Alf Kirkevaag, May 2016. 

   use oslo_control, only: oslo_getopts, dir_string_length
   use shr_kind_mod, only: r8 => shr_kind_r8
   use commondefinitions, only: nmodes, nbmodes
   use opttab,   only: cate, cat, fac, faq, fbc, rh, fombg, fbcbg
   use cam_logfile,  only: iulog

   implicit none

#include <aerocopt.h>
#include <aerocopt2.h>

      integer kcomp, irelh, ictot, ifac, ifbc, ifaq
      integer ifombg, ifbcbg
      integer ic, ifil, lin, iv
      real(r8) catot, relh, frombg, frbcbg, frac, fabc, fraq,  & 
               bext440, babs440, bext500, babs500, babs550,    &
               bext670, babs670, bext870, babs870,             &
               bebg440, babg440, bebg500, babg500, babg550,    &
               bebg670, babg670, bebg870, babg870,             &
               bebc440, babc440, bebc500, babc500, babc550,    &
               bebc670, babc670, bebc870, babc870,             &
               beoc440, baoc440, beoc500, baoc500, baoc550,    &
               beoc670, baoc670, beoc870, baoc870,             &
               besu440, basu440, besu500, basu500, basu550,    &
               besu670, basu670, besu870, basu870,             &
               bebg550lt1, bebg550gt1, bebc550lt1, bebc550gt1, &
               beoc550lt1, beoc550gt1, besu550lt1, besu550gt1, &
               backscat550

      real(r8) :: eps2 = 1.e-2_r8
      real(r8) :: eps4 = 1.e-4_r8
      real(r8) :: eps6 = 1.e-6_r8
      real(r8) :: eps7 = 1.e-7_r8
      
      character(len=dir_string_length) :: aerotab_table_dir

      call oslo_getopts(aerotab_table_dir_out = aerotab_table_dir)

!ccccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5ccccccccc6ccccccccc7cc

      open(11,file=trim(aerotab_table_dir)//'/aerocomk2.out'  &
             ,form='formatted',status='old')
      open(12,file=trim(aerotab_table_dir)//'/aerocomk3.out'  &
             ,form='formatted',status='old')
      open(13,file=trim(aerotab_table_dir)//'/aerocomk4.out' &
             ,form='formatted',status='old')
      open(14,file=trim(aerotab_table_dir)//'/aerocomk5.out'  &
             ,form='formatted',status='old')
      open(15,file=trim(aerotab_table_dir)//'/aerocomk6.out'  &
             ,form='formatted',status='old')
      open(16,file=trim(aerotab_table_dir)//'/aerocomk7.out'  &
             ,form='formatted',status='old')
      open(17,file=trim(aerotab_table_dir)//'/aerocomk8.out'  &
             ,form='formatted',status='old')
      open(18,file=trim(aerotab_table_dir)//'/aerocomk9.out'  &
             ,form='formatted',status='old')
      open(19,file=trim(aerotab_table_dir)//'/aerocomk10.out' &
             ,form='formatted',status='old')
      open(20,file=trim(aerotab_table_dir)//'/aerocomk0.out' &
             ,form='formatted',status='old')
      open(21,file=trim(aerotab_table_dir)//'/aerocomk1.out' &
             ,form='formatted',status='old')

!     Skipping the header-text in all input files (Later: use it to check AeroTab - CAM5-Oslo consistency!)
      do ifil = 11,21
        call checkTableHeader (ifil)
      enddo
 

!ccccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5ccccccccc6ccccccccc7cc
!       Mode 0, BC(ax
!ccccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5ccccccccc6ccccccccc7cc

          ifil = 11

          read(9+ifil,996) kcomp, relh,  &
           bex440, bax440, bex500, bax500, bax550, bex670, bax670, &
           bex870, bax870, bex550lt1, bex550gt1, backscx550

     if(bex440<=0.0_r8) then
      write(*,*) 'bex440 =', bex440
      write(*,*) 'Error in initialization of bex1'
      stop
     endif

        write(iulog,*)'aerocom mode 0 ok'


!ccccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5ccccccccc6ccccccccc7cc
!       New mode 1 (H2SO4 and SOA + condensate from H2SO4 and SOA)
!ccccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5ccccccccc6ccccccccc7cc

        ifil = 1
        do lin = 1,5760     ! 10x6x16x6

          read(20+ifil,997) kcomp, relh, frombg, catot, frac, &
           bext440, bext500, bext670, bext870,             &
           bebg440, bebg500, bebg670, bebg870,             &
           bebc440, bebc500, bebc670, bebc870,             &
           beoc440, beoc500, beoc670, beoc870,             &
           besu440, besu500, besu670, besu870,             &
           babs440, babs500, babs550, babs670, babs870,    &
           bebg550lt1, bebg550gt1, bebc550lt1, bebc550gt1, &
           beoc550lt1, beoc550gt1, besu550lt1, besu550gt1, &
           backscat550, babg550, babc550, baoc550, basu550

       	  do ic=1,10
	   if(abs(relh-rh(ic))<eps4) then
	    irelh=ic
	    goto 50
	   endif
	  end do
   50     continue

 	  do ic=1,6
	   if(abs(frombg-fombg(ic))<eps4) then
	    ifombg=ic
	    goto 52
	   endif
	  end do
   52     continue

 	  do ic=1,16
! 	   if(abs(catot-cate(kcomp,ic))<eps7) then
	   if(abs((catot-cate(kcomp,ic))/cate(kcomp,ic))<eps2) then
	    ictot=ic
	    goto 53
	   endif
	  end do
   53     continue

 	  do ic=1,6
	   if(abs(frac-fac(ic))<eps4) then
	    ifac=ic
	    goto 54
	   endif
	  end do
   54     continue

           bep1(1,irelh,ifombg,ictot,ifac)=bext440 ! unit km^-1
           bep1(2,irelh,ifombg,ictot,ifac)=bext500
           bep1(3,irelh,ifombg,ictot,ifac)=bext670 
           bep1(4,irelh,ifombg,ictot,ifac)=bext870
           bep1(5,irelh,ifombg,ictot,ifac)=bebg440
           bep1(6,irelh,ifombg,ictot,ifac)=bebg500
           bep1(7,irelh,ifombg,ictot,ifac)=bebg670
           bep1(8,irelh,ifombg,ictot,ifac)=bebg870
           bep1(9,irelh,ifombg,ictot,ifac)=bebc440  !=0
           bep1(10,irelh,ifombg,ictot,ifac)=bebc500 !=0
           bep1(11,irelh,ifombg,ictot,ifac)=bebc670 !=0
           bep1(12,irelh,ifombg,ictot,ifac)=bebc870 !=0
           bep1(13,irelh,ifombg,ictot,ifac)=beoc440
           bep1(14,irelh,ifombg,ictot,ifac)=beoc500
           bep1(15,irelh,ifombg,ictot,ifac)=beoc670
           bep1(16,irelh,ifombg,ictot,ifac)=beoc870
           bep1(17,irelh,ifombg,ictot,ifac)=besu440
           bep1(18,irelh,ifombg,ictot,ifac)=besu500
           bep1(19,irelh,ifombg,ictot,ifac)=besu670
           bep1(20,irelh,ifombg,ictot,ifac)=besu870
           bep1(21,irelh,ifombg,ictot,ifac)=babs440
           bep1(22,irelh,ifombg,ictot,ifac)=babs500
           bep1(23,irelh,ifombg,ictot,ifac)=babs550
           bep1(24,irelh,ifombg,ictot,ifac)=babs670
           bep1(25,irelh,ifombg,ictot,ifac)=babs870
           bep1(26,irelh,ifombg,ictot,ifac)=bebg550lt1
           bep1(27,irelh,ifombg,ictot,ifac)=bebg550gt1
           bep1(28,irelh,ifombg,ictot,ifac)=bebc550lt1 !=0
           bep1(29,irelh,ifombg,ictot,ifac)=bebc550gt1 !=0
           bep1(30,irelh,ifombg,ictot,ifac)=beoc550lt1
           bep1(31,irelh,ifombg,ictot,ifac)=beoc550gt1
           bep1(32,irelh,ifombg,ictot,ifac)=besu550lt1
           bep1(33,irelh,ifombg,ictot,ifac)=besu550gt1
           bep1(34,irelh,ifombg,ictot,ifac)=backscat550
           bep1(35,irelh,ifombg,ictot,ifac)=babg550
           bep1(36,irelh,ifombg,ictot,ifac)=babc550 !=0
           bep1(37,irelh,ifombg,ictot,ifac)=baoc550
           bep1(38,irelh,ifombg,ictot,ifac)=basu550

        end do  ! lin

    do irelh=1,10
    do ifombg=1,6
    do ictot=1,16
    do ifac=1,6
     if(bep1(1,irelh,ifombg,ictot,ifac)<=0.0_r8) then
      write(*,*) 'bep1 =', irelh,ifombg, ictot, ifac, bep1(1,irelh,ifombg,ictot,ifac)
      write(*,*) 'Error in initialization of bep1'
      stop
     endif
    enddo
    enddo
    enddo
    enddo

        write(iulog,*)'aerocom mode 1 ok' 

!ccccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5ccccccccc6ccccccccc7cc
!       Modes 2 to 3 (BC/OC + condesate from H2SO4 and SOA)
!ccccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5ccccccccc6ccccccccc7cc

!      do ifil = 2,3
      do ifil = 2,2
        do lin = 1,960     ! 10x16x6

          read(9+ifil,994) kcomp, relh, catot, frac, &
           bext440, bext500, bext670, bext870,             &
           bebg440, bebg500, bebg670, bebg870,             &
           bebc440, bebc500, bebc670, bebc870,             &
           beoc440, beoc500, beoc670, beoc870,             &
           besu440, besu500, besu670, besu870,             &
           babs440, babs500, babs550, babs670, babs870,    &
           bebg550lt1, bebg550gt1, bebc550lt1, bebc550gt1, &
           beoc550lt1, beoc550gt1, besu550lt1, besu550gt1, &
           backscat550, babg550, babc550, baoc550, basu550


       	  do ic=1,10
	   if(abs(relh-rh(ic))<eps4) then
	    irelh=ic
	    goto 61
	   endif
	  end do
   61     continue

 	  do ic=1,16
!	   if(abs(catot-cate(kcomp,ic))<eps7) then
	   if(abs((catot-cate(kcomp,ic))/cate(kcomp,ic))<eps2) then
	    ictot=ic
	    goto 71
	   endif
	  end do
   71     continue

 	  do ic=1,6
	   if(abs(frac-fac(ic))<eps4) then
	    ifac=ic
	    goto 72
	   endif
	  end do
   72     continue

           bep2to3(1,irelh,ictot,ifac,kcomp)=bext440 ! unit km^-1
           bep2to3(2,irelh,ictot,ifac,kcomp)=bext500
           bep2to3(3,irelh,ictot,ifac,kcomp)=bext670 
           bep2to3(4,irelh,ictot,ifac,kcomp)=bext870
           bep2to3(5,irelh,ictot,ifac,kcomp)=bebg440
           bep2to3(6,irelh,ictot,ifac,kcomp)=bebg500
           bep2to3(7,irelh,ictot,ifac,kcomp)=bebg670
           bep2to3(8,irelh,ictot,ifac,kcomp)=bebg870
           bep2to3(9,irelh,ictot,ifac,kcomp)=bebc440  !=0
           bep2to3(10,irelh,ictot,ifac,kcomp)=bebc500 !=0
           bep2to3(11,irelh,ictot,ifac,kcomp)=bebc670 !=0
           bep2to3(12,irelh,ictot,ifac,kcomp)=bebc870 !=0
           bep2to3(13,irelh,ictot,ifac,kcomp)=beoc440
           bep2to3(14,irelh,ictot,ifac,kcomp)=beoc500
           bep2to3(15,irelh,ictot,ifac,kcomp)=beoc670
           bep2to3(16,irelh,ictot,ifac,kcomp)=beoc870
           bep2to3(17,irelh,ictot,ifac,kcomp)=besu440
           bep2to3(18,irelh,ictot,ifac,kcomp)=besu500
           bep2to3(19,irelh,ictot,ifac,kcomp)=besu670
           bep2to3(20,irelh,ictot,ifac,kcomp)=besu870
           bep2to3(21,irelh,ictot,ifac,kcomp)=babs440
           bep2to3(22,irelh,ictot,ifac,kcomp)=babs500
           bep2to3(23,irelh,ictot,ifac,kcomp)=babs550
           bep2to3(24,irelh,ictot,ifac,kcomp)=babs670
           bep2to3(25,irelh,ictot,ifac,kcomp)=babs870
           bep2to3(26,irelh,ictot,ifac,kcomp)=bebg550lt1
           bep2to3(27,irelh,ictot,ifac,kcomp)=bebg550gt1
           bep2to3(28,irelh,ictot,ifac,kcomp)=bebc550lt1 !=0
           bep2to3(29,irelh,ictot,ifac,kcomp)=bebc550gt1 !=0
           bep2to3(30,irelh,ictot,ifac,kcomp)=beoc550lt1
           bep2to3(31,irelh,ictot,ifac,kcomp)=beoc550gt1
           bep2to3(32,irelh,ictot,ifac,kcomp)=besu550lt1
           bep2to3(33,irelh,ictot,ifac,kcomp)=besu550gt1
           bep2to3(34,irelh,ictot,ifac,kcomp)=backscat550
           bep2to3(35,irelh,ictot,ifac,kcomp)=babg550
           bep2to3(36,irelh,ictot,ifac,kcomp)=babc550 !=0
           bep2to3(37,irelh,ictot,ifac,kcomp)=baoc550
           bep2to3(38,irelh,ictot,ifac,kcomp)=basu550

        end do
      end do

!   Prescribed dummy values for unused kcomp=3
    kcomp=3
    do irelh=1,10
    do ictot=1,16
    do ifac=1,6
    do iv=1,38
        bep2to3(iv,irelh,ictot,ifac,kcomp)=1.0_r8
    enddo
    enddo
    enddo
    enddo

    do kcomp=2,3
    do irelh=1,10
    do ictot=1,16
    do ifac=1,6
     if(bep2to3(1,irelh,ictot,ifac,kcomp)<=0.0_r8) then
      write(*,*) 'bep2to3 =', irelh, ictot, ifac, bep2to3(1,irelh,ictot,ifac,kcomp)
      write(*,*) 'Error in initialization of bep2to3'
      stop
     endif
    enddo
    enddo
    enddo
    enddo

        write(iulog,*)'aerocom mode 2-3 ok' 

!ccccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5ccccccccc6ccccccccc7cc
!       Mode 4 (BC&OC + condesate from H2SO4 and SOA + wetphase (NH4)2SO4)
!ccccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5ccccccccc6ccccccccc7cc

        ifil = 4
        do lin = 1,34560     ! 10x16x6x6x6

          read(9+ifil,995) kcomp, relh, frbcbg, catot, frac, fraq, &
           bext440, bext500, bext670, bext870,             &
           bebg440, bebg500, bebg670, bebg870,             &
           bebc440, bebc500, bebc670, bebc870,             &
           beoc440, beoc500, beoc670, beoc870,             &
           besu440, besu500, besu670, besu870,             &
           babs440, babs500, babs550, babs670, babs870,    &
           bebg550lt1, bebg550gt1, bebc550lt1, bebc550gt1, &
           beoc550lt1, beoc550gt1, besu550lt1, besu550gt1, &
           backscat550, babg550, babc550, baoc550, basu550


       	  do ic=1,10
	   if(abs(relh-rh(ic))<eps4) then
	    irelh=ic
	    goto 81
	   endif
	  end do
   81     continue

 	  do ic=1,6
!	   if(abs(frbcbg-fbcbg(ic))<eps4) then
!	   if(abs(frbcbg-fbcbg(ic))<eps3) then
	   if(abs((frbcbg-fbcbg(ic))/fbcbg(ic))<eps2) then
	    ifbcbg=ic
	    goto 86
	   endif
	  end do
   86     continue

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

           bep4(1,irelh,ifbcbg,ictot,ifac,ifaq)=bext440 ! unit km^-1
           bep4(2,irelh,ifbcbg,ictot,ifac,ifaq)=bext500
           bep4(3,irelh,ifbcbg,ictot,ifac,ifaq)=bext670 
           bep4(4,irelh,ifbcbg,ictot,ifac,ifaq)=bext870
           bep4(5,irelh,ifbcbg,ictot,ifac,ifaq)=bebg440
           bep4(6,irelh,ifbcbg,ictot,ifac,ifaq)=bebg500
           bep4(7,irelh,ifbcbg,ictot,ifac,ifaq)=bebg670
           bep4(8,irelh,ifbcbg,ictot,ifac,ifaq)=bebg870
           bep4(9,irelh,ifbcbg,ictot,ifac,ifaq)=bebc440
           bep4(10,irelh,ifbcbg,ictot,ifac,ifaq)=bebc500
           bep4(11,irelh,ifbcbg,ictot,ifac,ifaq)=bebc670
           bep4(12,irelh,ifbcbg,ictot,ifac,ifaq)=bebc870
           bep4(13,irelh,ifbcbg,ictot,ifac,ifaq)=beoc440 !=0
           bep4(14,irelh,ifbcbg,ictot,ifac,ifaq)=beoc500 !=0
           bep4(15,irelh,ifbcbg,ictot,ifac,ifaq)=beoc670 !=0
           bep4(16,irelh,ifbcbg,ictot,ifac,ifaq)=beoc870 !=0
           bep4(17,irelh,ifbcbg,ictot,ifac,ifaq)=besu440
           bep4(18,irelh,ifbcbg,ictot,ifac,ifaq)=besu500
           bep4(19,irelh,ifbcbg,ictot,ifac,ifaq)=besu670
           bep4(20,irelh,ifbcbg,ictot,ifac,ifaq)=besu870
           bep4(21,irelh,ifbcbg,ictot,ifac,ifaq)=babs440
           bep4(22,irelh,ifbcbg,ictot,ifac,ifaq)=babs500
           bep4(23,irelh,ifbcbg,ictot,ifac,ifaq)=babs550
           bep4(24,irelh,ifbcbg,ictot,ifac,ifaq)=babs670
           bep4(25,irelh,ifbcbg,ictot,ifac,ifaq)=babs870
           bep4(26,irelh,ifbcbg,ictot,ifac,ifaq)=bebg550lt1
           bep4(27,irelh,ifbcbg,ictot,ifac,ifaq)=bebg550gt1
           bep4(28,irelh,ifbcbg,ictot,ifac,ifaq)=bebc550lt1
           bep4(29,irelh,ifbcbg,ictot,ifac,ifaq)=bebc550gt1
           bep4(30,irelh,ifbcbg,ictot,ifac,ifaq)=beoc550lt1 !=0
           bep4(31,irelh,ifbcbg,ictot,ifac,ifaq)=beoc550gt1 !=0
           bep4(32,irelh,ifbcbg,ictot,ifac,ifaq)=besu550lt1
           bep4(33,irelh,ifbcbg,ictot,ifac,ifaq)=besu550gt1
           bep4(34,irelh,ifbcbg,ictot,ifac,ifaq)=backscat550
           bep4(35,irelh,ifbcbg,ictot,ifac,ifaq)=babg550
           bep4(36,irelh,ifbcbg,ictot,ifac,ifaq)=babc550 !=0
           bep4(37,irelh,ifbcbg,ictot,ifac,ifaq)=baoc550 !=0
           bep4(38,irelh,ifbcbg,ictot,ifac,ifaq)=basu550

        end do

    do irelh=1,10
    do ifbcbg=1,6
    do ictot=1,16
    do ifac=1,6
    do ifaq=1,6
     if(bep4(1,irelh,ifbcbg,ictot,ifac,ifaq)<=0.0_r8) then
      write(*,*) 'bep4 =', irelh, ifbcbg, ictot, ifac, ifaq, bep4(1,irelh,ifbcbg,ictot,ifac,ifaq)
      write(*,*) 'Error in initialization of bep4'
      stop
     endif
    enddo
    enddo
    enddo
    enddo
    enddo

        write(iulog,*)'aerocom mode 4 ok'


!ccccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5ccccccccc6ccccccccc7cc
!       Modes 5 to 10 (SO4(Ait75) and mineral and seasalt-modes + cond./coag./aq.)
!ccccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5ccccccccc6ccccccccc7cc

      do ifil = 5,10
        do lin = 1,12960     ! 10x6x6x6x6

          read(9+ifil,993) kcomp, relh, catot, frac, fabc, fraq, &
               bext440, bext500, bext670, bext870,             &
               bebg440, bebg500, bebg670, bebg870,             &
               bebc440, bebc500, bebc670, bebc870,             &
               beoc440, beoc500, beoc670, beoc870,             &
               besu440, besu500, besu670, besu870,             &
               babs440, babs500, babs550, babs670, babs870,    &
               bebg550lt1, bebg550gt1, bebc550lt1, bebc550gt1, &
               beoc550lt1, beoc550gt1, besu550lt1, besu550gt1, &
               backscat550, babg550, babc550, baoc550, basu550

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

           bep5to10(1,irelh,ictot,ifac,ifbc,ifaq,kcomp)=bext440 ! unit km^-1
           bep5to10(2,irelh,ictot,ifac,ifbc,ifaq,kcomp)=bext500
           bep5to10(3,irelh,ictot,ifac,ifbc,ifaq,kcomp)=bext670 
           bep5to10(4,irelh,ictot,ifac,ifbc,ifaq,kcomp)=bext870
           bep5to10(5,irelh,ictot,ifac,ifbc,ifaq,kcomp)=bebg440
           bep5to10(6,irelh,ictot,ifac,ifbc,ifaq,kcomp)=bebg500
           bep5to10(7,irelh,ictot,ifac,ifbc,ifaq,kcomp)=bebg670
           bep5to10(8,irelh,ictot,ifac,ifbc,ifaq,kcomp)=bebg870
           bep5to10(9,irelh,ictot,ifac,ifbc,ifaq,kcomp)=bebc440
           bep5to10(10,irelh,ictot,ifac,ifbc,ifaq,kcomp)=bebc500
           bep5to10(11,irelh,ictot,ifac,ifbc,ifaq,kcomp)=bebc670
           bep5to10(12,irelh,ictot,ifac,ifbc,ifaq,kcomp)=bebc870
           bep5to10(13,irelh,ictot,ifac,ifbc,ifaq,kcomp)=beoc440
           bep5to10(14,irelh,ictot,ifac,ifbc,ifaq,kcomp)=beoc500
           bep5to10(15,irelh,ictot,ifac,ifbc,ifaq,kcomp)=beoc670
           bep5to10(16,irelh,ictot,ifac,ifbc,ifaq,kcomp)=beoc870
           bep5to10(17,irelh,ictot,ifac,ifbc,ifaq,kcomp)=besu440
           bep5to10(18,irelh,ictot,ifac,ifbc,ifaq,kcomp)=besu500
           bep5to10(19,irelh,ictot,ifac,ifbc,ifaq,kcomp)=besu670
           bep5to10(20,irelh,ictot,ifac,ifbc,ifaq,kcomp)=besu870
           bep5to10(21,irelh,ictot,ifac,ifbc,ifaq,kcomp)=babs440
           bep5to10(22,irelh,ictot,ifac,ifbc,ifaq,kcomp)=babs500
           bep5to10(23,irelh,ictot,ifac,ifbc,ifaq,kcomp)=babs550
           bep5to10(24,irelh,ictot,ifac,ifbc,ifaq,kcomp)=babs670
           bep5to10(25,irelh,ictot,ifac,ifbc,ifaq,kcomp)=babs870
           bep5to10(26,irelh,ictot,ifac,ifbc,ifaq,kcomp)=bebg550lt1
           bep5to10(27,irelh,ictot,ifac,ifbc,ifaq,kcomp)=bebg550gt1
           bep5to10(28,irelh,ictot,ifac,ifbc,ifaq,kcomp)=bebc550lt1
           bep5to10(29,irelh,ictot,ifac,ifbc,ifaq,kcomp)=bebc550gt1
           bep5to10(30,irelh,ictot,ifac,ifbc,ifaq,kcomp)=beoc550lt1
           bep5to10(31,irelh,ictot,ifac,ifbc,ifaq,kcomp)=beoc550gt1
           bep5to10(32,irelh,ictot,ifac,ifbc,ifaq,kcomp)=besu550lt1
           bep5to10(33,irelh,ictot,ifac,ifbc,ifaq,kcomp)=besu550gt1
           bep5to10(34,irelh,ictot,ifac,ifbc,ifaq,kcomp)=backscat550
           bep5to10(35,irelh,ictot,ifac,ifbc,ifaq,kcomp)=babg550
           bep5to10(36,irelh,ictot,ifac,ifbc,ifaq,kcomp)=babc550
           bep5to10(37,irelh,ictot,ifac,ifbc,ifaq,kcomp)=baoc550
           bep5to10(38,irelh,ictot,ifac,ifbc,ifaq,kcomp)=basu550

        end do
      end do

    do kcomp=5,10
    do irelh=1,10
    do ictot=1,6
    do ifac=1,6
    do ifaq=1,6
     if(bep5to10(1,irelh,ictot,ifac,ifbc,ifaq,kcomp)<=0.0_r8) then
      write(*,*) 'bep5to10 =', kcomp, irelh, ictot, ifac, ifbc, ifaq, &
                               bep5to10(1,irelh,ictot,ifac,ifbc,ifaq,kcomp)
      write(*,*) 'Error in initialization of bep5to10'
      stop
     endif
    enddo
    enddo
    enddo
    enddo
    enddo

        write(iulog,*)'aerocom mode 5-10 ok'


  993 format(I2,f6.3,3e10.3,f5.2,38e10.3)
  994 format(I2,f6.3,2e10.3,38e10.3)
  995 format(I2,f6.3,3e10.3,f5.2,38e10.3)
  996 format(I2,f6.3,12e11.4)
  997 format(I2,f6.3,3e10.3,38e10.3)

      do ifil=10,21
        close (ifil)
      end do 

      return
end subroutine initaeropt

