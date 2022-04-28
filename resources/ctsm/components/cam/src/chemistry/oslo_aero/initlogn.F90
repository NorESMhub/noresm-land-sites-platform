subroutine initlogn

!  Created for CAM3 by Trude Storelvmo, Fall 2007.
!  This subroutine reads the tabulated parameters for "best lognormal fits"
!  of the aerosol size distribution wrt CCN activation as calculated by Alf Kirkevaag.
!  Updated for new kcomp1.out including condensed SOA - Alf Kirkevaag, May 2013
!  Updated for reading inout files with extra header info - Alf Kirkevaag, May 2015,
!  and for new tables including SOA September 2015.
!  Modified for optimized added masses and mass fractions for concentrations from 
!  condensation, coagulation or cloud-processing - Alf Kirkevaag, May 2016. 

   use shr_kind_mod, only: r8 => shr_kind_r8
   use aerosoldef
   use opttab, only: cat,fac,fbc,faq,cate
   use const
   use cam_logfile,  only: iulog
   use oslo_control, only: oslo_getopts,dir_string_length

   implicit none

   integer kcomp, ictot, ifac, ifbc, ifaq, irk, istdv
   integer ic, ifil, lin
   character(len=dir_string_length) :: aerotab_table_dir
   real(r8) :: eps2 = 1.e-2_r8
   real(r8) :: eps4 = 1.e-4_r8

       write(iulog,*)'b4 nlog open ok' 

   !Where are the tables stored??
   call oslo_getopts(aerotab_table_dir_out=aerotab_table_dir)

   open(20,file=trim(aerotab_table_dir)//'/logntilp1.out' &   ! SO4&SOA(n/Ait)
          ,form='formatted',status='old')
   open(21,file=trim(aerotab_table_dir)//'/logntilp2.out' &   ! BC(n/Ait)
          ,form='formatted',status='old')
   open(22,file=trim(aerotab_table_dir)//'/logntilp3.out' &   ! OC(n/Ait)
          ,form='formatted',status='old')
   open(23,file=trim(aerotab_table_dir)//'/logntilp4.out' &   ! BC&OC(n/Ait)
          ,form='formatted',status='old')
   open(24,file=trim(aerotab_table_dir)//'/logntilp5.out' &   ! SO4(Ait75)
          ,form='formatted',status='old')
   open(25,file=trim(aerotab_table_dir)//'/logntilp6.out' &   ! MINACC
          ,form='formatted',status='old')
   open(26,file=trim(aerotab_table_dir)//'/logntilp7.out' &   ! MINCOA
          ,form='formatted',status='old')
   open(27,file=trim(aerotab_table_dir)//'/logntilp8.out' &   ! SEASF
          ,form='formatted',status='old')
   open(28,file=trim(aerotab_table_dir)//'/logntilp9.out' &   ! SEASACC
          ,form='formatted',status='old')
   open(29,file=trim(aerotab_table_dir)//'/logntilp10.out' &  ! SEASCOA
          ,form='formatted',status='old')

       write(iulog,*)'nlog open ok' 

!     Skipping the header-text in all input files (Later: use it to check AeroTab - CAM5-Oslo consistency!)
      do ifil = 20,29
        call checkTableHeader (ifil)
      enddo

!  ************************************************************************
!       Mode 1 (SO4&SOA + condesate from H2SO4 and SOA)
!       Modes 2 to 3 (BC/OC + condesate from H2SO4 and SOA)
!
!       These two are treated the same way since there is no dependence on 
!       fombg (SOA fraction in the background) for mode 1. 
!  ************************************************************************

!         do ifil = 1,3
         do ifil = 1,2
          do lin = 1,96   ! 16*6 entries   
           read(19+ifil,993) kcomp, calog1to3(ifil,lin), fraclog1to3 (ifil, lin), &
             rk1to3(ifil,lin), stdv1to3(ifil,lin) 

	do ic=1,16
!	 if(calog1to3(ifil,lin).eq.cate(kcomp,ic)) then
         if(abs((calog1to3(ifil,lin)-cate(kcomp,ic))/cate(kcomp,ic))<eps2) then
	  ictot=ic
	  goto 71
	 endif
	end do
   71 continue

	do ic=1,6
!	 if(fraclog1to3(ifil,lin).eq.fac(ic)) then
         if(abs(fraclog1to3(ifil,lin)-fac(ic))<eps4) then
	  ifac=ic
	  goto 81
	 endif
	end do
   81 continue

        sss1to3(kcomp,ictot,ifac) = stdv1to3(ifil,lin)
        rrr1to3(kcomp,ictot,ifac) = rk1to3(ifil,lin)

          end do   ! lin
         end do    ! ifil

!    Prescribed dummy values for kcomp=3
     kcomp=3
     do ictot=1,16
       do ifac=1,6
          sss1to3(kcomp,ictot,ifac)=1.0_r8
          rrr1to3(kcomp,ictot,ifac)=1.0_r8
       enddo
     enddo
       

!    do kcomp=1,3
    do kcomp=1,2
    do ictot=1,16
    do ifac=1,6
     if(sss1to3(kcomp,ictot,ifac)<=0.0_r8) then
      write(*,*) 'sss1to3 =',  ictot, ifac, sss1to3(kcomp,ictot,ifac)
      write(*,*) 'Error in initialization of sss1to3'
      stop
     endif
     if(rrr1to3(kcomp,ictot,ifac)<=0.0_r8) then
      write(*,*) 'rrr1to3 =', ictot, ifac, rrr1to3(kcomp,ictot,ifac)
      write(*,*) 'Error in initialization of rrr1to3'
      stop
     endif
    enddo
    enddo
    enddo

       write(iulog,*)'nlog mode 1-3 ok' 


!  ************************************************************************
!       Mode 4 (BC&OC + condesate from H2SO4 + wetphase (NH4)2SO4)
!  ************************************************************************

         ifil = 4
          do lin = 1,576   ! 16 entries   
           read(19+ifil,994) kcomp, calog4(lin) &
             ,fraclog4(lin), fraqlog4(lin), rk4(lin), stdv4(lin)

	do ic=1,16
!	 if(calog4(lin).gt.0.9999_r8*cate(kcomp,ic).and. &
!	    calog4(lin).lt.1.0001_r8*cate(kcomp,ic)) then
         if(abs((calog4(lin)-cate(kcomp,ic))/cate(kcomp,ic))<eps2) then
	  ictot=ic
	  goto 91
	 endif
	end do
   91 continue

	do ic=1,6
!	 if(fraclog4(lin).eq.fac(ic)) then
         if(abs(fraclog4(lin)-fac(ic))<eps4) then
	  ifac=ic
	  goto 101
	 endif
	end do
  101 continue

	do ic=1,6
!	 if(fraqlog4(lin).eq.faq(ic)) then
	 if(abs(fraqlog4(lin)-faq(ic))<eps4) then
          ifaq=ic
          goto 111
	 endif
	end do
  111 continue

        rrr4(ictot,ifac,ifaq) = rk4(lin)
	sss4(ictot,ifac,ifaq) = stdv4(lin)

          end do   ! lin

    do ifac=1,6
    do ifaq=1,6
    do ictot=1,16
     if(rrr4(ictot,ifac,ifaq)<=0.0_r8) then
      write(*,*) 'rrr4 =',ictot,ifac,ifaq,rrr4(ictot,ifac,ifaq)
      write(*,*) 'Error in initialization of rrr4'
      stop
     endif
     if(sss4(ictot,ifac,ifaq)<=0.0_r8) then
      write(*,*) 'sss4 =',ictot,ifac,ifaq,sss4(ictot,ifac,ifaq)
      write(*,*) 'Error in initialization of sss4'
      stop
     endif
    enddo
    enddo
    enddo

       write(iulog,*)'nlog mode 4 ok' 


!  ************************************************************************
!       Modes 5 to 10 (SO4(ait75) and mineral and seasalt-modes + cond./coag./aq.)
!  ************************************************************************

         do ifil = 5,10
          do lin = 1,1296   ! 6**4 entries
!           read(9+ifil,995) kcomp, calog(ifil,lin) &
           read(19+ifil,995) kcomp, calog(ifil,lin) &
             ,fraclog5to10(ifil,lin), fabclog5to10(ifil,lin), fraqlog5to10(ifil,lin) &
             ,rk5to10(ifil,lin), stdv5to10(ifil,lin)

	do ic=1,6
!	 if(calog(ifil,lin).eq.cat(kcomp,ic)) then
         if(abs((calog(ifil,lin)-cat(kcomp,ic))/cat(kcomp,ic))<eps2) then
	  ictot=ic
	  goto 21
	 endif
	end do
   21 continue

	do ic=1,6
!	 if(fraclog5to10(ifil,lin).eq.fac(ic)) then
         if(abs(fraclog5to10(ifil,lin)-fac(ic))<eps4) then
	  ifac=ic
	  goto 31
	 endif
	end do
        PRINT*,'ifac not found',fraclog5to10(ifil,lin)
   31 continue

	do ic=1,6
!	 if(fabclog5to10(ifil,lin).eq.fbc(ic)) then
         if(abs((fabclog5to10(ifil,lin)-fbc(ic))/fbc(ic))<eps2) then
	  ifbc=ic
	  goto 41
	 endif
	end do
        PRINT*,'ifbc not found',fabclog5to10(ifil,lin)
   41 continue

	do ic=1,6
!	 if(fraqlog5to10(ifil,lin).eq.faq(ic)) then
	 if(abs(fraqlog5to10(ifil,lin)-faq(ic))<eps4) then
	  ifaq=ic
	  goto 51
	 endif
	end do
        PRINT*,'ifaq not found',fraqlog5to10(ifil,lin)
   51 continue

        rrr(kcomp,ictot,ifac,ifbc,ifaq) = rk5to10(ifil,lin)
	sss(kcomp,ictot,ifac,ifbc,ifaq) = stdv5to10(ifil,lin)

          end do   ! lin
         end do    ! ifil

    do kcomp=5,10
    do ifac=1,6
    do ifbc=1,6
    do ictot=1,6
     if(rrr(kcomp,ictot,ifac,ifbc,ifaq)<=0.0_r8) then
      write(*,*) 'rrr =',kcomp,ictot,ifac,ifbc,ifaq,rrr(kcomp,ictot,ifac,ifbc,ifaq)
      write(*,*) 'Error in initialization of rrr'
      stop
     endif
     if(sss(kcomp,ictot,ifac,ifbc,ifaq)<=0.0_r8) then
      write(*,*) 'sss =',ictot,ifac,ifbc,ifaq,sss(kcomp,ictot,ifac,ifbc,ifaq)
      write(*,*) 'Error in initialization of sss'
      stop
     endif
    enddo
    enddo
    enddo
    enddo

       write(iulog,*)'nlog mode 5-10 ok' 



        do ifil=20,29
          close (ifil)
        end do 


  993 format(I3,4(x,e12.5))
  994 format(I3,5(x,e12.5))
  995 format(I3,6(x,e12.5))


	return
	end








