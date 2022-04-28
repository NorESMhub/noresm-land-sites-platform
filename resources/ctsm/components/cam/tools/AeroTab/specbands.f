      subroutine specbands(ib, xlami, xlam, xlamb, xlame, fband, fb, 
     $                     ibcam)

c **********************************************************************************
c     Created by Alf Kirkevåg.
c **********************************************************************************

c     Define spectral bands and calculate fractional fluxes in bands that
c     are to be used in Chandrasekhar averaging of optical parameters

      implicit none

      INTEGER ib, iband, ibm, ibmb, ibme, i, ibcam
      REAL    xlami(32), xlam(31), xlamb(31), xlame(31), fband(31), 
     $ flux(5000), fb(16) 
      REAL Fplanck, Fpint, temp

ccccc6ccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5ccccccccc6ccccccccc7cc
c     define wavelength intervals, depending on desired number of bands
c     (=ib-1). xlami marks the beginning of the band (um), while xlam (um) 
c     is the center, where Mie calculations are to be performed. 

c19/9-2013:  From rrsw_aer.f90
c! rrtmg_sw 14 spectral intervals (microns):
c!  3.846 -  3.077
c!  3.077 -  2.500
c!  2.500 -  2.150
c!  2.150 -  1.942
c!  1.942 -  1.626
c!  1.626 -  1.299
c!  1.299 -  1.242
c!  1.242 -  0.7782
c!  0.7782-  0.6250
c!  0.6250-  0.4415
c!  0.4415-  0.3448
c!  0.3448-  0.2632
c!  0.2632-  0.2000
c! 12.195 -  3.846
c5/11-2013:  From radconstants.F90 (in CAM5.3)
c  real(r8),parameter :: wavenum_low(nbndsw) = & ! in cm^-1
c  (/2600._r8, 3250._r8, 4000._r8, 4650._r8, 5150._r8, 6150._r8, 7700._r8, &
c    8050._r8,12850._r8,16000._r8,22650._r8,29000._r8,38000._r8,  820._r8/)
c  real(r8),parameter :: wavenum_high(nbndsw) = & ! in cm^-1
c  (/3250._r8, 4000._r8, 4650._r8, 5150._r8, 6150._r8, 7700._r8, 8050._r8, &
c   12850._r8,16000._r8,22650._r8,29000._r8,38000._r8,50000._r8, 2600._r8/)
      if(ib.eq.31) then     ! Chandrasekhar-averaging  -> CAM4/5-Oslo SW bands (+ CAM4-Oslo AEROCOM)
          xlami(1) = 0.2    ! 1' begins
          xlami(2) = 0.24   ! 
          xlami(3) = 0.263  ! 2'
          xlami(4) = 0.29   !
          xlami(5) = 0.32   !
          xlami(6) = 0.345  ! 3'
          xlami(7) = 0.376  ! 
          xlami(8) = 0.407  ! 
          xlami(9) = 0.438  !
          xlami(10)= 0.442  ! 4'
          xlami(11)= 0.48   ! 
          xlami(12)= 0.52   !
          xlami(13)= 0.58   !
          xlami(14)= 0.625  ! 5'
          xlami(15)= 0.64   !
          xlami(16)= 0.70   !
          xlami(17)= 0.74   !
          xlami(18)= 0.778  ! 6'
          xlami(19)= 0.962  !
          xlami(20)= 1.078  !
          xlami(21)= 1.16   !
          xlami(22)= 1.242  ! 7'
          xlami(23)= 1.299  ! 8'
          xlami(24)= 1.46   !
          xlami(25)= 1.626  ! 9'
          xlami(26)= 1.784  ! 
          xlami(27)= 1.942  ! 10'
          xlami(28)= 2.15   ! 11'
          xlami(29)= 2.5    ! 12'
          xlami(30)= 3.077  ! 13'  
          xlami(31)= 3.846  ! 14'
          xlami(32)= 12.195 !  
          do iband = 1,ib
            xlam(iband)=0.5*(xlami(iband)+xlami(iband+1)) 
c            write(*,*) iband, xlam(iband)
c            write(*,*) iband, xlami(iband), xlami(iband+1)
          enddo 
          ibcam=14
      elseif(ib.eq.19) then  !   CAM5-Oslo LW bands
c7/11-2013:  From radconstants.F90 (in CAM5.3)
c real(r8), parameter :: wavenumber1_longwave(nlwbands) = &! Longwave spectral band limits (cm-1)
c     (/   10._r8,  350._r8, 500._r8,   630._r8,  700._r8,  820._r8,  980._r8, 1080._r8, &
c        1180._r8, 1390._r8, 1480._r8, 1800._r8, 2080._r8, 2250._r8, 2390._r8, 2600._r8 /)
c real(r8), parameter :: wavenumber2_longwave(nlwbands) = &! Longwave spectral band limits (cm-1)
c     (/  350._r8,  500._r8,  630._r8,  700._r8,  820._r8,  980._r8, 1080._r8, 1180._r8, &
c        1390._r8, 1480._r8, 1800._r8, 2080._r8, 2250._r8, 2390._r8, 2600._r8, 3250._r8 /)
          xlami(1) = 3.077  ! = SW xlami(30)
          xlami(2) = 3.846  ! = SW xlami(31)
          xlami(3) = 4.184
          xlami(4) = 4.444
          xlami(5) = 4.808
          xlami(6) = 5.556
          xlami(7) = 6.757
          xlami(8) = 7.194
          xlami(9) = 8.475
          xlami(10)= 9.259
          xlami(11)= 10.204
          xlami(12)= 12.195 ! = SW xlami(32)
          xlami(13)= 14.286
          xlami(14)= 15.873
          xlami(15)= 20.0
c          xlami(16)= 28.571 ! 1'  (starting from 1 to avoid resizing the fb array)
          xlami(16)= 28.571 ! 16'
          xlami(17)= 40.0 
          xlami(18)= 50.0 
          xlami(19)= 70.0 
          xlami(20)= 100.0  ! cf. 1000 in CAM5: very little energy left at these wavelengths
          do iband = 1,ib
            xlam(iband)=0.5*(xlami(iband)+xlami(iband+1)) 
c            write(*,*) iband, xlam(iband)
c            write(*,*) iband, xlami(iband), xlami(iband+1)
          enddo 
          ibcam=16
      elseif(ib.eq.16) then  !   CAM5-Oslo LW bands (without Chandrasekhar averaging)
          xlami(1) = 3.077  ! = SW xlami(30)
          xlami(2) = 3.846  ! = SW xlami(31)
          xlami(3) = 4.184
          xlami(4) = 4.444
          xlami(5) = 4.808
          xlami(6) = 5.556
          xlami(7) = 6.757
          xlami(8) = 7.194
          xlami(9) = 8.475
          xlami(10)= 9.259
          xlami(11)= 10.204
          xlami(12)= 12.195 ! = SW xlami(32)
          xlami(13)= 14.286
          xlami(14)= 15.873
          xlami(15)= 20.0
          xlami(16)= 28.571
c          xlami(17)= 1000.0 ! as defined in CAM5 
          xlami(17)= 45.0    ! redefined based on the "Planck function" discussion below
          do iband = 1,ib
            xlam(iband)=0.5*(xlami(iband)+xlami(iband+1)) 
            write(*,*) iband, xlam(iband)
c            write(*,*) iband, xlami(iband), xlami(iband+1)
          enddo 
ctest       Planck function (W m-2 m-1) for black body emissivity (temp=300 gives Fig. 1.8 of Seinfeld & Pandis, OK).
c          We use the equivalent black body temperature T=255 (K), and wavelength is given in units um below:
c            temp=255.0
c            Fpint=0.0
c            do i=1,1000
c             Fplanck=(3.74e8/((real(i))**5))
c     $            /(2.718281828**(1.44e4/(temp*real(i)))-1.0)
c             write(77,*) i, Fplanck 
c            wavelength integrated energy in iband=16 is 45.7 (cf. 239 for the whole spectrum, i.e. 19%):
c             if(i.ge.1) then
c             if(i.ge.28) then
c               Fpint=Fpint+Fplanck
c               write(78,*) i, Fpint 
c             endif 
c            enddo
c           half of the energy found in the last band (22.8) is found for i<~37um, which we
c           therefore redefine as mid-band, giving xlami(17)=45.0 instead of 1000. E.g., we do
c           not perform any Chandrasekhar averaging here (due to the small absoption at these 
c           long wavelengths).
ctest
      elseif(ib.eq.14) then  !   CAM3/4-Oslo/CAM3/4 SW bands
          xlami(1) = 0.2
          xlami(2) = 0.263
          xlami(3) = 0.345
          xlami(4) = 0.442 
          xlami(5) = 0.625
          xlami(6) = 0.778
          xlami(7) = 1.242
          xlami(8) = 1.299 
          xlami(9) = 1.626
          xlami(10)= 1.942
          xlami(11)= 2.15
          xlami(12)= 2.5
          xlami(13)= 3.077   
          xlami(14)= 3.846  
          xlami(15)= 12.195   
          do iband = 1,ib
            xlam(iband)=0.5*(xlami(iband)+xlami(iband+1)) 
c            write(*,*) iband, xlam(iband)
c            write(*,*) iband, xlami(iband), xlami(iband+1)
          enddo 
      elseif(ib.eq.29) then     ! Chandrasekhar-averaging  -> CAM3/4-Oslo SW bands 
          xlami(1) = 0.2        ! 1' begins
          xlami(2) = 0.245      ! 2' 
          xlami(3) = 0.265      ! 3'
          xlami(4) = 0.275      ! 4'
          xlami(5) = 0.285      ! 5'
          xlami(6) = 0.295      ! 6'
          xlami(7) = 0.305      ! 7'
          xlami(8) = 0.35       ! 8'
          xlami(9) = 0.4        ! 
          xlami(10)= 0.47       ! 
          xlami(11)= 0.53       ! 
          xlami(12)= 0.57       ! 
          xlami(13)= 0.64       ! 9'
          xlami(14)= 0.69       ! 10'
          xlami(15)= 0.76       ! 
          xlami(16)= 0.83       !
          xlami(17)= 0.9        !
          xlami(18)= 0.98       !
          xlami(19)= 1.08       !
          xlami(20)= 1.19       ! 11'
          xlami(21)= 1.39       ! 
          xlami(22)= 1.59       !
          xlami(23)= 1.79       !
          xlami(24)= 1.99       !
          xlami(25)= 2.19       !
          xlami(26)= 2.38       ! 12'
          xlami(27)= 2.7        ! 
          xlami(28)= 3.1        !
          xlami(29)= 3.5        !
          xlami(30)= 4.0        !
          do iband = 1,ib
            xlam(iband)=0.5*(xlami(iband)+xlami(iband+1)) 
          enddo 
      elseif(ib.eq.12) then         ! CAM(3)-Oslo SW bands
          xlami(1) = 0.2
          xlami(2) = 0.245
          xlami(3) = 0.265
          xlami(4) = 0.275
          xlami(5) = 0.285
          xlami(6) = 0.295
          xlami(7) = 0.305
          xlami(8) = 0.35
          xlami(9) = 0.64
          xlami(10)= 0.69
          xlami(11)= 1.19 
          xlami(12)= 2.38
          xlami(13)= 4.0
          do iband = 1,ib
            xlam(iband)=0.5*(xlami(iband)+xlami(iband+1)) 
          enddo 
          do iband=ib+1,29
            xlami(iband) = 0.0
            xlam(iband)  = 0.0
          enddo 
      else
          write(*,*) 'Not programmed for this number of bands'
          stop
      endif

c     start and end-band wavelengths in nm
      do iband=1,ib
        xlamb(iband)=1000*xlami(iband)+1
        xlame(iband)=1000*xlami(iband+1)
      enddo


        if(ib.eq.29.or.ib.eq.31) then
    
c     Open and read solar fluxes at TOA from xtspec.dat

      open(11,file='input/xtspec.dat', status='old')      

      i=0
      do while(i.lt.5000)
        read(11,*) i, flux(i)
      enddo

       endif

c     Calculate spectral fluxes in the broad bands (ibm)
 
        if(ib.eq.31) then

      close(11)

c     initialize spectral parameters
      do iband = 1, 31
        fband(iband)= 0.0
      enddo
      do ibm = 1,9
        fb(ibm) = 0.0
      enddo

c     calculate band solar fluxes
      do iband = 1, 31
        do i = int(xlamb(iband)), int(xlame(iband))
          fband(iband) = fband(iband) + flux(i) 
        enddo
      enddo

c     for iband' =ibm = 1 (iband=1-2)
      ibm = 1
      ibmb= 1
      ibme= 2
      do iband = ibmb, ibme
        do i = int(xlamb(iband)), int(xlame(iband))
          fb(ibm) = fb(ibm) + flux(i) 
        enddo
      enddo
c     for iband' = ibm = 2 (iband=3-5)
      ibm = 2
      ibmb= 3
      ibme= 5
      do iband = ibmb, ibme
        do i = int(xlamb(iband)), int(xlame(iband))
          fb(ibm) = fb(ibm) + flux(i) 
        enddo
      enddo
c     for iband' = ibm = 3 (iband=6-9)
      ibm = 3
      ibmb= 6
      ibme= 9
      do iband = ibmb, ibme
        do i = int(xlamb(iband)), int(xlame(iband))
          fb(ibm) = fb(ibm) + flux(i) 
        enddo
      enddo
c     for iband' = ibm = 4 (iband=10-13)
      ibm = 4
      ibmb= 10
      ibme= 13
      do iband = ibmb, ibme
        do i = int(xlamb(iband)), int(xlame(iband))
          fb(ibm) = fb(ibm) + flux(i) 
        enddo
      enddo
c     for iband' = ibm = 5 (iband=14-17)
      ibm = 5
      ibmb= 14
      ibme= 17
      do iband = ibmb, ibme
        do i = int(xlamb(iband)), int(xlame(iband))
          fb(ibm) = fb(ibm) + flux(i) 
        enddo
      enddo
c     for iband' = ibm = 6 (iband=18-21)
      ibm = 6
      ibmb= 18
      ibme= 21
      do iband = ibmb, ibme
        do i = int(xlamb(iband)), int(xlame(iband))
          fb(ibm) = fb(ibm) + flux(i) 
        enddo
      enddo
c     for iband' = ibm = 7 (iband=22)
c      ibm = 7
c      ibmb= 22
c      ibme= 22
c      do iband = ibmb, ibme
c        do i = int(xlamb(iband)), int(xlame(iband))
c          fb(ibm) = fb(ibm) + flux(i) 
c        enddo
c      enddo
c     for iband' = ibm = 8 (iband=23-24)
      ibm = 8
      ibmb= 23
      ibme= 24
      do iband = ibmb, ibme
        do i = int(xlamb(iband)), int(xlame(iband))
          fb(ibm) = fb(ibm) + flux(i) 
        enddo
      enddo
c     for iband' = ibm = 9 (iband=25-26)
      ibm = 9
      ibmb= 25
      ibme= 26
      do iband = ibmb, ibme
        do i = int(xlamb(iband)), int(xlame(iband))
          fb(ibm) = fb(ibm) + flux(i) 
        enddo
      enddo
!      do ibm=1,9
!        write(*,*) 'ibm, fb =', ibm, fb(ibm)
!      enddo

        elseif(ib.eq.29) then

c      rewind 11
c     for iband' = ibm = 8 (iband=8-12)
      ibm = 8
      ibmb= 8
      ibme= 12
c      call fsum(ibm,ibmb,ibme,xlamb,xlame)
c      rewind 11
c     for iband' = ibm = 10 (iband=14-19)
      ibm = 10
      ibmb= 14
      ibme= 19
c      call fsum(ibm,ibmb,ibme,xlamb,xlame)
c      rewind 11
c     for iband' = ibm = 11 (iband=20-25)
      ibm = 11
      ibmb= 20
      ibme= 25
c      call fsum(ibm,ibmb,ibme,xlamb,xlame)
c      rewind 11
c     for iband' = ibm = 12 (iband=26-29)
      ibm = 12
      ibmb= 26
      ibme= 29
c      call fsum(ibm,ibmb,ibme,xlamb,xlame)

      close(11)

c     initialize spectral parameters
      do iband = 1, 29
        fband(iband)= 0.0
      enddo
      do ibm = 8,12
        fb(ibm) = 0.0
      enddo

c     calculate band solar fluxes
      do iband = 1, 29
        do i = int(xlamb(iband)), int(xlame(iband))
          fband(iband) = fband(iband) + flux(i) 
        enddo
      enddo

c     band 8'=8-12
      ibm=8
      do iband = 8, 12
        do i = int(xlamb(iband)), int(xlame(iband))
          fb(ibm) = fb(ibm) + flux(i) 
        enddo
      enddo

c     band 10'=14-19
      ibm=10
      do iband = 14, 19
        do i = int(xlamb(iband)), int(xlame(iband))
          fb(ibm) = fb(ibm) + flux(i) 
        enddo
      enddo

c     band 11'=20-25
      ibm=11
      do iband = 20, 25
        do i = int(xlamb(iband)), int(xlame(iband))
          fb(ibm) = fb(ibm) + flux(i) 
        enddo
      enddo

c     band 12'=26-29
      ibm=12
      do iband = 26, 29
        do i = int(xlamb(iband)), int(xlame(iband))
          fb(ibm) = fb(ibm) + flux(i) 
        enddo
      enddo

        endif ! ib=29or31

        if(ib.eq.19) then

c     initialize spectral parameters
      do iband = 1, 31
        fband(iband)= 0.0
      enddo
c      do ibm = 1,12
      do ibm = 1,16
        fb(ibm) = 0.0
      enddo

c     Calculate approximate LW flux (in one wide band) from the Planck function (W m-2 m-1) for the 
c     black body emissivity (temp=300 gives Fig. 1.8 of Seinfeld & Pandis, OK). We use the equivalent 
c     black body temperature T=255 (K), and wavelength 'i' is given in units um below:
      temp=255.0
      ibm=16   ! (ibmb=16, ibme=19)
      do i=int(xlami(16)),int(xlami(20))  ! i=28-100, i.e. integrating over ib = 16 - 19
          flux(i)=(3.74e8/((real(i))**5))
     $            /(2.718281828**(1.44e4/(temp*real(i)))-1.0)
          fb(ibm)=fb(ibm)+flux(i)
      enddo
c      write(*,*) fb(ibm) 

      do iband = 16, 19
        do i = int(xlami(iband)), int(xlami(iband+1))-1
          fband(iband) = fband(iband) + flux(i)
        enddo
c        write(*,*) iband, fband(iband) 
      enddo

        endif ! ib=19


      return
      end 


c********************************************************************

      subroutine fsum (ibm,ibmb,ibme,xlamb,xlame)

      implicit none

      INTEGER ibm, ibmb, ibme, i
      REAL    xlamb(31), xlame(31), fsol(31), flux, fsolar 

      fsol(ibm)=0.0
      fsolar=0.0
      i=0

      do while(i.lt.5000)
        read(11,*) i, flux
        fsolar=fsolar+flux
        if(real(i).ge.xlamb(ibmb).and.real(i).le.xlame(ibme)) then
          fsol(ibm)=fsol(ibm)+flux
        endif
      enddo
      write(*,*) 'fsol(ibm)/fsolar =', fsol(ibm)/fsolar      

      return
      end 

c********************************************************************
