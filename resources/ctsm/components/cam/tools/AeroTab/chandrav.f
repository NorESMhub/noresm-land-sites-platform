ccccc6ccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5ccccccccc6ccccccccc7cc
      subroutine chandrav (ib, xlam, xlamb, xlame, fband, fb, 
     $                     omega, gass, bext, omch, gch, bch)

c **********************************************************************************
c     Created by Alf Kirkevåg.
c **********************************************************************************

c     Here Chandrasekhar averaged optical parameters are calculated 
c     for the wavelength bands, ibm (each covering several iband's) 

      implicit none

      integer i, iband, ib, ibm, ibmb, ibme 
      real    xlam(31), xlamb(31), xlame(31), fband(31), omega(31), 
     $  gass(31), bext(31), omch(16), gch(16), bch(16), fb(16)


       if(ib.eq.31) then

c     initialize spectral optical parameters
      do ibm = 1, 9
        omch(ibm) = 0.0
        gch(ibm)  = 0.0
        bch(ibm)  = 0.0
      enddo
c     Chandrasekhar averaging band 1' (iband=1-2:   0.2   - 0.263 um)
      ibm =1
      ibmb=1
      ibme=2
      call chsub (ibm, ibmb, ibme, fb, fband, 
     $            omega, gass, bext, omch, gch, bch)
c     Chandrasekhar averaging band 2' (iband=3-5:   0.263 - 0.345 um)
      ibm =2
      ibmb=3
      ibme=5
      call chsub (ibm, ibmb, ibme, fb, fband, 
     $            omega, gass, bext, omch, gch, bch)
c     Chandrasekhar averaging band 3' (iband=6-9:   0.345 - 0.442 um)
      ibm =3
      ibmb=6
      ibme=9
      call chsub (ibm, ibmb, ibme, fb, fband, 
     $            omega, gass, bext, omch, gch, bch)
c     Chandrasekhar averaging band 4' (iband=10-13: 0.442 - 0.625 um)
      ibm =4
      ibmb=10
      ibme=13
      call chsub (ibm, ibmb, ibme, fb, fband, 
     $            omega, gass, bext, omch, gch, bch)
c     Chandrasekhar averaging band 5' (iband=14-17: 0.625 - 0.778 um)
      ibm =5
      ibmb=14
      ibme=17
      call chsub (ibm, ibmb, ibme, fb, fband, 
     $            omega, gass, bext, omch, gch, bch)
c     Chandrasekhar averaging band 6' (iband=18-21: 0.778 - 1.242 um)
      ibm =6
      ibmb=18
      ibme=21
      call chsub (ibm, ibmb, ibme, fb, fband, 
     $            omega, gass, bext, omch, gch, bch)
c     Band 7' (iband=22:    1.242 - 1.299 um)  no Chandrasekhar averaging needed...
c      ibm =7
c      ibmb=22
c      ibme=22
c      call chsub (ibm, ibmb, ibme, fb, fband, 
c     $            omega, gass, bext, omch, gch, bch)
        omch(7) = omega(22)
        gch(7)  = gass(22)
        bch(7)  = bext(22)
c     Chandrasekhar averaging band 8' (iband=23-24: 1.299 - 1.626 um)
      ibm =8
      ibmb=23
      ibme=24
      call chsub (ibm, ibmb, ibme, fb, fband, 
     $            omega, gass, bext, omch, gch, bch)
c     Chandrasekhar averaging band 9' (iband=25-26: 1.626 - 1.942 um)
      ibm =9
      ibmb=25
      ibme=26
      call chsub (ibm, ibmb, ibme, fb, fband, 
     $            omega, gass, bext, omch, gch, bch)

c*****************************************************************

       elseif(ib.eq.19) then

c     initialize spectral optical parameters
c      do ibm = 1, 4
      do ibm = 1, 16
        omch(ibm) = 0.0
        gch(ibm)  = 0.0
        bch(ibm)  = 0.0
      enddo
c     Chandrasekhar averaging the last band (28.571-100um):
      ibm =16
      ibmb=16
      ibme=19
      call chsub (ibm, ibmb, ibme, fb, fband, 
     $            omega, gass, bext, omch, gch, bch)

c*****************************************************************

       elseif(ib.eq.29) then

c     initialize spectral optical parameters
      do ibm = 8, 12
        omch(ibm) = 0.0
        gch(ibm)  = 0.0
        bch(ibm)  = 0.0
      enddo
c     Chandrasekhar averaging band 8' (0.35-0.64um):
      ibm =8
      ibmb=8
      ibme=12
      call chsub (ibm, ibmb, ibme, fb, fband, 
     $            omega, gass, bext, omch, gch, bch)
c     Chandrasekhar averaging band 10' (0.69-1.19um):
      ibm =10
      ibmb=14
      ibme=19
      call chsub (ibm, ibmb, ibme, fb, fband, 
     $            omega, gass, bext, omch, gch, bch)
c     Chandrasekhar averaging band 11' (1.19-2.38um):
      ibm =11
      ibmb=20
      ibme=25
      call chsub (ibm, ibmb, ibme, fb, fband, 
     $            omega, gass, bext, omch, gch, bch)
c     Chandrasekhar averaging band 12' (2.38-4.0um):
      ibm =12
      ibmb=26
      ibme=29
      call chsub (ibm, ibmb, ibme, fb, fband, 
     $            omega, gass, bext, omch, gch, bch)

       endif ! ib = 29 or 31


      return
      end

         
c*************************************************************************
c     Here the Chandrasekhar averaging itself is carried out (weighting
c     of optical properties with the spectrally resolved TOA irradiance).

      subroutine chsub (ibm, ibmb, ibme, fb, fband, 
     $            omega, gass, bext, omch, gch, bch)

      implicit none

      integer iband, ibm, ibmb, ibme 
      real    fband(31), omega(31), gass(31), bext(31)
      real    fb(16), omch(16), gch(16), bch(16)

      do iband = ibmb, ibme
        omch(ibm)=omch(ibm)+omega(iband)*fband(iband)
        gch(ibm) =gch(ibm) +gass(iband)*fband(iband)
        bch(ibm) =bch(ibm) +bext(iband)*fband(iband)
c        write(123,*) 'i, fband, om=', iband, fband(iband), omega(iband)
c        write(*,*) 'i, fband, om=', iband, fband(iband), omega(iband)
      enddo
      omch(ibm)=omch(ibm)/fb(ibm)
      gch(ibm) =gch(ibm)/fb(ibm)
      bch(ibm) =bch(ibm)/fb(ibm)
c      write(124,*) 'ibm, omch =', ibm, omch(ibm)
c      write(125,*) 'ibm, gch  =', ibm, gch(ibm)
c      write(126,*) 'ibm, bch  =', ibm, bch(ibm)
c      write(127,*) 'i, fb =', ibm, fb(ibm)
c      write(*,*) 'i, fb, bch =', ibm, fb(ibm), bch(ibm)

      return
      end
