      subroutine openfiles(kcomp,iopt,ib,extradiag)

c **********************************************************************************
c     Opening .out files for use as input (look-up tables) to NorESM (only for
c     kcomp=0-10) and .dat files for control or plotting purposes (if extradiag=.true.)
c  
c     Created by Alf Kirkevåg.
c **********************************************************************************

      integer kcomp, iopt, ib
      logical extradiag

       open(999, file='runlog.dat')

       if(iopt.eq.1) then

       if(ib.eq.31) then  ! SW aerocom optics only
        if(kcomp.eq.1) then
          open(9500, file='aerocomk1.out')
        elseif(kcomp.eq.2) then
          open(9500, file='aerocomk2.out')
        elseif(kcomp.eq.3) then
          open(9500, file='aerocomk3.out')
        elseif(kcomp.eq.4) then
          open(9500, file='aerocomk4.out')
        elseif(kcomp.eq.5) then
          open(9500, file='aerocomk5.out')
        elseif(kcomp.eq.6) then
          open(9500, file='aerocomk6.out')
        elseif(kcomp.eq.7) then
          open(9500, file='aerocomk7.out')
        elseif(kcomp.eq.8) then
          open(9500, file='aerocomk8.out')
        elseif(kcomp.eq.9) then
          open(9500, file='aerocomk9.out')
        elseif(kcomp.eq.10) then
          open(9500, file='aerocomk10.out')
        elseif(kcomp.eq.0) then
          open(9500, file='aerocomk0.out')
        endif        
        if(kcomp.eq.1) then
          open(9600, file='aerodryk1.out')
        elseif(kcomp.eq.2) then
          open(9600, file='aerodryk2.out')
        elseif(kcomp.eq.3) then
          open(9600, file='aerodryk3.out')
        elseif(kcomp.eq.4) then
          open(9600, file='aerodryk4.out')
        elseif(kcomp.eq.5) then
          open(9600, file='aerodryk5.out')
        elseif(kcomp.eq.6) then
          open(9600, file='aerodryk6.out')
        elseif(kcomp.eq.7) then
          open(9600, file='aerodryk7.out')
        elseif(kcomp.eq.8) then
          open(9600, file='aerodryk8.out')
        elseif(kcomp.eq.9) then
          open(9600, file='aerodryk9.out')
        elseif(kcomp.eq.10) then
          open(9600, file='aerodryk10.out')
        elseif(kcomp.eq.0) then
          open(9600, file='aerodryk0.out')
        endif        
       endif ! ib=31

       if(ib.ne.19) then  ! SW CAM optics only
        if(kcomp.eq.1) then
          open(9000, file='kcomp1.out')
        elseif(kcomp.eq.2) then
          open(9000, file='kcomp2.out')
        elseif(kcomp.eq.3) then
          open(9000, file='kcomp3.out')
        elseif(kcomp.eq.4) then
          open(9000, file='kcomp4.out')
        elseif(kcomp.eq.5) then
          open(9000, file='kcomp5.out')
        elseif(kcomp.eq.6) then
          open(9000, file='kcomp6.out')
        elseif(kcomp.eq.7) then
          open(9000, file='kcomp7.out')
        elseif(kcomp.eq.8) then
          open(9000, file='kcomp8.out')
        elseif(kcomp.eq.9) then
          open(9000, file='kcomp9.out')
        elseif(kcomp.eq.10) then
          open(9000, file='kcomp10.out')
        elseif(kcomp.eq.0) then
          open(9000, file='kcomp0.out')
        endif        
       endif

       if(ib.eq.19) then  ! LW optics only
        if(kcomp.eq.1) then
          open(9009, file='lwkcomp1.out')
        elseif(kcomp.eq.2) then
          open(9009, file='lwkcomp2.out')
        elseif(kcomp.eq.3) then
          open(9009, file='lwkcomp3.out')
        elseif(kcomp.eq.4) then
          open(9009, file='lwkcomp4.out')
        elseif(kcomp.eq.5) then
          open(9009, file='lwkcomp5.out')
        elseif(kcomp.eq.6) then
          open(9009, file='lwkcomp6.out')
        elseif(kcomp.eq.7) then
          open(9009, file='lwkcomp7.out')
        elseif(kcomp.eq.8) then
          open(9009, file='lwkcomp8.out')
        elseif(kcomp.eq.9) then
          open(9009, file='lwkcomp9.out')
        elseif(kcomp.eq.10) then
          open(9009, file='lwkcomp10.out')
        elseif(kcomp.eq.0) then
          open(9009, file='lwkcomp0.out')
        endif        
       endif  ! ib=19

      else  ! iopt=0

        if(kcomp.eq.1) then
          open(9003, file='logntilp1.out')
        elseif(kcomp.eq.2) then
          open(9003, file='logntilp2.out')
        elseif(kcomp.eq.3) then
          open(9003, file='logntilp3.out')
        elseif(kcomp.eq.4) then
          open(9003, file='logntilp4.out')
        elseif(kcomp.eq.5) then
          open(9003, file='logntilp5.out')
        elseif(kcomp.eq.6) then
          open(9003, file='logntilp6.out')
        elseif(kcomp.eq.7) then
          open(9003, file='logntilp7.out')
        elseif(kcomp.eq.8) then
          open(9003, file='logntilp8.out')
        elseif(kcomp.eq.9) then
          open(9003, file='logntilp9.out')
        elseif(kcomp.eq.10) then
          open(9003, file='logntilp10.out')
        endif        

      endif  ! iopt

      if(extradiag) then

        open(12, file='dndlogr0.dat')
        open(13, file='dndlogrmod.dat')
        open(14, file='dvdlogrmod.dat')        
        open(60, file='vsi.dat')
        open(61, file='vbci.dat')
        open(62, file='voci.dat')
        open(63, file='vai.dat')
        open(132, file='vssol.dat')
        open(133, file='vbcsol.dat')
        open(134, file='vocsol.dat')
        open(135, file='vasol.dat')
        open(136, file='vw.dat')
        if(iopt.eq.1) then
          open(40, file='omega.dat')
          open(41, file='gass.dat')
          open(42, file='bext.dat')
          open(43, file='kext.dat')
          open(44, file='mec.dat')
        endif
        if(kcomp.eq.0) then
          open(9001, file='nkcomp0.dat')
        elseif(kcomp.eq.1) then
          open(9001, file='nkcomp1.dat')
        elseif(kcomp.eq.2) then
          open(9001, file='nkcomp2.dat')
        elseif(kcomp.eq.3) then
          open(9001, file='nkcomp3.dat')
        elseif(kcomp.eq.4) then
          open(9001, file='nkcomp4.dat')
        elseif(kcomp.eq.5) then
          open(9001, file='nkcomp5.dat')
        elseif(kcomp.eq.6) then
          open(9001, file='nkcomp6.dat')
        elseif(kcomp.eq.7) then
          open(9001, file='nkcomp7.dat')
        elseif(kcomp.eq.8) then
          open(9001, file='nkcomp8.dat')
        elseif(kcomp.eq.9) then
          open(9001, file='nkcomp9.dat')
        elseif(kcomp.eq.10) then
          open(9001, file='nkcomp10.dat')
        endif                  

      endif
            
      return
      end
