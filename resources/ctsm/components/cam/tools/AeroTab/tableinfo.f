      subroutine tableinfo (kcomp, xbc, xdst, xoc, xs, xa, xss, relh, 
     $ fombg, fbcbg, catote, catot, fac, fbc, faq, ib, ibcam, itilp)

c **********************************************************************************
c     Add header information for the opened look-up table (LUT) files, both as an
c     explanation what the files contain, and to facilitate checking important input-
c     info assumed in AeroTab against CAM5-Oslo (to make sure that this particular
c     LUT is compatible with the CAM5-Oslo version which is being used).
c
c     Created by Alf Kirkevåg April 2015. Updated for new SOA treatment July 2015.
c **********************************************************************************
c     Note tha dry lognormal the fitted size parameters do not depend
c     on the mass fraction fombc for kcomp=1, nor on fbcbg for kcomp=4. 
c **********************************************************************************

      use commondefinitions

      implicit none

      INTEGER kcomp, ib, ibcam, itilp, i, iu, iumax, iunr(3)
      REAL xbc, xoc, xs, xa, xss, xdst
      REAL relh(10), catote(16), catot(6), fac(6), fbc(6), faq(6), 
     $ fombg(6), fbcbg(6)

      if(itilp.eq.1) then
        if(kcomp.eq.0) return  ! no logntilp0.out needed (constant r and logsigma)
        iumax=1
        iunr(1)=9003   ! logntilp*.out   
      elseif(itilp.eq.0.and.(ib.eq.31.or.ib.eq.19)) then
cx        iumax=3
        if(ib.eq.31) then
          iumax=3
          iunr(1)=9000   ! kcomp*.out
          iunr(2)=9500   ! aerocomk*.out       
          iunr(3)=9600   ! aerodryk*.out      
        else
cx        iumax=3
          iumax=1
          iunr(1)=9009   ! lwkcomp*.put
        endif
cx        iunr(2)=9500   ! aerocomk*.out       
cx        iunr(3)=9600   ! aerodryk*.out      
      endif     

      do i=1,iumax
        iu=iunr(i)

       if(iu.eq.9000) then
c     Adding LUT header info for kcomp*.out:

      write(iu,*) 'This look-up table (LUT) contains the following ',
     $ 'SW gross optical properties for a normalized aerosol size ', 
     $ 'distribution (N=1/cm3):' 
      write(iu,*) '' 
      if(kcomp.eq.0) then
        write(iu,*) 'kcomp, iband, rh, '
      elseif(kcomp.eq.1) then
        write(iu,*) 'kcomp, iband, rh, fombg, catot, fac, '
      elseif(kcomp.eq.2.or.kcomp.eq.3) then
        write(iu,*) 'kcomp, iband, rh, catot, fac, '
      elseif(kcomp.eq.4) then
        write(iu,*) 'kcomp, iband, rh, fbcbg, catot, fac, faq, '
      else ! (kcomp = 5, 6, 7, 8, 9 or 10)
        write(iu,*) 'kcomp, iband, rh, catot, fac, fbc, faq, '
      endif ! kcomp
      write(iu,*) 'omega(iband), gass(iband), bext(iband), kext(iband)'
      write(iu,*) '' 
      write(iu,*) 'Here omega is the single scattering albedo (1),', 
     $ 'gass is the asymmetry factor (1), bext is the extiction ',
     $ '(1/km), and kext is the specific extinction (m2/g)' 

       elseif(iu.eq.9003) then
c     Adding LUT header info for logntilp*.out:
      write(iu,*) 'This look-up table (LUT) contains the following ',
     $ 'fitted lognormal mode parameters for the '
      write(iu,*) 'modified (dry, generally non-lognormal) ',
     $ 'aerosol size distribution (normalized so that N=1/cm3):' 
      write(iu,*) '' 
      if(kcomp.eq.1) then
        write(iu,*) 'kcomp, catot, fac, r, logsigma,'
      elseif(kcomp.eq.2.or.kcomp.eq.3) then
        write(iu,*) 'kcomp, catot, fac, r, logsigma,'
      elseif(kcomp.eq.4) then
        write(iu,*) 'kcomp, catot, fac, faq, r, logsigma'
      else ! (kcomp = 5, 6, 7, 8, 9 or 10)
        write(iu,*) 'kcomp, catot, fac, fbc, faq, r, logsigma'
      endif
      write(iu,*) '' 
      write(iu,*) 'where r and logsigma are dry modal median radius ', 
     $ 'and logarithm (base 10) of the standard deviation, '

       elseif(iu.eq.9009) then
c     Adding LUT header info for lwkcomp*.out:
      write(iu,*) 'This look-up table (LUT) contains the following ',
     $ 'LW gross optical properties for a normalized aerosol size ', 
     $ 'distribution (N=1/cm3):' 
      write(iu,*) '' 
      if(kcomp.eq.0) then
        write(iu,*) 'kcomp, iband, rh, '
      elseif(kcomp.eq.1) then
        write(iu,*) 'kcomp, iband, rh, fombg, catot, fac, '
      elseif(kcomp.eq.2.or.kcomp.eq.3) then
        write(iu,*) 'kcomp, iband, rh, catot, fac, '
      elseif(kcomp.eq.4) then
        write(iu,*) 'kcomp, iband, rh, fbcbg, catot, fac, faq, '
      else ! (kcomp = 5, 6, 7, 8, 9 or 10)
        write(iu,*) 'kcomp, iband, rh, catot, fac, fbc, faq, '
      endif
      write(iu,*) 'kabs(iband)'
      write(iu,*) '' 
      write(iu,*) 'Here kabs is the specific absorption (m2/g),', 
     $  'i.e. kabs(iband)=(1.0-omega(iband))*kext(iband)'

       elseif(iu.eq.9500) then
c     Adding LUT header info for aerocomk*.out:
      write(iu,*) 'This look-up table (LUT) contains the following ',
     $ 'SW gross optical properties for a normalized aerosol size ', 
     $ 'distribution (N=1/cm3):' 
      write(iu,*) '' 
      if(kcomp.eq.0) then
        write(iu,*) 'kcomp, rh, ',
     $ 'bebg(440), babg(440), bebg(500), babg(500), babg(550), ',
     $ 'bebg(670), babg(670), bebg(870), babg(870), ', 
     $ 'bebglt1, bebggt1, backsc '
      elseif(kcomp.eq.1) then
        write(iu,*) 'kcomp, rh, fombg, catot, fac, '
      elseif(kcomp.eq.2.or.kcomp.eq.3) then
        write(iu,*) 'kcomp, rh, catot, fac, '
      elseif(kcomp.eq.4) then
        write(iu,*) 'kcomp, rh, fbcbg, catot, fac, faq, '
      else ! (kcomp = 5, 6, 7, 8, 9 or 10)
        write(iu,*) 'kcomp, rh, catot, fac, fbc, faq, '
      endif ! kcomp
      if(kcomp.ne.0) then
        write(iu,*) 'bext(440),  bext(500), bext(670), bext(870), ', 
     $ 'bebg(440),  bebg(500), bebg(670), bebg(870), ',
     $ 'bebc(440),  bebc(500), bebc(670), bebc(870), '
        write(iu,*) 'beoc(440),  beoc(500), beoc(670), beoc(870), ',
     $ 'besu(440),  besu(500), besu(670), besu(870), ',
     $ 'babs(440),  babs(500), babs(550), babs(670), babs(870), '
        write(iu,*) 'bebglt1, bebggt1, bebclt1, bebcgt1, ', 
     $ 'beoclt1, beocgt1, besult1, besugt1, backsc, ',
     $ 'babg(550), babc(550), baoc(550), basu(550)' 
      endif ! kcomp
      write(iu,*) '' 
      if(kcomp.eq.0) then
       write(iu,*) 'Here bebg and babg are the total extinction and ',
     $ 'absorption coefficients (1/km) for the '
       write(iu,*) 'background aerosol ',
     $ '(which is the total aerosol for this kcomp)' 
      else ! (kcomp = 1 - 10)
        write(iu,*) 'Here bext and babs are the total extinction and ',
     $ 'absorption coefficients (1/km), '
        write(iu,*) 'bebg and babg are the corresponding extinction ',
     $ 'and absorption for the background aerosol, '
        write(iu,*) 'and similarly for the internally mixed ',
     $ 'BC (bebc and babc), OC (beoc and baoc), and sulfate (besu ',
     $ 'and basu), '
      endif ! kcomp
      if(kcomp.eq.0) then 
        write(iu,*) 'all for the wavelengths (nm) given inside ',
     $ 'the brackets. bebglt1 and bebggt1 are bebg(550) for '
        write(iu,*) 'particle diameters d < 1 um and d > 1 um, ',
     $ 'respectively. '
      else ! (kcomp = 1 - 10)      
        write(iu,*) 'all for the wavelengths (nm) given inside ',
     $ 'the brackets. bebglt1 and bebggt1 are bebg(550) for '
        write(iu,*) 'particle diameters d < 1 um and d > 1 um, ',
     $ 'respectively, and similarly for the contribution'
        write(iu,*) 'of internally mixed BC (bebc*1), OC (beoc*1) ',
     $ 'and sulfate (besu*1). '
      endif
      write(iu,*) 'backsc is the 550 nm backscatter coefficient ',
     $ '(1/sr/km) at 180 degrees' 

       elseif(iu.eq.9600) then
c     Adding LUT header info for aerodryk*.out:
      write(iu,*) 'This look-up table (LUT) contains the following ',
     $ 'size integrated constituent mass and total area and volume '
      write(iu,*) 'concentrations for the modified, dry ',
     $ 'aerosol size distribution (normalized so that N=1/cm3):' 
      write(iu,*) '' 
      if(kcomp.eq.0) then
        write(iu,*) 'kcomp, cintbg, cintbg05, cintbg125, ', 
     $ 'aaeros, aaerol, vaeros, vaerol'          
      elseif(kcomp.eq.1) then
         write(iu,*) 'kcomp, fombg, catot, fac, '
      elseif(kcomp.eq.2.or.kcomp.eq.3) then
         write(iu,*) 'kcomp, catot, fac, '
      elseif(kcomp.eq.4) then
        write(iu,*) 'kcomp, fbcbg, catot, fac, faq, '
      else ! (kcomp = 5, 6, 7, 8, 9 or 10)
        write(iu,*) 'kcomp, catot, fac, fbc, faq, '
      endif ! kcomp
      if(kcomp.ne.0) then
        write(iu,*) 'cintbg, cintbg05, cintbg125, cintbc, cintbc05, ',
     $  'cintbc125,'  
        write(iu,*) 'cintoc, cintoc05, cintoc125, cintsc, cintsc05, ',
     $  'cintsc125,'        
        write(iu,*) 'cintsa, cintsa05, cintsa125, ',
     $  'aaeros, aaerol, vaeros, vaerol'          
      endif ! kcomp
      write(iu,*) '' 
      if(kcomp.eq.0) then
        write(iu,*) 'Here cintbg, cintbg05 and cintbg125 are ',
     $ 'size-integrated dry mass concentrations intergrated over '
        write(iu,*) 'all radii r, r<0.5um and r>1.25um of the ',
     $ 'background aerosol ',
     $ '(which is the total aerosol for this kcomp)' 
      else ! (kcomp = 1 - 10)
        write(iu,*) 'Here cintbg, cintbg05 and cintbg125 are ',
     $ 'size-integrated dry mass concentrations intergrated over '
        write(iu,*) 'all radii r, r<0.5um and r>1.25um of the ',
     $ 'background aerosol, and similarly (whether present or not for ', 
     $ 'this mode) '
        write(iu,*) 'cint*, cint*05 and cint*125 denote the ',
     $ 'size-integrals for internally mixed ',
     $ 'BC (bc), OC (oc), and sulfate '
        write(iu,*) 'in the form of H2SO4 (sc) and as (NH4)2SO4 (sa)'
      endif ! kcomp
      write(iu,*) 'aaeros and aaerol are size-integrated aerosol ',
     $ 'areas for radii r<0.5um and r>0.5um, '
      write(iu,*) 'and vaeros and vaerol are size-integrated aerosol ',
     $ 'volumes for radii r<0.5um and r>0.5um '

       endif ! iu


       if(iu.eq.9000.or.iu.eq.9009.or.iu.eq.9500) then  ! for kcomp*.out, lwkcomp*.out or aerocomk*.out
         write(iu,*) 'kcomp is the aerosol size mode number, ', 
     $ 'iband is the wavelength band number, rh is RH(%)/100 ' 
       else
         write(iu,*) 'kcomp is the aerosol size mode number '
       endif

      if(kcomp.eq.0) then
        write(iu,*) '(no added internally mixed mass to be taken ',
     $ 'into account for this mode)'
      elseif(kcomp.eq.1) then
        write(iu,*) 'catot is total added internally mixed mass ',
     $ '(ug/m3), fac is the OC (SOA) mass fraction ',
     $ 'of catot (OC & H2SO4) '
        write(iu,*) 'and fombg is the mass fraction of OC (as SOA) ', 
     $ 'in the internally mixed background (of H2SO4 and SOA)'
        if(iu.eq.9003) then
        write(iu,*) '(Note: r and logsigma do not depend on fombg) ' 
        endif
      elseif(kcomp.eq.2.or.kcomp.eq.3) then
        write(iu,*) 'catot is total added internally mixed mass ',
     $ '(ug/m3), fac is the OC (SOA) mass fraction ',
     $ 'of catot (OC & H2SO4) '
      elseif(kcomp.eq.4) then
        write(iu,*) 'catot is total added internally mixed mass ',
     $ '(ug/m3) as H2SO4 and (NH4)2SO4, fac is the mass fraction ', 
     $ 'of added SOA/(SOA+Sulfate) '
        write(iu,*) 'faq is the mass fraction of wet phase sulfate, ',
     $ 'as (NH4)2SO4, of total added sulfate (catot), and'
        write(iu,*) 'fbcbg is the mass mass fraction of BC ', 
     $ 'in the internally mixed background of OC and BC '
        if(iu.eq.9003) then
        write(iu,*) '(Note: r and logsigma do not depend on fbcbg) ' 
        endif
      else ! (kcomp = 5, 6, 7, 8, 9 or 10)
        write(iu,*) 'catot is total added internally mixed mass ',
     $ '(ug/m3), fac is the OC & BC mass fraction of catot, ',
     $ 'fbc is BC fraction of that OC & BC '
        write(iu,*) 'and faq is the mass fraction of wet ',
     $ 'phase sufate, as (NH4)2SO4, of total added sulfate'
      endif  ! kcomp
      write(iu,*) '' 

c++++ Part with values for checking against CAM5-Oslo (X-CHECK...)
c     This part can be used for all LUT, except RH, hygroscopicity and
c     wavelength info, whis is not written out for dry aerosol tables 
c     (logntilp*.out and aerodryk*.out).     

      write(iu,*) 'This LUT is made with an AeroTab version ', 
     $ 'with the following input parameters, to be checked ' 
      write(iu,*) 'against CAM5-Oslo (producing error messages on ',
     $ 'inconsistencies):'
       if(iu.eq.9000.or.iu.eq.9009.or.iu.eq.9500) then
      write(iu,*) 'Discrete input-values of'
      write(iu,*) 'X-CHECK relh ='
      write(iu,500) relh
       endif ! iu
      if(kcomp.eq.0) then
        write(iu,*) '(catot, fac, fbc, faq: not used)'
      elseif(kcomp.eq.1) then
       if(iu.ne.9003) then
        write(iu,*) 'X-CHECK fombg ='
        write(iu,400) fombg
       endif
        write(iu,*) 'X-CHECK catot ='
        write(iu,300) catote
        write(iu,*) 'X-CHECK fac ='
        write(iu,400) fac
        write(iu,*) '(fbc, faq: not used)'
      elseif(kcomp.eq.2.or.kcomp.eq.3) then
        write(iu,*) 'X-CHECK catot ='
        write(iu,300) catote
        write(iu,*) 'X-CHECK fac ='
        write(iu,400) fac
        write(iu,*) '(fombg, fbc, faq: not used)'
      elseif(kcomp.eq.4) then
       if(iu.ne.9003) then
        write(iu,*) 'X-CHECK fbcbg ='
        write(iu,400) fbcbg
       endif
        write(iu,*) 'X-CHECK catot ='
        write(iu,300) catote
        write(iu,*) 'X-CHECK fac ='
        write(iu,400) fac
        write(iu,*) '(fbc, faq: not used)'
      else ! (kcomp = 5, 6, 7, 8, 9 or 10)
        write(iu,*) 'X-CHECK catot ='
        write(iu,400) catot
        write(iu,*) 'X-CHECK fac ='
        write(iu,400) fac
        write(iu,*) 'X-CHECK fbc ='
        write(iu,400) fbc
        write(iu,*) 'X-CHECK faq ='
        write(iu,400) faq
      endif ! kcomp
      
      write(iu,*) 'Modal median dry radius (m) at point of emission '
      write(iu,*) 'X-CHECK originalNumberMedianRadius =' 
      write(iu,200) originalNumberMedianRadius(kcomp)
      write(iu,*) 'and corresponding standard deviation ' 
      write(iu,*) 'X-CHECK originalSigma =' 
      write(iu,200) originalSigma(kcomp)
      write(iu,*) 'Aerosol type mass densities (kg/m3) for ',
     $ 'ammonium sulfate, BC, OC, dust and sea-salt '
      write(iu,*) 'X-CHECK aerosol_type_density ='
      write(iu,100) aerosol_type_density 
       if(iu.eq.9000.or.iu.eq.9009.or.iu.eq.9500) then  ! for kcomp*.out, lwkcomp*.out or aerocomk*.out
      write(iu,*) 'and their corresponding hygroscopicities ',
     $ 'at RH~100%'
      write(iu,*) 'X-CHECK hygroscopicities ='
      write(iu,100) xa, xbc, xoc, xdst, xss
      write(iu,*) 'Number of wavelength bands in AeroTab ', 
     $ '(before Chandrasekhar averaging) '
      write(iu,*) 'X-CHECK ib ='     
      write(iu,50) ib
      write(iu,*) 'and the number of wavelength bands used ',
     $ 'in CAM5-Oslo:'
      write(iu,*) 'X-CHECK ibcam ='     
      write(iu,50) ibcam
       endif ! iu

c     Finally, the table values to be used for look-ups and interpolations in CAM5-Oslo:
      write(iu,*) '' 
      write(iu,*) 'X-CHECK LUT: table values to be used ',
     $ 'for look-ups and interpolations in CAM5-Oslo:'
c----

      enddo ! i (iu value)


  50  format(x,i2)
 100  format(5(x,e11.5))
 200  format(x,e11.5)
 300  format(16(x,e10.4))
 400  format(6(x,e10.4))
 500  format(10(x,e9.3))

      return
      end  
