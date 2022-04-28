#!/bin/tcsh -f

set ARCDIR = /scratch/kirkevag/pnsd/
set OUTDIR = /scratch/kirkevag/pnsd/

set AVAILABLEEXP = (PD_MG15MegVadSOA)

set AVAILABLEMONTHS = (01 02 03 04 05 06 07 08 09 10 11 12)

set AVAILABLEVARS = (NNAT_0 NNAT_1 NNAT_2 NNAT_4 NNAT_5 NNAT_6 NNAT_7 NNAT_8 NNAT_9 NNAT_10 NNAT_12 NNAT_14 NMR01 NMR02 NMR04 NMR05 NMR06 NMR07 NMR08 NMR09 NMR10 NMR12 NMR14 SIGMA01 SIGMA02 SIGMA04 SIGMA05 SIGMA06 SIGMA07 SIGMA08 SIGMA09 SIGMA10 SIGMA12 SIGMA14) 

foreach EXP ($AVAILABLEEXP) 

   foreach MNTH ($AVAILABLEMONTHS)
	    echo 'month' $MNTH
  	    echo ''

        echo 'Take out surface values of all 3D fields for each monthly file:'
	echo ''
        ncks -d lev,29 $ARCDIR/${EXP}.cam.h0.climyr3-12_${MNTH}.nc  -O  $OUTDIR/${EXP}.cam.h0.climyr3-12surf_${MNTH}.nc

        echo 'Then take out only the required fields --> new and smaller output:'
	echo ''
        foreach VAR ($AVAILABLEVARS)
	    echo 'VAR' $VAR
  	    echo ''
            ncks -v $VAR,time_bnds $OUTDIR/${EXP}.cam.h0.climyr3-12surf_${MNTH}.nc -A $OUTDIR/${EXP}.cam.h0.climyr3-12surface_${MNTH}.nc
        end

   end

	echo 'Join all the monthly data into one file'
	echo ''
    	ncrcat $OUTDIR/${EXP}.cam.h0.climyr3-12surface_??.nc -O $OUTDIR/${EXP}.cam.h0.climyr3-12surface_monthly.nc 

	echo 'Calculate new variables which are necessary for the PNSD fortran code'
	echo ''
        ncap -s 'NMR00=float(0.1)*NMR01/NMR01' \
             -s 'SIGMA00=float(1.60)*NMR01/NMR01' $OUTDIR/${EXP}.cam.h0.climyr3-12surface_monthly.nc -O $OUTDIR/${EXP}.cam.h0.climyr3-12surfNewVar_monthly.nc
  
       
	echo 'And finally remove lev as dimension'
	echo ''
        ncwa -a lev $OUTDIR/${EXP}.cam.h0.climyr3-12surface_monthly.nc $OUTDIR/${EXP}.cam.h0.climyr3-12surface_monthly-nolev.nc


end

exit



