#!/bin/tcsh -f

	echo ''
	echo 'Note: if not working, try "module load nco-cnl (or nco)" first'
	echo ''

set ARCDIR = /projects/NS2345K/noresm/cases/PD_MG15MegVadSOA/atm/hist/
set ARCDIR2 = /scratch/kirkevag/pnsd/
set AVAILABLEEXP = (PD_MG15MegVadSOA)
set AVAILABLEMONTHS = (01 02 03 04 05 06 07 08 09 10 11 12)
#set AVAILABLEYEARS = (0003 0004 0005 0006 0007 0008 0009 0010 0011 0012)

foreach EXP ($AVAILABLEEXP) 

        echo ''
	echo '------------------------ climatological means ----------------------------'
	echo ''
	echo ''

	echo 'Calculating ensemble means for each month for all variables (i.e. only 12 months of data)'
        echo ''
	foreach MONTH ($AVAILABLEMONTHS) 
	    echo 'MONTH' $MONTH

	    ncea $ARCDIR/${EXP}.cam.h0.0003-${MONTH}.nc $ARCDIR/${EXP}.cam.h0.0004-${MONTH}.nc $ARCDIR/${EXP}.cam.h0.0005-${MONTH}.nc $ARCDIR/${EXP}.cam.h0.0006-${MONTH}.nc $ARCDIR/${EXP}.cam.h0.0007-${MONTH}.nc $ARCDIR/${EXP}.cam.h0.0008-${MONTH}.nc $ARCDIR/${EXP}.cam.h0.0009-${MONTH}.nc $ARCDIR/${EXP}.cam.h0.0010-${MONTH}.nc $ARCDIR/${EXP}.cam.h0.0011-${MONTH}.nc $ARCDIR/${EXP}.cam.h0.0012-${MONTH}.nc -O $ARCDIR2/${EXP}.cam.h0.climyr3-12_${MONTH}.nc

	end
       
end

exit

