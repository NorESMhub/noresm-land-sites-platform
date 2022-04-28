#!/bin/sh

#Create the input directory here with
#mkdir linkToInDirectory and then
#sshfs -r alfgr@hexagon.bccs.uib.no:/work/olivie/emissions/ECLIPSE-V5/CLE/1990-2050/version2014-05-26 ./linkToInDirectory/
inDir="linkToInDirectory"
outDir="/disk1/alfg/noresm2emis"

rm $outDir/*.nc

####################################################################
#SO2
infile="ECLIPSE-V5_emissions_SO2_surface_CLE_1990-2050_1.9x2.5.nc"
infileFullPath="$inDir/$infile"

outfile_SO2=$outDir/`basename $infile .nc`_SO2G.nc
outfile_SO4=$outDir/`basename $infile .nc`_SO4PR.nc

#Multiply to get primary SO4
cdo mulc,0.025 $infileFullPath $outfile_SO4
cdo mulc,0.975 $infileFullPath $outfile_SO2

#Set back the date (since it was also multiplied by factors)
ncks -O -a -x -v date $outfile_SO4 $outfile_SO4 #take it away
ncks -A -v date $infileFullPath $outfile_SO4    #put in original one

ncks -O -a -x -v date $outfile_SO2 $outfile_SO2 #take it away
ncks -A -v date $infileFullPath $outfile_SO2    #put in a new one

#default is cycle-year = 0, so create files for this!
ncap2 -O -s "date=date-19850000" $outfile_SO2 $outfile_SO2 #make sure year zero exist
ncap2 -O -s "date=date-19850000" $outfile_SO4 $outfile_SO4 #make sure year zero exist
########################################################
#OC
infile1="ECLIPSE-V5_emissions_OC1_surface_CLE_1990-2050_1.9x2.5.nc"
infile2="ECLIPSE-V5_emissions_OC2_surface_CLE_1990-2050_1.9x2.5.nc"

infileFullPath1="$inDir/$infile1"
infileFullPath2="$inDir/$infile2"

outfile=$outDir/`basename $infile1 .nc`_OCTOT.nc

#add the files
cdo add $infileFullPath1 $infileFullPath2 $outfile

#convert oc ==> om (make sure consistent with molecular weight)
#NOT DONE YET

#Set back the date (since it was also added)
ncks -O -a -x -v date $outfile $outfile #take it away
ncks -A -v date $infileFullPath1 $outfile    #put in original one

#default is cycle-year = 0, so create files for this!
ncap2 -O -s "date=date-19850000" $outfile $outfile #make sure year zero exist

######################################################
#BC

infile1="ECLIPSE-V5_emissions_CB1_surface_CLE_1990-2050_1.9x2.5.nc"
infile2="ECLIPSE-V5_emissions_CB2_surface_CLE_1990-2050_1.9x2.5.nc"

infileFullPath1="$inDir/$infile1"
infileFullPath2="$inDir/$infile2"

outfile_tmp=$outDir/`basename $infile1 .nc`_BCTOT.nc

outfile_ff=$outDir/`basename $infile1 .nc`_BCFF.nc
outfile_bb=$outDir/`basename $infile1 .nc`_BCBB.nc

outfile_ff_x=$outDir/`basename $infile1 .nc`_BCFFX.nc
outfile_ff_n=$outDir/`basename $infile1 .nc`_BCFFN.nc

#add the files
cdo add $infileFullPath1 $infileFullPath2 $outfile_tmp

#pick out the biomass related fields
ncks -v deforestation,forest,peat,savanna,woodland,awb $outfile_tmp $outfile_bb
ncks -v dom,ene,ind,tra,wst,ship $outfile_tmp $outfile_ff

#10 % of ff goes to ax-mode, 90 % of ff goes to "n" mode
cdo mulc,0.9 $outfile_ff $outfile_ff_n
cdo mulc,0.1 $outfile_ff $outfile_ff_x

#insert the dates in the ff-files
ncks -A -v date $infileFullPath1 $outfile_ff_n   #put in original one
ncks -A -v date $infileFullPath1 $outfile_ff_x   #put in original one
ncks -A -v date $infileFullPath1 $outfile_bb    #put in original one


#default is cycle-year = 0, so create files for this!
ncap2 -O -s "date=date-19850000" $outfile_ff_n $outfile_ff_n #make sure year zero exist
ncap2 -O -s "date=date-19850000" $outfile_ff_x $outfile_ff_x #make sure year zero exist
ncap2 -O -s "date=date-19850000" $outfile_bb $outfile_bb #make sure year zero exist

rm -f $outfile_tmp
####################################################
