#!/bin/sh

#Create the input directory here with
#mkdir linkToInDirectory and then
#sshfs -r alfgr@hexagon.bccs.uib.no:/work/olivie/emissions/ECLIPSE-V5/CLE/1990-2050/version2014-05-26 ./linkToInDirectory/
inDir="/disk1/alfg/camOsloEmis"
outDir="/disk1/alfg/noresm2emis"

#rm $outDir/*.nc
######################################################
#BC

infile_ff="BC_ff_3D_default.nc"
infile_bb="BC_bb_3D_default.nc"

outfile_bb=$outDir/`basename $infile_bb .nc`_BCBB.nc
outfile_ff_x=$outDir/`basename $infile_ff .nc`_BCFFX.nc
outfile_ff_n=$outDir/`basename $infile_ff .nc`_BCFFN.nc

echo $outfile_ff_x $outfile_ff_n $outfile_bb

#10 % of ff goes to ax-mode, 90 % of ff goes to "n" mode
cp $inDir/$infile_bb $outfile_bb
ncap2 -O -s "emiss_air=0.9f*emiss_air;emiss_shp=0.9f*emiss_shp"  $inDir/$infile_ff $outfile_ff_n
ncap2 -O -s "emiss_air=0.1f*emiss_air;emiss_shp=0.1f*emiss_shp"  $inDir/$infile_ff $outfile_ff_x

#default is cycle-year = 0, so create files for this!
ncap2 -O -s "date=date-19900000" $outfile_ff_n $outfile_ff_n #make sure year zero exist
ncap2 -O -s "date=date-19900000" $outfile_ff_x $outfile_ff_x #make sure year zero exist
ncap2 -O -s "date=date-19900000" $outfile_bb $outfile_bb #make sure year zero exist

####################################################
