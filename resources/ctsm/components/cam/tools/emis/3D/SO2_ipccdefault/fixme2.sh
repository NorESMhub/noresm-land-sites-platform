#!/bin/sh

#PURPOSE: CONVERT INPUT FILES TO UNITS "molec/cm^2" OR "molec/cm^3" AND
#THEN RUN THE MERGER-PROGRAM WHICH CREATES ONE OUTPUT FILE PER COMPONENT

#NOTE: CHECK CONSISTENCY WITH SO2_ipcc.xml

tmpDir=/disk1/alfg/noresm2emis/tmp

echo $tmpDir
rm $tmpDir/*.nc

#1) First create volcano 3D-file which has "height" as vertical coordinates using "cdo ml2hlx"
#==============================================================================
#Some variables in this file have different horizontal coordinates and this stops cdo ml2hlx from working
ncks -v SO2,lev,PS,hyai,hybi,hyam,hybm ECLIPSE-V5_volc_SO2_0.975_1990-2050_vertical_xxxx.nc $tmpDir/tmp1.nc
#cdo ml2hlx needs surface pressure to be called "aps" 
ncrename -v PS,aps $tmpDir/tmp1.nc $tmpDir/tmp2.nc
#transfrom from hybrid to height vertical coordinates Note need consistenecy with input levels in xml-file
cdo ml2hlx,100,500,1000,5000,10000,20000 $tmpDir/tmp2.nc $tmpDir/ECLIPSE_VOLC_1985_2050_vertical_HEIGHT.nc
#Remove tmp-files
rm $tmpDir/tmp?.nc
#The volcano emissions are already in molec/cm3 so OK no unit conversion needed :-)

#2) #Then, copy ship emissions to that same directory
#=================================================================
#These emissions are in kg/m2 so need to transform to "molec/cm2"
cp /disk1/alfg/linkToNorstoreEmissions/cmipemis/*SO2*ship*_2000_*.nc $tmpDir/tmp1.nc
cdo expr,"emiss_shp=emiss_shp*6.02e23/64.0e-3/1.0e4" $tmpDir/tmp1.nc $tmpDir/tmp2.nc #converted units
ncatted -a units,emiss_shp,o,c,molec/cm2/s $tmpDir/tmp2.nc $tmpDir/SO2_ship_2000.nc
rm $tmpDir/tmp?.nc

#3) Run the converter / merger script
#================================================================
export PYTHONPATH=$PYTHONPATH:/home/alfg/workspace/camOsloInputGenerator
python /home/alfg/workspace/camOsloInputGenerator/main.py -x SO2_3D.xml -r /home/alfg/workspace/camOsloInputGenerator/camRegularGrid144x72

