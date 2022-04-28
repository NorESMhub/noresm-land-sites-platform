#!/bin/sh

#PURPOSE: CONVERT INPUT FILES TO UNITS "molec/cm^2" OR "molec/cm^3" AND
#THEN RUN THE MERGER-PROGRAM WHICH CREATES ONE OUTPUT FILE PER COMPONENT

#NOTE: CHECK CONSISTENCY WITH SO2_ipcc.xml

tmpDir=/disk1/alfg/noresm2emis/tmp
#conversion factor kg/m2 ==> mol/m2 ==> molec/m2 ==> molec/cm2
conversionFactor2D="*1.0/12e-3*6.02e23*1.e-4" 
#same as above but 1.e-6 factor for m3 ==> cm3
conversionFactor3D="*1.0/12e-3*6.02e23*1.e-6"

echo $tmpDir
rm $tmpDir/*.nc

echo "Getting air craft"
cdo cat /disk1/alfg/linkToNorstoreEmissions/cmipemis/IPCC*emis*BC*aircraft*0.5x0.5*.nc $tmpDir/BC_3D_air_in.nc
echo "Selecting only some years"
cdo selyear,1980,1990,2000 $tmpDir/BC_3D_air_in.nc $tmpDir/tmp.nc
echo "converting to molec/cm3"
expr="emiss_air=emiss_air"$conversionFactor3D
cmd="cdo expr,$expr $tmpDir/tmp.nc $tmpDir/BC_3D_air_moleccm3.nc"
echo using command $cmd
$cmd
rm $tmpDir/tmp.nc

################################################
echo "Getting antrhopopgenic"
cdo cat /disk1/alfg/linkToNorstoreEmissions/cmipemis/IPCC_*BC*anthr*.nc $tmpDir/BC_2D_anthr.nc
echo "Converting to molec/cm3"
expr="emiss_dom=emiss_dom"$conversionFactor2D
expr=$expr";emiss_ene=emiss_ene"$conversionFactor2D
expr=$expr";emiss_ind=emiss_ind"$conversionFactor2D
expr=$expr";emiss_tra=emiss_tra"$conversionFactor2D
expr=$expr";emiss_wst=emiss_wst"$conversionFactor2D
expr=$expr";emiss_awb=emiss_awb"$conversionFactor2D
cmd="cdo expr,$expr $tmpDir/BC_2D_anthr.nc $tmpDir/BC_2D_anthr_moleccm2.nc"
echo using command $cmd
$cmd
rm $tmpDir/BC_2D_anthr.nc

##########################################
echo "Getting ships"
cdo cat /disk1/alfg/linkToNorstoreEmissions/cmipemis/IPCC*BC*ships*.nc $tmpDir/BC_2D_ships.nc
echo "converting ships"
expr="emiss_shp=emiss_shp"$conversionFactor2D
cmd="cdo expr,$expr $tmpDir/BC_2D_ships.nc $tmpDir/BC_2D_ships_moleccm2.nc"
$cmd
echo "Fixing ship dates"
cdo shifttime,-14days $tmpDir/BC_2D_ships_moleccm2.nc $tmpDir/BC_2D_ships_moleccm2_date.nc
rm $tmpDir/BC_2D_ships.nc
rm $tmpDir/BC_2D_ships_moleccm2.nc

############################################

echo "Getting biomass burning"
cdo cat /disk1/alfg/linkToNorstoreEmissions/cmipemis/IPCC_GriddedBiomassBurningEmissions_BC*decadalmonthlymean*.nc $tmpDir/BC_2D_Biomass.nc
expr="grassfire=grassfire"$conversionFactor2D
expr=$expr";forestfire=forestfire"$conversionFactor2D
echo "converting biomass"
cmd="cdo expr,$expr $tmpDir/BC_2D_Biomass.nc $tmpDir/BC_2D_Biomass_moleccm2.nc"
$cmd
rm $tmpDir/BC_2D_Biomass.nc

####################################################

#level fraction files
echo "Getting level fractions"
cp /disk1/alfg/linkToNorstoreEmissions/cmipemis/levelFractionFiles/GFED*level*.nc $tmpDir
