#!/bin/sh

tmpfile1=tmp1.nc
tmpfile2=tmp2.nc

input=${1:-input.nc}
output=${2:-outFractions.nc}

echo $input

#Remove all wrongly named attributes
ncatted -a ",^,d,," $input $tmpfile1

#Remove also all global attributes
ncatted -a ",global,d,," $tmpfile1 $tmpfile2

#Save the file without attributes
cp $tmpfile2 $output
rm $tmpfile1 
rm $tmpfile2

#Create a time -variable, first 
ncwa -a lat,lon,lev $output $tmpfile1

#Rename field ==> time in tmpfile1
ncrename -O -v field,time $tmpfile1

#put (append) the time variable in output file
ncks -A -v time $tmpfile1 $output

#Create a record dimension "record"
rm $tmpfile1
ncecat $output $tmpfile1

#Change record dimension to be time
rm $tmpfile2
ncpdq -a time,record $tmpfile1 $tmpfile2

#Remove the extra record dimension
rm $tmpfile1
ncwa -a record $tmpfile2 $tmpfile1

#Set time-axis
rm $tmpfile2
cdo settaxis,2000-01-15,12:00:00,1mon $tmpfile1 $tmpfile2

#Set z-levels
rm $tmpfile1
rm $output
cdo setzaxis,zaxisdef $tmpfile2 $output

#Do the sum
rm $tmpfile2
cdo vertsum $output $tmpfile2

#Rename the field 
ncrename -O -v field,fieldsum $tmpfile2
cdo expr,"fieldsum=fieldsum+1.e-10" $tmpfile2 $tmpfile1
rm $tmpfile2
mv $tmpfile1 $tmpfile2

#merge sum back
ncks -v fieldsum $tmpfile2 $tmpfile1
rm $tmpfile2
cdo merge $tmpfile1 $output $tmpfile2 

#Fix the missing values
#rm $tmpfile1
#cdo setmissval,0.0 $tmpfile2 $tmpfile1

#create fractions
mv $tmpfile2 $tmpfile1
#rm $tmpfile2
cdo expr,"fraction=field/fieldsum" $tmpfile1 $tmpfile2

#Set missing value to zero
#cdo setmisstoc,0.0 $tmpfile2 $tmpfile1

#Copy to output file
rm $output
rm $tmpfile1
mv $tmpfile2 $output
