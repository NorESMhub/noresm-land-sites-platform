
#!/bin/bash

#LL=29
LL=31
#MODELNAME=CAM53-Oslo
#MODELNAME=CAM6-Oslo
MODELNAME=NorESM2

INPUTDIRECTORY=/projects/NS9560K/noresm/cases/NFHISTnorpddmsbc_f19_mg17_20191025/

AVAILABLEYEARS=(2010)
#AVAILABLEYEARS=(2005 2006 2007 2008 2009 2010 2011 2012 2013 2014)
#AVAILABLEYEARS=(0003 0004 0005 0006 0007 0008 0009 0010 0011 0012 0013 0014 0015 0016 0017 0018 0019 0020 0021 0022 0023 0024 0025 0026 0027 0028 0029 0030)
AVAILABLEMONTHS=(01 02 03 04 05 06 07 08 09 10 11 12)

EXPERIMENTNAME=NFHISTnorpddmsbc_f19_mg17_20191025

FULLEXPERIMENTNAME=NorESM2-met2010_AP3-CTRL

OUTPUTDIRECTORY=/scratch/kirkevag/NFHISTnorpddmsbc_f19_mg17_20191025/

coordinateType="" #Just for initialization , leave this
#PERIOD=9999
#PERIOD=1850
PERIOD=2010
FREQUENCY="monthly"
#converts from sulfuric acid (H2SO4) to SO4 (96/98 MW)
SF1="0.9796f"
#converts from ammonium sulfate (NH4_2SO4) to SO4 (96/134 MW)
SF2="0.7273f"
#mass fraction of DST_A3 for d>10 um (from AeroTab, assuming no growth))
F10DSTA3="0.23f"
#mass fraction of SS_A3 for d>10 um (from AeroTab, assuming no growth))
F10SSA3="0.008f"
#Rair
rair="287.0f"

#Function to get Level coordinate
#This is just needed since I think is cleaner to use
#abbreviations for this in the variable-array below
function getVariableCoordinateString()
{
	local __input=$1
	if [ $__input = "M" ];then
		coordinateType="ModelLevel"
	elif [ $__input = "S" ];then 
		coordinateType="Surface"
	elif [ $__input = "C" ];then 
		coordinateType="Column"
	elif [ $__input = "SS" ];then 
		coordinateType="SurfaceAtStations"
	elif [ $__input = "MS" ];then 
		coordinateType="ModelLevelAtStations"
	else
		coordinateType="INVALIDCOORDINATETYPE"
	fi
}

#Create output directory if it does not exist
if [ ! -d "$OUTPUTDIRECTORY" ];then
	echo CREATING OUTPUT DIR $OUTPUTDIRECTORY
	mkdir -p $OUTPUTDIRECTORY
fi

#ARRAY HAS SYNTAX: 
#AEROCOMNAME&CAMOSLONAME(OR FORMULA)&UNIT&CoordinateType
ARRAY=("area&GRIDAREA&m2&S"
	"landf&LANDFRAC&1&S"
	"ps&PS&Pa&S"
   "od550csaer&CDOD550/(CLDFREE+1.e-4f)&1&C"
   "od440csaer&CDOD440/(CLDFREE+1.e-4f)&1&C"
   "od870csaer&CDOD870/(CLDFREE+1.e-4f)&1&C"
	"od550aer&DOD550&1&C"
	"od440aer&DOD440&1&C"
	"od500aer&DOD500&1&C"
	"od670aer&DOD670&1&C"
	"od870aer&DOD870&1&C"
	"abs440aer&ABS440&1&C"
	"abs500aer&ABS500&1&C"
	"abs670aer&ABS670&1&C"
	"abs870aer&ABS870&1&C"
	"abs550aer&ABS550AL&1&C"
	"abs550bc&A550_BC&1&C"
	"abs550dust&A550_DU&1&C"
	"abs550oa&A550_POM&1&C"
	"abs550so4&A550_SO4&1&C"
	"abs550ss&A550_SS&1&C"
	"od550so4&D550_SO4&1&C"
	"od550bc&D550_BC&1&C"
	"od550oa&D550_POM&1&C"
	"od550ss&D550_SS&1&C"
	"od550dust&D550_DU&1&C"
	"od550lt1dust&DLT_DUST&1&C"
	"abs550aercs&CABS550/(CLDFREE+1.e-4f)&1&C"
	"od550lt1aer&DLT_SS+DLT_DUST+DLT_SO4+DLT_BC+DLT_POM&1&C"
	"od550gt1aer&DOD550-DLT_SS-DLT_DUST-DLT_SO4-DLT_BC-DLT_POM&1&C"
	"od550aerh2o&DOD550-OD550DRY&1&C"
	"emiso2&SFSO2+SO2_CMXF&kg m-2 s-1&S"
	"emidms&SFDMS&kg m-2 s-1&S"
	"emidust&SFDST_A2+SFDST_A3&kg m-2 s-1&S"
	"emiss&SFSS_A1+SFSS_A2+SFSS_A3&kg m-2 s-1&S"
	"emibc&SFBC_A+SFBC_AX+SFBC_AC+SFBC_N+SFBC_NI+SFBC_AI+BC_N_CMXF+BC_NI_CMXF+BC_AX_CMXF&kg m-2 s-1&S"
	"emioa&SFOM_AC+SFOM_AI+SFOM_NI+OM_NI_CMXF&kg m-2 s-1&S"
	"emiso4&$SF1*(SFSO4_A1+${SF2}/${SF1}*SFSO4_A2+SFSO4_AC+SFSO4_NA+SFSO4_PR+SO4_PR_CMXF)&kg m-2 s-1&S"
	"chepso2&(GS_SO2-SO2_CMXF)-GS_H2SO4&kg m-2 s-1&S"
	"chegpso4&$SF1*GS_H2SO4&kg m-2 s-1&S"
	"cheaqpso4&$SF2*AQ_SO4_A2_OCW&kg m-2 s-1&S"
	"wetdms&WD_A_DMS&kg m-2 s-1&S"
	"wetso2&WD_A_SO2&kg m-2 s-1&S"
	"wetoa&-1.0f*(OM_ACSFWET+OM_AISFWET+OM_NISFWET+SOA_A1SFWET+SOA_NASFWET+OM_AC_OCWSFWET+OM_AI_OCWSFWET+OM_NI_OCWSFWET+SOA_NA_OCWSFWET+SOA_A1_OCWSFWET)&kg m-2 s-1&S"
	"wetbc&-1.0f*(BC_ASFWET+BC_AISFWET+BC_AXSFWET+BC_ACSFWET+BC_NSFWET+BC_NISFWET+BC_A_OCWSFWET+BC_AI_OCWSFWET+BC_AC_OCWSFWET+BC_N_OCWSFWET+BC_NI_OCWSFWET)&kg m-2 s-1&S"
	"wetdust&-1.0f*(DST_A2SFWET+DST_A3SFWET+DST_A2_OCWSFWET+DST_A3_OCWSFWET)&kg m-2 s-1&S"
	"wetss&-1.0*(SS_A1SFWET+SS_A2SFWET+SS_A3SFWET+SS_A1_OCWSFWET+SS_A2_OCWSFWET+SS_A3_OCWSFWET)&kg m-2 s-1&S"
	"wetso4&-1.0f*$SF1*(SO4_ACSFWET+SO4_A1SFWET+${SF2}/${SF1}*SO4_A2SFWET+SO4_PRSFWET+SO4_NASFWET+SO4_AC_OCWSFWET+SO4_A1_OCWSFWET+${SF2}/${SF1}*SO4_A2_OCWSFWET+SO4_PR_OCWSFWET+SO4_NA_OCWSFWET)&kg m-2 s-1&S"
	"dryoa&-1.0*(OM_ACDDF+OM_NIDDF+OM_AIDDF+SOA_NADDF+SOA_A1DDF+OM_AC_OCWDDF+OM_NI_OCWDDF+OM_AI_OCWDDF+SOA_NA_OCWDDF+SOA_A1_OCWDDF)&kg m-2 s-1&S"
	"drybc&-1.0f*(BC_ADDF+BC_AIDDF+BC_ACDDF+BC_AXDDF+BC_NDDF+BC_NIDDF+BC_A_OCWDDF+BC_AI_OCWDDF+BC_AC_OCWDDF+BC_N_OCWDDF+BC_NI_OCWDDF)&kg m-2 s-1&S"
	"dryss&-1.0f*(SS_A1DDF+SS_A2DDF+SS_A3DDF+SS_A1_OCWDDF+SS_A2_OCWDDF+SS_A3_OCWDDF)&kg m-2 s-1&S"
	"drydust&-1.0f*(DST_A2DDF+DST_A3DDF+DST_A2_OCWDDF+DST_A3_OCWDDF)&kg m-2 s-1&S"
	"dryso4&-1.0f*$SF1*(SO4_A1DDF+${SF2}/${SF1}*SO4_A2DDF+SO4_NADDF+SO4_PRDDF+SO4_ACDDF+SO4_A1_OCWDDF+${SF2}/${SF1}*SO4_A2_OCWDDF+SO4_NA_OCWDDF+SO4_PR_OCWDDF+SO4_AC_OCWDDF)&kg m-2 s-1&S"
	"dryso2&DF_SO2&kg m-2 s-1&S"
	"drydms&DF_SO2*0.0f&kg m-2 s-1&S"
	"loadoa&cb_OM+cb_OM_NI_OCW+cb_OM_AI_OCW+cb_OM_AC_OCW+cb_SOA_NA_OCW+cb_SOA_A1_OCW&kg m-2&C"
	"loadbc&cb_BC+cb_BC_NI_OCW+cb_BC_N_OCW+cb_BC_A_OCW+cb_BC_AI_OCW+cb_BC_AC_OCW&kg m-2&C"
	"loadss&cb_SALT+cb_SS_A1_OCW+cb_SS_A2_OCW+cb_SS_A3_OCW&kg m-2&C"
	"loaddust&cb_DUST+cb_DST_A2_OCW+cb_DST_A3_OCW&kg m-2&C"
	"loadso2&cb_SO2&kg m-2&C"
	"loadso4&$SF1*(cb_SO4_A1+${SF2}/${SF1}*cb_SO4_A2+cb_SO4_NA+cb_SO4_PR+cb_SO4_AC+cb_SO4_A1_OCW+${SF2}/${SF1}*cb_SO4_A2_OCW+cb_SO4_AC_OCW+cb_SO4_NA_OCW+cb_SO4_PR_OCW)&kg m-2&C"
	"loaddms&cb_DMS&kg m-2&C"
	"clt&CLDTOT&1&C"
	"rsdt&SOLIN&W m-2&S"
	"rsds&FSDS&W m-2&S"
	"rsut&FSUTOA&W m-2&S"
	"rsus&FSDS-FSNS&W m-2&S"
	"rsdscs&FSDSC&W m-2&S"
	"rlutcs&FLUTC&W m-2&C"
	"rlut&FLUT&W m-2&C"
	"rlds&FLDS&W m-2&C"
	"rlus&FLDS-FLNS&W m-2&C"
	"orog&PHIS/9.81f&m&S"
	"precip&(PRECC+PRECL)*1.e3f&kg m-2 s-1&M"
	"temp&T&K&M"
	"hus&Q&K&M"
	"airmass&AIRMASS&kg m-2&M"
	"ec550aer&EC550AER&m-1&M"
	"ec550dryaer&ECDRYAER&m-1&M"
	"abs550dryaer&ABSDRYAE&m-1&M"
	"absc550aer&ABS550_A&m-1&M"
	"bc5503Daer&BS550AER&m-1 sr-1&M"
	"cl3D&CLOUD&1&M"
	"asy3Daer&ASYMMVIS&1&M"
	"mmraerh2o&MMR_AH2O&kg kg-1&M"
	"vmrso2&SO2&m3 m-3&M"
	"vmrdms&DMS&m3 m-3&M"
	"mmrso4&$SF1*(SO4_A1+${SF2}/${SF1}*SO4_A2+SO4_AC+SO4_NA+SO4_PR+SO4_A1_OCW+${SF2}/${SF1}*SO4_A2_OCW+SO4_AC_OCW+SO4_NA_OCW+SO4_PR_OCW)&kg kg-1&M"
	"mmroa&OM_AC+OM_AI+OM_NI+SOA_NA+SOA_A1+OM_AC_OCW+OM_AI_OCW+OM_NI_OCW+SOA_NA_OCW+SOA_A1_OCW&kg kg-1&M"
	"mmrbc&BC_A+BC_AC+BC_AX+BC_N+BC_NI+BC_AI+BC_A_OCW+BC_AC_OCW+BC_N_OCW+BC_NI_OCW+BC_AI_OCW&kg kg-1&M"
	"mmrss&SS_A1+SS_A2+SS_A3+SS_A1_OCW+SS_A2_OCW+SS_A3_OCW&kg kg-1&M"
	"mmrdu&DST_A2+DST_A3+DST_A2_OCW+DST_A3_OCW&kg kg-1&M"
	"pressure[time,lev,lat,lon]&float(P0*hyam+PS*hybm)&Pa&M"
	"rho[time,lev,lat,lon]&float(P0*hyam+PS*hybm)/(${rair}*T(:,:,:,:))&kg m-3&M"
	"sconcso4[time,lat,lon]&(PS(:,:,:)/287.0f/TS(:,:,:))*$SF1*(SO4_A1(:,$LL,:,:)+${SF2}/${SF1}*SO4_A2(:,$LL,:,:)+SO4_PR(:,$LL,:,:)+SO4_NA(:,$LL,:,:)+SO4_A1_OCW(:,$LL,:,:)+${SF2}/${SF1}*SO4_A2_OCW(:,$LL,:,:)+SO4_PR_OCW(:,$LL,:,:)+SO4_NA_OCW(:,$LL,:,:))*1.e9f&ug m-3&S"
	"sconcso2[time,lat,lon]&(PS(:,:,:)/287.0f/TS(:,:,:))*SO2(:,$LL,:,:)*1.e9f*64.066f/28.97f&ug m-3&S"
	"sconcdms[time,lat,lon]&(PS(:,:,:)/287.0f/TS(:,:,:))*DMS(:,$LL,:,:)*1.e9f*62.13f/28.97f&ug m-3&S"
	"sconcss[time,lat,lon]&(PS(:,:,:)/287.0f/TS(:,:,:))*(SS_A1(:,$LL,:,:)+SS_A2(:,$LL,:,:)+SS_A3(:,$LL,:,:)+SS_A1_OCW(:,$LL,:,:)+SS_A2_OCW(:,$LL,:,:)+SS_A3_OCW(:,$LL,:,:))*1.e9f&ug m-3&S"
	"sconcdust[time,lat,lon]&(PS(:,:,:)/287.0f/TS(:,:,:))*(DST_A2(:,$LL,:,:)+DST_A3(:,$LL,:,:)+DST_A2_OCW(:,$LL,:,:)+DST_A3_OCW(:,$LL,:,:))*1.e9f&ug m-3&S"
	"sconcbc[time,lat,lon]&(PS(:,:,:)/287.0f/TS(:,:,:))*(BC_A(:,$LL,:,:)+BC_AC(:,$LL,:,:)+BC_AX(:,$LL,:,:)+BC_N(:,$LL,:,:)+BC_NI(:,$LL,:,:)+BC_AI(:,$LL,:,:)+BC_A_OCW(:,$LL,:,:)+BC_AC_OCW(:,$LL,:,:)+BC_N_OCW(:,$LL,:,:)+BC_NI_OCW(:,$LL,:,:)+BC_AI_OCW(:,$LL,:,:))*1.e9f&ug m-3&S"
	"sconcoa[time,lat,lon]&(PS(:,:,:)/287.0f/TS(:,:,:))*(OM_AC(:,$LL,:,:)+OM_AI(:,$LL,:,:) +OM_NI(:,$LL,:,:)+SOA_NA(:,$LL,:,:)+SOA_A1(:,$LL,:,:)+OM_AC_OCW(:,$LL,:,:)+OM_AI_OCW(:,$LL,:,:)+OM_NI_OCW(:,$LL,:,:)+SOA_NA_OCW(:,$LL,:,:)+SOA_A1_OCW(:,$LL,:,:))*1.e9f&ug m-3&S"
	"sconcpm25&PM25&ug m-3&S"
        "sconcpm10[time,lat,lon]&PMTOT(:,:,:)-PS(:,:,:)/287.0f/TS(:,:,:)*1.e9f*(${F10DSTA3}*DST_A3(:,$LL,:,:)+${F10SSA3}*SS_A3(:,$LL,:,:))&ug m-3&S"
	"sconcpm10by20[time,lat,lon]&(PMTOT(:,:,:)-PS(:,:,:)/287.0f/TS(:,:,:)*1.e9f*(${F10DSTA3}*DST_A3(:,$LL,:,:)+${F10SSA3}*SS_A3(:,$LL,:,:)))/(PMTOT(:,:,:))&1&S"
      )


#For each month ==> do an ensemble average of the variables in question
for aMonth in ${AVAILABLEMONTHS[@]};do
	fileList=""
	for aYear in ${AVAILABLEYEARS[@]}; do
		echo $aYear $aMonth
   		fileList+="$INPUTDIRECTORY/atm/hist/$EXPERIMENTNAME.cam.h0.$aYear-$aMonth.nc"
		fileList+=" "
		#echo $fileList
	done
	echo Will perform ncea for month $aMonth $fileList $OUTPUTDIRECTORY/ENSEMBLE_AVG_${EXPERIMENTNAME}_$aMonth.nc
   	ncea -O $fileList $OUTPUTDIRECTORY/ENSEMBLE_AVG_${EXPERIMENTNAME}_$aMonth.nc
#	echo Convert from nc4 to nc3 to enable the final file operations below 
 	ncks -6 $OUTPUTDIRECTORY/ENSEMBLE_AVG_${EXPERIMENTNAME}_$aMonth.nc $OUTPUTDIRECTORY/ENSEMBLE_AVGnc3_${EXPERIMENTNAME}_$aMonth.nc
	echo $fileList
done


#Now that we have an enesmble for each month ==> merge them with ncrcat!
TMPOUTPUTFILE=$OUTPUTDIRECTORY/${EXPERIMENTNAME}_MONTHLY.nc
    ncrcat -O $OUTPUTDIRECTORY/ENSEMBLE_AVGnc3_${EXPERIMENTNAME}_*.nc $TMPOUTPUTFILE 

#The rest of the script operates on the ensemble average file

#Go through all entries and apply formulas as specified by table
for entry in "${ARRAY[@]}"; do
	#VARIABLE TO CREATE
	KEY=${entry%%&*}     #This is Aerocom-name (including dimensions if needed by ncap2)
	UNITKEY=${KEY%%[*}   #This is Aerocom-name (skip anything following "[")
   TMPVALUE=${entry#*&} #This is the formula to be used (AND UNITS AND COORDINATES)
	#VALUE TO EXTRACT
	VALUE=${TMPVALUE%%&*} #This is the formula to be used (REMOVED UNITS)
	#UNIT OF OUTPUT VALUE
	TMPUNIT=${TMPVALUE#*&}   #These are the units AND MODEL COORDINATE

	UNIT=${TMPUNIT%%&*}
	TMPCOORD=${TMPUNIT#*&}

	getVariableCoordinateString $TMPCOORD
	
	#BASED ON VARIABLE NAME, THIS IS OUTPUT FILE
	OUTFILE=$OUTPUTDIRECTORY/aerocom3_${FULLEXPERIMENTNAME}_${UNITKEY}_${coordinateType}_${PERIOD}_${FREQUENCY}.nc
	echo "...extracting ${KEY} using formula " $KEY "=" $VALUE "  , unit= " $UNIT
	ncap2 -O -v -s "$KEY=$VALUE" $TMPOUTPUTFILE $OUTFILE
	#Add some more variables to final output file
	ncks -A -v gw,time_bnds $TMPOUTPUTFILE $OUTFILE
	#Make sure unit is correct
	#echo "...changing unit for ${UNITKEY} "
	ncatted -O -a units,${UNITKEY},o,c,"${UNIT}" ${OUTFILE}
done

#Clean up and make sure anyone can read this..
chmod -R a+r $OUTPUTDIRECTORY
rm $OUTPUTDIRECTORY/ENSEMBLE_AVG_${EXPERIMENTNAME}_??.nc

exit
