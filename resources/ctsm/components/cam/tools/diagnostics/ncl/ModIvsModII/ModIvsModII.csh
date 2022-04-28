echo ' Note: on norstore cruncher, run "module load ncl" first!'
echo ''
echo 'This script calls all the *ModIvsModII.ncl scripts'
echo 'and produces plots for all available/listed plot_types'
echo 'Note: All ncl scripts assumes that the input data on'
echo 'the listed directories are on a integer number times'
echo '12 nc-files for monthly model data for model I and II,'
echo 'and that no other files on the same name form are present.' 
echo ''
echo 'If the number of years is so large that some of the'
echo 'ncl scripts run out of memory, with the error message'
echo 'systemfunc: cannot create child process:[errno=12],'
echo 'then make climatological input for each mponth in,'
echo 'advance, e.g. by use of the ncea command...'
echo ''
echo '     (Created by Alf Kirkevåg, April 2014)'
echo ''

# ncl 'dataFile=addfile("./modelData.nc", "r")' plot_type=0 Emis_ModIvsModII.ncl
#************************* To be edited by the user ********************************************
# Plot type and plot output format:
plotf=png  # chosen output format for figures (ps, eps, pdf, png)
#
# Path (pth), name (fnm), and common name stem/root (fnmp) of input files from model version I and II (PD case): 
#
#
#pthI=/projects/NS2345K/noresm/cases/divClim4aerosoldiag/altRHpiclimaer/atm_clim_yr3-30/nc3/
#fnmI=altRHpiclimaer.cam.h0.0003-0030_01.nc
#fnmpI=altRHpiclimaer.cam.h0.0003-0030_
#
#pthI_PI=/projects/NS2345K/noresm/cases/divClim4aerosoldiag/altRHpiclimctrl4/atm_clim_yr3-30/nc3/
#fnmI_PI=altRHpiclimctrl4.cam.h0.0003-0030_01_nc3.nc
#fnmpI_PI=altRHpiclimctrl4.cam.h0.0003-0030_
#
#pthI_PI=/projects/NS2345K/noresm/cases/divClim4aerosoldiag/NF1850norbc_f19_20190727/atm_clim_yr3-30/nc3/
#fnmI_PI=NF1850norbc_f19_20190727.cam.h0.0003-0030_01_nc3.nc
#fnmpI_PI=NF1850norbc_f19_20190727.cam.h0.0003-0030_
#
#pthI=/projects/NS2345K/noresm/cases/divClim4aerosoldiag/NF1850norbc_aer2014_f19_20190727/atm_clim_yr3-30/nc3/
#fnmI=NF1850norbc_aer2014_f19_20190727.cam.h0.0003-0030_01.nc
#fnmpI=NF1850norbc_aer2014_f19_20190727.cam.h0.0003-0030_
#
pthI=/projects/NS9560K/noresm/cases/piClim-lu2deg/atm_clim_yr3-30/nc3/
fnmI=piClim-lu2deg.cam.h0.0003-0030_01.nc
fnmpI=piClim-lu2deg.cam.h0.0003-0030_
#
pthI_PI=/projects/NS2345K/noresm/cases/divClim4aerosoldiag/NF1850norbc_f19_20190727/atm_clim_yr3-30/nc3/
fnmI_PI=NF1850norbc_f19_20190727.cam.h0.0003-0030_01_nc3.nc
fnmpI_PI=NF1850norbc_f19_20190727.cam.h0.0003-0030_
#
pthII=/projects/NS2345K/noresm/cases/divClim4aerosoldiag/NF1850norbc_aer2014_f19_20190727/atm_clim_yr3-30/nc3/
fnmII=NF1850norbc_aer2014_f19_20190727.cam.h0.0003-0030_01.nc
fnmpII=NF1850norbc_aer2014_f19_20190727.cam.h0.0003-0030_
#
pthII_PI=/projects/NS2345K/noresm/cases/divClim4aerosoldiag/NF1850norbc_f19_20190727/atm_clim_yr3-30/nc3/
fnmII_PI=NF1850norbc_f19_20190727.cam.h0.0003-0030_01_nc3.nc
fnmpII_PI=NF1850norbc_f19_20190727.cam.h0.0003-0030_

#
#pthII=/projects/NS2345K/noresm/cases/divClim4aerosoldiag/piClim-SpAer-anthro/atm_clim_yr3-30/nc3/
#fnmII=piClim-SpAer-anthro.cam.h0.0003-0030_01.nc
#fnmpII=piClim-SpAer-anthro.cam.h0.0003-0030_
#
#pthII_PI=/projects/NS2345K/noresm/cases/divClim4aerosoldiag/NF1850norbc_f19_20190727/atm_clim_yr3-30/nc3/
#fnmII_PI=NF1850norbc_f19_20190727.cam.h0.0003-0030_01_nc3.nc
#fnmpI_PI=NF1850norbc_f19_20190727.cam.h0.0003-0030_

#
#pthII=/projects/NS2345K/noresm/cases/divClim4aerosoldiag/NHIST_spAer_f19_tn14_20190925/yr1965-1965/
#fnmII=ENSEMBLE_AVGnc3_yr1965-1965_NHIST_spAer_f19_tn14_20190925_01.nc
#fnmpII=ENSEMBLE_AVGnc3_yr1965-1965_NHIST_spAer_f19_tn14_20190925_
#pthII=/projects/NS2345K/noresm/cases/divClim4aerosoldiag/NHIST_spAer_f19_tn14_20190925/yr2005-2005/
#fnmII=ENSEMBLE_AVGnc3_yr2005-2005_NHIST_spAer_f19_tn14_20190925_01.nc
#fnmpII=ENSEMBLE_AVGnc3_yr2005-2005_NHIST_spAer_f19_tn14_20190925_
#
#pthII_PI=/projects/NS9560K/noresm/cases/NHISTpiaer_f19_tn14_20190721/atm/hist/
#fnmII_PI=NHISTpiaer_f19_tn14_20190721.cam.h0.1850-01.nc
#fnmpII_PI=NHISTpiaer_f19_tn14_20190721.cam.h0.1850
#




#pthII_PI=/projects/NS2345K/noresm/cases/divClim4aerosoldiag/NF1850norbc_f19_20190727/atm_clim_yr3-30/nc3/
#fnmII_PI=NF1850norbc_f19_20190727.cam.h0.0003-0030_01_nc3.nc
#fnmpII_PI=NF1850norbc_f19_20190727.cam.h0.0003-0030_
#
#pthII=/projects/NS2345K/noresm/cases/divClim4aerosoldiag/piClim-SpAer2014/atm_clim_yr3-30/nc3/
#fnmII=piClim-SpAer2014.cam.h0.0003-0030_01.nc
#fnmpII=piClim-SpAer2014.cam.h0.0003-0030
#
#pthII_PI=/projects/NS2345K/noresm/cases/divClim4aerosoldiag/NF1850norbc_f19_20190727/atm_clim_yr3-30/nc3/
#fnmII_PI=NF1850norbc_f19_20190727.cam.h0.0003-0030_01_nc3.nc
#fnmpII_PI=NF1850norbc_f19_20190727.cam.h0.0003-0030_
#
#pthII=/projects/NS2345K/noresm/cases/NF2kSPtest-atm/NF2kSPt5fram/tst1_eq23vilje/mnd2-13/
#fnmII=NF2kSPt5fram.cam.h0.0001-02.nc
#fnmpII=NF2kSPt5fram.cam.h0.000
#
#pthII_PI=/projects/NS2345K/noresm/cases/NF2kSPtest-atm/NF2kSPt5fram/tst1_eq23vilje/mnd2-13/
#fnmII_PI=NF2kSPt5fram.cam.h0.0001-02.nc
#fnmpII_PI=NF2kSPt5fram.cam.h0.000
#
#pthII_PI=/projects/NS2345K/noresm/cases/divClim4aerosoldiag/NF1850norbc_f19_20190727/atm_clim_yr3-30/nc3/
#fnmII_PI=NF1850norbc_f19_20190727.cam.h0.0003-0030_01_nc3.nc
#fnmpII_PI=NF1850norbc_f19_20190727.cam.h0.0003-0030_
#
#pthII=/projects/NS2345K/noresm/cases/divClim4aerosoldiag/NF1850norbc_oc2014_f19_20190727/atm_clim_yr3-30/nc3/
#fnmII=NF1850norbc_oc2014_f19_20190727.cam.h0.0003-0030_01.nc
#fnmpII=NF1850norbc_oc2014_f19_20190727.cam.h0.0003-0030_
#
#pthII=/projects/NS2345K/noresm/cases/altRHpiclimctrl/atm_clim_yr5-30/
#fnmII=altRHpiclimctrl.cam.h0.0005-0030_01.nc
#fnmpII=altRHpiclimctrl.cam.h0.0005-0030_
#
#pthII_PI=/projects/NS2345K/noresm/cases/altRHpiclimctrl/dirks-atm_clim_yr5-30/nc3/
#fnmII_PI=NF1850norbc_f19_mg17_20190721.cam.h0.0005-0030_01_nc3.nc
#fnmpII_PI=NF1850norbc_f19_mg17_20190721.cam.h0.0005-0030_
#
#pthII=/projects/NS2345K/FRAM/noresm/cases/NF2kio4dmszm-atm/
#fnmII=NF2kio4dmszm.cam.h0.0002-01.nc
#fnmpII=NF2kio4dmszm.cam.h0.000
#
#pthII_PI=/projects/NS2345K/FRAM/noresm/cases/NFpiio4dmszm-atm/
#fnmII_PI=NFpiio4dmszm.cam.h0.0002-01.nc
#fnmpII_PI=NFpiio4dmszm.cam.h0.000
#
#pthII=/projects/NS2345K/FRAM/noresm/cases/NF2koptice5g288-atm/mnthclim_yr3-6nc3/
#fnmII=NF2koptice5g288.cam.h0.0003-0006_01_nc3.nc
#fnmpII=NF2koptice5g288.cam.h0.0003-0006_
#
#pthII_PI=/projects/NS2345K/FRAM/noresm/cases/NFpioptice5g288-atm/mnthclim_yr3-6nc3/
#fnmII_PI=NFpioptice5g288.cam.h0.0003-0006_01_nc3.nc
#fnmpII_PI=NFpioptice5g288.cam.h0.0003-0006_
#
#pthII=/projects/NS2345K/noresm/cases/AlfKtests/CAM6-Oslo/NF2000climoPDt/
#fnmII=NF2000climoPDt.cam.h0.0001-04.nc
#fnmpII=NF2000climoPDt.cam.h0.000
#
# Paths and names of input files necessary for forcing plots (PI case)):
#
#pthI_PI=/projects/NS2345K/noresm/cases/NF2000PIERF/mnthclim_yr3-30/
#fnmI_PI=NF2000PIERF.cam.h0.0003-0030_01.nc
#fnmpI_PI=NF2000PIERF.cam.h0.0003-0030_
#
#pthI_PI=/projects/NS2345K/noresm/cases/NF2000PIERF/2-07to3-06/
#fnmI_PI=NF2000PIERF.cam.h0.0002-07.nc
#fnmpI_PI=NF2000PIERF.cam.h0.000
#
#pthI_PI=/projects/NS2345K/noresm/cases/NF2000PIERF/mnthclim_yr3-30/
#fnmI_PI=NF2000PIERF.cam.h0.0003-0030_01.nc
#fnmpI_PI=NF2000PIERF.cam.h0.0003-0030_
#
#pthI_PI=/projects/NS2345K/noresm/cases/NF1998HygCfree/orig/
#fnmI_PI=NF1998HygCfree.cam.h0.0001-05.nc
#fnmpI_PI=NF1998HygCfree.cam.h0.000
#
#pthI_PI=/projects/NS2345K/noresm/cases/F2000ERF/
#fnmI_PI=F2000ERF.cam.h0.0001-05.nc
#fnmpI_PI=F2000ERF.cam.h0.000
#
#pthII_PI=/projects/NS2345K/noresm/cases/NF2000PIERF/mnthclim_yr3-30/
#fnmII_PI=NF2000PIERF.cam.h0.0003-0030_01.nc
#fnmpII_PI=NF2000PIERF.cam.h0.0003-0030_
#
#pthII_PI=/projects/NS2345K/noresm/cases/NF1998HygCfree/
#fnmII_PI=NF1998HygCfree.cam.h0.0001-05.nc
#fnmpII_PI=NF1998HygCfree.cam.h0.000
#
#pthII_PI=/projects/NS2345K/noresm/cases/AlfKtests/CAM6-Oslo/NF2kNucl-all/mnth5-17b4corrsconc/
#fnmII_PI=NF2kNucl.cam.h0.0001-05.nc
#fnmpII_PI=NF2kNucl.cam.h0.000
#
#
#ModelI=CAM4-Oslo  # gives CAM4-Oslo vs. new CAM5-Oslo comparison plots
ModelI=CAM5-Oslo  # gives CAM5/6-Oslo version I vs. CAM5/6-Oslo version II comparison plots
#
# Note: when using CAM5.5 or higher for ModI, include also GasdepI options, as for ModII below
GasdepI=Neu   # use diagnostics as in the Moz or in the Neu (>CAM5.3) gas deposition scheme for ModI
GasdepII=Neu  # use diagnostics as in the Moz or in the Neu (>CAM5.3) gas deposition scheme for ModII
# Note: when using 32 level version for ModI or ModII, include also the LevModI & LevModII options L32
# (this does not work if ModI is CAM4-Oslo), for use (only) in LevelCloudProp_ModIvsModII.ncl 
LevModI=L32 
LevModII=L32 
#**********************************************************************************************



# Run TMP version x instead of the above for ModII data before correction w.r.t. mmr vs. vmr in model output of individual tracers
# or the y version if the ModI data have this shortcoming, or z if both ModI and ModII do: 
 echo ''
 echo 'Running ZonalAero_ModIvsModII_TMP.ncl'
 echo ''
 for I in {0..7};do
 ncl  plot_type=$I format=\"$plotf\" filepath_I=\"$pthI\" filename_I=\"$fnmI\" filepath_II=\"$pthII\" filename_II=\"$fnmII\" filenamep_I=\"$fnmpI\" filenamep_II=\"$fnmpII\" ModI=\"$ModelI\" ZonalAero_ModIvsModII.ncl
# ncl  plot_type=$I format=\"$plotf\" filepath_I=\"$pthI\" filename_I=\"$fnmI\" filepath_II=\"$pthII\" filename_II=\"$fnmII\" filenamep_I=\"$fnmpI\" filenamep_II=\"$fnmpII\" ModI=\"$ModelI\" ZonalAero_ModIvsModII_TMPx.ncl
# ncl  plot_type=$I format=\"$plotf\" filepath_I=\"$pthI\" filename_I=\"$fnmI\" filepath_II=\"$pthII\" filename_II=\"$fnmII\" filenamep_I=\"$fnmpI\" filenamep_II=\"$fnmpII\" ModI=\"$ModelI\" ZonalAero_ModIvsModII_TMPy.ncl
# ncl  plot_type=$I format=\"$plotf\" filepath_I=\"$pthI\" filename_I=\"$fnmI\" filepath_II=\"$pthII\" filename_II=\"$fnmII\" filenamep_I=\"$fnmpI\" filenamep_II=\"$fnmpII\" ModI=\"$ModelI\" ZonalAero_ModIvsModII_TMPz.ncl
 done


#Unless you want to compare intermediate model versions with specialized script changes (#'ed out), no changes by the user should be necessary below...

echo ''
echo 'Running AODvolc-and-SP_ModIvsModII.ncl'
echo ''
for I in {1..6};do
 ncl plot_type=$I format=\"$plotf\" filepath_I=\"$pthI\" filename_I=\"$fnmI\" filepath_II=\"$pthII\" filename_II=\"$fnmII\" filenamep_I=\"$fnmpI\" filenamep_II=\"$fnmpII\" AODvolc-and-SP_ModIvsModII.ncl
done

echo ''
echo 'Running SP_ERF_ModIvsModII.ncl'
echo ''
for I in {0..5};do
ncl  plot_type=$I format=\"$plotf\" filepathPD_I=\"$pthI\" filenamePD_I=\"$fnmI\" filepathPD_II=\"$pthII\" filenamePD_II=\"$fnmII\" filenamepPD_I=\"$fnmpI\" filenamepPD_II=\"$fnmpII\" filepathPI_I=\"$pthI_PI\" filenamePI_I=\"$fnmI_PI\" filepathPI_II=\"$pthII_PI\" filenamePI_II=\"$fnmII_PI\" filenamepPI_I=\"$fnmpI_PI\" filenamepPI_II=\"$fnmpII_PI\" ModI=\"$ModelI\" SP_ERF_ModIvsModII.ncl 
done


echo ''
echo 'Running Emis_ModIvsModII.ncl'
echo ''
for I in {-1..7};do
 ncl plot_type=$I format=\"$plotf\" filepath_I=\"$pthI\" filename_I=\"$fnmI\" filepath_II=\"$pthII\" filename_II=\"$fnmII\" filenamep_I=\"$fnmpI\" filenamep_II=\"$fnmpII\" ModI=\"$ModelI\" Emis_ModIvsModII.ncl
 # ncl plot_type=$I format=\"$plotf\" filepath_I=\"$pthI\" filename_I=\"$fnmI\" filepath_II=\"$pthII\" filename_II=\"$fnmII\" filenamep_I=\"$fnmpI\" filenamep_II=\"$fnmpII\" ModI=\"$ModelI\" EmisTMP_ModIvsModII.ncl
done

echo ''
echo 'Running diffTOAbalance.ncl'
echo ''
for I in {0..9};do
ncl  plot_type=$I format=\"$plotf\" filepathPD_I=\"$pthI\" filenamePD_I=\"$fnmI\" filepathPD_II=\"$pthII\" filenamePD_II=\"$fnmII\" filenamepPD_I=\"$fnmpI\" filenamepPD_II=\"$fnmpII\" filepathPI_I=\"$pthI_PI\" filenamePI_I=\"$fnmI_PI\" filepathPI_II=\"$pthII_PI\" filenamePI_II=\"$fnmII_PI\" filenamepPI_I=\"$fnmpI_PI\" filenamepPI_II=\"$fnmpII_PI\" ModI=\"$ModelI\" diffTOAbalance.ncl 
done

echo ''
echo 'Running LevelCloudProp_ModIvsModII.ncl'
echo ''
for I in {1..13};do
 ncl plot_type=$I format=\"$plotf\" filepath_I=\"$pthI\" filename_I=\"$fnmI\" filepath_II=\"$pthII\" filename_II=\"$fnmII\" filenamep_I=\"$fnmpI\" filenamep_II=\"$fnmpII\" ModI=\"$ModelI\" LevModI=\"$LevModI\" LevModII=\"$LevModII\" LevelCloudProp_ModIvsModII.ncl
done

echo ''
echo 'Running Cld2d_ModIvsModII.ncl'
echo ''
for I in {0..9};do
 ncl plot_type=$I format=\"$plotf\" filepath_I=\"$pthI\" filename_I=\"$fnmI\" filepath_II=\"$pthII\" filename_II=\"$fnmII\" filenamep_I=\"$fnmpI\" filenamep_II=\"$fnmpII\" ModI=\"$ModelI\" Cld2d_ModIvsModII.ncl 
done

echo ''
echo 'Running Load_ModIvsModII.ncl'
echo ''
for I in {-1..19};do
 ncl plot_type=$I format=\"$plotf\" filepath_I=\"$pthI\" filename_I=\"$fnmI\" filepath_II=\"$pthII\" filename_II=\"$fnmII\" filenamep_I=\"$fnmpI\" filenamep_II=\"$fnmpII\" ModI=\"$ModelI\" Load_ModIvsModII.ncl
done

echo ''
echo 'Running Ext_ModIvsModII.ncl'
echo ''
for I in {1..20};do
 ncl plot_type=$I format=\"$plotf\" filepath_I=\"$pthI\" filename_I=\"$fnmI\" filepath_II=\"$pthII\" filename_II=\"$fnmII\" filenamep_I=\"$fnmpI\" filenamep_II=\"$fnmpII\" ModI=\"$ModelI\" Ext_ModIvsModII.ncl 
done

echo ''
echo 'Running AODratio_ModIvsModII.ncl'
echo ''
for I in {0..6};do
 ncl plot_type=$I format=\"$plotf\" filepath_I=\"$pthI\" filename_I=\"$fnmI\" filepath_II=\"$pthII\" filename_II=\"$fnmII\" filenamep_I=\"$fnmpI\" filenamep_II=\"$fnmpII\" AODratio_ModIvsModII.ncl
done

echo ''
echo 'Running AOD_ModIvsModII.ncl'
echo ''
for I in {0..8};do
 ncl plot_type=$I format=\"$plotf\" filepath_I=\"$pthI\" filename_I=\"$fnmI\" filepath_II=\"$pthII\" filename_II=\"$fnmII\" filenamep_I=\"$fnmpI\" filenamep_II=\"$fnmpII\" AOD_ModIvsModII.ncl
done

echo ''
echo 'Running ZonalRHCl_ModIvsModII.ncl'
echo ''
for I in {0..8};do
 ncl plot_type=$I format=\"$plotf\" filepath_I=\"$pthI\" filename_I=\"$fnmI\" filepath_II=\"$pthII\" filename_II=\"$fnmII\" filenamep_I=\"$fnmpI\" filenamep_II=\"$fnmpII\" ModI=\"$ModelI\" ZonalRHCl_ModIvsModII.ncl
done

echo ''
echo 'Running Lifetimes_ModIvsModII.ncl'
echo ''
for I in {0..5};do
 ncl plot_type=$I format=\"$plotf\" filepath_I=\"$pthI\" filename_I=\"$fnmI\" filepath_II=\"$pthII\" filename_II=\"$fnmII\" filenamep_I=\"$fnmpI\" filenamep_II=\"$fnmpII\" ModI=\"$ModelI\" GdepI=\"$GasdepI\" GdepII=\"$GasdepII\" Lifetimes_ModIvsModII.ncl
# ncl plot_type=$I format=\"$plotf\" filepath_I=\"$pthI\" filename_I=\"$fnmI\" filepath_II=\"$pthII\" filename_II=\"$fnmII\" filenamep_I=\"$fnmpI\" filenamep_II=\"$fnmpII\" ModI=\"$ModelI\" GdepI=\"$GasdepI\" GdepII=\"$GasdepII\" LifetimesTMP_ModIvsModII.ncl
done

echo ''
echo 'Running WetDepRat_ModIvsModII.ncl'
echo ''
for I in {0..5};do
 ncl plot_type=$I format=\"$plotf\" filepath_I=\"$pthI\" filename_I=\"$fnmI\" filepath_II=\"$pthII\" filename_II=\"$fnmII\" filenamep_I=\"$fnmpI\" filenamep_II=\"$fnmpII\" ModI=\"$ModelI\" WetDepRat_ModIvsModII.ncl
done

echo ''
echo 'Running EffDryRad_ModIvsModII.ncl'
echo ''
for I in {0..2};do
 ncl plot_type=$I format=\"$plotf\" filepath_I=\"$pthI\" filename_I=\"$fnmI\" filepath_II=\"$pthII\" filename_II=\"$fnmII\" filenamep_I=\"$fnmpI\" filenamep_II=\"$fnmpII\" EffDryRad_ModIvsModII.ncl 
done

echo ''
echo 'Running ZonalModepar_ModIvsModII.ncl'
echo ''
for I in {1..9};do
 ncl plot_type=$I format=\"$plotf\" filepath_I=\"$pthI\" filename_I=\"$fnmI\" filepath_II=\"$pthII\" filename_II=\"$fnmII\" filenamep_I=\"$fnmpI\" filenamep_II=\"$fnmpII\" ModI=\"$ModelI\" ZonalModepar_ModIvsModII.ncl
done

echo ''
echo 'Running ZonalN_ModIvsModII.ncl'
echo ''
for I in {1..13};do
 ncl plot_type=$I format=\"$plotf\" filepath_I=\"$pthI\" filename_I=\"$fnmI\" filepath_II=\"$pthII\" filename_II=\"$fnmII\" filenamep_I=\"$fnmpI\" filenamep_II=\"$fnmpII\" ModI=\"$ModelI\" ZonalN_ModIvsModII.ncl
done

echo ''
echo 'Running PM_ModIvsModII.ncl'
echo ''
for I in {1..5};do
ncl  plot_type=$I format=\"$plotf\" filepathPD_I=\"$pthI\" filenamePD_I=\"$fnmI\" filepathPD_II=\"$pthII\" filenamePD_II=\"$fnmII\" filenamepPD_I=\"$fnmpI\" filenamepPD_II=\"$fnmpII\" filepathPI_I=\"$pthI_PI\" filenamePI_I=\"$fnmI_PI\" filepathPI_II=\"$pthII_PI\" filenamePI_II=\"$fnmII_PI\" filenamepPI_I=\"$fnmpI_PI\" filenamepPI_II=\"$fnmpII_PI\" ModI=\"$ModelI\" PM_ModIvsModII.ncl 
done

echo ''
echo 'Running RadBudg_ModIvsModII.ncl'
echo ''
for I in {1..7};do
ncl  plot_type=$I format=\"$plotf\" filepathPD_I=\"$pthI\" filenamePD_I=\"$fnmI\" filepathPD_II=\"$pthII\" filenamePD_II=\"$fnmII\" filenamepPD_I=\"$fnmpI\" filenamepPD_II=\"$fnmpII\" filepathPI_I=\"$pthI_PI\" filenamePI_I=\"$fnmI_PI\" filepathPI_II=\"$pthII_PI\" filenamePI_II=\"$fnmII_PI\" filenamepPI_I=\"$fnmpI_PI\" filenamepPI_II=\"$fnmpII_PI\" ModI=\"$ModelI\" RadBudg_ModIvsModII.ncl 
done

echo ''
echo 'Running divPD-PI_ModIvsModII.ncl'
echo ''
for I in {0..14};do
ncl  plot_type=$I format=\"$plotf\" filepathPD_I=\"$pthI\" filenamePD_I=\"$fnmI\" filepathPD_II=\"$pthII\" filenamePD_II=\"$fnmII\" filenamepPD_I=\"$fnmpI\" filenamepPD_II=\"$fnmpII\" filepathPI_I=\"$pthI_PI\" filenamePI_I=\"$fnmI_PI\" filepathPI_II=\"$pthII_PI\" filenamePI_II=\"$fnmII_PI\" filenamepPI_I=\"$fnmpI_PI\" filenamepPI_II=\"$fnmpII_PI\" ModI=\"$ModelI\" divPD-PI_ModIvsModII.ncl 
done

echo ''
echo 'Running divPD-PI_Zonal_ModIvsModII.ncl'
echo ''
for I in {0..10};do
ncl  plot_type=$I format=\"$plotf\" filepathPD_I=\"$pthI\" filenamePD_I=\"$fnmI\" filepathPD_II=\"$pthII\" filenamePD_II=\"$fnmII\" filenamepPD_I=\"$fnmpI\" filenamepPD_II=\"$fnmpII\" filepathPI_I=\"$pthI_PI\" filenamePI_I=\"$fnmI_PI\" filepathPI_II=\"$pthII_PI\" filenamePI_II=\"$fnmII_PI\" filenamepPI_I=\"$fnmpI_PI\" filenamepPI_II=\"$fnmpII_PI\" ModI=\"$ModelI\" divPD-PI_Zonal_ModIvsModII.ncl 
done

echo ''
echo 'Running ERFsurf_ModIvsModII.ncl'
echo ''
for I in {0..4};do
ncl  plot_type=$I format=\"$plotf\" filepathPD_I=\"$pthI\" filenamePD_I=\"$fnmI\" filepathPD_II=\"$pthII\" filenamePD_II=\"$fnmII\" filenamepPD_I=\"$fnmpI\" filenamepPD_II=\"$fnmpII\" filepathPI_I=\"$pthI_PI\" filenamePI_I=\"$fnmI_PI\" filepathPI_II=\"$pthII_PI\" filenamePI_II=\"$fnmII_PI\" filenamepPI_I=\"$fnmpI_PI\" filenamepPI_II=\"$fnmpII_PI\" ModI=\"$ModelI\" ERFsurf_ModIvsModII.ncl 
done

echo ''
echo 'Running ERF_ModIvsModII.ncl'
echo ''
for I in {0..6};do
ncl  plot_type=$I format=\"$plotf\" filepathPD_I=\"$pthI\" filenamePD_I=\"$fnmI\" filepathPD_II=\"$pthII\" filenamePD_II=\"$fnmII\" filenamepPD_I=\"$fnmpI\" filenamepPD_II=\"$fnmpII\" filepathPI_I=\"$pthI_PI\" filenamePI_I=\"$fnmI_PI\" filepathPI_II=\"$pthII_PI\" filenamePI_II=\"$fnmII_PI\" filenamepPI_I=\"$fnmpI_PI\" filenamepPI_II=\"$fnmpII_PI\" ModI=\"$ModelI\" ERF_ModIvsModII.ncl 
done

echo ''
echo 'Running Mass-budget_ModIvsModII.ncl'
echo ''
for I in {-1..5};do
 ncl plot_type=$I format=\"$plotf\" filepath_I=\"$pthI\" filename_I=\"$fnmI\" filepath_II=\"$pthII\" filename_II=\"$fnmII\" filenamep_I=\"$fnmpI\" filenamep_II=\"$fnmpII\" ModI=\"$ModelI\" GdepI=\"$GasdepI\" GdepII=\"$GasdepII\" Mass-budget_ModIvsModII.ncl
# ncl plot_type=$I format=\"$plotf\" filepath_I=\"$pthI\" filename_I=\"$fnmI\" filepath_II=\"$pthII\" filename_II=\"$fnmII\" filenamep_I=\"$fnmpI\" filenamep_II=\"$fnmpII\" ModI=\"$ModelI\" GdepI=\"$GasdepI\" GdepII=\"$GasdepII\" Mass-budgetTMP_ModIvsModII.ncl
done

echo ''
echo 'Running Mass-budget_ModIvsModII.ncl for PI emissions'
echo ''
for I in {-1..5};do
 ncl plot_type=$I format=\"$plotf\" filepath_I=\"$pthI_PI\" filename_I=\"$fnmI_PI\" filepath_II=\"$pthII_PI\" filename_II=\"$fnmII_PI\" filenamep_I=\"$fnmpI_PI\" filenamep_II=\"$fnmpII_PI\" ModI=\"$ModelI\" GdepI=\"$GasdepI\" GdepII=\"$GasdepII\" Mass-budget_ModIvsModII.ncl
# ncl plot_type=$I format=\"$plotf\" filepath_I=\"$pthI_PI\" filename_I=\"$fnmI_PI\" filepath_II=\"$pthII_PI\" filename_II=\"$fnmII_PI\" filenamep_I=\"$fnmpI_PI\" filenamep_II=\"$fnmpII_PI\" ModI=\"$ModelI\" GdepI=\"$GasdepI\" GdepII=\"$GasdepII\" Mass-budgetTMP_ModIvsModII.ncl
done

echo ''
echo 'All ncl script runs completed'
echo ''

echo "trim whitespace in images"
for i in `ls *.png`
do
  convert -trim $i $i
done

exit


