#!/bin/bash
#Runs with CAM-Oslo but missing output from MG2_0
#MG10InputFile="/lustre/storeB/users/alfg/CESM1_5/MGRuns/MG10PDb2d4Free/atm/hist/MG10PDb2d4Free_YA_0003_0004.nc"
#MG15InputFile="/lustre/storeB/users/alfg/CESM1_5/MGRuns/MG15PDb2d4Free/atm/hist/MG15PDb2d4Free_YA_0003_0004.nc"
#MG20InputFile="/lustre/storeB/users/alfg/CESM1_5/MGRuns/MG2PDb2d4Free2/atm/hist/MG2PDb2d4Free2_YA_0003_0004.nc"

#Runs with CAM (alpha06_cntrlexp07) but with Oslo branch (bug with uninitialized ndep)
MG10InputFile="/lustre/storeB/users/alfg/CESM1_5/MGRuns/FC5MG1001595/atm/hist/FC5MG1001595_YA_0003_0004_depfix.nc"
MG15InputFile="/lustre/storeB/users/alfg/CESM1_5/MGRuns/FC5MG1501595/atm/hist/FC5MG150159_YA_0003_0004.nc"
MG20InputFile="/lustre/storeB/users/alfg/CESM1_5/MGRuns/FC55CL01595/atm/hist/FC55CL01595_YA_0003_0004.nc"

projection="global"
#You have to know which ice nucleation scheme you used!
mg10icenucleation="cam53"
mg15icenucleation="cam53"
mg20icenucleation="classnuc"

python mg_budget_plot.py --icenucleation=$mg10icenucleation --inputFile=$MG10InputFile --projection=$projection --version="MG10" --plottype="normal" --state="cldliq" --budget="number" --outputFile="CLDLIQ_NTOT_MG10.png"
python mg_budget_plot.py --icenucleation=$mg10icenucleation --inputFile=$MG10InputFile --projection=$projection --version="MG10" --plottype="normal" --state="cldliq" --budget="mass" --outputFile="CLDLIQ_MTOT_MG10.png"
python mg_budget_plot.py --icenucleation=$mg10icenucleation --inputFile=$MG10InputFile --projection=$projection --version="MG10" --plottype="normal" --state="cldice" --budget="mass" --outputFile="CLDICE_MTOT_MG10.png"
python mg_budget_plot.py --icenucleation=$mg10icenucleation --inputFile=$MG10InputFile --projection=$projection --version="MG10" --plottype="normal" --state="cldice" --budget="number" --outputFile="CLDICE_NTOT_MG10.png"

python mg_budget_plot.py --icenucleation=$mg15icenucleation --inputFile=$MG15InputFile --projection=$projection --version="MG15" --plottype="normal" --state="cldliq" --budget="number" --outputFile="CLDLIQ_NTOT_MG15.png"
python mg_budget_plot.py --icenucleation=$mg15icenucleation --inputFile=$MG15InputFile --projection=$projection --version="MG15" --plottype="normal" --state="cldliq" --budget="mass" --outputFile="CLDLIQ_MTOT_MG15.png"
python mg_budget_plot.py --icenucleation=$mg15icenucleation --inputFile=$MG15InputFile --projection=$projection --version="MG15" --plottype="normal" --state="cldice" --budget="mass" --outputFile="CLDICE_MTOT_MG15.png"
python mg_budget_plot.py --icenucleation=$mg15icenucleation --inputFile=$MG15InputFile --projection=$projection --version="MG15" --plottype="normal" --state="cldice" --budget="number" --outputFile="CLDICE_NTOT_MG15.png"

python mg_budget_plot.py --icenucleation=$mg20icenucleation --inputFile=$MG20InputFile --projection=$projection --version="MG20" --plottype="normal" --state="cldliq" --budget="number" --outputFile="CLDLIQ_NTOT_MG20.png"
python mg_budget_plot.py --icenucleation=$mg20icenucleation --inputFile=$MG20InputFile --projection=$projection --version="MG20" --plottype="normal" --state="cldliq" --budget="mass" --outputFile="CLDLIQ_MTOT_MG20.png"
python mg_budget_plot.py --icenucleation=$mg20icenucleation --inputFile=$MG20InputFile --projection=$projection --version="MG20" --plottype="normal" --state="cldice" --budget="mass" --outputFile="CLDICE_MTOT_MG20.png"
python mg_budget_plot.py --icenucleation=$mg20icenucleation --inputFile=$MG20InputFile --projection=$projection --version="MG20" --plottype="normal" --state="cldice" --budget="number" --outputFile="CLDICE_NTOT_MG20.png"
