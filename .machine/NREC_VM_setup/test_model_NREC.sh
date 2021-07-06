# Download input data
#mkdir -p ~/inputdata
cd ${CESMDATAROOT}
wget https://ns2806k.webs.sigma2.no/EMERALD/EMERALD_platform/inputdata_fates_platform/inputdata_version2.0.0_ALP1.tar \
    ${CESMDATAROOT}
tar xvf inputdata_version2.0.0_ALP1.tar && rm inputdata_version2.0.0_ALP1.tar

#Download inputdata and add the missing dataset
#Hui: This part needs to be improved. Current inputdata and its folder structure needs to be revised
#cd ${CESMDATAROOT}
#wget https://ns2806k.webs.sigma2.no/EMERALD/EMERALD_platform/inputdata_fates_platform/inputdata_version2.0.0_ALP1.tar
#tar xvf inputdata_version2.0.0_ALP1.tar
cd ${CESMDATAROOT}/inputdata/lnd/clm2/paramdata/
svn export https://svn-ccsm-inputdata.cgd.ucar.edu/trunk/inputdata/lnd/clm2/paramdata/clm50_params.c210208.nc
cd ${CESMDATAROOT}/inputdata/atm/cam/chem/trop_mozart/emis/
svn export https://svn-ccsm-inputdata.cgd.ucar.edu/trunk/inputdata/atm/cam/chem/trop_mozart/emis/megan21_emis_factors_78pft_c20161108.nc


# Validate XMLs
xmllint --noout --schema ~/NorESM_LandSites_Platform/noresm2/cime/config/xml_schemas/config_machines.xsd \
    $HOME/.cime/config_machines.xml
xmllint --noout --schema ~/NorESM_LandSites_Platform/noresm2/cime/config/xml_schemas/config_compilers_v2.xsd \
    $HOME/.cime/config_compilers.xml

# Env
sudo ldconfig /usr/local/lib

# Create case
cd ~/NorESM_LandSites_Platform/noresm2/cime/scripts/
./create_newcase --case ~/ctsm_cases/test_sp \
    --compset 2000_DATM%1PTGSWP3_CLM50%SP_SICE_SOCN_MOSART_SGLC_SWAV \
    --res 1x1_ALP1 --machine container-nlp --run-unsupported

# Setup case
cd ~/ctsm_cases/test_sp
python2 case.setup

# Force cold start
./xmlchange CLM_FORCE_COLDSTART="on"

# Build case
python2 case.build

# Run case
python2 case.submit --no-batch
