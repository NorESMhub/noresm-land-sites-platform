# Download input data
mkdir -p ~/NorESM_LandSites_Platform/data/input
cd ~/NorESM_LandSites_Platform/data/input
wget https://ns2806k.webs.sigma2.no/EMERALD/EMERALD_platform/inputdata_fates_platform/inputdata_version2.0.0_ALP1.tar \
    ~/NorESM_LandSites_Platform/data/input/
tar xvf inputdata_version2.0.0_ALP1.tar && rm inputdata_version2.0.0_ALP1.tar

# Validate XMLs
xmllint --noout --schema ~/NorESM_LandSites_Platform/noresm2/cime/config/xml_schemas/config_machines.xsd \
    $HOME/.cime/config_machines.xml
xmllint --noout --schema ~/NorESM_LandSites_Platform/noresm2/cime/config/xml_schemas/config_compilers_v2.xsd \
    $HOME/.cime/config_compilers.xml

# Env
sudo ldconfig /usr/local/hdf5/lib /usr/local/netcdf/lib

# Create case
cd ~/NorESM_LandSites_Platform/noresm2/cime/scripts/
./create_newcase --case ~/ctsm_cases/test_sp \
    --compset 2000_DATM%1PTGSWP3_CLM50%SP_SICE_SOCN_MOSART_SGLC_SWAV \
    --res 1x1_ALP1 --machine centos7-nrec --run-unsupported --compiler gnu 
#cd ~/NorESM_LandSites_Platform/noresm2/cime/scripts/
#./create_newcase --case ~/ctsm_cases/test_1 \
#    --compset 2000_DATM%1PTGSWP3_CLM50%FATES_SICE_SOCN_MOSART_SGLC_SWAV \
#    --res 1x1_ALP1 --machine centos7-nrec --run-unsupported --compiler gnu 

# Setup case
cd ~/ctsm_cases/test_sp
python2 case.setup

# Force cold start
./xmlchange CLM_FORCE_COLDSTART="on"

# Build case
python2 case.build

# Run case
python2 case.submit --no-batch
