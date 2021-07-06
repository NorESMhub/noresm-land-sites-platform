#! /usr/bin/bash
set -e # Exit if any command fails

# Paths and URLs
dir_platform=~/NorESM_LandSites_Platform
dir_noresm=$dir_platform/noresm2
url_plaform=https://github.com/NorESMhub/NorESM_LandSites_Platform.git 
url_noresm=https://github.com/NorESMhub/NorESM.git
branch_platform=platform_dev
branch_noresm=noresm_landsites

# Clone NorESM Land Sites Platform: platform_dev branch
if ! [ -d $dir_platform ]; then
    git clone -b $branch_platform $url_plaform $dir_platform
fi;

# Clone/update NorESM
mkdir -p $dir_noresm
cd $dir_noresm
if ! [ -d $dir_noresm/.git ]; then
    git clone -b $branch_noresm $url_noresm $dir_noresm
    echo "NorESM cloned into $dir_noresm: on branch $branch_noresm"
else
    git checkout $branch_noresm && git pull
    echo "NorESM already exists in $dir_noresm: $branch_noresm branch up to date"
fi;

# Get externals
if ! [ -d $dir_noresm/components ]; then
    cd $dir_noresm
    python2 manage_externals/checkout_externals # TO CHECK: not working with Python3
else
    for cur_component in clm ../cime; do
        cd $dir_noresm/components/$cur_component/ && git checkout -- . 
    done
fi;

# Set up the environment - create the group and user, the shell variables, the input data directory and sudo access:
echo 'export CESMDATAROOT=${HOME}' | sudo tee /etc/profile.d/escomp.sh
echo 'export CIME_MACHINE=container' | sudo tee -a /etc/profile.d/escomp.sh
echo 'export USER=$(whoami)' | sudo tee -a /etc/profile.d/escomp.sh
#echo 'export PS1="[\u@cesm2.2 \W]\$ "' | sudo tee -a /etc/profile.d/escomp.sh
echo 'ulimit -s unlimited' | sudo tee -a /etc/profile.d/escomp.sh
echo 'export PATH=${PATH}:${dir_noresm}/cime/scripts' | sudo tee -a /etc/profile.d/escomp.sh

# Configuration files
cd && git clone https://github.com/ESCOMP/ESCOMP-Containers.git
cd ~/ESCOMP-Containers/CESM/2.2/Files
git fetch origin 502ca4a974122accc32aba5a5f3b4665dfa0691f
## Add the container versions of the config_machines & config_compilers settings - later, integrate these into CIME
#cp config_compilers.xml $dir_noresm/cime/config/cesm/machines/   # use .cime option instead
#cp config_machines.xml $dir_noresm/cime/config/cesm/machines/    # use .cime option instead
cp config_inputdata.xml $dir_noresm/cime/config/cesm/
cp case_setup.py $dir_noresm/cime/scripts/lib/CIME/case/case_setup.py
## Add the container changes to the XML files (to be included in stock CIME soon):
cp config_compsets.xml $dir_noresm/cime_config/
#cp config_pes.xml $dir_noresm/cime_config/
#cp configs/cam/config_pes.xml $dir_noresm/components/cam/cime_config/
#cp configs/cice/config_pes.xml $dir_noresm/components/cice/cime_config/
#cp configs/cism/config_pes.xml $dir_noresm/components/cism/cime_config/
#cp configs/pop/config_pes.xml $dir_noresm/components/pop/cime_config/
#p configs/clm/config_pes.xml $dir_noresm/components/clm/cime_config/
### Fix for SCAM with GNU in DEBUG mode (ESCOMP/CAM issue #257)
#cp micro_mg3_0.F90 $dir_noresm/components/cam/src/physics/pumas/micro_mg3_0.F90
## Fix for issue with mpi-serial:
cp scam_shell_commands $dir_noresm/components/cam/cime_config/usermods_dirs/scam_mandatory/shell_commands
## And add the fixed 'create_scam6_iop' script for SCAM:
cp create_scam6_iop $dir_noresm/components/cam/bld/scripts
# Delete ESCOMP-Containers folder
cd && rm -rf ESCOMP-Containers

# CESMROOT
export CESMROOT=$dir_noresm

# Platform patching
mkdir $HOME/.cime
cp $dir_platform/config/cime/config_compilers.xml \
   $HOME/.cime
cp $dir_platform/config/cime/config_machines.xml \
   $HOME/.cime
cp $dir_platform/config/ctsm/config_component_ctsm.xml \
   $dir_noresm/components/clm/cime_config/config_component.xml
cp $dir_platform/config/ctsm/namelist_defaults_ctsm.xml \
   $dir_noresm/components/clm/bld/namelist_files/
cp $dir_platform/config/ctsm/CLMBuildNamelist.pm \
   $dir_noresm/components/clm/bld/
cp $dir_platform/config/ctsm/bug_fix/clmfates_interfaceMod.F90 \
   $dir_noresm/components/clm/src/utils/clmfates_interfaceMod.F90
 
# Copy configuration files into noresm/cime component
cp $dir_platform/config/cime/config_batch.xml \
   $dir_noresm/cime/config/cesm/machines/
cp $dir_platform/config/cime/config_grids.xml \
   $dir_noresm/cime/config/cesm/
cp $dir_platform/config/cime/config_component_datm.xml \
   $dir_noresm/cime/src/components/data_comps_mct/datm/cime_config/config_component.xml
cp $dir_platform/config/cime/namelist_definition_datm.xml \
   $dir_noresm/cime/src/components/data_comps_mct/datm/cime_config/namelist_definition_datm.xml
cp $dir_platform/config/cime/configure \
   $dir_noresm/cime/src/externals/mct/configure
