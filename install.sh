#!/bin/bash

# 1. Directory of this script (and of the platform)
dir_script=$PWD/$(dirname "${BASH_SOURCE[0]}")

# 2. Install conda virtual environment
module purge --silent
if [[ $HOSTNAME == *"fram.sigma2.no" ]]; then
    module load Anaconda3/2019.07
elif [[ $HOSTNAME == *"saga.sigma2.no" ]]; then
    module load Anaconda3/2019.03
fi # HARD-CODED WORKAROUND -> JSON FILE WITH MODULES FOR EACH MACHINE?
dir_env=$dir_script/env
if ! [ -d $dir_env ]; then
    conda env create --prefix $dir_env --file $dir_script/environment_dev.yml
else
    echo "$dir_env exists: make sure it is the required conda environment!"
fi;
module purge --silent
# (users may need to run "conda init bash"; however this changes ~/.bashrc,
#  which we should not do in this script)
# (run "conda clean -all" to avoid large number of cached files in ~/.conda/pkgs)

# 3. Clone/update NorESM and get external tools
dir_noresm=$dir_script/noresm2
url_noresm=https://github.com/NorESMhub/NorESM.git
branch_noresm=noresm_landsites
mkdir -p $dir_noresm
cd $dir_noresm
if ! [ -d $dir_noresm/.git ]; then
    git clone -b $branch_noresm $url_noresm $dir_noresm
    echo "NorESM cloned into $dir_noresm: on branch $branch_noresm"
else
    git checkout $branch_noresm && git pull
    echo "NorESM already exists in $dir_noresm: $branch_noresm branch up to date"
fi;

### check cime and ctsm folders
cd $dir_noresm
if ! [ -d $dir_noresm/components ]; then
    python2 manage_externals/checkout_externals # TO CHECK: not working with Python3
else
    for cur_component in clm ../cime
    do
        cd $dir_noresm/components/$cur_component/ && git checkout -- . 
    done
fi;

### Fix patching
cd $dir_script
cp config/ctsm/config_component_ctsm.xml noresm2/components/clm/cime_config/config_component.xml
cp config/ctsm/namelist_defaults_ctsm.xml noresm2/components/clm/bld/namelist_files/
cp config/ctsm/CLMBuildNamelist.pm noresm2/components/clm/bld/
cp config/ctsm/bug_fix/clmfates_interfaceMod.F90 noresm2/components/clm/src/utils/clmfates_interfaceMod.F90
 
# copy configuration files into noresm/cime component
cp config/cime/config_batch.xml noresm2/cime/config/cesm/machines/
cp config/cime/config_compilers.xml noresm2/cime/config/cesm/machines/
cp config/cime/config_machines.xml noresm2/cime/config/cesm/machines/
cp config/cime/config_grids.xml noresm2/cime/config/cesm/
cp config/cime/config_component_datm.xml noresm2/cime/src/components/data_comps_mct/datm/cime_config/config_component.xml
cp config/cime/namelist_definition_datm.xml noresm2/cime/src/components/data_comps_mct/datm/cime_config/namelist_definition_datm.xml
cp config/cime/configure noresm2/cime/src/externals/mct/configure

