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
git clone -b $branch_platform $url_plaform $dir_platform

# Clone/update NorESM and get external tools
mkdir -p $dir_noresm
cd $dir_noresm
if ! [ -d $dir_noresm/.git ]; then
    git clone -b $branch_noresm $url_noresm $dir_noresm
    echo "NorESM cloned into $dir_noresm: on branch $branch_noresm"
else
    git checkout $branch_noresm && git pull
    echo "NorESM already exists in $dir_noresm: $branch_noresm branch up to date"
fi;

## Check cime and ctsm folders
cd $dir_noresm
if ! [ -d $dir_noresm/components ]; then
    python2.7 manage_externals/checkout_externals # TO CHECK: not working with Python3
else
    for cur_component in clm ../cime; do
        cd $dir_noresm/components/$cur_component/ && git checkout -- . 
    done
fi;

## Fix patching
cp $dir_platform/config/ctsm/config_component_ctsm.xml \
   $dir_noresm/components/clm/cime_config/config_component.xml
cp $dir_platform/config/ctsm/namelist_defaults_ctsm.xml \
   $dir_noresm/components/clm/bld/namelist_files/
cp $dir_platform/config/ctsm/CLMBuildNamelist.pm \
   $dir_noresm/components/clm/bld/
cp $dir_platform/config/ctsm/bug_fix/clmfates_interfaceMod.F90 \
   $dir_noresm/components/clm/src/utils/clmfates_interfaceMod.F90
 
## Copy configuration files into noresm/cime component
cp $dir_platform/config/cime/config_batch.xml \
   $dir_noresm/cime/config/cesm/machines/
cp $dir_platform/config/cime/config_compilers.xml \
   $dir_noresm/cime/config/cesm/machines/
cp $dir_platform/config/cime/config_machines.xml \
   $dir_noresm/cime/config/cesm/machines/
cp $dir_platform/config/cime/config_grids.xml \
   $dir_noresm/cime/config/cesm/
cp $dir_platform/config/cime/config_component_datm.xml \
   $dir_noresm/cime/src/components/data_comps_mct/datm/cime_config/config_component.xml
cp $dir_platform/config/cime/namelist_definition_datm.xml \
   $dir_noresm/cime/src/components/data_comps_mct/datm/cime_config/namelist_definition_datm.xml
cp $dir_platform/config/cime/configure \
   $dir_noresm/cime/src/externals/mct/configure
