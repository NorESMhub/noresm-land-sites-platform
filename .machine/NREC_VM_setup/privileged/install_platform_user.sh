#! /usr/bin/bash
set -e # Exit if any command fails

# Paths and URLs
dir_platform=/home/user/NorESM_LandSites_Platform
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
    #TODO this asks for root password. why?
    manage_externals/checkout_externals # TO CHECK: not working with Python3
else
    for cur_component in clm ../cime; do
        cd $dir_noresm/components/$cur_component/ && git checkout -- .
    done
fi;

# Hui: Copy setting files for the containers
# Hui: need a if statement to judge if this is need to be rerun or not

#---cd && git clone https://github.com/ESCOMP/ESCOMP-Containers.git
#--cd ~/ESCOMP-Containers/CESM/2.2/Files
#--git fetch origin 502ca4a974122accc32aba5a5f3b4665dfa0691f

#### Hui: The following settings are relevant
## Fix for issue with mpi-serial:
#--- cp scam_shell_commands $dir_noresm/components/cam/cime_config/usermods_dirs/scam_mandatory/shell_commands
## And add the fixed 'create_scam6_iop' script for SCAM:
#--- cp create_scam6_iop $dir_noresm/components/cam/bld/scripts
# Delete ESCOMP-Containers folder
#--- cd && rm -rf ESCOMP-Containers

# CESMROOT
export CESMROOT=$dir_noresm

# Platform patching
if [ -d $HOME/.cime ] ; then
    echo "Warning: Hidden .cime machine configuration folder already exists and will be replaced."
    rm -rf $HOME/.cime
fi

mkdir $HOME/.cime
cp $dir_platform/config/cime/config_compilers.xml \
$HOME/.cime
cp $dir_platform/config/cime/config_machines.xml \
$HOME/.cime

# Hui: For the new versions, all the following updates need to be retested (turned off) first
cp $dir_platform/config/ctsm/config_component_ctsm.xml \
$dir_noresm/components/clm/cime_config/config_component.xml
cp $dir_platform/config/ctsm/namelist_defaults_ctsm.xml \
$dir_noresm/components/clm/bld/namelist_files/

# Copy configuration files into noresm/cime component
cp $dir_platform/config/cime/component_grids_nuopc.xml \
$dir_noresm/ccs_config/component_grids_nuopc.xml
cp $dir_platform/config/cime/modelgrid_aliases_nuopc.xml \
$dir_noresm/ccs_config/modelgrid_aliases_nuopc.xml

cp $dir_platform/config/cime/config_component_datm.xml \
$dir_noresm/components/cdeps/datm/cime_config/config_component.xml
cp $dir_platform/config/cime/namelist_definition_datm.xml \
$dir_noresm/components/cdeps/datm/cime_config/namelist_definition_datm.xml
cp $dir_platform/config/cime/stream_definition_datm.xml \
$dir_noresm/components/cdeps/datm/cime_config/stream_definition_datm.xml

cp $dir_platform/config/ctsm/namelist_definition_mosart.xml \
$dir_noresm/components/mosart/cime_config/namelist_definition_mosart.xml
