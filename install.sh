#!/bin/bash

# 1. Directories of this script; and NorESM (from settings file)
dir_script=$PWD/$(dirname "${BASH_SOURCE[0]}")
path_settings=$dir_script/settings.txt
dir_platform=$(grep -oP 'dir_platform\s*[=:]\s*\K(.+)' $path_settings)
dir_platform="${dir_platform/#~/$HOME}" # needed to get dir_noresm
dir_noresm=$(grep -oP 'dir_noresm\s*[=:]\s*\K(.+)' $path_settings)
dir_noresm=`eval echo $dir_noresm` # evaluate $dir_platform

# 2. Install conda virtual environment
module purge && module load Anaconda3/2019.07
dir_env=$dir_script/env
if ! [ -d $dir_env ]; then
    conda env create --prefix $dir_env --file $dir_script/environment_test.yml
else
    echo "$dir_env exists: make sure it is the required conda environment!"
fi;
module purge
# (run "conda clean -all" to avoid large number of cached files in ~/.conda/pkgs)

# 3. Clone/update NorESM and get external tools
url_noresm=$(grep -oP 'url_noresm\s*[=:]\s*\K(.+)' $path_settings)
branch_noresm=$(grep -oP 'branch_noresm\s*[=:]\s*\K(.+)' $path_settings)
mkdir -p $dir_noresm
cd $dir_noresm
if ! [ -d $dir_noresm/.git ]; then
    git clone -b $branch_noresm $url_noresm $dir_noresm
    echo "NorESM cloned into $dir_noresm: on branch $branch_noresm"
else
    git checkout $branch_noresm && git pull
    echo "NorESM already exists in $dir_noresm: $branch_noresm branch up to date"
fi;
python2 manage_externals/checkout_externals # TO CHECK: not working with Python3
# TO DO: run $dir_platform/LandsitesTools/ctsm_patching.ipynb ?
