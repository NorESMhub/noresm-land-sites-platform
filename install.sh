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
    conda env create --prefix $dir_env --file $dir_script/environment_test.yml
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
python2 manage_externals/checkout_externals # TO CHECK: not working with Python3
# TO DO: run $dir_platform/LandsitesTools/ctsm_patching.ipynb ?
