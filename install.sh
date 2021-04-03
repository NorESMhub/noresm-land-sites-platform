#!/bin/bash

# 1. Directory (relative path) of this script
dir_script=`dirname "${BASH_SOURCE[0]}"`

# 2. Ctsm directory and branch name (from settings.txt)
dir_ctsm=$(grep -oP 'dir_ctsm\s*[=:]\s*\K(.+)' $dir_script/settings.txt)
dir_ctsm="${dir_ctsm/#~/$HOME}" # expand ~ to user home, if needed
branch_ctsm=$(grep -oP 'branch_ctsm\s*[=:]\s*\K(.+)' $dir_script/settings.txt)

# 3. Install conda virtual environment
module purge && module load Anaconda3/2019.07
dir_env=$dir_script/env
if ! [ -d $dir_env ]; then
    conda env create --prefix $dir_env --file $dir_script/environment_test.yml
else
    echo "$dir_env exists: make sure it is the required conda environment!"
fi;
module purge
# (run "conda clean -all" to avoid large number of cached files in ~/.conda/pkgs)

# 4. Clone/update ctsm (master) from NorESMhub
mkdir -p $dir_ctsm
cd $dir_ctsm
if ! [ -d $dir_ctsm/.git ]; then
    git clone git@github.com:NorESMhub/CTSM.git $dir_ctsm
    echo "ctsm cloned into $dir_ctsm"
elif [ $(basename `git rev-parse --show-toplevel`) = ctsm ]; then
    git checkout $branch_ctsm && git pull && cd -
    echo "ctsm already exists in $dir_ctsm: $branch_ctsm branch up to date"
else
    echo "ERROR: $dir_ctsm already hosts another repository than CTSM!"
    exit 1
fi;
