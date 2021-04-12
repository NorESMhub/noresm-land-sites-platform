#!/usr/bin/bash

#SBATCH --account=nn2806k 
#SBATCH --job-name=mkmapdata
#SBATCH --mem-per-cpu=256G --partition=bigmem
#SBATCH --ntasks=1
#SBATCH --time=07:00:00

# 1. Directory of this script
dir_script=$PWD/$(dirname "${BASH_SOURCE[0]}")

# 2. Load conda module
module purge && module load Anaconda3/2019.07
source $(conda info --base)/etc/profile.d/conda.sh

# 3. Activate virtual environment
conda activate $dir_script/../env

# 4. Run input preparation Python script
python $dir_script/surface_data.py $dir_script/../settings.txt
