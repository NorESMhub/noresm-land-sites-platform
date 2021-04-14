# 1. Directory of this script
dir_script=$PWD/$(dirname "${BASH_SOURCE[0]}")

# 2. Load conda module
module purge --silent
if [[ $HOSTNAME == *"fram.sigma2.no" ]]; then
    module load Anaconda3/2019.07
elif [[ $HOSTNAME == *"saga.sigma2.no" ]]; then
    module load Anaconda3/2019.03
fi # HARD-CODED WORKAROUND -> JSON FILE WITH MODULES FOR EACH MACHINE?
source $(conda info --base)/etc/profile.d/conda.sh

# 3. Activate virtual environment
conda activate $dir_script/../env

# 4. Run input preparation Python script
python $dir_script/example_interface.py $dir_script/../settings.txt
