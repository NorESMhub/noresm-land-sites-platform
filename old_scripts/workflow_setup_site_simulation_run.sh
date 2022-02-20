#!/bin/bash

#SBATCH --account=nn2806k 
#SBATCH --job-name=fatesrun
#SBATCH --ntasks=1
#SBATCH --mem-per-cpu=4G --partition=bigmem
#SBATCH --array=0-11              
#SBATCH --time=16:00:00

# "creat_mapping" need extra large memory and long time (several hours) to finish. Thus, it is bettter to be run in a queue system (This is why the batch configuration is provided at the begining.)

date="200927"         # Date when you create the inputdata. 
input_dir="/cluster/shared/noresm/inputdata_fates_platform" # home folder for storing the inputdata for each site. 
input_raw="/cluster/shared/noresm/inputdata"                # home folder for storing the raw date required by surface data file
version_model="1.1.1"                                    # version of inputdata
version_input="version1.0.0" 

######### SeedClim Sites
plotlat=(61.0243 60.8231 60.8328 60.9335 60.8203 60.8760 61.0866 60.5445 61.0355 60.8803 60.6652 60.6901)
plotlon=(8.12343 7.27596 7.17561 6.41504 8.70466 7.17666 6.63028 6.51468 9.07876 7.16982 6.33738 5.96487)
plotname=(ALP1 ALP2 ALP3 ALP4 SUB1 SUB2 SUB3 SUB4 BOR1 BOR2 BOR3 BOR4)

######### Landpress Sites
#plotlat=(60.70084 65.83677 64.779 65.79602)
#plotlon=(5.092566 12.224506 11.2193 12.219299)
#plotname=(LYG BUO HAV SKO)

######### Three-D Sites
#plotlat=(60.88019 60.86183 60.85994)
#plotlon=(7.16990 7.16800 7.19504)
#plotname=(VIKE JOAS LIAH)

######### Finnmark Site
#plotlat=(69.341088)
#plotlon=(25.293524)
#plotname=(FINN)


######### Switch for all the steps related to setting a site simulation. 
######### Set all "creat_****" switches to "T" to create inputdata for the site simulation
######### Set "tar_input" to "T" to wrap up all the inputdata to a tar file
######### Set "run_***" to "T" to run the site simulation

run_case="T"                   # T or F, switch for running site simulations automatically. "T" can only be used when all the inputdata are ready!!!!  
run_case_first="F"             # T or F, swtich for creating, building and submitting short test runs
run_case_second="T"            # T or F, swtich for running long experiments

case_name="${plotname[SLURM_ARRAY_TASK_ID]}_default_${version_model}" 

echo ${case_name}

######## Create a case
if [ ${run_case} == "T" ]
then
  
  module purge

#### Following is to print the version of the model used, please double check the model versions are what you want. 
    cd ~/ctsm
    git status
    cd ~/ctsm/cime
    git status
    cd ~/ctsm/src/fates
    git status
    cd ~/ctsm/components/cism
    git status
    cd ~/ctsm/components/rtm
    git status
    cd ~/ctsm/components/mosart
    git status
####

  if [ ${run_case_first} == "T" ]
  then

    if [ ${SLURM_ARRAY_TASK_ID} == "0" ]
    then
     echo "remove inputdata folder"
     rm -r /cluster/work/users/$USER/inputdata
    fi

    cd ~/ctsm/cime/scripts
    rm -r ~/ctsm_cases/${case_name}
    rm -r /cluster/work/users/$USER/ctsm/${case_name}
    ./create_newcase --case ../../../ctsm_cases/${case_name} --compset 2000_DATM%1PTGSWP3_CLM50%FATES_SICE_SOCN_MOSART_SGLC_SWAV --res 1x1_${plotname[SLURM_ARRAY_TASK_ID]} --machine saga --run-unsupported --project nn2806k

    cp ${input_dir}/inputdata_${version_input}_${plotname[SLURM_ARRAY_TASK_ID]}.tar /cluster/work/users/$USER/
    cd /cluster/work/users/$USER/
    tar xvf inputdata_${version_input}_${plotname[SLURM_ARRAY_TASK_ID]}.tar

######## Build the case and run the test
    cd ~/ctsm_cases/${case_name}
    ./case.setup
    ./case.build
    ./.case.run
  fi

######## Run the case for longer time
  if [ ${run_case_second} == "T" ]
  then
    cd ~/ctsm_cases/${case_name}
    ./xmlchange --file env_run.xml --id STOP_OPTION --val nyears                 # set up the time unit (e.g., nyears, nmonths, ndays).
    ./xmlchange --file env_run.xml --id STOP_N --val 300                         # set up the length of the simulation.
    ./xmlchange --file env_run.xml --id CONTINUE_RUN --val TRUE                 # if you want to continue your simulation from the restart file, set it to TRUE.
    ./xmlchange --file env_run.xml --id RESUBMIT --val 9                         # set up how many times you want to resubmit your simulation.
    ./xmlchange --file env_run.xml --id DATM_CLMNCEP_YR_START --val 1901           # set up atmospheric forcing end year  
    ./xmlchange --file env_run.xml --id DATM_CLMNCEP_YR_END --val 1930           # set up atmospheric forcing end year  
    ./xmlchange --file env_workflow.xml --id JOB_WALLCLOCK_TIME --val 11:00:00   # set up longer queue time for runing the simulation. 
    ./xmlchange --file env_workflow.xml --id JOB_QUEUE --val normal              # set up which queue to be used. Both "normal" and "bigmem" can be used depending on their availability.   
  
    ./.case.run
  fi

fi

done

