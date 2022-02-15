#! /usr/bin/bash
# Set up the environment - create the group and user, the shell variables, the input data directory and sudo access:
echo 'export CESMDATAROOT=${HOME}' | sudo tee /etc/profile.d/escomp.sh
echo 'export CIME_MACHINE=container' | sudo tee -a /etc/profile.d/escomp.sh
echo 'export USER=$(whoami)' | sudo tee -a /etc/profile.d/escomp.sh
#echo 'export PS1="[\u@cesm2.2 \W]\$ "' | sudo tee -a /etc/profile.d/escomp.sh
echo 'ulimit -s unlimited' | sudo tee -a /etc/profile.d/escomp.sh
echo 'export PATH=${PATH}:${dir_noresm}/cime/scripts' | sudo tee -a /etc/profile.d/escomp.sh