#!/bin/sh


#3) Run the converter / merger script
#================================================================
export PYTHONPATH=$PYTHONPATH:/home/alfg/workspace/camOsloInputGenerator

#Merge the 3D BC ff emissions
python /home/alfg/workspace/camOsloInputGenerator/main.py -x BC_3D_ff.xml -r /home/alfg/workspace/camOsloInputGenerator/camRegularGrid144x72

#Merge the 3D BC bb emissions
python /home/alfg/workspace/camOsloInputGenerator/main.py -x BC_3D_bb.xml -r /home/alfg/workspace/camOsloInputGenerator/camRegularGrid144x72
