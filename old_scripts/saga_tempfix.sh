#!/bin/bash

# Fix utf related bug
LANG=en_US.utf8
LC_ALL=en_US.utf8

# Activate virtual environment
module purge --silent
module load Anaconda3/2019.03
source activate ./env

# Install package in development mode
pip install -e .
