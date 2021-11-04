#!/usr/bin/env python

"""postprocess.py: post-process the CTSM simulation results, e.g. create plots
for output variables defined in a settings file."""

# Read in which output variables to plot from the settings file
import sys
sys.path.append('~/landsites_tools/inputprep/output_variables_subset.json')
import output_variables_subset.json as vars


import os
import xarray as xr
xr.set_options(display_style="html") # is this necessary?
import matplotlib.pyplot as plt

# get the output files in the dir_cases and dir_output specified in settings file
path = os.path.join(dir_output, 'archive', dir_cases, 'lnd', 'hist')

# read all the netCDF files available in the history folder.
dset = xr.open_mfdataset(path + '/*.nc', combine='by_coords') # slow!

# look at the data set
dset

# plot a subset of the model output variables
#-----------------------------------------------
"""add code for automatic plotting off/on"""
# 2-dimensional plots with lines over time
dset['ED_NCOHORTS'].plot(aspect=3, size=6)  # Total number of ED cohorts per site | unitless
dset['ED_NPATCHES'].plot(aspect=3, size=6) # Total number of ED patches per site | unitless
dset['ELAI'].plot(aspect=3, size=6) # exposed one-sided leaf area index | unit m2/m2
# ... etc

# 3-dimensional plots with age or PFT classes
dset['CANOPY_AREA_BY_AGE'].plot(aspect=3, size=6) # canopy area by age bin | units: m2/m2
dset['LEAF_HEIGHT_DIST'].plot(aspect=3, size=6) # leaf height distribution | units: m2/m2
dset['RECRUITMENT'].plot(aspect=3, size=6) # Rate of recruitment by PFT | units: indiv/ha/yr

# to save a plot, use this (from Anne's tut) and change variable name
p = dset['CANOPY_HEIGHT_DIST'].plot(aspect=3, size=6, col_wrap=1, col='fates_levheight')
p.fig.savefig('CANOPY_HEIGHT_DIST.png') # where does this save to?


# Model validation
#-------------------------------------------------
# Maybe this should be a separate script? Get inspiration from scripts by Charlie Koven
