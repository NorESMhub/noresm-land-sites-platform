#!/usr/bin/env python

"""postprocess.py: post-process the CTSM simulation results, e.g. create plots
for output variables defined in a settings file."""

import os
import sys
import xarray as xr
import matplotlib.pyplot as plt

# get the output files in the dir_cases and dir_output specified in settings file
from settings.txt import dir_output
from settings.txt import dir_cases
path = os.path.join(dir_output, 'archive', dir_cases, 'lnd', 'hist')

# read all the netCDF files available in the history folder.
# NB! Slow if there are many files!
dset = xr.open_mfdataset(path + '/*.nc', combine='by_coords')

# look at the data set
dset

# plot a subset of the model output variables
#-----------------------------------------------
"""add code for automatic plotting off/on.
if settings.txt/variables_plot = TRUE, run all plotting code,
else print 'automated output plotting is OFF because settings.txt/variables_plot = FALSE' """

from settings.txt import variables_plot
if variables_plot == TRUE :
    #Read in which output variables to plot from the settings file]
    sys.path.append('~/landsites_tools/inputprep/output_variables_subset.json')
    import output_variables_subset.json as vars

    for var in vars:
        dset[var].plot(aspect=3, size=6)
else :
    print('automated output plotting is OFF because settings.txt/variables_plot = FALSE')

#to save a plot, use this (from Anne's tut) and change variable name
p = dset['var'].plot(aspect=3, size=6, col_wrap=1, col='fates_levheight')
p.fig.savefig('myplot.png') # where does this save to?

# Model validation
#-------------------------------------------------
"""add code for automatic validation off/on.
if settings.txt/model_validation = TRUE, run all validation code,
else print 'automated model validation is OFF because settings.txt/model_validation = FALSE' """
# Maybe this should be a separate script? Get inspiration from scripts by Charlie Koven
