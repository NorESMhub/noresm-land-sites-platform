#!/usr/bin/env python

"""postprocess.py: post-process the CTSM simulation results, e.g. create plots
for output variables defined in a settings file."""

# Task for Eva: Look through CLM output (https://www.cesm.ucar.edu/models/cesm1.2/clm/models/lnd/clm/doc/UsersGuide/history_fields_table_40.xhtml)
# and make a list of most interesting variables.
# Store these as a list(?) somewhere

# Read in which output variables to plot from the settings file
from ../settings.txt import variables_output as vars

import os
import xarray as xr
xr.set_options(display_style="html") # is this necessary?
import matplotlib.pyplot as plt

# get the output files in the dir_cases and dir_output specified in settings file
# re-write this as a for loop for each case?
path = os.path.join(dir_output, 'archive', dir_cases, 'lnd', 'hist','subset')
# open_mfdataset to read all the netCDF files available in the history folder.
# The option combine='by_coords') is used to tell the method open_mfdataset how
# to combine the different files together
dset = xr.open_mfdataset(path + '/*.nc', combine='by_coords') # slow!
