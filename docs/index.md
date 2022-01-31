# Welcome to the documentation of the NorESM LandSites Platform

This page describes what the platform contains, how the input data were made, the main functionalities, and model output. Users should go to [this user guide](), and refer to this documentation for further detail. Advanced users may also be interested in the technical documentation of [FATES](https://fates-docs.readthedocs.io/en/stable/) and [CLM](https://www.cesm.ucar.edu/models/clm/).

The [GitHub repository](https://en.wikipedia.org/wiki/Git "a place to store code with version control") with the main code is stored [here](https://github.com/NorESMhub/NorESM_LandSites_Platform). We also have additional repositories for [preparing new forcing data for our sites](https://github.com/huitang-earth/NLP_prep) and [illustrating site locations](https://github.com/evalieungh/map_scripts).

To run model simulations, go to our user guide. Advanced users who want to do development, in addition to just running simulations, can request resources on [NREC](https://nrec.no/ "Norwegian Research and Education Cloud: Fast, standardized servers and storage for the Norwegian higher education sector"). An early version of the platform is also available on [Galaxy](https://training.galaxyproject.org/training-material/topics/climate/tutorials/fates/tutorial.html "an open, web-based platform for accessible, reproducible, and transparent computational biological research").

*****************************

## Platform content

You can use the platform to run [single-cell model simulations](https://en.wikipedia.org/wiki/Climate_model#/media/File:Global_Climate_Model.png "the world is divided into a 3-dimensional grid, where equations can be solved within each gridcell, and information passed between gridcells at certain time points. Single-cell model 'runs' only simulate model processes for a single gridcell, which takes a lot less computational power"). We provide [18 sites](https://noresmhub.github.io/NorESM_LandSites_Platform/land-sites/) with high quality input data (atmospheric forcing, land surface data, 'spin-up'), and narrow down the long list of possible output variables into something manageable.

**add an illustration here**

## Input data

Running the model requires specifying compsets, atmospheric forcing, land surface parameters, and spin-up to get realistic simulations.

### Compsets

Short for component sets, compsets specify which components of the larger land model (CLM) and earth system model to use. We support the following compsets:

- **under construction**
- ...


### Atmospheric forcing

Atmospheric forcing data drives the modelled climate using a time series of climatic variables. Downloadable data products exist, but is often on too coarse scales for realistic single-point simulations. Here is a list of atmospheric forcing variables used in CLM:
- Incident solar radiation (FSDS), 	W/m2
- Temperature at the lowest atmospheric level (TBOT), degrees K (or can be C)
- Precipitation (PRECTmms), mm/s
- Wind speed at the lowest atmospheric level (WIND), m/s

More variables can be provided, but the above list are the minimum required. Forcing data for our sites are stored here:
- **under construction**

If you have your own data, you can replace the default input files with your own. ***under construction: Instruction for how to do this***. Make sure the format and units are the same, otherwise the model will not be able to use them. 

For more information on using custom input to CLM, see the [CLM documentation](https://www.cesm.ucar.edu/models/cesm1.0/clm/models/lnd/clm/doc/UsersGuide/x9798.html)

### Surface data

***under construction***

### Spin-up

To get realistic simulations, the model needs to run for a while to reach a state of equilibrium under the applied forcing. Starting the model from "bare ground", the climate is not in equilibrium, there is no or only unrealistic soil, and the model needs time to grow and kill vegetation to get appropriate soil properties and a stable climate. We provide "restart" files for our sites with the following spin-up phase settings:

- **under construction**

## Simulation


***under construction***

### Settings file


***under construction***


### make_cases.py


***under construction***

### run_cases.py


***under construction***

## Postprocess

***under construction***
- output files
- history variables
- plotting suggestions

*****************************
