# Welcome to the documentation of the NorESM LandSites Platform

This page describes what the platform contains, how the input data were made, the main functionalities, and model output. Users should go to [this user guide](), and refer to this documentation for further detail. Advanced users may also be interested in the technical documentation of [FATES](https://fates-docs.readthedocs.io/en/stable/) and [CLM](https://www.cesm.ucar.edu/models/clm/).

The [GitHub repository](https://en.wikipedia.org/wiki/Git "a place to store code with version control") with the main code is stored [here](https://github.com/NorESMhub/NorESM_LandSites_Platform). We also have additional repositories for [preparing new forcing data for our sites](https://github.com/huitang-earth/NLP_prep) and [illustrating site locations](https://github.com/evalieungh/map_scripts).

To run model simulations, go to our user guide. Advanced users who want to do development, in addition to just running simulations, can request resources on [NREC](https://nrec.no/ "Norwegian Research and Education Cloud: Fast, standardized servers and storage for the Norwegian higher education sector"). An early version of the platform is also available on [Galaxy](https://training.galaxyproject.org/training-material/topics/climate/tutorials/fates/tutorial.html "an open, web-based platform for accessible, reproducible, and transparent computational biological research").

*****************************

## Platform content

You can use the platform to run [single-cell model simulations](https://en.wikipedia.org/wiki/Climate_model#/media/File:Global_Climate_Model.png "the world is divided into a 3-dimensional grid, where equations can be solved within each gridcell, and information passed between gridcells at certain time points. Single-cell model 'runs' only simulate model processes for a single gridcell, which takes a lot less computational power"). We provide [sites](https://noresmhub.github.io/NorESM_LandSites_Platform/land-sites/) with high quality input data (atmospheric forcing, land surface data, 'spin-up'), and narrow down the long list of possible output variables into something manageable.

*add an illustration here*


## Compsets

Short for component sets, compsets specify which component models are used as well as specific settings for forcing scenarios and physics options. Compsets have a short name and a longer name with abbreviations denoting the components included. See more in the [CLM user guide](https://escomp.github.io/ctsm-docs/versions/release-clm5.0/html/users_guide/setting-up-and-running-a-case/choosing-a-compset.html).

Currently, we only support the following 'compset', with long name:
>2000_DATM%1PTGSWP3_CLM50%FATES_SICE_SOCN_MOSART_SGLC_SWAV

The notation for the compset longname is: TIME_ATM[%phys]\_LND[%phys]\_ICE[%phys]\_OCN[%phys]\_ROF[%phys]\_GLC[%phys]\_WAV[%phys]
The compset longname has a specified order: atm, lnd, ice, ocn, river, glc, and wave. Each component model version may be "active," "data," "dead," or "stub". Stub components are used instead of active to save computation time and input requirements when that component is not needed for the model configuration. For instance, the active land component forced with atmospheric data, does not need ice, ocn, or glc components to be active and can replace them with stubs. BGC, for biogeophysics, can also be added

- TIME: Initialization Time, here for the year 2000 which gives present day conditions (as opposed to pre-industrial)
- ATM: Atmosphere, here DATM%1PTGSWP3 for data driven (D) atmosphere (ATM) component driven in a point (PT) by [GSWP3](https://www.isimip.org/gettingstarted/input-data-bias-correction/details/4/) forcing data
- LND: Land, here CLM50%FATES for active Community Land Model version 5.0 and FATES vegetation
- ICE: Sea-ice, here SICE stub ice
- OCN: Ocean, here SOCN stub ocean
- ROF: River runoff, here MOSART the MOdel for Scale Adaptive River Transport
- GLC: Land Ice, here SGLC stub glacier (land ice) component
- WAV: Wave, here SWAV stub wave component 

We plan to make more compsets available in future versions.


## Input data

Running the model requires specifying compsets, atmospheric forcing, land surface parameters, and spin-up to get realistic simulations. 

The input data are [here](https://ns2806k.webs.sigma2.no/EMERALD/EMERALD_platform/inputdata_fates_platform/), with a [readme](https://ns2806k.webs.sigma2.no/EMERALD/EMERALD_platform/inputdata_fates_platform/readme.inpudata_emerald_platform) file with further detail. The .tar files are compressed, and can be opened as a folder with e.g. 7zip by right-clicking and choosing 'open archive', and used after extracting (unzipping). The data files are stored in [.nc (NetCDF)](https://www.unidata.ucar.edu/software/netcdf/) format, which can be viewed using Panoply, or packages in Python or [R](https://cran.r-project.org/web/packages/ncdf4/index.html). The output data from simulations are stored in the same format.

The input data .tar file contains three folders: (1) 'shared' domain files with gridcell longitude, latitude, and area; (2) [land (=lnd)](https://noresmhub.github.io/NorESM_LandSites_Platform/#surface-data) surface data, and (3) [atmosphere (=atm)](https://noresmhub.github.io/NorESM_LandSites_Platform/#atmospheric-forcing) data. The input data are [site](https://noresmhub.github.io/NorESM_LandSites_Platform/land-sites/)-specific. 

### Atmospheric forcing

Atmospheric forcing data drives the modelled climate using a time series of climatic variables. Downloadable data products exist, but is often on too coarse scales for realistic single-point simulations. Here is a list of atmospheric forcing variables used in CLM:
- Incident solar radiation (FSDS), 	W/m2
- Temperature at the lowest atmospheric level (TBOT), degrees K (or can be C)
- Precipitation (PRECTmms), mm/s
- Wind speed at the lowest atmospheric level (WIND), m/s

More variables can be provided, but the above list are the minimum required. 
Forcing data for our sites are stored with the rest of the [input data](https://ns2806k.webs.sigma2.no/EMERALD/EMERALD_platform/inputdata_fates_platform/) and is organised in the following folder structure:
- CAM/chem/trop_mozart_aero/aero/
  - aerosol deposition: dust, black carbon, organic carbon
- datm7
  - GSWP3v1
    - ...

If you have your own data, you can replace the default input files with your own. ***under construction: Instruction for how to do this***. Make sure the format and units are the same, otherwise the model will not be able to use them. 

For more information on using custom input to CLM, see the [CLM documentation](https://www.cesm.ucar.edu/models/cesm1.0/clm/models/lnd/clm/doc/UsersGuide/x9798.html)

### Surface data

Surface data contains information the model needs about the land surface, such as land use trajectories, soil properties, vegetation parameters, and [albedo](https://en.wikipedia.org/wiki/Albedo). Here are some exaples of what the input data .tar contains: 

- 'firedata'
  - population density 
- 'paramdata'
  - CLM5 parameters: many parameters, covering e.g. Plant Functional Types (PFTs), allocation of carbon, photosynthetic pathway
  - FATES parameters: many parameters, covering e.g. Plant Functional Types (PFTs), allometry, carbon pools, nitrogen uptake, mortality, recruitment, fuel load for fire module 
- 'snicardata'
  - snow "growth" parameters
  - snow "optics"
- 'surfdata_map'
  - surface data:  soil depth & other properties, albedo & thermal conductance of different surface types, and fraction of gridcell covered by vegetation, land, & other land cover types
- 'urbandata'
  - urban classes, building interior temperature

Note that when CLM is running without FATES, a simpler 'big-leaf'-version of vegetation is used instead. The CLM5 and FATES parameters thus overlap to some degree, with FATES replacing some and adding other parameters when activated. 

For the [Vestland climate grid sites](https://noresmhub.github.io/NorESM_LandSites_Platform/land-sites/), surface data has been created from raw data sets, using [this script](https://github.com/huitang-earth/NLP_prep/blob/main/create_inputdata.sh "a script in Hui's repository for preparing input data"). There are publicly available data products that could be downloaded and extracted for the nearest model gridcell, given points coordinates, that contain all the necessary surface data. We have not used this; our surface data is interpolated from raw data sets of better resolution. This data should be more accurate than the alternative data products. 


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
