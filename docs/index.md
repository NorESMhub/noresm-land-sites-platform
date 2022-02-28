# Welcome to the documentation of the NorESM LandSites Platform

This page describes what the platform contains, how the input data were made, the main functionalities, and model output. Users should go to [this user guide](https://noresmhub.github.io/NorESM_LandSites_Platform/user_guide), and refer to this documentation for further detail. Advanced users may also be interested in the technical documentation of [FATES](https://fates-docs.readthedocs.io/en/stable/) and [CLM](https://www.cesm.ucar.edu/models/clm/).


The [GitHub repository](https://en.wikipedia.org/wiki/Git "a place to store code with version control") with the main code is stored [here](https://github.com/NorESMhub/NorESM_LandSites_Platform). We also have additional repositories for [preparing new forcing data for our sites](https://github.com/huitang-earth/NLP_prep) and [illustrating site locations](https://github.com/evalieungh/map_scripts).

To run model simulations, go to our user guide. Advanced users who want to do development, in addition to just running simulations, can request resources on [NREC](https://nrec.no/ "Norwegian Research and Education Cloud: Fast, standardized servers and storage for the Norwegian higher education sector"). An early version of the platform is also available on [Galaxy](https://training.galaxyproject.org/training-material/topics/climate/tutorials/fates/tutorial.html "an open, web-based platform for accessible, reproducible, and transparent computational biological research").

🚧
*NB! This documentation is still under construction. Some parts are missing or may be written for older testing versions. Please let us know if you have suggestions for something to add or explain better in the [issues](https://github.com/NorESMhub/NorESM_LandSites_Platform/issues)*
🚧

*****************************

## Platform content


You can use the platform to run [single-cell model simulations](https://en.wikipedia.org/wiki/Climate_model#/media/File:Global_Climate_Model.png "the world is divided into a 3-dimensional grid, where equations can be solved within each gridcell, and information passed between gridcells at certain time points. Single-cell model 'runs' only simulate model processes for a single gridcell, which takes a lot less computational power"). We provide [sites](https://noresmhub.github.io/NorESM_LandSites_Platform/land-sites/) with high quality input data (atmospheric forcing, land surface data, 'spin-up'), and narrow down the long list of possible output variables into something manageable.

🚧*add an illustration here*

### Docker file

A [Docker file](https://docs.docker.com/get-started/overview/ "a text document that contains all the commands a user could call on the command line to assemble an image") is used to enable simulations on any machine, such as a laptop or an HPC cluster. 

### Model versions

The platform is built to run the land model (CLM) with the Norwegian Earth System Model (as opposed to e.g. CESM which also uses the same land model). The versions of FATES and CLM therefore have to be in line with stable NorESM versions. NorESM is taken in to the platform using the `noresm_landsites` branch in the [NorESMhub/NorESM repository](https://github.com/NorESMhub/NorESM/tree/noresm_landsites). 

| Model | Version |
| --- | --- |
| NorESM |  |
| CLM |  |
| FATES |  |



**************************************


## Input data

Running the model requires specifying compsets, atmospheric forcing, land surface parameters, and spin-up to get realistic simulations. 

The input data are [here](https://ns2806k.webs.sigma2.no/EMERALD/EMERALD_platform/inputdata_fates_platform/), with a [readme](https://ns2806k.webs.sigma2.no/EMERALD/EMERALD_platform/inputdata_fates_platform/readme.inpudata_emerald_platform) file with further detail. The .tar files are compressed, and can be opened as a folder with e.g. 7-zip by right-clicking and choosing 'open archive', and used after extracting (unzipping). The data files are stored in [.nc (NetCDF)](https://www.unidata.ucar.edu/software/netcdf/) format, which can be viewed using Panoply, or packages in Python or [R](https://cran.r-project.org/web/packages/ncdf4/index.html). The output data from simulations are stored in the same format.

The input data .tar file contains three folders: (1) 'shared' domain files with gridcell longitude, latitude, and area; (2) [land (=lnd)](https://noresmhub.github.io/NorESM_LandSites_Platform/#surface-data) surface data, and (3) [atmosphere (=atm)](https://noresmhub.github.io/NorESM_LandSites_Platform/#atmospheric-forcing) data. The input data are [site](https://noresmhub.github.io/NorESM_LandSites_Platform/land-sites/)-specific. 


### Compsets

Short for component sets, compsets specify which component models are used as well as specific settings for forcing scenarios and physics options. Compsets have a short name and a longer name with abbreviations denoting the components included. See more in the [CLM user guide](https://escomp.github.io/ctsm-docs/versions/release-clm5.0/html/users_guide/setting-up-and-running-a-case/choosing-a-compset.html).

Currently, we only support the following three compsets, with long name:
>2000_DATM%1PTGSWP3_CLM50%FATES_SICE_SOCN_MOSART_SGLC_SWAV
>2000_DATM%1PTGSWP3_CLM50%BGC_SICE_SOCN_MOSART_SGLC_SWAV
>2000_DATM%1PTGSWP3_CLM50%SP_SICE_SOCN_MOSART_SGLC_SWAV

The notation for the compset longname is: `TIME_ATM[%phys]\_LND[%phys]\_ICE[%phys]\_OCN[%phys]\_ROF[%phys]\_GLC[%phys]\_WAV[%phys]`
The compset longname has a specified order: atm, lnd, ice, ocn, river, glc, and wave. Each component model version may be "active," "data," "dead," or "stub". Stub components are used instead of active to save computation time and input requirements when that component is not needed for the model configuration. For instance, the active land component forced with atmospheric data, does not need ice, ocn, or glc components to be active and can replace them with stubs. 

- TIME: Initialization Time, here for the year 2000 which gives present day conditions (as opposed to pre-industrial or future) of e.g. CO<sub>2</sub> ppm.
- ATM: Atmosphere, here DATM%1PTGSWP3 for data driven (D) atmosphere (ATM) component driven in a point (PT) by [GSWP3](https://www.isimip.org/gettingstarted/input-data-bias-correction/details/4/) forcing data
- LND: Land, here CLM50%FATES/BGC/SP for active Community Land Model version 5.0 and one of the following vegetation modes:
    1. FATES vegetation
    2. BioGeoChemistry
    3. Satellite Phenology
- ICE: Sea-ice, here SICE stub ice
- OCN: Ocean, here SOCN stub ocean
- ROF: River runoff, here MOSART the MOdel for Scale Adaptive River Transport
- GLC: Land Ice, here SGLC stub glacier (land ice) component
- WAV: Wave, here SWAV stub wave component 

More compsets for pre-industrial or future simulations require additional input data and may be included in future versions of the platform. For now, if you need other compsets you need to dig deeper into the CLM technical documentation and provide the necessary input data yourself. 


### Atmospheric forcing

Atmospheric forcing data drives the modelled climate using a time series of climatic variables. Downloadable data products exist, but is often on too coarse scales for realistic single-point simulations. Here is a list of atmospheric forcing variables used in CLM:

- Incident solar radiation (FSDS), 	W/m2
- Temperature at the lowest atmospheric level (TBOT), degrees K (or can be C)
- Precipitation (PRECTmms), mm/s
- Wind speed at the lowest atmospheric level (WIND), m/s


Forcing data for our sites are stored with the rest of the [input data](https://ns2806k.webs.sigma2.no/EMERALD/EMERALD_platform/inputdata_fates_platform/). Here are some exaples of what the input data .tar contains: 

- CAM/chem/trop_mozart_aero/aero/
    - aerosol deposition: dust, black carbon, organic carbon
- datm7
    - GSWP3v1: monthly atmospheric forcing from the [GSWP3](https://www.isimip.org/gettingstarted/input-data-bias-correction/details/4/) data product. The climatic variables in the above list are stored in these files.
    - NASA_LIS: lightning frequency
    - topo_forcing: topography height

If you have your own data, you can replace the default input files with your own. *under construction: Instruction for how to do this*. Make sure the format and units are the exact same, otherwise the model will not be able to use them. For more information on using custom input to CLM, see the [CLM documentation](https://www.cesm.ucar.edu/models/cesm1.0/clm/models/lnd/clm/doc/UsersGuide/x9798.html)

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

If you have your own data, you can replace the default input files with your own. ***under construction: Instruction for how to do this***. Make sure the format and units are the same, otherwise the model will not be able to use them. 

For more information on using custom input to CLM, see the [CLM documentation](https://www.cesm.ucar.edu/models/cesm1.0/clm/models/lnd/clm/doc/UsersGuide/x9798.html)


### Spin-up

To get realistic simulations, the model needs to run for a while to reach a state of equilibrium under the applied forcing. Starting the model from "bare ground", the climate is not in equilibrium, there is no or only unrealistic soil, and the model needs time to grow and kill vegetation to get appropriate soil properties and a stable climate. We provide "restart" files for our sites with the following spin-up phase settings:

- *under construction*

**************************************

## Running simulations

To run a simulation, you need to set up a [case](https://esmci.github.io/cime/versions/master/html/glossary/index.html#term-case-CASE "An instance of a model simulation. A case is defined by a component set, a model grid, a machine, a compiler, and any other additional customizations.") which tells the model how to run. A case can be run several times, or stopped and started again. The NorESM platform provides a [settings file](https://noresmhub.github.io/NorESM_LandSites_Platform/#settings-file) that will set some basic information, and scripts to simplify running the case(s). Under `NorESM_LandSites_Platform/landsites_tools/simulation/` there are python scripts `make_cases.py` and `run_cases.py`. These scripts set up a simulation case directory and creates, builds and downloads [input data](https://noresmhub.github.io/NorESM_LandSites_Platform/#input-data "atmospheric forcing, land surface data") for the model. For more detailed information on what goes on in CLM and its coupler (which connects CLM to other model components), see this [CIME user guide](https://esmci.github.io/cime/versions/master/html/users_guide/index.html), but note that the NorESM modelling platform uses these commands and scripts more indirectly. The settings file and python scripts combine several of these options and commands to simplify the process of running simulations.



### Settings file

All required information to prepare and run CLM-FATES cases for the available Norwegian land sites is provided via a settings.txt file. A template, which also serves as the default settings if no changes are made, is stored under [~/NorESM_LandSites_Platform/landsites_tools/](https://github.com/NorESMhub/NorESM_LandSites_Platform/tree/platform_dev/landsites_tools). 

The settings file has the following contents:

|setting | description |
|-------|-------|
|dir_cases | cases root folder, absolute or relative to project dir|
|dir_clm_input | clm input root folder, absolute or relative to project dir|
|dir_output | output root folder, absolute or relative to project dir|
|start_time   | at what time should simulation start? Format yyyy-mm-dd hh:mm, default 2000-01-01|
|end_time     | at what time should simulation stop? Format yyyy-mm-dd, default 2001-01-01|
|sites2run  | which [sites](https://noresmhub.github.io/NorESM_LandSites_Platform/land-sites/) should be simulated? Make sure the names are correct! Fetches from data/.dicts/sites.json. Default is ALP1 and ALP2|
|type_run  | What type of [model run]() do you want? startup, hybrid, branch, restart. Default is startup|
|type_model  | CLM-SP, CLM-BGC, CLM-FATES, FATES-SP, FATES-nocomp, FATES-hydro. Default is CLM-SP|
|initial_file | initial conditions (empty: cold start)|
|frequency_output | At what frequency should output be stored? Monthly/daily/hourly. Default is monthly, which gives all variables|
|variables_output | which output variables to store. [CLM has many more to choose from](https://www.cesm.ucar.edu/models/cesm1.2/clm/models/lnd/clm/doc/UsersGuide/history_fields_table_40.xhtml)|
|variables_plot | Which variables to plot |
|frequency_plot | Timestep of plotting. Default daily.|
|vegetation_types_FATES |to be implemented|
|output_groups |to be implemented|

The last lines (not shown here) also specify some paths that are set automatically and users should not touch. If you are changing the settings file manually, be careful wirh formatting! Upper/lowercase, spaces and symbols need to be correct for it to work.

#### model run types

| model run type | description |
|----------------|-------------|
| startup | a 'cold' start from bare ground. The vegetation and climate is not in equilibrium and the model will not produce realistic output. No [spin-up](https://noresmhub.github.io/NorESM_LandSites_Platform/#spin-up) is included. Use this mode for quick testing, or for making your own spin-up. |
| hybrid  | the model is initialized similar to a startup run, but uses initialization datasets from a previous case. Suitable when you already have good spin-up files and want a more realistic simulation. |
| branch  | the model is initialized using a consistent set of restart files from a previous run. The case name is generally changed for a branch run, although it does not have to be. Branch runs are suitable for sensitivity or parameter studies, or when settings for history file output streams need to be modified while still maintaining bit-for-bit reproducibility. |
| restart | continues running an existing case after it has been stopped. |

#### model types

| model run type | description *under construction* |
|----------------|-------------|
| CLM-SP | satellite phenology.|
| CLM-BCG| biogeochemistry |
| CLM-FATES | FATES vegetation |
| FATES-SP| FATES but with satellite phenology|
| FATES-nocomp| FATES without competition |
| FATES-hydro| FATES with different hydrology |




### make_cases.py

This python script creates, builds, and sets up CTSM cases for predefined or custom [site locations](https://noresmhub.github.io/NorESM_LandSites_Platform/land-sites/). It is either using the simulation options specified in a 'settings.txt' file or asks for interactive command line input to create one. Input data is automatically downloaded unless custom input data has been added. When making a new case, it first creates a case directory containing the scripts and XML files to configure a case. Then, it creates scripts needed to run the model along with [namelist files](https://esmci.github.io/cime/versions/master/html/glossary/index.html#term-user-namelist-files-CASEROOT-user_nl_ "Files containing input parameters for CLM. User modifications for a given case can be added in these files."). Finally, it [compiles](https://en.wikipedia.org/wiki/Compiler "translate source code from a human-readable programming language to a machine-readable language to create an executable program") the model and builds the executable file from which the case is run. 

The `make_cases.py` script uses helper scripts stored in the `utils/` folder and commands experienced CLM users will be familiar with, namely `create_newcase`, `case.setup` and `case.build`.



### run_cases.py

This python script runs previously built cases made with `make_cases.py`. The respective paths need to be defined in a 'settings.txt' file. The model output files will be created in the `data/output/CASE_NAME` directory by default, but a different path can be specified. Depending on the length of simulation, and computational resources available, this can take some time. The script calls on the `case.submit` command that experienced CLM users will be familiar with. 



**************************************

## Postprocess

### output files

Output is stored in [.nc (NetCDF)](https://www.unidata.ucar.edu/software/netcdf/) format, which can be viewed using Panoply, or packages in Python or [R](https://cran.r-project.org/web/packages/ncdf4/index.html)

### history variables

Each output .nc file contains information for several history variables, such as ...*under construction*

### plotting

Suggestions for plotting output are given in a Jupyter notebook in the repository under the `/notebooks` directory. In future versions, we hope to add more postprocessing and plotting functionality.
