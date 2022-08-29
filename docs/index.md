[![NorESM](img/NORESM-logo.png "the Norwegian Earth System Model")](https://www.noresm.org/)
[![EMERALD](img/Emerald_darktext_whiteBG_small.png "EMERALD project")](https://www.mn.uio.no/geo/english/research/projects/emerald/)
[![LATICE](img/UiO_LATICE_logo_black_small.png "Land-ATmosphere Interactions in Cold Environments research group")](https://www.mn.uio.no/geo/english/research/groups/latice/)

# Welcome to the technical documentation of the NorESM Land Sites Platform

This page describes what the platform contains, how the input data were made, the main functionalities, and model output. See the navigation panel on the left for our [user guide](https://noresmhub.github.io/noresm-land-sites-platform/user_guide), [available sites](https://noresmhub.github.io/noresm-land-sites-platform/land-sites/), [about us](https://noresmhub.github.io/noresm-land-sites-platform/about/), and [contribution guidelines and Code of Conduct](https://noresmhub.github.io/noresm-land-sites-platform/contributing/). 

For more details about the models, see the technical documentation of [FATES](https://fates-docs.readthedocs.io/en/stable/), [CLM](https://escomp.github.io/ctsm-docs/versions/release-clm5.0/html/users_guide/index.html), and [NorESM](https://noresm-docs.readthedocs.io/en/latest/).

#### Quick links to central [GitHub repositories](https://en.wikipedia.org/wiki/Git "a place to store code with version control")

- [NorESMhub/noresm-land-sites-platform](https://github.com/NorESMhub/noresm-land-sites-platform)
- [NorESMhub/noresm-lsp-ui](https://github.com/NorESMhub/noresm-lsp-ui)
- [NorESMhub/noresm-lsp-input](https://github.com/NorESMhub/noresm-lsp-input)
- [NorESMhub/ctsm-api](https://github.com/NorESMhub/ctsm-api)
- [NorESMhub/NorESM](https://github.com/NorESMhub/NorESM)

The main code is stored in the [NorESMhub/noresm-land-sites-platform](https://github.com/NorESMhub/noresm-land-sites-platform) repository. The `main` branch stores the latest functioning version. Older versions can be accessed under [Releases](https://github.com/NorESMhub/noresm-land-sites-platform/releases). This documentation page is made with GitHub pages (`gh-pages` branch) and [Mkdocs](https://www.mkdocs.org/). To contribute to the code or documentation, see our [Contributing](https://noresmhub.github.io/noresm-land-sites-platform/contributing/) instructions. There you will also find our [Code of Conduct](https://noresmhub.github.io/noresm-land-sites-platform/contributing/#code-of-conduct).

Advanced users who want to do development in addition to just running simulations can request resources on e.g. [NREC](https://nrec.no/ "Norwegian Research and Education Cloud: Fast, standardized servers and storage for the Norwegian higher education sector"). An early version of the platform is also available on [Galaxy](https://training.galaxyproject.org/training-material/topics/climate/tutorials/fates/tutorial.html "an open, web-based platform for accessible, reproducible, and transparent computational biological research").

Please let us know if you have questions, suggestions, or trouble using the platform by opening a new [issue](https://github.com/NorESMhub/noresm-land-sites-platform/issues) on GitHub.

*****************************

## Glossary

### Software engineering terms

|Term | Definition |
|-----|------------|
| API = Application Programming Interface | An interface for computer programs to communicate with each other. It offers a service to other pieces of software. An API specification is a document or standard that describes how to build or use such a connection or interface. A computer system that meets this standard is said to implement or expose an API. The term API may refer either to the specification or to the implementation. [(Wikipedia)](https://en.wikipedia.org/wiki/API)|
|Container, </br>Container image, </br>Docker | Containers are isolated, virtualized computer environments based on an image, a read-only file with source code, libraries, dependencies, and tools. Docker Inc. provides containers for free personal and educational use. See the [Docker webpage] (https://www.docker.com/resources/what-container/) and e.g. the [CLASSIC model's containerisation description](https://doi.org/10.5194/gmd-13-2825-2020)|
|Git, </br>GitHub, </br>repository | Git software enables version-control by tracking changes in files. GitHub is an online host of repositories, i.e. data structures of files and their version history.|
|Graphical User Interface (GUI) | An interface between humans and software or hardware, with clickable icons. The NorESM-LSP GUI is accessed after installation and setup at localhost:8080 in a browser, and lets users set up and run model experiments (simulations)|
|Terminal | A computer program where users interact with an operating system by typing in commands (as opposed to clicking and seeing programs in windows). Examples are Windows PowerShell and puTTy. [(Wikipedia)](https://en.wikipedia.org/wiki/Terminal_emulator)|

### Earth System Modelling terms

| Term | Definition |
|------|------------|
|DGVM, </br>FATES|  |
|LSM,</br>CLM/CTSM| |
|ESM,</br>NorESM| |
|Model coupling, </br>CIME| |
|Stub,</br>Data model| |
|PFT = Plant Functional Type| Plant Functional Types are non-phylogenetic groups of plants, defined by a set of parameters. Parameters include e.g. leaf shape, deciduousness, growth form, allometry equation parameters, maximum specific leaf area, and minimum temperature tolerance.|
|Cohort |A group of individual trees of the same PFT and size.|
|Patch|Tile with a defined age since some disturbance (fire, logging).|
|Forcing data| Data that is not impacted by the model but provided as input, generally from observation data sets. Defines the necessary boundary conditions to the model, e.g. temperature and precipitation, throughout its simulation time.|

*****************************

## Software architecture

You can use the platform to run [single-point model simulations](https://en.wikipedia.org/wiki/Climate_model#/media/File:Global_Climate_Model.png "the world is divided into a 3-dimensional grid, where equations can be solved within each gridcell, and information passed between gridcells at certain time points. Single-point model 'runs' only simulate model processes for a single gridcell, which takes a lot less computational power") from a browser on your local computer. We provide [sites](https://noresmhub.github.io/noresm-land-sites-platform/land-sites/) with input data (atmospheric forcing, land surface data), and provide Jupyter notebooks with example python code to plot some input and output data. The NorESM-LSP relies on Docker containers and an Applicatino Programming Interface (API).

![Architecture](img/architecture-Page-1.drawio.svg)

*Illustration of the software architecture. The `model` container is expanded to show the two services running in there (i.e. `API` and `Tasks`) in addition to hosting the model and its dependencies. `./resources` contains all the folders that are mounted into the containers by `docker-compose`. The model and the API manage the ones on the left, and the folders on the right are created by the code maintainers -- an asterisk indicates the folder is optional. After [first-time installation and setup](https://noresmhub.github.io/noresm-land-sites-platform/user_guide/#0-prerequisites-first-time-setup), users can access the Web User Interface (UI) and Jupyter server. The UI uses and Application Programming Interface (API) to send commands between the users and containers.*

![Repositories and containers](img/repos_and_containers-MASTER.drawio.svg)

*Illustration of the GitHub repositories and Docker images and containers that make up the NorESM Land Sites Platform.*

### API

An Application Programming Interface (API) is a set of tools that enables the end-users to interact with a program. The interaction happens by receiving some commands from the users, performing some actions if necessary, and then sending back some results. We created an HTTP API for the model using FastAPI, a popular high-performance framework for Python. It means the API can be used through any medium that can send and receive HTTP requests, e.g., browsers and libraries like python-requests. FastAPI generates a REST API based on [OpenAPI specifications](https://github.com/OAI/OpenAPI-Specification). It also automatically generates documentation for the API from the docstrings of the python functions, which includes a description of the inputs and outputs and examples. The documentation is interactive and can be accessed through its web-based user interface.

The Platform API is responsible for:

- Getting a copy of the model code (via version control or by downloading the source code).
- Setting up the model's external components.
- Overwriting parts of the model with the specified code in `resources/overwrites` (optional).
- Defining the machine config, i.e., the Docker container, for the model (see `resources/dotcime`).
- Creating, configuring, and running cases.
- Serving inputs and outputs of the created cases.

The API code can be found at [https://github.com/NorESMhub/ctsm-api](https://github.com/NorESMhub/ctsm-api).

### Docker containers and model dependencies

CTSM and NorESM depend on many external libraries, which can be challenging to install on a personal computer. The difficulties can be due to a lack of cross-platform support, version conflict with existing libraries, or system architecture.

One solution to this is containerization, which is the process of packaging and distributing software in a way that can be run on various platforms. Containers use Operating System-level virtualization. This allows efficient use of resources while isolating the software from other processes running on the host system. All the requirements for packaged software are included in the container. We used [Docker](https://www.docker.com/) for this purpose. Docker is a widely used and popular containerization tool. 

The packaged software is called an Image. When a Docker Image is run, it is called a Container, i.e., a running instance of the software. The main Image created for the Platform is [ctsm-api](https://github.com/NorESMhub/ctsm-api/pkgs/container/ctsm-api). It contains all the dependencies for the model, the scripts to initialize and configure the model, and the API code that provides access to the model. The Image can be configured via an environment file (`.env`), which gives control to users to adjust some initial properties of the model and the Platform, e.g., what version of the model to use and what drivers should be enabled.

In order to allow easier maintenance and better use of resources, some dependencies are not included in the Image. For example, the message queuing broker (RabbitMQ) required by the API, which is needed to manage asynchronous communications between the model and the API, is not included. This service can be added by using the official [RabbitMQ Docker Image](https://hub.docker.com/_/rabbitmq). Keeping this service out of the Image lets more savvy users pick a different message broker for their use cases.

To address the needs of non-technical users, we have taken care of the extra configurations for the containers by using an orchestration tool called [Docker Compose](https://docs.docker.com/compose/overview/). Docker Compose is a wrapper around Docker, which allows configuring and organizing all the containers in a single `YAML` file.

In addition to the previously mentioned Images, we have included an Image for Jupyter Server and one for our Web User Interface (UI) for `ctsm-api`.

The reason for using a container is that the NorESM model code is not an app but rather a collection of code, and that code needs to be modified in order to run on a new machine. To make the model more like a downloadable app for any machine (e.g. your mac/windows/linux laptop/pc), we put the code in a Docker container that works as a mini-machine within your machine (laptop/pc). A [Docker file](https://docs.docker.com/get-started/overview/ "a text document that contains all the commands a user could call on the command line to assemble an image") is used to enable simulations on any machine, such as a laptop or an HPC cluster. When we release a new version, we have to build a container using this Docker file. Users can then download the dontainer and run simulations there. 

### Web User Interface

Users can set up cases, change some model settings, and run simulations via the Web User Interface. Once the platform is correctly up and running, the UI will be available at [localhost:8080](localhost:8080). The web UI is an application that represents some configurable parameters of the model in a user-friendly way. It comes with built-in validations and error handling for the acceptable values of the parameters. Its goal is to streamline the process of editing a case and help users focus on the scientific aspects of their simulations rather than on the technical configuration.

The Web User Interface (UI) code can be found at [https://github.com/NorESMhub/noresm-lsp-ui](https://github.com/NorESMhub/noresm-lsp-ui). The UI is created using [Typescript](https://www.typescriptlang.org/), a superset of JavaScript language, with the [React](https://reactjs.org/) framework.

### Jupyter Server

The included Jupyter Server Image comes with some commonly used python libraries for data analysis. The list of bundled libraries is available in the [Jupyter Dockerfile](https://github.com/NorESMhub/noresm-land-sites-platform/blob/main/docker/jupyter/Dockerfile).

When the platform is up and running, the Jupyter server is available at [localhost:8888](localhost:8888). Example and tutorial Jupyter notebooks are stored in  `/notebooks`. 


### Model versions

The platform is built to run the land model (CLM) with the Norwegian Earth System Model (as opposed to e.g. CESM which also uses the same land model). The versions of FATES and CLM therefore have to be in line with stable NorESM versions. NorESM is taken in to the platform using the `noresm_landsites` branch in the [NorESMhub/NorESM repository](https://github.com/NorESMhub/NorESM/tree/noresm_landsites). 


## Model input data

Running the model requires specifying compsets, atmospheric forcing, land surface parameters, and sometimes spin-up to get realistic simulations with stable vegetation. 

Input data is created using the [NorESMhub/noresm-lsp-input](https://github.com/NorESMhub/noresm-lsp-input) repository, and requires access to large amounts of storage to create the necessary input data. This is because the global data files are very large and must be stored somewhere accessible while we subset from them. The scripts in the input repository are therefore run on e.g. Saga or another supercomputer. To add new sites to the LSP or to make new input data, visit the [input repository](https://github.com/NorESMhub/noresm-lsp-input) and read the instructions there. In future releases, we may transition to using NCAR's newly developed [tool for subsetting big input data sets](https://github.com/ESCOMP/CTSM/blob/master/tools/site_and_regional/subset_data) which unfortunately doesn't work with the current version of NorESM (July 2022). 

The versioned input data are [in a shared folder on sigma2.no](https://ns2806k.webs.sigma2.no/EMERALD/EMERALD_platform/inputdata_noresm_landsites/). The .tar files are compressed and can be opened as a folder with e.g. 7-zip by right-clicking and choosing 'open archive', and used after extracting (unzipping). The data files are stored in [.nc (NetCDF)](https://www.unidata.ucar.edu/software/netcdf/) format, which can be viewed using [Panoply](https://www.giss.nasa.gov/tools/panoply/), or packages in Python or [R](https://cran.r-project.org/web/packages/ncdf4/index.html). The output data from simulations are stored in the same format but in the specific case folder.

### Component sets (compsets)

Short for component sets, compsets specify which component models are used as well as specific settings for forcing scenarios and physics options. NorESM consists of several sub-models (components) for the atmosphere, land, ocean, etc, plus some common infrastructure code that allows the components to pass information back and forth at certain time steps. Component sets have a short name and a longer name with abbreviations denoting the components included. 

The compset is specified by combining components in this order: atm, lnd, ice, ocn, river, glc, and wave. Each component model version may be "active," "data," "dead," or "stub". Stub components are used instead of active to save computation time and input requirements when that component is not needed for the model configuration. For instance, the active land component forced with atmospheric data, does not need ice, ocn, or glc components to be active and can replace them with stubs. The compset longname defines it in the code with the following notation: `TIME_ATM[%phys]\_LND[%phys]\_ICE[%phys]\_OCN[%phys]\_ROF[%phys]\_GLC[%phys]\_WAV[%phys]`. Currently, we only support the following compset using FATES:

>2000_DATM%1PTGSWP3_CLM50%FATES_SICE_SOCN_MOSART_SGLC_SWAV

- TIME: Initialization Time, here for the year 2000 which gives present day conditions (as opposed to pre-industrial or future) of e.g. CO<sub>2</sub> ppm.
- ATM: Atmosphere, here DATM%1PTGSWP3 for data driven (D) atmosphere (ATM) component driven in a point (PT) by [GSWP3](https://www.isimip.org/gettingstarted/input-data-bias-correction/details/4/) forcing data
- LND: Land, here CLM50%FATES/BGC/SP for active Community Land Model version 5.0 and one of the following vegetation modes:
    1. Functionally Assembled Terrestrial Ecosystem Simulator vegetation (FATES)
    2. FATES with BioGeoChemistry (BGC)
    3. FATES simplified mode with Satellite Phenology (SP)
- ICE: Sea-ice, here SICE stub ice
- OCN: Ocean, here SOCN stub ocean
- ROF: River runoff, here MOSART the MOdel for Scale Adaptive River Transport
- GLC: Land Ice, here SGLC stub glacier (land ice) component
- WAV: Wave, here SWAV stub wave component 

More compsets for pre-industrial or future simulations require additional input data and may be included in future versions of the platform. For now, if you need other compsets you need to dig deeper into the CLM technical documentation and provide the necessary input data and code changes yourself. See the [CLM user guide](https://escomp.github.io/ctsm-docs/versions/release-clm5.0/html/users_guide/setting-up-and-running-a-case/choosing-a-compset.html).

### Atmospheric forcing

Atmospheric forcing data drives the modelled climate using a time series of climatic variables. Downloadable data products exist, but is often on too coarse scales for realistic single-point simulations. Here is a list of atmospheric forcing variables used in CLM:

- Incident solar radiation (FSDS), 	W/m2
- Temperature at the lowest atmospheric level (TBOT), degrees K (or can be C)
- Precipitation (PRECTmms), mm/s
- Wind speed at the lowest atmospheric level (WIND), m/s

Forcing data for our sites are stored with the rest of the [input data](https://ns2806k.webs.sigma2.no/EMERALD/EMERALD_platform/inputdata_noresm_landsites/). Here are some exaples of what the input data .tar contains: 

- CAM/chem/trop_mozart_aero/aero/
    - aerosol deposition: dust, black carbon, organic carbon
- datm7
    - GSWP3v1: monthly atmospheric forcing from the [GSWP3](https://www.isimip.org/gettingstarted/input-data-bias-correction/details/4/) data product. The climatic variables in the above list are stored in these files.
    - NASA_LIS: lightning frequency
    - topo_forcing: topography height

If you have your own data, you can replace the default input files with your own. Make sure the format and units are the exact same, otherwise the model will not be able to use them. For more information on using custom input to CLM, see the [CLM documentation](https://www.cesm.ucar.edu/models/cesm1.0/clm/models/lnd/clm/doc/UsersGuide/x9798.html)

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

For the [Vestland climate grid sites](https://noresmhub.github.io/noresm-land-sites-platform/land-sites/), surface data has been created from raw data sets, using [this script](https://github.com/huitang-earth/NLP_prep/blob/main/create_inputdata.sh "a script in Hui's repository for preparing input data"). There are publicly available data products that could be downloaded and extracted for the nearest model gridcell, given points coordinates, that contain all the necessary surface data. We have not used this; our surface data is interpolated from raw data sets of better resolution. This data should be more accurate than the alternative data products. 

If you have your own data, you can replace the default input files with your own. Make sure the format and units are the same, otherwise the model will not be able to use them. For more information on using custom input to CLM, see the [CLM documentation](https://www.cesm.ucar.edu/models/cesm1.0/clm/models/lnd/clm/doc/UsersGuide/x9798.html)

### Reaching equilibrium (spin-up phase)

To get realistic simulations, the model needs to run for a while, often hundreds to thousands of years, to reach a state of equilibrium under the applied atmospheric forcing. Starting the model from "bare ground" (= startup run type), the model needs time to grow and kill vegetation to get appropriate soil properties and Plant Functional Type distribution. 

**************************************

## Model parameters and site configuration

For an overview of the model settings and parameters users can change, see the [user guide](https://noresmhub.github.io/noresm-land-sites-platform/user_guide/). This section describes how the NorESM-LSP code handles the settings that are displayed in the Graphical User Interface.

To be able to set model parameters in the web UI, some configuration files are needed. Both model parameters and sites configurations are provided by the maintainers as JSON files in `resources/config/variables_config.json` and `resources/config/sites.json`. They can be modified by users who are familiar with the model. The model parameters file contains a list of JSON objects. Attributes of each object are described in table 1. Note that not all types of variables accepted by the model are supported in the user interface at this point. The tables below describe how these configuration files work and handle the model settings users can change in the UI.

####*Table 1: Model parameter attributes, compare to the [variables_config.json](https://github.com/NorESMhub/noresm-land-sites-platform/blob/main/resources/config/variables_config.json) file in `/resources/config`*

| Attribute         | Type                            | default | Required | Scope      | Description                                                                                                                                              |
|-------------------|---------------------------------|---------|----------|------------|----------------------------------------------------------------------------------------------------------------------------------------------------------|
| name              | string                          | -       | yes      | Model      | Name of the variable exactly as it should be passed to the model.                                                                                        |
| label             | string                          | -       | no       | UI         | A human-readable label describing the parameter.                                                                                                         |
| category          | string                          | -       | yes      | API        | One of the following: `ctsm_xml`, `user_nl_clm`, `user_nl_clm_history_file`, `fates`, `fates_param`.                                                     |
| type              | string                          | -       | yes      | Model, API | One of the following: `char`, `integer`, `float`, `logical`, `date`.                                                                                     |
| description       | string                          | -       | no       | UI         | Detailed description of the parameter.                                                                                                                   |
| readonly          | boolean                         | false   | no       | UI         | Whether the parameter can be edited by the user.                                                                                                         |
| hidden            | boolean                         | false   | no       | UI         | Whether the parameter should be hidden from the user.                                                                                                    |
| allow_multiple    | boolean                         | false   | no       | API, UI    | Whether the parameter accepts multiple values.                                                                                                           |
| allow_custom      | boolean                         | false   | no       | UI         | Whether users can enter values other than the ones provided as choices (only applies to those parameters with `choices` in their `validation` attribute. |
| validation        | Validation object               | -       | no       | API, UI    | See `Validation` table below ([TODO: X3]).                                                                                                               |
| default           | integer, float, string, boolean | -       | no       | API        | A default value to use. It must match the type specified by the `type` attribute. If `allow_multiple` is set to `true`, it must be a list of values.     |
| placeholder       | string                          | -       | no       | UI         | A placeholder value to show to the user. This value is not actually applied if no value is entered by the user.                                          |
| append_input_path | boolean                         | false   | no       | API        | Whether to adjust a path value based on its relative location in the input folder.                                                                       |


Adjusted values can be validated using the `validation` attribute. Currently, only the validators described in table 2 are supported.

####*Table 2: validation attributes to define which values are accepted for each model parameter*

| Attribute     | Type                              | Description                                                             |
|---------------|-----------------------------------|-------------------------------------------------------------------------|
| min           | float                             | A minimum value for numeric attributes.                                 |
| max           | float                             | A maximum value for numeric attributes.                                 |
| pattern       | string                            | A regular expression to match the value against.                        |
| pattern_error | string                            | A custom error message for values not matching the `pattern` attribute. |
| choices       | [integer, float, string, boolean] | A list of choices for users to select from.                             |


Sites in `resources/config/sites.json` are described as [GeoJSON points](https://datatracker.ietf.org/doc/html/rfc7946#section-3.1.2). The site map in the web UI draws on these site definitions. Their configuration is set in the `properties` attribute of the GeoJSON object, as described in table 3.

####*Table 3: Site geoJSON properties, compare with the [sites.json](https://github.com/NorESMhub/noresm-land-sites-platform/blob/main/resources/config/sites.json) file in `/resources/config`*

| Attribute | Type            | Required | Description                                                                                                                                                                                                                                                                       |
|-----------|-----------------|----------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| name      | string          | yes      | Name of the site.                                                                                                                                                                                                                                                                 |
| compset   | string          | yes      | The component set to use with the model.                                                                                                                                                                                                                                          |
| res       | string          | yes      | The resolution/grid to use with the model.                                                                                                                                                                                                                                        |
| config    | [Config object] | no       | Config is an object with two keys: `name` and `value`.<br/>The former must point to a parameter in `resources/config/variables_config.json`.<br/>The latter must be a valid value for that parameters.<br/>These are used as default values for the given parameter for the site. |



**************************************

## Running simulations in the web User Interface

See the [user guide](https://noresmhub.github.io/noresm-land-sites-platform/user_guide) for instructions on running simulations. Advanced users who want to change additional or different model parameters or settings can do so manually.

**************************************

## Postprocess

### output files

Output is stored in [.nc (NetCDF)](https://www.unidata.ucar.edu/software/netcdf/) format, which can be viewed using Panoply, or packages in Python or [R](https://cran.r-project.org/web/packages/ncdf4/index.html). From the UI, when a case has finished successfully you can open the Jupyter server (at localhost:8888) and work with the output there, using e.g. the provided notebook in the tutorials folder. The model output is stored locally in the noresm-land-sites-platform repository under `resources/cases/<case-id>/archive`, and can optionally be downloaded to another location with the Download Data button in the User Interface.


**************************************

## Versions

Platform versions follow standard numbering, and releases below 1 should be considered unstable and preliminary. 

#### List of versions (newest on top):

- [release 1]()
- [archive tag](https://github.com/NorESMhub/noresm-land-sites-platform/releases/tag/archive)
- [tag 0.1.0-dev](https://github.com/NorESMhub/noresm-land-sites-platform/releases/tag/v0.1.0)
