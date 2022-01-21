# Welcome to the documentation of the NorESM LandSites Platform

This page describes what the platform contains, how the input data were made, the main functionalities, and model output. Users should go to [this user guide](), and refer to this documentation for further detail. Advanced users may also be interested in the technical documentation of [FATES](https://fates-docs.readthedocs.io/en/stable/) and [CLM](https://www.cesm.ucar.edu/models/clm/).

The GitHub repository with the main code is stored [here](https://github.com/NorESMhub/NorESM_LandSites_Platform). We also have additional repositories for [preparing new forcing data for our sites]() and [illustrating site locations]()

So far, platform development is on [NREC](https://nrec.no/), and an outdated prototype is available on [Galaxy](https://training.galaxyproject.org/training-material/topics/climate/tutorials/fates/tutorial.html). We have plans for making a better solution using Docker, which should be flexible for advanced users and at the same time easy to use for beginners. 

*****************************

## Platform content

You can use the platform to run single-cell model simulations. We provide pre-defined [sites](https://noresmhub.github.io/NorESM_LandSites_Platform/#sites) with high quality input data (atmospheric forcing, land surface data, 'spin-up'), and narrow down the long list of possible output variables into something manageable.

**add an illustration here**

## Sites

We currently support 18 sites: 12 [Vestland climate grid](https://betweenthefjords.w.uib.no/vestland-climate-grid/) sites and 6 climate station sites of interest to the [LATICE](https://www.mn.uio.no/geo/english/research/groups/latice/) group. These are locations where there are climate stations and ecological field experiments that make these sites of particular interest.

### [Vestland climate grid](https://betweenthefjords.w.uib.no/vestland-climate-grid/) sites, geographical center coordinates
| Site name | Lon (X) | Lat (Y) | Elevation (Z) |
| --- | --- | --- | --- |
| ALP1 | 8.12343 | 61.0243 | 1208 |
| ALP2 | 7.27596 | 60.8231 | 1097 |
| ALP3 | 7.17561 | 60.8328 | 1213 |
| ALP4 | 6.41504 | 60.9335 | 1088 |
| SUB1 | 8.70466 | 60.8203 | 815  |
| SUB2 | 7.17666 | 60.8760 | 700  |
| SUB3 | 6.63028 | 61.0866 | 769  |
| SUB4 | 6.51468 | 60.5445 | 797  |
| BOR1 | 9.07876 | 61.0355 | 589  |
| BOR2 | 7.16982 | 60.8803 | 474  |
| BOR3 | 6.33738 | 60.6652 | 431  |
| BOR4 | 5.96487 | 60.6901 | 346  |

### LATICE-MIP sites
| Site/station name | Lon (X)     |	Lat (Y)     |	Elevation (Z) |
|------------------ | ------      |  --------   | -------------- |
| finseflux         |	7.527008533 | 60.59383774 | 1210  |
| hisaasen_upper    |	12.25481033 | 61.10516357 | 680   |
| hisaasen_lower    |	12.25089836 | 61.1115036  | 640   |
| iskoras           |	25.29547425 | 69.3408715  | 360   |
| aas               |	10.781667   | 59.660278   | 93.2  |
| hurdal            | 11.078142   | 60.372387   | NA    |

## Input data

Running the model requires specifying compsets, atmospheric forcing, land surface parameters, and spin-up to get realistic simulations.

### Compsets

"Compsets", short for component sets, specify which components of the larger land model (CLM) and earth system model to use. We support the following compsets:
- **under construction**
- ...


### Atmospheric forcing

Atmospheric forcing data drives the modelled climate using a time series of climatic variables. Downloadable data products exist, but is often on too coarse scales for realistic single-point simulations. Here is a list of atmospheric forcing variables used in CLM:
- Incident solar radiation (FSDS), 	W/m2
- Temperature at the lowest atm level (TBOT), degrees K (or can be C)
- Precipitation (PRECTmms), mm/s
- Wind speed at the lowest atm level (WIND), m/s

More variables can be provided, but the above list are the minimum required. Forcing data for our sites are stored here:
- **under construction**

If you have your own data and want to adjust or replace the

For more information, see the [CLM documentation](https://www.cesm.ucar.edu/models/cesm1.0/clm/models/lnd/clm/doc/UsersGuide/x9798.html)

### Surface data



### Spin-up

To get realistic simulations, the model needs to run for a while to reach a state of equilibrium under the applied forcing. Starting the model from "bare ground", the climate is not in equilibrium, there is no or only unrealistic soil, and the model needs time to grow and kill vegetation to get appropriate soil properties and a stable climate. We provide "restart" files for our sites with the following spin-up phase settings:
- **under construction**

## Simulation



### Settings file



### make_cases.py



### run_cases.py



## Postprocess

- output files
- history variables
- plotting suggestions

*****************************
