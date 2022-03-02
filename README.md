
[![NorESM logo](https://tinyimg.io/i/9AdhM6J.png "the Norwegian Earth System Model")](https://www.noresm.org/)
[![EMERALD logo](https://tinyimg.io/i/O6Vkl1F.png "EMERALD project")](https://www.mn.uio.no/geo/english/research/projects/emerald/)
[![LATICE logo](https://tinyimg.io/i/4IM1ogh.png "Land-ATmosphere Interactions in Cold Environments research group")](https://www.mn.uio.no/geo/english/research/groups/latice/)


# Platform for NorESM site-level simulation over land

###### doi:

## Overview and documentation
The platform aims to facilitate site-level simulations over land using [NorESM](https://github.com/NorESMhub/NorESM) and its land component [CTSM](https://github.com/NorESMhub/CTSM) with the developing vegetation demographic module [FATES](https://github.com/NGEET/fates). It provides **optimized model setup, input data and reproducible workflows for running the model and analyzing model output over sites** with meteorological, ecological and hydrological observations accross Nordic regions with minimal techincal obstacles.

The platform aims to bridge the gaps between observations and modelling, and **promote the usage of site-level observations to test, validate and improve NorESM** and its land component on the one hand, and **promote the usage of NorESM and its land model component by non-modellers** on the other hand.

The observation sites currently included in the platform:

![sites](https://tinyimg.io/i/cOVIgv4.png)

For more information on the platform, please see our [technical documentation](NorESMhub.github.io/NorESM_LandSites_Platform).


## Quick guides for using the platform


- ### [User guide](https://noresmhub.github.io/NorESM_LandSites_Platform/user_guide)
  - Out [main user guide](https://noresmhub.github.io/NorESM_LandSites_Platform/user_guide) relies on jupyter notebooks and a docker container to run the newest platform version. 
  - An older version i also available on Galaxy: a graphic user interface for running site-level simulation on the cloud computing platform [GALAXY](https://galaxyproject.org/) as a tool (CTSM/FATES-EMERALD), with a dedicated [tutorial](https://training.galaxyproject.org/training-material/topics/climate/tutorials/fates/tutorial.html). Please contact [Anne Fouilloux](https://github.com/annefou) for the development of GALAXY tools related to the platform.


- ### Installation on NREC for developers
Note that we are using NREC for development and testing for the time being. Docker can also be used, but has the drawback of requiring the docker image to be re-built before changes take place. To use NREC, you need a user account and to apply for resources, so get in touch if you need this!

Clone the repository:
```
git clone https://github.com/NorESMhub/NorESM_LandSites_Platform.git
cd NorESM_LandSites_Platform
```
*Remember to switch to the correct branch of interest, e.g.:*
```
git branch -a # Lists all remote branches
git checkout -b my_new_branch origin/platform_dev
```
To install the NorESM and NREC dependencies:
```
cd .machine/NREC_VM_setup
chmod +x install_platform.sh
./install_platform.sh
```
To install the Python package and its dependencies:
```
cd ~/NorESM_LandSites_Platform
pip install -e . # ATTENTION, note the "."!
```

## Code development team
* [Hui Tang](https://github.com/huitang-earth)
* [Lasse Torben Keetz](https://github.com/lasseke)
* [Stefanie Falk](https://github.com/ziu1986)
* [Emiliano Gelati](https://github.com/emiliano-gelati)
* [Elin Aas](https://github.com/ecaas)
* [Anne Fouilloux](https://github.com/annefou)
* [Yeliz Yilmaz](https://github.com/yelizy)
* [Eva Lieungh](https://github.com/evalieungh)
* [Peter Horvath](https://github.com/peterhor)
* [Kjetil Aas](https://github.com/kjetilaas)
* [Sunniva Indrehus](https://github.com/sunnivin)
* [Devaraju Narayanappa](https://github.com/devarajun)

## Data contributors
* [Sonya Geange](https://github.com/srg101)
* [Inge Althuizen](https://github.com/ingealthuizen)

## Supporting projects
* [EMERALD](https://www.mn.uio.no/geo/english/research/projects/emerald/)
* [LATICE](https://www.mn.uio.no/geo/english/research/groups/latice/)
* [SeedClim](https://www.uib.no/en/rg/EECRG/55395/seedclim)
* [LandPress](https://www.uib.no/en/rg/EECRG/95156/landpress)
* [THREE-D](https://www.uib.no/en/rg/EECRG/126712/three-d)
* [INES](https://www.ines.noresm.org/)
* [EOSC-Nordic](https://www.eosc-nordic.eu/)

## How to cite

## Acknowledgement
