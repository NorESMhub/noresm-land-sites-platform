# User guide

This is the user guide for running single-site simulations with the [NorESM LandSites Platform](https://noresmhub.github.io/noresm-land-sites-platform/).

You will need to open two new windows in the process, so it's a good idea to use two screens or to make this window narrower so you can see both the user guide and another window next to it.

***********************************************

## TL;DR quick start 🏃‍♀️🏃‍♂️

If you have already completed [first-time setup](https://noresmhub.github.io/noresm-land-sites-platform/user_guide/#0-prerequisites-first-time-setup) and know what you are doing, here is the extremely quick user guide. In a terminal where you have the repository: 
    
    $ cd noresm-land-sites-platform
    $ docker-compose up
    
Wait until the terminal messages stop. Open the GUI: [localhost:8080](http://localhost:8080) and push buttons, and access jypyter notebooks on [localhost:8888](http://localhost:8888) 🎉


***********************************************

## Step by step guide for using the platform


### 0. Prerequisites (first time setup) 🌱

To use the NorESM land sites platform, you need to install [Git](https://git-scm.com/downloads "click the pc screen button if you are on Windows") (and make a user account) and [Docker desktop](https://www.docker.com/products/docker-desktop) before you can clone the [repository](https://github.com/NorESMhub/noresm-land-sites-platform "repository for the NorESM Land Sites platform") (= download platform scripts) and start working with the Docker container. If you don't want to use Git, you can try to download and unpack the repository manually instead by clicking the `code` button and `download zip`. Step by step (with steps in brackets only sometimes necessary, depending on your computer):

1. [Create a GitHub account](https://github.com/) (optional, but generally recommended to be able to open issues, etc.)
2. Install Git on your machine. For Windows: https://gitforwindows.org/, other: https://github.com/git-guides/install-git
3. Install [Docker desktop](https://docs.docker.com/get-docker), might require restart
4. (Install Docker Compose; should already be included in the Docker installation described above for Mac and Windows: [install docker-compose](https://docs.docker.com/compose/install) )
5. (Remarks for Windows: You may have to install WSL2 (the 'two' is important here) manually if prompted. Follow the steps described [here](https://www.omgubuntu.co.uk/how-to-install-wsl2-on-windows-10). To open the Windows Command Prompt terminal as an administrator, type `cmd` into the Windows search bar located next to the Start Symbol (lower-left corner of the screen), right-click on 'Command Prompt', and select 'Run as administrator'. Also, note that some steps in the guide are executed in the 'Command Prompt' and some are executed in the 'Windows PowerShell'; to open the latter, type 'PowerShell' into the search bar and open as administrator. If Docker complains about you not belonging to the correct 'user group' after successful installation, follow the steps described [here](https://stackoverflow.com/questions/61530874/docker-how-do-i-add-myself-to-the-docker-users-group-on-windows-10-home)
6. Open the file explorer and find a suitable folder to serve as working directory. This is where you will store the repository and installation files needed by the platform, as well as your output files (which may take up quite a bit of space!). Your working directory should be easy to find, stable, and have plenty of free space available. For instance `C:/Users/yourusername` (OneDrive, USB sticks or your overly-cluttered desktop not recommended 👀).
7. When you are in your chosen working directory, right-click and choose "Git Bash here" (Windows); or directly use `cd [path_to_directory]` in a terminal with Git installed. Copy and paste the following line into the terminal (note that in some terminals such as Git Bash, you need to click the right mouse button to paste from the clipboard):


```
$ git clone https://github.com/NorESMhub/noresm-land-sites-platform.git --config core.autocrlf=input 
```


This will download (= clone) the repository (= folder structure and files) to your working directory. You can now see the folder and files in your file explorer. Most of the files can be opened in a text editor like Notepad if you want to look at their contents.

Once Git, Docker Desktop and the repository are in place, you don't have to do this again. If the platform has been updated the next time you want to use it, you might want to download the updates by typing first `git pull` and then `docker-compose pull` into Git Bash from your local clone of the repository. 

### 1. Start the container 🧰

In the working directory where you have cloned the repository, open a terminal (e.g. by right-clicking and choosing "Git Bash here"; or use the one you already have open if you just did the first time setup). Make sure you are inside the folder containing the `docker-compose.yaml` file (type `ls` and hit enter to list the files in the current folder; if you see noresm-land-sites-platform, you need to change directory into that folder by typing ´cd [local_path]/noresm-land-sites-platform´). Then write this command and hit enter to get the container up and running:

    $ docker-compose up

The first time you execute this command, a lot of files will be downloaded first and it might take some time. When the container is up and running, the bottom messages in your terminal will look something like the screenshot below (NB! Do not close the container by pressing `Ctrl+C`, only close it once you are finished with this user guide). Now you can access the container through your browser by opening this link (right-click and open in new tab or window): [localhost:8080](http://localhost:8080)

![Screenshot of terminal when container is up](https://tinyimg.io/i/qk68R7Q.JPG)

You can also see the container in the Docker desktop app under Containers/Apps.


### 2. Inside GUI: Set simulation settings ⚙️

Now you can open the graphical user interface (=GUI) at [localhost:8080](http://localhost:8080) for setting up and running simulations. This interface has access to the Docker container you started in step 1. If you are doing a quick test with [default platform settings](https://noresmhub.github.io/noresm-land-sites-platform/#settings-file), just pick a site and click  `create case` and then `run`. The GUI should look like this:

![https://tinyimg.io/i/7iQiTKP.JPG](https://tinyimg.io/i/7iQiTKP.JPG)

#### 2.1 Choose a site 

...by clicking either a button or a point on the map! 

If you need a different site, you can [request one via GitHub](https://github.com/NorESMhub/noresm-land-sites-platform/issues/new?assignees=&labels=enhancement&template=new-site.md&title=New+site+request%3A). NB! This requires the developers to do some manual work, so you should have a clear reason to request a new site.

#### 2.2 Download site data button (optional)

The `download site data` button allows you to download the input data for your selected site. The data is already available in the container, so you don't need to do this. We provide a notebook called `input_visualization.ipynb` which you can open in Jupyterlab on [localhost:8888](localhost:8888) to explore some of the data that goes in to a simulation. This is a good thing to do while your case is running (which might take some time).

#### 2.3 Create case

With the `create case` button, you can look at and edit some model settings and parameters as you create a new case. To run a simulation, you need to set up a [case](https://esmci.github.io/cime/versions/master/html/glossary/index.html#term-case-CASE "An instance of a model simulation. A case is defined by a component set, a model grid, a machine, a compiler, and any other additional customizations.") which tells the model how to run. A case can be run several times, or stopped and started again. For more detailed information on what goes on in CLM and its coupler (which connects CLM to other model components), see this [CIME user guide](https://esmci.github.io/cime/versions/master/html/users_guide/index.html), but note that the NorESM modelling platform uses these commands and scripts more indirectly. 

In the web UI, once you have chosen a site you get options to download site data (optional) and to create a new case. When you create a new case, you can change some model parameters as defined in the [variables_config.json](https://github.com/NorESMhub/noresm-land-sites-platform/blob/main/resources/config/variables_config.json) file described above. There are more customisation options for the models that advanced users can change manually, but for simplicity and explainability we have restricted the options in the UI and grouped them into Case, General, CLM namelist, History fields, and FATES settings. This is not an exhaustive list of possible changes (by far), but give you easy access to some options in the different tabs. All the boxes have default values for a quick but not especially realistic simulation.

|| Case settings   |
|-|----------------|
|Case name| Free text. Specify a name for your case. This will be combined with a unique ID number generated randomly. Good case names say something about e.g. site, settings, or simulation period. For example: ALP1-10yr-default, ADV-1000yr-400ppm, FNS-200yr-notreepft|
|Driver name | Dropdown. ['nuopc'](https://earthsystemmodeling.org/nuopc/) by default since 2021|

|| Run environment |
|-|----------------|
|Calendar | Dropdown. No-leap or Gregorian. Some input data (like our default GSWP3 data set) omits leap years because they complicate long simulations where input data is 'looped' (recycled) several times. Gregorian is the 'normal' calendar including leap years etc.|
| DATM_YR_START | Integer (year). Starting year to loop data over. Example: If the input data spans 1900-2010, set = 2000 to tell the model to use the input data from 2000 onwards.|
| DATM_YR_ALIGN | Integer (year). Used to align the simulation year with the forcing data set. It specifies the simulation year corresponding to DATM_YR_START. For example, setting DATM_YR_ALIGN=DATM_YR_START means that the forcing calendar will be the same as the model calendar: the forcing for a given model year would be the forcing of the same year. This is appropriate when the model calendar is set up to span the same year range as the forcing data. By setting it to the same year as RUN_STARTDATE (below), the forcing in the first year of the run will be the forcing of year DATM_YR_START.|
| DATM_YR_END | Integer (year). Ending year to loop data over. For example: If the input data spans 1900-2010, you may set this to 2005 to tell the model to use the input data up to 2005. If you run a longer simulation than the span between YR_START and YR_END, the input will be looped.|
|RUN_STARTDATE | Run start date (yyyy-mm-dd); normally at the start of the same year as YR_START. Only used for startup or hybrid run types.|
|STOP_N | Integer. Sets the run length together with STOP_OPTION. It's the number of STOP_OPTION to simulate. E.g. STOP_N=5 and STOP_OPTION=ndays sets a simulation period of 5 days.|
| STOP_OPTION | Dropdown. This sets the run length together with STOP_N, and represents the unit of time (years, months, days, seconds, or model timesteps). E.g. STOP_N=5 and STOP_OPTION=ndays sets a simulation period of 5 days.|
| STOP_DATE | Integer. Alternative yyyymmdd date option that sets the run length with STOP_OPTION and STOP_N. Negative value implies that this alternative is not used.|
| RUN_TYPE | Dropdown. Startup or Restart. *Startup*: a 'cold' start from bare ground, where the vegetation and climate is not in equilibrium and the model may  produce unrealistic output unless it is run for a very long time (hundreds or thousands of years). Startup mode does not allow using spin-up files (='restart' files of an existing simulation where the vegetation and other conditions have reached a steady state). Use this mode for quick testing, or for making your own spin-up. *Restart*: continues running an existing case after it has been stopped.|
|LND_TUNING_MODE| Dropdown. Land tuning mode. Tuning parameters and initial conditions for a CLM model version and meteorological forcing combination.|


||CLM namelist simulation settings|
|-|-------------------------------|
| co2_ppmv | Atmospheric CO2 concentration to use when co2_type is constant.|
| fates_spitfire_mode | If or how to use advanced fire behaviour with the SPITFIRE module. 0 : Simulations of fire are off, 1 : use a global constant lightning rate found in fates_params, 2 : use an external lightning dataset, 3 : use an external confirmed ignitions dataset (not available through standard CSEM dataset collection), 4 : use external lightning and population datasets to simulate both natural and anthropogenic ignitions. Read more [in the FATES documentation](https://fates-users-guide.readthedocs.io/en/latest/user/SPITFIRE-Namelist-Options.html)|
| use_bedrock | True/false. When true (default), a data set overwrites the CLM default soil depth. If present on surface dataset, use depth to bedrock information to specify spatially variable soil thickness. If not present, use bottom of soil column (nlevsoi). Read more in [Brunke et al.2016](https://journals.ametsoc.org/view/journals/clim/29/9/jcli-d-15-0307.1.xml)|

**History files**

By default, **the model records output in one tape** (hist_fincl1), **as one** (hist_mfilt=1) **average** (hist_avgflag_pertape=A), **monthly** (hist_nhtfrq=0) **value, for a subset of variables** (Active=T in [this list](https://escomp.github.io/ctsm-docs/versions/master/html/users_guide/setting-up-and-running-a-case/master_list_fates.html "Full list of possible CTSM History Fields with FATES")), **in a long-lat grid** (hist_dov2xy=TRUE). Each column in the History files tab corresponds to a history tape, which is a series of files created for the simulation period. If you want output to be recorded for [additional variables](https://escomp.github.io/ctsm-docs/versions/master/html/users_guide/setting-up-and-running-a-case/master_list_fates.html "Full list of possible CTSM History Fields with FATES") or at different time steps, you can modify the first column or fill in additional columns to add history tapes. If you want to run the model without saving any output, set hist_mfilt=0 in the first column. By modifying additional columns, you add tapes (series of files) with with e.g. different output variables recorded at its maximum value per day and in a long string instead of in the default lat-lon grid (some vegetation demographic output is only accessible in that format).

**FATES settings**

You can remove or modify Plant Functional Types (PFTs) by checking/unchecking PFTs and typing in custom values for a subset of parameters. The full list of parameters is in the [FATES model code](https://github.com/NGEET/fates/blob/master/parameter_files/fates_params_default.cdl).

| Index no. | Plant Functional Type |
|-----------|-----------------------|
|1| broadleaf_evergreen_tropical_tree|
|2| needleleaf_evergreen_extratrop_tree|
|3| needleleaf_colddecid_extratrop_tree|
|4| broadleaf_evergreen_extratrop_tree|
|5| broadleaf_hydrodecid_tropical_tree|
|6| broadleaf_colddecid_extratrop_tree|
|7| broadleaf_evergreen_extratrop_shrub|
|8| broadleaf_hydrodecid_extratrop_shrub|
|9| broadleaf_colddecid_extratrop_shrub|
|10| arctic_c3_grass|
|11| cool_c3_grass|
|12| c4_grass|


#### Start building your case with the ´SUBMIT´ button

Once you click `submit`, the case will appear in a list of cases with the case ID, status, creation date, grid information, component set, a link to view the settings you specified, and some buttons with more options. Pay attention to the status, which will transition from pending to ready when the case has been built.

### 3. Run your simulations 👩‍💻

Once the case is ready, you can start the simulation with ´run´. There is a button to ´download´ the output when the run is finished, and to ´edit´ the settings and create a new case, or ´delete´ the case. Depending of how long you asked the model to run for (default is 1 year), and your hardware, the simulation can take some time ⏳. On a regular laptop, one year of simulation of a single site (= gridcell) might take ~5-20 minutes. It may help to not run other heavy programs simultaneously (like GIS, Photoshop or 1000 browser tabs 👀). The Docker container you started in step 1 is performing the simulation using your local computer. If you are running more or longer simulations than your computer can handle, you may want to look at our [alternatives for remote simulations](https://noresmhub.github.io/noresm-land-sites-platform/).

> While you wait for the simulation to finish, you may like to inspect the input data more closely to understand what data drives the model. Open a new browser tab and go to the Docker container at [localhost:8888](http://localhost:8888). Navigate to the `notebooks/` folder and open the `input_visualization.ipynb` notebook. It will guide you through some of the input data for the model.

Soon your simulations will be finished! 🎉 

If you got an error message somewhere along the way, please head over to our GitHub and write an [issue](https://github.com/NorESMhub/noresm-land-sites-platform/issues/new) describing what happened, what machine you are on (mac/windows/HPC etc.), and copy in the error message and any other relevant information. 

Output will be stored at time intervals you set (default is monthly).

### 4. Look at your output 📈

Model output is most easily accessed in Jupyter lab, [localhost:8888](http://localhost:8888), but is stored locally in the noresm-land-sites-platform repository under `resources/cases/<case-id>/archive`. Optionally, output data can also be downloaded to another location with the Download Data button in the User Interface. 

In JupyterLab, nagivate to the `notebooks/` folder and open the `output_visualization.ipynb` notebook. It guides you through some ways of looking at the model output, though there are many other ways to do this and much more model output available!

[Output files](https://noresmhub.github.io/noresm-land-sites-platform/#postprocess) can alternatively be opened in Panoply (included for convenience under http://localhost:5800/ when containers are running), R, or using Python on your local computer.

### 5. Close the container

NB! The container will continue to run unless you stop it. When you are finished with simulations and output processing and downloading things you might need offline, go back to your terminal and press `Ctrl+c`.


## Troubleshooting

Please help us by reporting errors and questions on our [issues page](https://github.com/NorESMhub/noresm-land-sites-platform/issues/). Things you can try yourself include "switching it off and on again" by stopping, deleting, and reinstalling everything carefully.

If the container (or another process) is running in your terminal, you can stop it with `Ctrl+c`. To remove the repository, you can type `rm -r noresm-land-sites-platform` (if it complains about permissions you may need `sudo` in front, and you can use -rf instead of -r) in your working directory. It's also possible to delete everything manually in Docker desktop (check containers, images, and volumes), and to delete the repository manually from your working directory.

## Reproducibility
To make your simulations reproducible by others, e.g. for a thesis or scientific paper, *note down the version of the NorESM-LSP* and save these three directories that have been created under `resources/` in your working directory (e.g. C:/Users/yourusername/noresm-land-sites-platform/resources):

- the case folder, i.e. ´resources/cases/casename´
- the case input data, i.e. ´resources/data/casename´
- code modifications in overwrites, i.e. ´resources/overwrites´

You could also simply save the whole 'resources' folder (whith some redundant files).

## When and how to cite the NorESM-LSP
If you end up publishing your model experiments, e.g. in a thesis or scientific paper, we would like you to acknowledge the NorESM-LSP software and the development team behind it, and to properly cite the software and our upcoming technical description paper. See the [Contributing](https://noresmhub.github.io/noresm-land-sites-platform/contributing/) and [About](https://noresmhub.github.io/noresm-land-sites-platform/about/) section for more information. 


***************************************************


***Please help us by reporting errors and questions on our [issues page](https://github.com/NorESMhub/noresm-land-sites-platform/issues/new).***


***************************************************

[![NorESM](img/NORESM-logo.png "the Norwegian Earth System Model")](https://www.noresm.org/)
[![EMERALD](img/Emerald_darktext_whiteBG_small.png "EMERALD project")](https://www.mn.uio.no/geo/english/research/projects/emerald/)
[![LATICE](img/UiO_LATICE_logo_black_small.png "Land-ATmosphere Interactions in Cold Environments research group")](https://www.mn.uio.no/geo/english/research/groups/latice/)

