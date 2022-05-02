# User guide

🚧 *under construction - please help us by reporting errors and questions on our [issues page](https://github.com/NorESMhub/NorESM_LandSites_Platform/issues/new)*

This is the user guide for running point simulations with the [NorESM LandSites Platform](https://noresmhub.github.io/NorESM_LandSites_Platform/) 🌍

You will need to open two new windows in the process, so it's a good idea to use two screens or to make this window narrower so you can see both the user guide and another window next to it.

***********************************************

## TL;DR quick start 🏃‍♀️🏃‍♂️

go to repository and start the Docker container: 
    
    $ cd NorESM_LandSites_platform 
    $ docker-compose up
    
go to container GUI: [localhost:8080](http://localhost:8080) and push buttons

access jypyter notebooks on [localhost:8888](http://localhost:8888) 🎉


***********************************************

## Step by step guide for using the platform


### 0. Prerequisites (first time setup) 🌱

To use the NorESM LandSites Platform, you need to install 

- [Git](https://git-scm.com/downloads "click the pc screen button if you are on Windows") (and make a user account) and
- [Docker desktop](https://www.docker.com/products/docker-desktop) 

before you can clone the [repository](https://github.com/NorESMhub/NorESM_LandSites_Platform "repository for the NorESM LandSites platform") (= download platform scripts) and start working with the Docker container. If you don't want to make a GitHub account, you may try to download and unpack the repository manually instead with the `code` button and ´download zip´.

Open file explorer and find a suitable folder to serve as working directory. This is where you will store the repository and installation files needed by the platform, as well as your output files (which may take up quite a bit of space!). Your working directory should be somewhere on your C: drive, for instance `C:/Users/yourusername`.

When you are in your chosen working directory, right-click and choose "Git Bash here". In the terminal that pops up, paste in the following line by right-clicking:
    
    $ git clone https://github.com/NorESMhub/NorESM_LandSites_Platform.git

This will download (= clone) the repository (= folder structure and files) to your working directory. You can now see the folder and files in your file explorer. Most of the files can be opened in a text editor like Notepad if you want to look at their contents.

Once Git, Docker desktop and the repository are in place, you don't have to do this again. If the platform has been updated the next time you want to use it, you might want to download the updates by typing `git pull` into Git Bash from your local clone of the repository. 

### 1. Start the container 🧰

In the working directory where you have cloned the repository, open a terminal by right-clicking and choosing "Git Bash here" (or use the one you already have open if you just did the first time setup). Make sure you are in the folder containing the `docker-compose.yml` file (type `ls` and hit enter to list the files in the current folder; if you see NorESM_LandSites_Platform, you need to change directory by typing ´cd NorESM_LandSites_Platform´). Then write this command and hit enter to get the container up and running:

    $ docker-compose up

This will download some files and give you the following address to access the container through your browser: [localhost:8080](http://localhost:8080)

(To stop the container running press ´Ctrl+c´ (in Git Bash). NB! Only do this after you have finished this tutorial.)


### 2. Inside GUI: Set simulation settings ⚙️

You should now be looking at the graphical user interface (=GUI) for setting up and running simulations. This interface has access to the Docker container you started in step 1.

(If you are doing a quick test with [default platform settings](https://noresmhub.github.io/NorESM_LandSites_Platform/#settings-file), just pick a site and click  ´create case´ and then ´run´.)

#### 2.1 Download site data button

The ´download site data´ button gives you the input data for your selected site. We provide a notebook called ´input_visualization.ipynb´ which you can open in Jupyterlab on [localhost:8888](localhost:8888) to explore some of the data that goes in to a simulation. This is a good thing to do while your case is running (which might take some time).

#### 2.2 Create case button

With the ´create case´ button, you can look at and edit some model settings and parameters as you create a new case. This is not an exhaustive list of possible changes (by far), but give you easy access to some options. All the boxes have default values for a quick but not especially realistic simulation.

The box that pops up with settings has three tabs:

- **CTSM** (=Community Terrestrial Systems Model) settings for general options like simulation period
- **CLM / Namelist** settings where you can change CO2 concentration, switch some modules of/off, and modify how some model output is stored.
- **FATES** parameters where you can disable some Plant Functional Types (PFTs) and change some vegetation parameters.

Once you have created your case with the desired settings, it will appear in a list of cases with the case ID, status, creation date, grid information, component set, a link to view the settings you specified, and some buttons with more options. Pay attention to the Status, which will transition from pending to ready when the case has been built. 

### 3. Run your simulations 👩‍💻

Once the case is ready, you can start the simulation with ´run´, ´download´ the output when the run is finished, ´edit´ the settings and create a new case, or ´delete´ the case. Depending of how long you asked the model to run for (default is 1 year), and your hardware, this can take some time ⏳. On a regular laptop, one year of simulation of a single site (= gridcell) might take ~5-20 minutes. Make sure your computer is not running other heavy programs simultaneously (like GIS, Photoshop or 1000 open browser tabs 👀). The Docker container you started in step 1 is performing the simulation using your local computer. Advanced users may also use the container on an HPC cluster to speed up long simulations.

> While you wait for the simulation to finish, you may like to inspect the model input data more closely. Open a new browser tab and go to the Docker container at [localhost:8888](http://localhost:8888). Navigate to the ´notebooks´ folder and open the ´input_visualization.ipynb´ notebook. It will guide you through some of the input data for the model. 

Soon your simulations will be finished! 🎉 

If you got an error message somewhere along the way, please head over to our GitHub and write an [issue](https://github.com/NorESMhub/NorESM_LandSites_Platform/issues/new) describing what happened, what machine you are on (mac/windows/HPC etc.), and copy in the error message and any other relevant information. 

Output will be stored at time intervals you set (default is monthly), and can be downloaded from the container with the ´download´ button or viewed in the `data/output` folder in the container. 

### 4. Look at your output 📈

Output is stored in the `data/output` folder in the container, accessed on [localhost:8888](http://localhost:8888).

Nagivate to the ´notebooks´ folder and open the ´output_visualization.ipynb´ notebook. It guides you through some ways of looking at the model output, though there are many other ways to do this and much more model output available!

[Output files](https://noresmhub.github.io/NorESM_LandSites_Platform/#postprocess) can alternatively be opened in Panoply, R, or using python on your local computer.

### 5. Close the container

NB! The container will continue to run unless you stop it. When you are finished with simulations and output processing and downloading things you might need offline, go back to your Git Bash terminal and press ´Ctrl+c´.


    🌲 ____ 🌳 ____ 🌲 ____ 🌳 ____ 🌲 ____ 🌳 ____ 🌲 ____ 🌳 ____ 🌲


***************************************************

[![NorESM logo](https://tinyimg.io/i/9AdhM6J.png "the Norwegian Earth System Model")](https://www.noresm.org/)
[![EMERALD logo](https://tinyimg.io/i/O6Vkl1F.png "EMERALD project")](https://www.mn.uio.no/geo/english/research/projects/emerald/)
[![LATICE logo](https://tinyimg.io/i/4IM1ogh.png "Land-ATmosphere Interactions in Cold Environments research group")](https://www.mn.uio.no/geo/english/research/groups/latice/)
