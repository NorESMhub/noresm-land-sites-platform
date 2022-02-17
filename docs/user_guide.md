# User guide

*under construction*

This is the user guide for running point simulations with the [NorESM LandSites Platform](https://noresmhub.github.io/NorESM_LandSites_Platform/) :earth_africa: 

***********************************************

## TL;DR quick start :running_woman::runner:

go to repository: 
    
    > cd NorESM_LandSites_platform
    
start the Docker container: 
    
    > docker-compose up
    
go to container: <localhost:8888> password = pass

open New Terminal and run commands:

    > cd landsites_tools/simulation
    > python make_cases.py
    > run_cases.py
 
access output in data/output :tada:


***********************************************

# Step by step guide for using the platform

### 0. Prerequisites (first time setup) :seedling:

To use the NorESM LandSites Platform, you need to install 

1. [Git](https://git-scm.com/downloads "click the pc screen button if you are on Windows") and 
2. [Docker desktop](https://www.docker.com/products/docker-desktop) 

before you can clone the [repository](https://github.com/NorESMhub/NorESM_LandSites_Platform "repository for the NorESM LandSites platform") (= download platform scripts) and start working with the Docker container. On your computer, 

3. open file explorer and find a suitable folder to serve as working directory. This is where you will store the repository and ~2(?)GB needed by the platform, as well as your output files (which may take up quite a bit more space!). Your working directory should be somewhere on your C: drive, for instance `C:/Users/yourusername`, and it should *not* be e.g. on OneDrive or shared servers or System32. 
4. When you are in your chosen working directory, right-click and choose "Git Bash here". In the terminal that pops up, paste in the following line by right-clicking:
    
    > git clone https://github.com/NorESMhub/NorESM_LandSites_Platform.git

This will download (= clone) the repository (= folder structure and files) to your working directory. You can now see the folder and files in your file explorer. Most of the files can be opened in a text editor like Notepad if you want to look at their contents. 

Once Git, Docker desktop and the repository are in place, you don't have to do this again. If the platform is updated the next time you want to use it, you probably want to update the repository by typing `git pull` into Git Bash from your local clone of the repository. 

### 1. Start the container :toolbox: 

In the working directory where you have cloned the repository, open a terminal by right-clicking and choosing "Git Bash here". Make sure you are in the folder containing the `docker-compose.yml` file (type `ls` and hit enter to list the files in the current folder). Then write this command and hit enter to get the container up and running:

    > docker-compose up

This will download some files and give you this address to access the container through your browser. 
To set custom simulation settings, start by going to <http://localhost:5006/make_settings>, 
or, if you want to skip that step, you can go directly to <localhost:8888> and use the password: pass.

<p style="color:d0d0d0">To stop the container running press Ctrl+c (in Git Bash).</p>

### 2. Set simulation settings :gear:

If you are doing a quick test with [default platform settings](https://noresmhub.github.io/NorESM_LandSites_Platform/#settings-file), you can skip this step and go straight to [3. running your simulations](https://noresmhub.github.io/NorESM_LandSites_Platform/user_guide/#run-your-simulations).  

To set custom [simulation settings](https://noresmhub.github.io/NorESM_LandSites_Platform/#settings-file), start by going to <http://localhost:5006/make_settings> in your browser. The page is quite self-explanatory and produces a new settings file that will be used to make [cases](https://noresmhub.github.io/NorESM_LandSites_Platform/#make_casespy "case= an instance of the model") that you can run. The different options are explained further in our [documentation](https://noresmhub.github.io/NorESM_LandSites_Platform/#settings-file).

(This step can also be done from a terminal by using make_cases.py interactively with the `-i` flag. -- see [this guide](https://github.com/NorESMhub/NorESM_LandSites_Platform/main/landsites_tools/simulation/README.md "how to use make_cases interactively"))

### 3. Run your simulations :man_technologist::woman_technologist:

Go to the container in your browser: <localhost:8888> and use the password: pass to enter the container. You will see a lot of files and folders (= the GitHub repository contents). 

Start running simulations by starting a terminal: In the upper right corner, click the `New` button and choose `Terminal` from the dropdown. There, you write (or copy+paste)...

    > cd landsites_tools/simulation
    > python make_cases.py

This will take a while, so grab a cup :coffee: and wait until the process stops and the last line ends with $ (for example `[user@a23ljnsdf234 simulation]$`), indicating you can enter new commands. You have now set up the cases you want! 


If you got an error message, head over to our GitHub and write an [issue](https://github.com/NorESMhub/NorESM_LandSites_Platform/issues/new) describing what happened, what machine you are on (mac/windows/HPC etc.), and copy in the error message. 
Providing everything went OK, you can now start the simulations with:

    > python run_cases.py

You are now running the model! Depending on your settings and machine hardware, the simulations might take some time to complete. :hourglass_flowing_sand:
As before, you know the model has finished running when you get back the command line ([user@... simulation]$ ) in the terminal, and there are no error messages. 

Output will be stored at time intervals set by the settings file (default = monthly) in the `data/output` folder. :tada: 

To stop the container running press Ctrl+c (in Git Bash).

### 4. Look at your output :chart_with_upwards_trend:

Output is stored in the `data/output` folder. 

An example of how you can visualize it with python is provided in the `plot_example.ipynb` Jupyter notebook insite the `notebooks` folder. You can execute this notebook directly in the container. 

[Output files](https://noresmhub.github.io/NorESM_LandSites_Platform/#postprocess) can also be opened in Panoply, R, or using python on your local computer.



🌲    🌳    🌲    🌳    🌲    🌳    🌲    🌳    🌲    🌳    🌲    🌳    🌲

***************************************************

[![NorESM logo](https://tinyimg.io/i/9AdhM6J.png "the Norwegian Earth System Model")](https://www.noresm.org/)
[![EMERALD logo](https://tinyimg.io/i/O6Vkl1F.png "EMERALD project")](https://www.mn.uio.no/geo/english/research/projects/emerald/)
[![LATICE logo](https://tinyimg.io/i/4IM1ogh.png "Land-ATmosphere Interactions in Cold Environments research group")](https://www.mn.uio.no/geo/english/research/groups/latice/)
