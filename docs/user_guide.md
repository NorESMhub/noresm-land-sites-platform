# User guide

🚧 *under construction - please help us by reporting errors and questions on our [issues page](https://github.com/NorESMhub/NorESM_LandSites_Platform/issues/new)*

This is the user guide for running point simulations with the [NorESM LandSites Platform](https://noresmhub.github.io/NorESM_LandSites_Platform/) 🌍

You will need to open two new windows in the process, so it's a good idea to make this window narrower so you can see both the user guide and another window next to it.

***********************************************

## TL;DR quick start 🏃‍♀️🏃‍♂️

go to repository: 
    
    $ cd NorESM_LandSites_platform
    
start the Docker container: 
    
    $ docker-compose up
    
go to container: [localhost:8888](http://localhost:8888) password = pass

open New Terminal and run commands:

    $ cd landsites_tools/simulation
    $ python make_cases.py
    $ python run_cases.py
 
access output in data/output 🎉


***********************************************

## Step by step guide for using the platform


### 0. Prerequisites (first time setup) 🌱

To use the NorESM LandSites Platform, you need to install 

- [Git](https://git-scm.com/downloads "click the pc screen button if you are on Windows") and 
- [Docker desktop](https://www.docker.com/products/docker-desktop) 

before you can clone the [repository](https://github.com/NorESMhub/NorESM_LandSites_Platform "repository for the NorESM LandSites platform") (= download platform scripts) and start working with the Docker container.

Open file explorer and find a suitable folder to serve as working directory. This is where you will store the repository and installation files needed by the platform, as well as your output files (which may take up quite a bit of space!). Your working directory should be somewhere on your C: drive, for instance `C:/Users/yourusername`.

When you are in your chosen working directory, right-click and choose "Git Bash here". In the terminal that pops up, paste in the following line by right-clicking:
    
    $ git clone https://github.com/NorESMhub/NorESM_LandSites_Platform.git

This will download (= clone) the repository (= folder structure and files) to your working directory. You can now see the folder and files in your file explorer. Most of the files can be opened in a text editor like Notepad if you want to look at their contents. 

Once Git, Docker desktop and the repository are in place, you don't have to do this again. If the platform is updated the next time you want to use it, you probably want to update the repository by typing `git pull` into Git Bash from your local clone of the repository. 


### 1. Start the container 🧰


In the working directory where you have cloned the repository, open a terminal by right-clicking and choosing "Git Bash here". Make sure you are in the folder containing the `docker-compose.yml` file (type `ls` and hit enter to list the files in the current folder). Then write this command and hit enter to get the container up and running:

    $ docker-compose up

This will download some files and give you this address to access the container through your browser. 
To set custom simulation settings, start by going to <http://localhost:5006/make_settings>, 
or, if you want to skip that step, you can go directly to [localhost:8888](http://localhost:8888) and use the password: pass.

<p style="color:d0d0d0">To stop the container running press Ctrl+c (in Git Bash).</p>


### 2. Set simulation settings ⚙️

If you are doing a quick test with [default platform settings](https://noresmhub.github.io/NorESM_LandSites_Platform/#settings-file), you can skip this step and go straight to [3. running your simulations](https://noresmhub.github.io/NorESM_LandSites_Platform/user_guide/#run-your-simulations).  

To set custom [simulation settings](https://noresmhub.github.io/NorESM_LandSites_Platform/#settings-file), start by going to <http://localhost:5006/make_settings> in your browser. The page is quite self-explanatory and produces a new settings file that will be used to make [cases](https://noresmhub.github.io/NorESM_LandSites_Platform/#make_casespy "case= an instance of the model") that you can run. The different options are explained further in our [documentation](https://noresmhub.github.io/NorESM_LandSites_Platform/#settings-file).

(This step can also be done from a terminal by using make_cases.py interactively with the `-i` flag. -- see [this guide](https://github.com/NorESMhub/NorESM_LandSites_Platform/main/landsites_tools/simulation/README.md "how to use make_cases interactively"))


### 3. Run your simulations 👩‍💻

Go to the container in your browser: [localhost:8888](http://localhost:8888) and use the password: pass to enter it. You will see a lot of files and folders (= the GitHub repository contents). Navigate to the `notebooks` folder and open the one called `run_simulations.ipynb`. It's an interactive Jupyter notebook where you can execute code in cells by clicking the play/run buttons next to them, and see the output directly below the cell. 

Alternatively, you can start a terminal in the container and execute the following commands. If you have changed the settings file (from step 2), add it like you see in the square brackets:

    $ cd landsites_tools/simulation
    $ python make_cases.py [-f yoursettingsfile.txt]
    $ python run_cases.py [-f yoursettingsfile.txt]

You are now running the model! 🎉 
Depending on your settings and machine hardware, making the case(s) and running the simulations might take some time to complete. ⏳

If you got an error message somewhere along the way, please head over to our GitHub and write an [issue](https://github.com/NorESMhub/NorESM_LandSites_Platform/issues/new) describing what happened, what machine you are on (mac/windows/HPC etc.), and copy in the error message. 

Output will be stored at time intervals set by the settings file (default = monthly) in the `data/output` folder. 
From there you can download them to your local computer, or continue working with them inside the container.

To stop the container once the simulations are complete (but not before!), go back to your Git Bash terminal and press Ctrl+c.

### 4. Look at your output 📈

Output is stored in the `data/output` folder.

An example of how you can visualize it with python is provided in the `plot_example.ipynb` Jupyter notebook insite the `notebooks` folder. You can execute this notebook directly in the container in the same way as the `run_simulations.ipynb` notebook. 

[Output files](https://noresmhub.github.io/NorESM_LandSites_Platform/#postprocess) can also be opened in Panoply, R, or using python on your local computer.



<p style="color:green">🌲 ____ 🌳 ____ 🌲 ____ 🌳 ____ 🌲 ____ 🌳 ____ 🌲 ____ 🌳 ____ 🌲</p>


***************************************************

[![NorESM logo](https://tinyimg.io/i/9AdhM6J.png "the Norwegian Earth System Model")](https://www.noresm.org/)
[![EMERALD logo](https://tinyimg.io/i/O6Vkl1F.png "EMERALD project")](https://www.mn.uio.no/geo/english/research/projects/emerald/)
[![LATICE logo](https://tinyimg.io/i/4IM1ogh.png "Land-ATmosphere Interactions in Cold Environments research group")](https://www.mn.uio.no/geo/english/research/groups/latice/)
