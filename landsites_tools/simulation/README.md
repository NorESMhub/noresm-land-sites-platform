# Instructions for executing NorESM Landsites simulations

All required information to prepare and run CLM-FATES cases for the available Norwegian land sites is provided via a `settings.txt` file. A template is provided in `~/NorESM_LandSites_Platform/landsites_tools/`. This template file is also the default input if you do not specify another `[my_settings].txt` file when executing the Python scripts. New settings files can also be created interactively for the available sites (see instructions below).

## 0. Set working directory
```
cd ~/landsites_tools/simulation/
```

# TL;DR
To run a demo simulation, follow these steps (requires the default `settings.txt` file to be present in `~/landsites_tools/`):
```
python3 make_cases.py
python3 run_cases.py
```
After it finishes, the case directories, CLM input, and simulation output will be in `~/data/`.

## 1. Settings files

### 1.1 Create a new settings file
To create a new settings file, execute the `make_cases.py` script as follows:
```
python3 make_cases.py -i [-n my_file_name.txt]
```
The `-i` flag will run the "interactive" mode. All sites that are included in the current version of the repository will be printed to the terminal. Subsequently, the user can enter which sites they want to simulate and these will be automatically added to a newly created settings file. The optional `-n my_file_name.txt` flag may be used to give a custom name to the new file - if this flag is omitted, the file name will be based on the current UNIX timestamp. Example:
```
python3 make_cases.py -i -n ALP_sites_settings.txt
```
### 1.2 Edit the settings file
Use an editor of your choice (e.g., VIM, Notepad++, etc.) to open the `settings.txt` file that you want to use for your simulation. Here, you can control the model settings (e.g., start and end time) and specify the input/output directories. A detailed description about model parameters will be given elsewhere.

## 2. Preparing cases
Once you adjusted the `settings.txt` file to your needs, it can be used to prepare the model runs. This is how how you specify which settings file to use:
```
python3 make_cases.py -f [name_of_settings_file].txt [-p path_to_file]
```
The `-p` flag, i.e., the path to the settings file, is optional. It is only needed if it is not stored in the default folder for custom settings files, i.e., `~/landsites_tools/custom_settings/`.

---

The following steps will be executed:

1. Download/check the input data
    - Input data for the available land sites will be automatically downloaded from `https://ns2806k.webs.sigma2.no/EMERALD/EMERALD_platform/inputdata_fates_platform/`
    - You can specify the corresponding target root directory in the settings file
    - You can provide a custom suffix (name ending that will be added to all input dirs) via the `-sfxi [_my_suffix]` flag, default is `_input`.
2. Create case directories
    - You can specify the corresponding root directory in the settings file
    - You can provide a custom suffix (name ending that will be added to all case dirs) via the `-sfxc [_my_suffix]` flag, default is none.
3. Change CLM and FATES parameters
    - Changes all parameter values for CLM and FATES that are provided in the settings file
4. Create custom output directories
    - You can specify the corresponding root directory in the settings file
    - You can provide a custom suffix (name ending that will be added to all output dirs) via the `-sfxo [_my_suffix]` flag, default is `_output`.
5. "Set up" and "build" cases
    - Executes `./case.setup` and `./case.build`
6. Automatically add specified case, input, and output directories to the settings.txt file

## 3. Running cases
Once you sucessfully executed all previous steps, you can start the simulations via:
```
python3 run_cases.py -f [my_settings].txt [-q]
```
The `-q` (quiet) flag suppresses a warning that reminds the user to check if all necessary steps were executed. After the run terminates, you can find the output in the directories specified in the `[my_settings].txt` file.
