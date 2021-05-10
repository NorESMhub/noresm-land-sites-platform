import argparse
import sys
import os
import json
import re
import subprocess
from pathlib import Path
from interface_settings import InterfaceSettings
import pandas as pd
pd.set_option('display.max_colwidth', None)

########################################################################################################
######################################### Argument parser setup ########################################
########################################################################################################

# Describe command line tool
description = "This tool creates, builds, and sets up CTSM cases " + \
"for the NorESM LandSites Platform. It is either using the cases specified " + \
"in the settings.txt file or interactive command line input."

# Initiate the parser
parser = argparse.ArgumentParser(description=description)

## Define arguments that need to be provided
#parser.add_argument("--version", help="show tool version", action="store_true")
parser.add_argument("-s","--case_name_suffix", help="optional suffixes for created case folder names", 
                    default="")
parser.add_argument("-p","--cfg_path", help="path to configuration file", 
                    default="../")
parser.add_argument("-f","--cfg_file", help="configuration file to use, default is 'settings.txt'",
                    default="settings.txt")
parser.add_argument("-i","--interactive", help="display and prepare cases interactively", 
                    action="store_true")
parser.add_argument("-n","--name_output_file", help="name of output settings file (interactive only)", 
                    default="interactive_settings.txt")

args = parser.parse_args()

########################################################################################################
############################################## File input ##############################################
########################################################################################################

### If not in interactive mode...
if not args.interactive:
    
    ### Check if the provided config path and file exist
    try:
        if not Path(args.cfg_path).is_dir():
            raise ValueError("Path does not exist.")
        
        ### Save full path to cfg file
        cfg_file_path = Path(args.cfg_path) / args.cfg_file
        if not cfg_file_path.is_file():
            raise ValueError("File does not exist at specified path.")
    
    except ValueError:
        print("Error in command line input!")
        raise
        
    ### Load settings file
    settings = InterfaceSettings(cfg_file_path)
    print(f"\nTrying to prepare sites specified in cfg file {args.cfg_file}...\n{settings.sites2run}")
    cases_to_build = settings.sites2run
    #print(f"\nsites specified in cfg file:\n{settings.parser['user']['sites2run']}")

########################################################################################################
########################################### Interactive mode ###########################################
########################################################################################################
        
else:
    ##### Start by printing all available cases to the console #####
    
    ### Read site info json file
    try:
        with open(Path("../config/sites.json"), "r") as json_file:
            cases_dict = json.load(json_file) # use `json.loads` to do the reverse
    except Exception as error:
        print("Error when reading the site information json file (../config/sites.json)!")
        raise error
        
    print("\n")
    print("*********************** Available cases ***********************")
    
    ### Function to print site info in table format
    def print_table_row(ind, name, res, lat, lon):
        print('|','%-7s' % str(ind), '|', '%-10s' % str(name), '|', '%-10s' % str(res),
              '|', '%-10s' % str(lat), '|', '%-10s' % str(lon),'|')
    
    ### Print table header...
    print('---------------------------------------------------------------')
    print_table_row("Index", "Name", "Resolution", "Lat.", "Lon.")
    print('---------------------------------------------------------------')
    
    ### Store results also in DataFrame
    cases_df = pd.DataFrame(columns=["name","res","lat","lon"])
    
    ### Loop through keys in site dictionary...
    for idx,case in enumerate(cases_dict.keys()):
        print_table_row(idx, cases_dict.get(case).get("name"), cases_dict.get(case).get("res"),
                        cases_dict.get(case).get("lat"), cases_dict.get(case).get("lon"))
        # Store in df
        cases_df.loc[idx,"name"] = cases_dict.get(case).get("name")
        cases_df.loc[idx,"res"] = cases_dict.get(case).get("res")
        cases_df.loc[idx,"lat"] = cases_dict.get(case).get("lat")
        cases_df.loc[idx,"lon"] = cases_dict.get(case).get("lon")
        
    print('---------------------------------------------------------------')
    
    
    ##### Get user input #####
    # Variable to evaluate user input
    build_in = "o"
    ### Repeat until choice is correct
    while(build_in == "o"):
        
        case_indices_str = \
        input("Enter the indices of the cases to build (seperated by space or comma if more than one): ")

        ### Turn into list, remove duplicates, check for valid input
        try:

            case_indices_int = [int(idx) for idx in re.split('[ ,;]+', case_indices_str)]
            case_indices_int = sorted(list(set(case_indices_int)))

            ### Any value out of array range?
            if max(case_indices_int) >= cases_df.shape[0] or min(case_indices_int) < 0:
                raise ValueError("At least one index out of range.")

        except ValueError:
            print("")
            print("Please only enter integers within index range!")
            print("")
            raise

        ### Make sure input is correct
        build_in = ""
        while build_in not in ["y", "o", "a"]:
            build_in = input("Prepare the following cases: " \
                             + ", ".join([cases_df.loc[idx,"name"] for idx in case_indices_int]) \
                             + "? ([y]es/[o]ther/[a]bort)")
    ### Exit if entered
    if build_in == "a":
        sys.exit("Exit by user.")
        
    ### Store names of cases
    cases_to_build = cases_df.loc[case_indices_int,"name"]
    
    
    ################################### Machine configs ###################################
    
    ### Should also be stored in an external file!
    machine_cfgs_df = pd.DataFrame(columns=["name","machine","compset","project"])
    saga = pd.Series(data={"name":"SAGA", 
                           "machine":"saga",
                           "compset":"2000_DATM%1PTGSWP3_CLM50%FATES_SICE_SOCN_MOSART_SGLC_SWAV", 
                           "project":"nn2806k"}, name="saga")
    fram = pd.Series(data={"name":"Fram",
                           "machine":"fram",
                           "compset":"2000_DATM%1PTGSWP3_CLM50%FATES_SICE_SOCN_MOSART_SGLC_SWAV", 
                           "project":"nn2806k"}, name="fram")
    galaxy = pd.Series(data={"name":"GALAXY",
                             "machine":"espresso",
                             "compset":"2000_DATM%1PTGSWP3_CLM50%FATES_SICE_SOCN_MOSART_SGLC_SWAV", 
                             "project":""}, name="galaxy")

    machine_cfgs_df = machine_cfgs_df.append(saga, ignore_index=True)
    machine_cfgs_df = machine_cfgs_df.append(fram, ignore_index=True)
    machine_cfgs_df = machine_cfgs_df.append(galaxy, ignore_index=True)

    ### Ask user which machine config to use
    print(machine_cfgs_df)
    machine_idx_str = input("Choose a machine configuration (index): ")

    ### Turn into list, remove duplicates, check for valid input
    try:
        machine_idx_int = [int(idx) for idx in re.split('[ ,;]+', machine_idx_str)]
        machine_idx_int = sorted(list(set(machine_idx_int)))

        ### Any value out of array range?
        if max(machine_idx_int) >= machine_cfgs_df.shape[0] or min(machine_idx_int) < 0 or len(machine_idx_int) > 1:
            raise ValueError("Index out of range or more than one given.")

    except ValueError:
        print("")
        print("Please only enter integers within index range!")
        print("")
        raise
    
    ### Save values
    compset_str = str(machine_cfgs_df.loc[machine_idx_int,"compset"].to_string(index=False))
    machine_str = str(machine_cfgs_df.loc[machine_idx_int,"machine"].to_string(index=False))
    project_str = str(machine_cfgs_df.loc[machine_idx_int,"project"].to_string(index=False))
        
    
    ###############################################
    ### WRITE USER CHOICES TO NEW SETTINGS FILE ###
    ###############################################
    
    settings = InterfaceSettings("../settings.txt")
    
    settings.parser['user']['sites2run'] = \
    ", ".join([cases_df.loc[idx,"name"] for idx in case_indices_int])
    
    ### ALSO ADD COMPSET ETC. WHEN DECIDED HOW TO STRUCTURE
    
    # write the modified settings to a new file
    settings.write_file(f"../{args.name_output_file}")

    
########################################################################################################
########################################### Build the cases ############################################
########################################################################################################

print("\nStart creating cases...\n")

### Check if cases folder exists, otherwise create it
try:
    if not Path('../nlp_cases').is_dir():
        Path('../nlp_cases').mkdir(parents=True, exist_ok=True)
except:
    print("Error when creating case folder!")
    raise

path = '../nlp_cases/'
res = '1x1_'
if args.case_name_suffix != "":
    suffix = "_" + args.case_name_suffix
else:
    suffix = ""

### store names of case folders
case_folders = []

for case_str in cases_to_build:
    
    ### Different syntax w and without project given
    if project_str != "":
        
        ### Store path to cur folder
        case_folders.append(str(path + case_str + suffix))
        #print(f"{path + case_str + args.case_name_suffix}")
        
        ### bash cmd string
        bashCommand = \
        f"../noresm2/cime/scripts/create_newcase --case {path + case_str + suffix} --compset {compset_str} --res {res+case_str} --machine {machine_str} --run-unsupported --project {project_str}"
        
        process = subprocess.Popen(bashCommand.split(), stdout=subprocess.PIPE)
        output, error = process.communicate()
        
    else:
        
        case_folders.append(str(path + case_str + suffix))
        
        bashCommand = \
        f"../noresm2/cime/scripts/create_newcase --case {path + case_str + suffix} --compset {compset_str} --res {res+case_str} --machine {machine_str} --run-unsupported"
        
        process = subprocess.Popen(bashCommand.split(), stdout=subprocess.PIPE)
        output, error = process.communicate()
        
print("\nCases created succesfully.\n")

##############################################
print("\nStart building cases...\n")

### Create and build cases
for case_dir in case_folders:
    
        bashCommand = f"cd {case_dir} ; ./case.setup ; ./case.build"
        process = subprocess.Popen(bashCommand.split(), stdout=subprocess.PIPE)
        output, error = process.communicate()
        
	
        #bashCommand = "./case.setup"
        #process = subprocess.Popen(bashCommand, stdout=subprocess.PIPE)
        #output, error = process.communicate()
        
        #bashCommand = "./case.build"
        #process = subprocess.Popen(bashCommand, stdout=subprocess.PIPE)
        #output, error = process.communicate()
        

print("\nCases built succesfully.\n")
