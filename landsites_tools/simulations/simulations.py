import argparse
import sys
import os
import json
import re
import subprocess
from pathlib import Path, PurePosixPath
from landsites_tools.interface_settings import InterfaceSettings
import pandas as pd
pd.set_option('display.max_colwidth', None)

### Import helper functions
import helpers as hlp

################################################################################
############################ Argument parser setup #############################
################################################################################

# Describe command line tool
description = "This tool creates, builds, and sets up CTSM cases for " + \
"predefined or custom sites (see README.md). It is either using the site " + \
"names specified in a 'settings.txt' file or interactive command line input."

# Initiate the parser
parser = argparse.ArgumentParser(description=description)

## Define arguments that need to be provided
#parser.add_argument("--version", help="show tool version", action="store_true")
parser.add_argument("-s","--case_name_suffix",
                    help="optional suffixes for created case folder names",
                    default="")
parser.add_argument("-p","--cfg_path", help="path to configuration file",
                    default="../")
parser.add_argument("-f","--cfg_file",
                    help="configuration file to use, default is 'settings.txt'",
                    default="settings.txt")
parser.add_argument("-i","--interactive",
                    help="display and prepare cases interactively",
                    action="store_true")
parser.add_argument("-n","--name_output_file",
                    help="name of output settings file (interactive only)",
                    default="interactive_settings.txt")

args = parser.parse_args()

################################################################################
################################## File input ##################################
################################################################################

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
    def_settings = InterfaceSettings(cfg_file_path)
    print(f"\nTrying to prepare sites specified in cfg file " + \
    f"'{args.cfg_file}'...\n{def_settings.sites2run}")

    ### Load site information from settings file
    cases_to_build = def_settings.sites2run
    cases_df = def_settings.sites_df

    ### All available cases
    nlp_cases = cases_df["name"].values

    for cur_case in cases_to_build:
        if cur_case not in cases_df["name"]:
            raise ValueError(f"Case '{cur_case}' is not valid! Valid cases:\n"+\
            f"{nlp_cases}")

    ### SET CASE CMD STRINGS HARDCODED FOR TESTING, REMOVE WITH NEW SETUP
    compset_str = "2000_DATM%1PTGSWP3_CLM50%FATES_SICE_SOCN_MOSART_SGLC_SWAV"
    machine_str = "saga"
    project_str = "nn2806k"

################################################################################
############################### Interactive mode ###############################
################################################################################

else:
    ### Read default settings file
    def_settings = InterfaceSettings(Path(__file__).absolute() \
    .parent.parent.parent / "settings.txt")
    ### Load case DataFrame
    cases_df = def_settings.sites_df

    ### Print cases
    print("\n")
    print("*********************** Available cases ***********************")

    ### Function to print site info in table format
    def print_table_row(ind, name, res, lat, lon):
        print('|','%-7s' % str(ind), '|', '%-10s' % str(name), '|',
         '%-10s' % str(res), '|', '%-10s' % str(lat), '|',
         '%-10s' % str(lon),'|')

    ### Print table header...
    print('---------------------------------------------------------------')
    print_table_row("Index", "Name", "Resolution", "Lat.", "Lon.")
    print('---------------------------------------------------------------')

    ### Loop through keys in site dictionary...
    for idx,(_,case) in enumerate(cases_df.iterrows()):
        print_table_row(idx, case["name"], case["res"],
        case["lat"], case["lon"])

    print('---------------------------------------------------------------')


    ##### Get user input #####
    # Variable to evaluate user input
    build_cases_user_input = "o"
    ### Repeat until choice is correct
    while(build_cases_user_input == "o"):

        case_indices_str = \
        input("Enter the indices of the cases to build "+\
        "(seperated by space or comma if more than one): ")

        ### Turn into list, remove duplicates, check for valid input
        try:

            case_indices_int = [int(idx) for idx in re.split('[ ,;]+',
             case_indices_str)]
            case_indices_int = sorted(list(set(case_indices_int)))

            ### Any value out of array range?
            if max(case_indices_int) >= cases_df.shape[0] or \
            min(case_indices_int) < 0:
                raise ValueError("At least one index out of range.")

        except ValueError:
            print("\nPlease only enter integers within index range!\n")
            raise

        case_idx = cases_df.index[case_indices_int]
        ### Make sure input is correct
        build_cases_user_input = ""
        while build_cases_user_input not in ["y", "o", "a"]:
            build_cases_user_input = input("Prepare the following cases: " \
            +", ".join(cases_df.loc[case_idx,"name"])\
            + "? ([y]es/[o]ther/[a]bort)")
    ### Exit if entered
    if build_cases_user_input == "a":
        sys.exit("Exit by user.")

    ### Store names of cases
    cases_to_build = cases_df.loc[case_idx,"name"]


    ############################################################################
    ############################### Machine configs ############################
    ############################################################################

    ### THIS NEEDS TO BE SOLVED DIFFERENTLY!
    machine_cfgs_df = \
    pd.DataFrame(columns=["name","machine","compset","project"])

    saga = pd.Series(data = {
    "name":"SAGA",
    "machine":"saga",
    "compset":"2000_DATM%1PTGSWP3_CLM50%FATES_SICE_SOCN_MOSART_SGLC_SWAV",
    "project":"nn2806k"}, name="saga")

    fram = pd.Series(data = {
    "name":"Fram",
    "machine":"fram",
    "compset":"2000_DATM%1PTGSWP3_CLM50%FATES_SICE_SOCN_MOSART_SGLC_SWAV",
    "project":"nn2806k"}, name="fram")

    galaxy = pd.Series(data = {
    "name":"GALAXY",
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
        machine_idx_int = \
        [int(idx) for idx in re.split('[ ,;]+', machine_idx_str)]

        machine_idx_int = sorted(list(set(machine_idx_int)))

        ### Any value out of array range?
        if max(machine_idx_int) >= machine_cfgs_df.shape[0] or \
        min(machine_idx_int) < 0 or len(machine_idx_int) > 1:
            raise ValueError("Index out of range or more than one given.")

    except ValueError:
        print("")
        print("Please only enter integers within index range!")
        print("")
        raise

    ### Save values
    compset_str = str(machine_cfgs_df.loc[machine_idx_int,"compset"] \
    .to_string(index=False))

    machine_str = str(machine_cfgs_df.loc[machine_idx_int,"machine"] \
    .to_string(index=False))

    project_str = str(machine_cfgs_df.loc[machine_idx_int,"project"] \
    .to_string(index=False))


    ###############################################
    ### WRITE USER CHOICES TO NEW SETTINGS FILE ###
    ###############################################

    def_settings.parser['user']['sites2run'] = \
    ", ".join(cases_df.loc[case_idx,"name"])

    ### TO DO: ALSO ADD COMPSET ETC. WHEN DECIDED HOW TO STRUCTURE

    ### Write the modified settings to a new file
    # Create/use 'custom_settings' directory
    try:
        cust_settings_dir = def_settings.platform_dir / "data"/"custom_settings"

        if not cust_settings_dir.is_dir():
            cust_settings_dir.mkdir(parents=True, exist_ok=True)

        def_settings.write_file(f"{cust_settings_dir}/{args.name_output_file}")

    except:
        print("\nError when creating new settings file!\n")
        raise


################################################################################
############################## Download input data #############################
################################################################################

print("\nStart preparing input data...\n")

### Loop through chosen cases
for case_str in cases_to_build:
    ### Test if input data already there, if not, download
    cur_url = cases_df.loc[case_str,"url"]

    hlp.download_input_data(case_str, def_settings.version,
    cur_url, def_settings.platform_dir)

sys.exit("Just a test.")


################################################################################
################################ Build the cases ###############################
################################################################################

print("\nStart creating cases...\n")

### Check if cases folder exists, otherwise create it
try:
    nlp_cases_path = def_settings.platform_dir / "data" / "nlp_cases"
    if not nlp_cases_path.is_dir():
        nlp_cases_path.mkdir(parents=True, exist_ok=True)
except:
    print("Error when creating case folder!")
    raise

if args.case_name_suffix != "":
    suffix = "_" + args.case_name_suffix
else:
    suffix = ""

### store names of case folders
case_names = []
platform_path = Path(def_settings.platform_dir)

### Loop through chosen cases
for case_str in cases_to_build:

    ### Store path to cur folder
    case_names.append(case_str + suffix)

    cur_path = Path(nlp_cases_path / (case_str + suffix))

    print(cur_path)
    ### bash cmd string
    bashCommand = \
    f"{platform_path}/noresm2/cime/scripts/create_newcase "+\
    f"--case {cur_path} --compset {compset_str} "+\
    f"--res 1x1_{case_str} --machine {machine_str} --run-unsupported "

    ### If project is given, add to bash cmd string
    if project_str != "":
        bashCommand += f"--project {project_str}"

    subprocess.run(bashCommand, shell=True, check=True)
    #process = subprocess.Popen(bashCommand.split(), stdout=subprocess.PIPE)
    #output, error = process.communicate()

print("\nCases created succesfully.\n")

################################################################################

print("\nStart building cases...\n")

### Create and build cases
for case_name in case_names:

    print(f"Building {case_name}... ", end="")

    cur_path = PurePosixPath(nlp_cases_path / case_name)

    bashCommand = f"cd {cur_path} ; ./case.setup ; ./case.build"
    subprocess.run(bashCommand, shell=True, check=True)
    #process = subprocess.Popen(bashCommand.split(), stdout=subprocess.PIPE)
    #output, error = process.communicate()

    print("Done!")

print("\nCases built succesfully!\n")
