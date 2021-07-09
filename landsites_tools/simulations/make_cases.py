import argparse
import sys
import os
import json
import re
import subprocess
from pathlib import Path, PurePosixPath
import pandas as pd
import time

### Import helper functions
import helpers as hlp
from landsites_tools.interface_settings import SettingsParser
from landsites_tools import general_helpers as ghlp

### General settings
pd.set_option('display.max_colwidth', None)
time_stamp = int(time.time())

################################################################################
############################ Argument parser setup #############################
################################################################################

# Describe command line tool
description = "This module creates, builds, and sets up CTSM cases for " \
+ "predefined or custom site locations (see README.md). It is either using " \
+ "the simulation options specified in a 'settings.txt' file or asks for " \
+ "interactive command line input to create one. Missing input data for NLP " \
+ "cases is automatically downloaded."

# Initiate the parser
parser = argparse.ArgumentParser(description=description)

## Define arguments that need to be provided
#parser.add_argument("--version", help="show tool version", action="store_true")
parser.add_argument("-s", "--case_name_suffix",
                    help="optional suffixes for created case folder names",
                    default="")
parser.add_argument("-p", "--settings_path",
                    help="path to settings file if not stored in std. folder",
                    default="")
parser.add_argument("-f", "--settings_file",
                    help="configuration file to use, default is " \
                    + "'settings.txt'",
                    default="")
parser.add_argument("-id", "--dir_input",
                    help="path to the input (forcing) data directory",
                    default="")
parser.add_argument("-i", "--interactive",
                    help="display options and make cases interactively",
                    action="store_true")
parser.add_argument("-n", "--name_settings_file",
                    help="name of output settings file (interactive only)",
                    default="")

args = parser.parse_args()

################################################################################
######################### Read or create settings file #########################
################################################################################

### Interactive mode
if args.interactive:

    import interactive_settings as iset

    # Store file name
    fname = args.name_settings_file
    # Call module to create settings file
    interface_settings = iset.create_settings_interactively(
        file_name = fname if fname != "" else f"settings_{time_stamp}.txt"
    )

    sys.exit("New settings file successfully created. Please change the model "\
    +"paramters in the file and rerun the script without interactive mode.")

### File provided or standard file
else:

    # Check provided file path
    try:
        if ghlp.is_valid_path(args.settings_path, type="dir"):
            settings_path = args.settings_path
        # Read from root directory otherwise
        else:
            settings_path = Path(__file__).absolute().parent.parent

        if args.settings_file != "" and \
        ghlp.is_valid_path(settings_path / args.settings_file, type="file"):

            interface_settings = SettingsParser(
                args.settings_path / args.settings_file
            )

        else:
            interface_settings = SettingsParser(
                settings_path / "settings.txt"
            )

    except:
        print("Input error regarding the provided settings file!")
        raise

################################################################################
######################### Read variables from settings #########################
################################################################################

# Cases to build
cases_to_build = interface_settings.get_parameter("sites2run")

# General NLP options
cases_df = interface_settings.get_parameter("info_all_sites")
available_cases = cases_df["name"].values
nlp_version = interface_settings.get_parameter("version")
compset_str = interface_settings.get_parameter("compset_str")
machine_str = interface_settings.get_parameter("machine_str")

# Paths
dir_platform = interface_settings.get_parameter("dir_platform")
dir_input = interface_settings.get_parameter("dir_input")
dir_output = interface_settings.get_parameter("dir_output")
dir_cases = interface_settings.get_parameter("dir_cases")
dir_info = interface_settings.get_parameter("dir_info")


################################################################################
############################## Prepare input data ##############################
################################################################################

print("Checking input data...\n")
time.sleep(0.3)

case_input_paths = []

### Loop through chosen cases
for case_str in cases_to_build:

    ### Check if input data already in place, if not, download
    cur_url = cases_df.loc[case_str,"url"]

    case_input_paths.extend(
        hlp.download_input_data(case_str, nlp_version, cur_url, dir_input)
    )


print("Input data is ready.")



################################################################################
############################### Create the cases ###############################
################################################################################

print("\nStart creating cases...\n")
time.sleep(0.3)

### Check if a suffix was provided that would be added to case dir names
if args.case_name_suffix != "":
    suffix = "_" + args.case_name_suffix
else:
    suffix = ""
# Generate names for case folders (nlp case name + suffix)
case_dir_names = \
[case + "_" + str(nlp_version) + suffix for case in cases_to_build]

### Check if case folders exists, otherwise create them
for case_dir_name, nlp_case_name in zip(case_dir_names, cases_to_build):

    hlp.create_case(case_dir_name, nlp_case_name, dir_platform,
                    dir_cases, compset_str, machine_str)

print("Done creating cases.\n")


################################################################################
############################### Change CLM config ##############################
################################################################################

print("\nChanging specified CLM parameters within each case...\n")
time.sleep(0.3)

### Import parameter dictionary
with open(dir_info / "params.json", 'r') as param_file:
    param_dict = json.load(param_file)


for case_dir_name,case_input_path in zip(case_dir_names, case_input_paths):

    cur_case_path = PurePosixPath(dir_cases / case_dir_name)

    hlp.change_case_parameters(cur_case_path, interface_settings, param_dict)

print("Done changing parameters.\n")

################################################################################
############################### Build the cases ################################
################################################################################

print("\nStart building cases...\n")
time.sleep(0.3)

### Create and build cases
for case_dir_name in case_dir_names:

    print(f"Building {case_dir_name}... ", end="")

    cur_path = PurePosixPath(nlp_cases_path / case_dir_name)

    bash_command = f"cd {cur_path} ; ./case.setup ; ./case.build"
    subprocess.run(bash_command, shell=True, check=True)
    #process = subprocess.Popen(bash_command.split(), stdout=subprocess.PIPE)
    #output, error = process.communicate()

    print("Done!")

print("\nAll cases were built succesfully. "\
+ "Execute 'run_cases.py' to start the simulations.\n")
