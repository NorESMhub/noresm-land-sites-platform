#!/usr/bin/env python

"""make_cases.py: Create, build, and set up single-point CTSM case directories,
also automatically download forcing input data for pre-defined land sites."""

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
#import helpers as hlp
from landsites_tools.utils.interface_settings import SettingsParser
from landsites_tools.utils import paths as pth
from landsites_tools.utils import input
from landsites_tools.simulation.utils import parameters as params
from landsites_tools.simulation.utils import cases

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
parser.add_argument("-i", "--interactive",
                    help="display options and make cases interactively",
                    action="store_true")
parser.add_argument("-n", "--name_new_settings_file",
                    help="name of new settings file (interactive only)",
                    default="")

args = parser.parse_args()

################################################################################
######################### Read or create settings file #########################
################################################################################

### Interactive mode
if args.interactive:

    import landsites_tools.simulation.utils.interactive_settings as iset

    # Retrieve file name
    fname = args.name_new_settings_file
    if fname == "":
        fname = f"settings_{time_stamp}.txt"

    ### Call module to create settings file
    # if no filename provided, use time stamp to create unique name
    interface_settings = iset.create_settings_interactively(
        file_name = fname
    )

    ### Stop execution
    sys.exit(
    f"New settings file successfully created. Please adapt the simulation " \
    + f"paramters in {fname} and rerun 'make_cases.py -f {fname}' to build " \
    + f"the cases."
    )

### File provided or standard file
else:

    # Check provided settings file input
    try:
        # Was a settings file path provided? If no...
        if args.settings_path == "":

            # Was a settings file name provided? If no...
            if args.settings_file == "":

                # ...set to '~/landsites_tools/' (std settings file location)
                settings_path = Path(__file__).absolute().parents[1]

                # Create SettingsParser instance from standard file, check if
                # it's there first
                if pth.is_valid_path(settings_path / "settings.txt",
                type="file"):

                    interface_settings = SettingsParser(
                        settings_path / "settings.txt"
                    )

                    print("No settings file name or path provided, " \
                    + "using '~/landsites_tools/settings.txt'...")

                else:
                    raise RuntimeError(
                    "The standard 'settings.txt' file is missing in the" \
                    + "'~/landsites_tools/' directory! Make sure it's there " \
                    + "and run again."
                    )

            # Was a settings file name provided but no file path? Then...
            else:
                # ...use the '~/landsites_tools/custom_settings/' directory
                settings_path = \
                Path(__file__).absolute().parents[1] / "custom_settings"

                # Create SettingsParser instance from given file name in custom
                # directory, check if it's there first
                if pth.is_valid_path(settings_path / args.settings_file,
                type="file"):

                    interface_settings = SettingsParser(
                        settings_path / args.settings_file
                    )

                    print(f"Using settings file '{args.settings_file}' in " \
                    + "'~/landsites_tools/custom_settings/'...")

                else:
                    raise RuntimeError(
                    f"The file '{args.settings_file}' does not exist in the " \
                    + "'~/landsites_tools/custom_settings/' directory! Make " \
                    + "sure to provide a valid name and run again."
                    )

        # Was a full settings file path provided? If yes and if it is valid...
        elif pth.is_valid_path(Path(args.settings_path) / args.settings_file,
        type="file"):

            # ...use it
            settings_path = Path(args.settings_path)
            interface_settings = SettingsParser(
                settings_path / args.settings_file
            )

            print(f"Using settings file '{args.settings_file}' in " \
            + f"'{settings_path}'...")

        # If input is wrong
        else:
            raise ValueError(
            f"There is no '{args.settings_file}' in " \
            + f"'{args.settings_path}'! Provide a correct absolute path.")

    except:
        print("\nInput error regarding the provided settings file!\n")
        raise

################################################################################
######################### Read variables from settings #########################
################################################################################

# Cases to build
cases_to_build = interface_settings.get_parameter("sites2run")

# General NLP options
cases_gdf = interface_settings.get_parameter("nlp_sites_gdf")
available_cases = interface_settings.get_parameter("valid_site_names")
nlp_version = interface_settings.get_parameter("version")
compset_str = interface_settings.get_parameter("compset_str")
machine_str = interface_settings.get_parameter("machine_str")

# Paths
dir_platform = interface_settings.get_parameter("dir_platform")
dir_clm_input = interface_settings.get_parameter("dir_clm_input")
dir_output = interface_settings.get_parameter("dir_output")
dir_cases = interface_settings.get_parameter("dir_cases")
dir_info = interface_settings.get_parameter("dir_info")


################################################################################
############################## Prepare input data ##############################
################################################################################

print("\nChecking input data...\n")

# Save paths to created input data directories in a list
case_input_paths = []

### Loop through chosen cases
for case_str in cases_to_build:

    ### Check if input data already in place, if not, download
    cur_url = cases_gdf[cases_gdf["name"] == case_str]["url"].array[0]

    case_input_paths.append(
        input.download_input_data(case_str, nlp_version, cur_url, dir_clm_input)
    )

### Save input paths into settings file
interface_settings.set_parameter("input_paths", case_input_paths)

print("\nInput data is ready.\n")


################################################################################
############################### Create the cases ###############################
################################################################################

print("\nStart creating cases...\n")

### Check if a suffix was provided that would be added to case dir names
if args.case_name_suffix != "":
    suffix = "_" + args.case_name_suffix
else:
    suffix = ""
# Generate names for case folders (nlp case name + suffix)
case_dir_names = \
[case + "_" + str(nlp_version) + suffix for case in cases_to_build]

case_dir_paths = []

### Check if case folders exists, otherwise create them
for case_dir_name, nlp_case_name in zip(case_dir_names, cases_to_build):

    case_dir_paths.append(
    cases.create_case(case_dir_name, nlp_case_name, dir_platform,
    dir_cases, compset_str, machine_str)
    )

### Save input paths into settings file
interface_settings.set_parameter("cases_paths", case_dir_paths)

print("\nDone creating cases.\n")

################################################################################
############################### Change CLM config ##############################
################################################################################

print("\nChanging specified CLM parameters within each case...\n")

### Import parameter dictionary
with open(dir_info / "params.json", 'r') as param_file:
    param_dict = json.load(param_file)


for case_dir_name,case_input_path in zip(case_dir_names, case_input_paths):

    cur_case_path = PurePosixPath(dir_cases / case_dir_name)

    print(case_input_path)

    params.change_case_parameters(cur_case_path, case_input_path,
    interface_settings, param_dict)

print("\nDone changing parameters.\n")

################################################################################
############################### Build the cases ################################
################################################################################

print("Start building cases...\n")

### Create and build cases
for case_dir_name in case_dir_names:

    print(f"Building {case_dir_name}... ", end="")

    cur_case_path = PurePosixPath(dir_cases / case_dir_name)

    bash_command = f"cd {cur_case_path} ; ./case.setup ; ./case.build"
    subprocess.run(bash_command, shell=True, check=True)
    #process = subprocess.Popen(bash_command.split(), stdout=subprocess.PIPE)
    #output, error = process.communicate()

    print("\nDone!\n")

### Update settings file
interface_settings.write_settings_file(new_file=False)

print("\nAll cases were built succesfully. "\
+ "Execute 'run_cases.py' to start the simulations.\n")
