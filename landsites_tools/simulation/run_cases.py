#!/usr/bin/env python3

"""run_cases.py: run CTSM simulations, requires existing case directories."""

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
from landsites_tools.simulation.utils import cases


# Describe command line tool
description = "This module runs previously built CTSM cases. The respective " \
+ "paths need to be defined in a 'settings.txt' file. The model output files " \
+ "will be created in the 'data/output/CASE_NAME' directory by default, but a "\
+ "different path can be specified."

# Initiate the parser
parser = argparse.ArgumentParser(description=description)

### Define command line arguments
parser.add_argument("-f", "--settings_file",
                    help="settings file specifying the paths of cases to run" \
                    + ", default is 'settings.txt' in 'landsites_tools'",
                    default="")
parser.add_argument("-q", "--quiet",
                    help="suppress warnings",
                    action="store_true")

args = parser.parse_args()

################################################################################
################################ Print warning! ################################
################################################################################

# Settings file provided?
if args.settings_file == "":

    # If not, try to read default file
    try:
        settings_path = Path(__file__).absolute().parents[1] / "settings.txt"

        interface_settings = SettingsParser(settings_path)

    except:
        raise ValueError(f"No settings file path provided and no default " \
        + f"found in {Path(__file__).absolute().parents[1]}")

# If provided settings file path does not point to a file, throw exception
elif not pth.is_valid_path(args.settings_file, type="file"):
    raise ValueError("Provided settings file path does not exist!")

### If path is valid, read in file
else:
    settings_path = Path(args.settings_file)
    interface_settings = SettingsParser(settings_path)

print(f"\nPreparing to run cases specified in {settings_path}.\n")

### Was "supress warnings" flag passed?
if not args.quiet:
    warning_msg = f"ATTENTION! If you have made any changes in the settings " \
    + f"file (e.g changed simulation parameters) after running " \
    + f"'make_cases.py', you need to rebuild the cases you are trying to run " \
    + f"(i.e., re-execute 'make_cases.py -f {settings_path}'). Are you " \
    + f"sure everything is correct and you want to continue? [y/n]: "

    run_bool = input(warning_msg)

    if run_bool.lower() != "y":
        sys.exit("Aborted by user.")

### Also check if provided output path is valid
try:
    output_path = Path(interface_settings.get_parameter("dir_output"))
    pth.is_valid_path(output_path)
except:
    raise ValueError(f"The output path specified in {settings_path} " \
        + f"does not exist!")

print(
f"\n Provided inputs valid, the output files will be in {output_path}.\n"
)

################################################################################
################################## Run cases ###################################
################################################################################

print("\nStart running cases. This step can take a while...\n")

### Read in case paths from settings file
cases_paths = interface_settings.get_parameter("cases_paths")
cases_paths = cases_paths.replace(" ", "").split(",")

### Set output path and run cases
for case_path in cases_paths:

    pth.is_valid_path(case_path, type="dir")
    print(f"Running {case_path}...", end="")
    ### Rebuild case for testing
    bash_command = f"cd {case_path} ; ./case.submit;"

    subprocess.run(bash_command, shell=True, check=True)

    print("\nDone!\n")
