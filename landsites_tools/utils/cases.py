#!/usr/bin/env python

"""cases.py: Utility functions to handle CTSM cases and NLP cases in
particular."""

import geopandas as gpd
import subprocess
from pathlib import Path, PurePosixPath

################################################################################
"""Case directories"""
################################################################################

### Check if a case dir exists
def _case_dir_exists(case_dir_path):
    """
    Check if the case directory already exists.

    To-do: make safer (e.g. check if dir contains necessary files).
    """
    # Return True or False
    return Path(case_dir_path).is_dir()

### Create bash cmd to create a case
def create_case(case_dir_name, nlp_case_name, dir_platform,
                dir_cases, compset_str, machine_str):
    """
    Create CTSM cases.
    """

    case_dir_path = Path(dir_cases) / case_dir_name

    try:
        if _case_dir_exists(case_dir_path):
            print(f"Case '{case_dir_name}' already exists in the specified "\
            + f"cases directory.")

            return case_dir_path

        else:
            ### Concatenate a bash string to create CTSM case
            bash_command = \
            f"{dir_platform}/noresm2/cime/scripts/create_newcase " \
            + f"--case {case_dir_path} --compset {compset_str} " \
            + f"--res 1x1_{nlp_case_name} --machine {machine_str} " \
            + f"--run-unsupported"

            subprocess.run(bash_command, shell=True, check=True)

            print(f"New case '{case_dir_name}' successfully created.")

            return case_dir_path

    except:
        print("Error when creating cases!\n")
        raise


################################################################################
"""Visualization"""
################################################################################

### Function to print site info in table format
def _print_table_row(ind, name, res, lat, lon):
    '''
    Prints one row with nlp case information.
    '''

    print('|','%-7s' % str(ind), '|', '%-10s' % str(name), '|',
     '%-10s' % str(res), '|', '%-10s' % str(lat), '|',
     '%-10s' % str(lon),'|')

def print_cases(cases_gdf: gpd.geodataframe.GeoDataFrame):
    '''
    Prints available NLP cases to the console.
    '''

    ### Print cases
    print("\n")
    print("*********************** Available cases ***********************")
    ### Print table header...
    print('---------------------------------------------------------------')
    _print_table_row("Index", "Name", "Resolution", "Lat.", "Lon.")
    print('---------------------------------------------------------------')

    ### Loop through Points stored in the GeoDataFrame...
    for idx,case in cases_gdf.iterrows():

        # Retrieve latitude from geometry
        cur_lat = case.geometry.coords.xy[1][0]
        # Retrieve longitude from geometry
        cur_lon = case.geometry.coords.xy[0][0]

        _print_table_row(idx, case["name"], case["res"], cur_lat, cur_lon)

    print('---------------------------------------------------------------')
