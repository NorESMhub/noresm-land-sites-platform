#!/usr/bin/env python3

"""parameters.py: Utility functions to handle CTSM parameters."""

import subprocess
from pathlib import Path
from landsites_tools.utils import paths as pth
from landsites_tools.utils.interface_settings import SettingsParser

################################################################################
"""Change parameters"""
################################################################################

def change_clm_path_setting(case_path, new_path, path_to_change="input"):
    """
    Change local CLM input or output path.

    Input:
        case_path : pathlib.Path
        case_input_path : pathlib.Path
        param_dict : dict
    Returns:
        True or throws error in case of erroneous input.
    """

    if path_to_change not in ["input", "output"]:
        raise ValueError("'path_to_change' must be 'input' or 'output'!")

    ### Check if provided paths are valid
    new_path = Path(new_path)
    case_path = Path(case_path)
    pth.is_valid_path(new_path, type="dir")
    pth.is_valid_path(case_path, type="dir")

    # Change workdir to case path
    bash_command = f"cd {case_path};"

    ### Changing clm parameter for path to input data ###
    try:
        if path_to_change == "input":

            bash_command += \
            f"./xmlchange --file env_run.xml DIN_LOC_ROOT=" \
            + f"{new_path / 'inputdata'};"

            clm_input_path = \
            new_path / 'inputdata' / 'atm' / 'datm7' / 'GSWP3v1'

            bash_command += \
            f"./xmlchange --file env_run.xml DIN_LOC_ROOT_CLMFORC=" \
            + f"{clm_input_path};"

        ### Changing clm parameter for path to output data ###
        elif path_to_change == "output":

            bash_command += \
            f"./xmlchange --file env_build.xml CIME_OUTPUT_ROOT=" \
            + f"{new_path};"

            bash_command += \
            f"./xmlchange --file env_run.xml DOUT_S_ROOT=" \
            + f"{new_path};"

    ### Throw exception if changing xml files fails
    except:
        msg = "Could not change path in case xml file. Are you sure the case " \
        + "directory is correct and contains the necessary files?"
        raise ValueError(msg)

    ### Execute bash command to change case xml file with updated path
    print(f"Updating output path in {case_path}...")
    subprocess.run(bash_command, shell=True, check=True)

    return True


def change_case_clm_params(case_path, interface_settings: SettingsParser,
param_dict: dict):
    """
    Change parameters in the CLM file structure.
    Input:
        case_path : pathlib.Path
        case_input_path : pathlib.Path
        interface_settings : SettingsParser
        param_dict : dict
    Returns:
        None.
    """
    # Change dir to current case path
    bash_command = f"cd {case_path};"

    try:
        ########################################################################
        ### Change NLP specific general settings for CESM parameters
        ########################################################################
        bash_command += \
        ";".join(
            [f"./xmlchange --file {vals['xml_file']} {ctsm_param}=" \
            + f"{vals['value']}" \
            for ctsm_param,vals in param_dict['nlp_general'].items()]
        ) + ";" # Closing semicolon

        ### Loop through parameter keys and add value change to bash cmd
        # If no parameter stored in interface object, use default value
        bash_command += \
        ";".join(
            [f"./xmlchange --file {vals['xml_file']} {vals['ctsm_param']}=" \
            + f"{interface_settings.get_parameter(nlp_param)}" \
            if interface_settings.get_parameter(nlp_param) is not None else \
            f"./xmlchange --file {vals['xml_file']} {vals['ctsm_param']}=" \
            + f"{vals['default']}" \
            for nlp_param,vals in param_dict['user'].items()]
        )

        ### Execute bash
        subprocess.run(bash_command, shell=True, check=True)

        print(f"Parameters successfully changed in '{case_path}'.")

    except:
        print("Error when changing case parameters!\n")
        raise
