#!/usr/bin/env python

"""parameters.py: Utility functions to handle CTSM parameters."""

import subprocess
from landsites_tools.interface_settings import SettingsParser
from pathlib import Path

################################################################################
"""Change parameters"""
################################################################################

def change_case_parameters(case_path, case_input_path,
interface_settings: SettingsParser, param_dict: dict):
    """

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

        ########################################################################
        ### Change input path to case directory
        ########################################################################
        bash_command += \
        f"./xmlchange --file env_run.xml DIN_LOC_ROOT={Path(case_input_path)};"

        clm_input_path = \
        Path(case_input_path / 'inputdata' / 'atm' / 'datm7' / 'GSWP3v1')

        bash_command += \
        f"./xmlchange --file env_run.xml DIN_LOC_ROOT_CLMFORC=" \
        + f"{clm_input_path};"


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
