### Imports
from pathlib import Path, PurePosixPath
import pandas as pd
import time
import re
import subprocess

# Custom
from landsites_tools.interface_settings import SettingsParser
from landsites_tools import general_helpers as ghlp
import helpers as hlp

################################################################################

def create_settings_interactively(file_name):
    """
    Uses interactive command line input to create a new settings.txt file that
    stores the information for building and running NLP cases.

    Arguments:
        file_name: name for the new settings.txt file.
        platform_path: path to the root directory.

    Returns:
        The created settings as a SettingsInterface instance.
    """

    if file_name == "" or not isinstance(file_name, str):
        raise ValueError("Please provide a valid filename!")

    #### TO-DO: additional checks: special characters, etc.

    if not file_name.endswith(".txt"):
        file_name += ".txt"

    ### Read default settings file
    try:
        def_settings = SettingsParser(Path(__file__).absolute() \
        .parents[1] / "settings.txt")
    except:
        print("Something went wrong when creating the SettingsParser object! "\
        + "Please also make sure a valid 'settings.txt' file is in the " \
        + "'landsites_tools' directory.")
        raise


    ##### Print cases, ask for input which ones to build #####
    hlp.print_cases(def_settings.get_parameter("info_all_sites"))

    ### Ask for user input to choose cases
    interactive_settings = _case_input(def_settings)

    ### Write new file
    # Define path
    custom_settings_dir = \
    interactive_settings.get_parameter("dir_platform") / "landsites_tools" \
    / "custom_settings"

    if not custom_settings_dir.is_dir():
        custom_settings_dir.mkdir(parents=True, exist_ok=True)

    # Write file
    interactive_settings.write_file(Path(custom_settings_dir / file_name))

    print(f"\nSettings file '{file_name}' successfully created in " \
    + f"{custom_settings_dir}. Remember to edit the file to adjust model " \
    + f"parameters!")
    time.sleep(0.5)

    return interactive_settings



################################################################################



def _case_input(def_settings):
    """
    Helper function to print and choose cases to make interactively.

    Arguments:
        cases_df: pandas.DataFrame of available NLP sites from a settings file.
    """

    cases_df = def_settings.get_parameter("info_all_sites")

    ##### Get user input #####
    # Variable to evaluate user input
    build_cases_user_input = "o"
    ### Repeat until choice is correct
    while(build_cases_user_input == "o"):

        case_indices_str = \
        input("Enter the indices of the cases to build " \
        + "(seperated by space or comma if more than one): ")

        if case_indices_str == "":
            continue

        ### Turn into list, remove duplicates, check for valid input
        try:
            # Split input by comma/semicolon/space, cast to int, store in list
            case_indices_int = \
            [int(x) for x in re.split('[ ,;]+', case_indices_str)]

            # Sort int indices and remove duplicates
            case_indices_int = sorted(list(set(case_indices_int)))

            ### Any value out of array range?
            if max(case_indices_int) >= cases_df.shape[0] or \
            min(case_indices_int) < 0:
                raise ValueError("At least one index out of range.")

        except:
            print("\nPlease only enter integers within index range seperated " \
            + "by commas or space!\n")
            continue

        case_idx = cases_df.index[case_indices_int]
        ### Make sure input is correct
        build_cases_user_input = ""
        while build_cases_user_input not in ["y", "o", "a"]:
            build_cases_user_input = input("Prepare the following cases: " \
            +", ".join(cases_df.loc[case_idx,"name"])\
            + "? ([y]es/[o]ther/[a]bort)")

    ### Exit if requested by user
    if build_cases_user_input == "a":
        sys.exit("Aborting.")

    ### Store names of cases
    cases_to_build = cases_df.loc[case_idx,"name"]

    cases_formatted_str = ", ".join(cases_df.loc[case_idx,"name"])

    ### Set list in instance
    def_settings.set_parameter('sites2run', cases_formatted_str)

    return def_settings


if __name__ == "__main__":
    """
    Run as main for test purposes.
    """
    create_settings_interactively("settings_test.txt")
