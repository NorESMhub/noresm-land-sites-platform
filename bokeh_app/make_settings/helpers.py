"""
Helper functions used exclusively within the application.
"""
from pathlib import Path
from landsites_tools.utils.interface_settings import SettingsParser


def write_settings_file(file_name, sites2run, dir_cases, dir_clm_input,
                        dir_output, start_time, end_time, type_run, type_model,
                        pft_indices):
    """
    Create a new settings file with inputs specified in the bokeh interface.
    Arguments:
        file_name (string): name of the new settings file
    """

    root_path = Path(__file__).parents[2]
    default_settings_path = root_path / 'data/.nlp/'
    default_save_path = root_path / 'landsites_tools/custom_settings/'

    # Load settings as SettingsParser object
    interface_settings = SettingsParser(
        default_settings_path / "default_settings.txt"
    )

    """
    Change settings
    """
    # Sites'
    interface_settings.set_parameter("sites2run", sites2run)

    # Paths
    interface_settings.set_parameter("dir_cases", dir_cases)
    interface_settings.set_parameter("dir_clm_input", dir_clm_input)
    interface_settings.set_parameter("dir_output", dir_output)

    # Simulation period
    interface_settings.set_parameter("start_time", start_time)
    interface_settings.set_parameter("end_time", end_time)

    # CLM settings
    interface_settings.set_parameter("type_run", type_run)
    interface_settings.set_parameter("type_model", type_model)

    # Fates settings
    interface_settings.set_parameter("included_pft_indices", pft_indices)

    # Write settings file
    interface_settings. \
        write_settings_file(new_file=True,
                            path=(default_save_path / f"{file_name}"))

    return True
