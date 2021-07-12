import re
import json
import csv
from configparser import ConfigParser, ExtendedInterpolation
from pathlib import Path
import pandas as pd
import numpy as np
from dateutil.parser import parse as parsetime
from datetime import *

# Custom
import landsites_tools


def interactive_settings():
    """
    Example workflow:
    1. get settings from command-line user input
    2. create ConfigParser instance with settings formatted as
       in the default settings file
    3. write settings file
    (https://docs.python.org/3.6/library/configparser.html#configparser-objects)
    """
    raise NotImplementedError


class SettingsParser:
    """NorESM Land Sites Platform settings parser"""

    def __init__(self, path_settings):
        """
        Parse the NorESM Land Site Platform settings file
        Arguments:
            paths_settings: path to settings file
        Returns:
            instance of SettingsParser class
        """
        ### Load user settings into a ConfigParser instance (low-level interface)
        self.interface = ConfigParser(interpolation=ExtendedInterpolation(),
                                      inline_comment_prefixes='#')
        # Path to settings file
        self.path_settings = Path(path_settings).absolute()
        self.interface.read(self.path_settings)

        ### Default information
        self.version = landsites_tools.__version__
        self.dir_platform = Path(__file__).absolute().parents[1]
        self.dir_info = self.dir_platform / 'data' / '.nlp'
        self.path_sites_table = self.dir_info / 'sites.json'
        self.info_all_sites = pd.read_json(self.path_sites_table,
                                           orient='index')
        self.path_constraints = self.dir_info / 'settings_constraints.json'

        ### Read parameter dictionary
        with open(self.path_constraints, 'r') as constraints_file:
            self.constraints = json.load(constraints_file)

        ### Parse user settings to convenient types
        # Paths
        self.dir_cases = self.dir_platform / self.get_path('path', 'dir_cases',
         'dir')
        self.dir_input = self.dir_platform / self.get_path('path', 'dir_input',
         'dir')
        self.dir_output = self.dir_platform / self.get_path('path',
        'dir_output', 'dir')
        # Dates
        self.start_time = self.get_time('run', 'start_time')
        self._end_time = self.get_time('run', 'end_time')
        self.end_time = self.get_n_days(self.start_time, self._end_time)
        # Sites
        self.sites2run = self.get_sites('run', 'sites2run')
        self._check_sites()
        # COMPSET
        self.compset_str = self._generate_compset_string() # TO-DO
        # MACHINE
        self.machine_str = "container-nlp" # TO-DO, SHOULDN'T BE HARD-CODED!
        # Other
        self.type_run = self.read_parameter('run', 'type_run')
        self.type_model = self.read_parameter('run', 'type_model')
        self.initial_file = self.read_parameter('run', 'initial_file')
        self.frequency_output = self.read_parameter('run', 'frequency_output')
        self.variables_output = self.read_parameter('run', 'variables_output')
        self.variables_plot = self.read_parameter('postprocess',
        'variables_plot')
        self.frequency_plot = self.read_parameter('postprocess',
        'frequency_plot')


    def write_file(self, path):
        """
        Write instance to a new settings file.
        """
        if self._is_valid_path(path, type="file", can_create=True):
            self.interface["run"]["sites2run"] = self.get_parameter('sites2run')
            # Writing our configuration file to 'example.cfg'
            with open(path, 'w') as configfile:
                self.interface.write(configfile)

            return True
        else:
            return False
    ############################################################################
    def read_parameter(self, section_name, param_name):
        '''
        Reads and returns the value for a requested parameter string from the
        settings file, raising an exception if any value is invalid.
        '''
        value = self.interface[section_name][param_name]

        ### Are constraints defined?
        if param_name in self.constraints.keys():
            try:
                ### Cast to correct Python object type
                cur_type = eval(self.constraints[param_name].get("type"))

                try:
                    # If list is expected, convert by splitting entries
                    if isinstance(cur_type, list):
                        # Must be seperated by comma, semicolon, or whitespace
                        value = re.split("[ ,;]+", value)
                    else:
                        value = cur_type(value)
                except:
                    raise TypeError(f"'{value}' must be of type '{cur_type}'!")

                self._is_valid_param_value(param_name, value)

            except:
                raise

        return value

    def get_parameter(self, param_name):
        '''
        Returns the requested parameter.
        '''
        param = getattr(self, param_name)

        ### Format datetime objects to necessary format here?
        if isinstance(param, datetime):
            return param.strftime('%Y-%m-%d')

        return param

    def set_parameter(self, param_name, value):
        """
        Set an instance parameter to a new value.
        """
        try:
            self._is_valid_param_value(param_name, value)

            setattr(self, param_name, value)

            return True
        except:
            raise


    ############################################################################
    def get_path(self, section, parameter, type="dir"):
        """
        Read and parse path from settings low-level interface
        Arguments:
            section: settings file section
            parameter: settings file parameter name
            type: object the path points to ('dir' or 'file')
        Returns:
            path
        """
        try:
            path = Path(self.read_parameter(section, parameter)).expanduser()
            self._is_valid_path(path, type=type)

            return path
        except:
            ### Ask user to create new folder?
            raise

    ############################################################################
    def get_sites(self, section, parameter, sep_regex='[ ,;]+'):
        """
        Read and parse sequence of values as list
        Arguments:
            section: settings file section
            parameter: settings file parameter name
            sep_regex: regular expression patter used for separation
        Returns:
            list of values
        """
        sites_raw_str = self.interface[section][parameter]

        return re.split(sep_regex, sites_raw_str)

    ############################################################################
    def get_time(self, section, parameter):
        """
        Read and parse time information as datetime.datetime
        Arguments:
            section: settings file section
            parameter: settings file parameter name
        Returns:
            datetime.datetime instance
        """
        return parsetime(self.read_parameter(section, parameter))

    def get_n_days(self, start_date, end_date):
        """
        Calculates the number of days for model run.
        Arguments:
            start_date: datetime object for model start date
            end_date: datetime object for model end date
        Returns:
            Simulation length in days as int.
        """
        return int((end_date-start_date).days)


    ############################################################################
    ############################################################################
    ############################################################################

    def _check_sites(self):
        '''
        Raise an exception if site names are not valid
        '''
        invalid = np.setdiff1d(self.sites2run, self.info_all_sites.index)
        if invalid.size:
            msg = f"{invalid.tolist()} are invalid sites. " +\
                  f"Valid sites: {self.info_all_sites.index.tolist()} " +\
                  f"(see {self.path_sites_table})"
            raise ValueError(msg)

    ############################################################################
    def _is_valid_path(self, path, type="dir", can_create=False):
        '''
        Check if a given directory or file exists. Returns False if the input is
        an empty string, True if it is a valid directory, and raises an
        exception if given path is not valid.
        '''
        if type not in ["dir", "file"]:
            raise ValueError("'type' must be either 'dir' or 'file'!")

        ### Return False if no string given
        if path == "":
            return False

        if not str(path).startswith(str(self.dir_platform)):
            path = Path(self.dir_platform / path)

        ### If not empty string, check if path is valid
        try:
            if type == "dir":
                if not Path(path).is_dir():
                    raise ValueError(f"Directory {path} does not exist!")
            elif type == "file":
                if can_create:
                    if Path(path).parent.is_dir():
                        return True
                    else:
                        raise ValueError(f"Directory {Path(path).parent} does not exist!")
                else:
                    if not Path(path).is_file():
                        raise ValueError(f"File {path} does not exist!")
            ### Return true if no exceptions
            return True
        except:
            raise

    ############################################################################
    def _is_valid_param_value(self, param_name, value):
        '''
        Validate the paramaters given in the settings file.
        Raises an exception if input is erroneous.
        '''

        if param_name not in self.constraints.keys():
            # TO-DO: smarter way to handle exceptions
            return True

        ### Check if the input is in valid entries
        # First check if valid values are stored in a seperate file
        if int(self.constraints[param_name].get("valid_in_txt_file")):
            # Read that file and save values in list
            with open(self.dir_info / self.constraints[param_name]. \
            get("valid_values")) as f:
                s_file = csv.reader(f, delimiter=",")
                valid_vals = []
                for row in s_file:
                    valid_vals.extend(row)
        # Otherwise, use values directly from dict
        else:
            valid_vals = self.constraints[param_name].get("valid_values")

        ### Test if provided values are valid
        try:
            if isinstance(value, list):
                # All items in provided parameter list also in the valid list?
                if any(item in value for item in valid_vals):
                    raise ValueError()
            else:
                # Provided parameter in valid list?
                if value not in valid_vals:
                    raise ValueError()
        # Catch exceptions
        except:
            # Raise value exception
            msg = f"Invalid value '{value}' for setting '{param_name}'. " +\
                  f"Valid values: {valid_vals} (see the documentation)."

            raise ValueError(msg)

        # If no exceptions were raised, return True
        return True

    ############################################################################

    def _generate_compset_string(self):
        """
        Create and store a compset string based on provided input.

        TO-DO: Implement different modes.
        """
        return "2000_DATM%1PTGSWP3_CLM50%FATES_SICE_SOCN_MOSART_SGLC_SWAV"
