import re
from configparser import ConfigParser, ExtendedInterpolation
from pathlib import Path
import pandas as pd
from dateutil.parser import parse as parsetime


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
        """Parse the NorESM Land Site Platform settings file
        Arguments:
            paths_settings: path to settings file
        Returns:
            instance of SettingsParser class
        """
        # Default information
        self.dir_platform = Path(__file__).absolute().parents[1]
        self.path_sites_table = self.dir_platform / 'data' / 'sites.json'
        self.info_all_sites = pd.read_json(self.path_sites_table,
                                           orient='index')
        # Load user settings into a ConfigParser instance (low-level interface)
        self.interface = ConfigParser(interpolation=ExtendedInterpolation(),
                                      inline_comment_prefixes='#')
        self.interface.read(path_settings)
        # Parse user settings to convenient types
        self.dir_cases = self.get_path('path', 'dir_cases')
        self.dir_input = self.get_path('path', 'dir_input')
        self.dir_output = self.get_path('path', 'dir_output')
        self.start_time = self.get_time('run', 'start_time')
        self.end_time = self.get_time('run', 'end_time')
        self.sites2run = self.get_sequence('run', 'sites2run')
        self.type_run = self.get_parameter('run', 'type_run')
        self.type_model = self.get_parameter('run', 'type_model')
        self.initial_file = self.get_parameter('run', 'initial_file')
        self.frequency_output = self.get_parameter('run', 'frequency_output')
        self.variables_output = self.get_sequence('run', 'variables_output')
        self.variables_plot = self.get_sequence('postprocess', 'variables_plot')
        self.frequency_plot = self.get_parameter('postprocess', 'frequency_plot')

    def get_parameter(self, section_name, param_name):
        '''Returns the value for a requested parameter string.'''
        return self.interface[section_name][param_name]

    def get_path(self, section, parameter):
        """Read and parse path from settings low-level interface
        Arguments:
            section: settings file section
            parameter: settings file parameter name
        Returns:
            path
        """
        return Path(self.get_parameter(section, parameter)).expanduser()

    def get_sequence(self, section, parameter, sep_regex='[ ,;]+'):
        """Read and parse sequence of values as list
        Arguments:
            section: settings file section
            parameter: settings file parameter name
            sep_regex: regular expression patter used for separation
        Returns:
            list of values
        """
        return re.split(sep_regex, self.get_parameter(section, parameter))

    def get_time(self, section, parameter):
        """Read and parse time information as datetime.datetime
        Arguments:
            section: settings file section
            parameter: settings file parameter name
        Returns:
            datetime.datetime instance
        """
        return parsetime(self.get_parameter(section, parameter))
