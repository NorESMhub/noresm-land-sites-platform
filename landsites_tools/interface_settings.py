import re
from configparser import ConfigParser, ExtendedInterpolation
from pathlib import Path
import pandas as pd
from dateutil.parser import parse as parsetime


def split_sequence(sequence, sep_regex='[ ,;]+'):
    """Interpret sequence of values as list
    Arguments:
        sequence: string with sequence of separable values
        sep_regex: regular expression patter used for separation
    Returns:
        list of values
    """
    return re.split('[ ,;]+', sequence)


class InterfaceSettings:
    """Interface to the settings of NorESM Land Sites Platform"""

    def __init__(self, pathsettings):
        """Parse the NorESM Land Site Platform settings file
        Arguments:
            pathsettings: path to settings file
        Returns:
            instance of LandSiteInput class
        """
        # Path of original settings file
        self.path_original_settings = pathsettings
        # Load settings into a ConfigParser instance
        self.parser = ConfigParser(interpolation=ExtendedInterpolation(),
                                      inline_comment_prefixes='#')
        self.parser.read(self.path_original_settings)

        # Platform directory
        self.dir_platform = Path(__file__).absolute().parent.parent

        # Names and coordinates of all available sites
        self.path_sites_table = self.dir_platform / \
        self.read_path('paths_advanced', 'sites_table')

        try:
            with open(self.path_sites_table, "r") as sites_json:
                self.info_all_sites = pd.read_json(sites_json,
                                                   orient='index')
        except Exception as error:
            print("\nError when reading specified site info .json file!\n")
            raise error

        self.info_all_sites = pd.read_json(self.path_sites_table,
                                           orient='index')
        print(self.info_all_sites)

        # Directory of raw NorESM input data
        self.input_raw = self.read_path('paths_advanced', 'input_raw')
        # Directory of NorESM code and script paths
        self.dir_clm_tools = self.read_path('scripts', 'dir_clm_tools')
        self.path_mknoocnmap = self.read_path('scripts', 'mknoocnmap')

    @property
    def sites2run(self):
        """Sites to be simulated"""
        return split_sequence(self.parser['user']['sites2run'])

    @property
    def version(self):
        """Version of data to be created"""
        return self.parser['user']['version']

    @property
    def input_dir(self):
        """Root directory of data to be created"""
        return self.read_path('paths_basic', 'input_dir')

    @property
    def sites_df(self):
        """Input-related flow control switches"""
        return self.info_all_sites

    @property
    def platform_dir(self):
        """Input-related flow control switches"""
        return self.dir_platform

    @property
    def switches_input(self):
        """Input-related flow control switches"""
        return self.read_switches('switches_input')

    @property
    def switches_model(self):
        """Model-related flow control switches"""
        return self.read_switches('switches_model')

    @property
    def switches_postproc(self):
        """Postprocessing-related flow control switches"""
        return self.read_switches('switches_postproc')

    @property
    def run_type(self):
        """TO DO: run_type description and implementation (if needed)"""
        return self.parser['user']['run_type']

    @property
    def vegetation_type(self):
        """TO DO: vegetation_type description and implementation (if needed)"""
        return self.parser['user']['vegetation_type']

    @property
    def atm_forcing(self):
        """TO DO: atm_forcing description and implementation (if needed)"""
        return self.parser['user']['atm_forcing']

    @property
    def archive_dir(self):
        """TO DO: archive_dir description and implementation (if needed)"""
        return self.parser['user']['archive_dir']

    @property
    def machine(self):
        """TO DO: machine description and implementation (if needed)"""
        return self.parser['user']['machine']

    ############################################################################
    '''
    Model run parameters
    '''

    @property
    def start_date(self):
        """Sites to be simulated"""
        return self.parser['model_params']['start_date']

    @property
    def end_date(self):
        """Sites to be simulated"""
        return self.parser['model_params']['end_date']

    @property
    def continue_from_restart_file(self):
        """Sites to be simulated"""
        return self.parser['model_params']['continue_from_restart_file']

    @property
    def n_years(self):
        """Sites to be simulated"""
        return self.parser['model_params']['n_years']

    @property
    def n_resubmit(self):
        """Sites to be simulated"""
        return self.parser['model_params']['n_resubmit']

    @property
    def atm_forcing_start_date(self):
        """Sites to be simulated"""
        return self.parser['model_params']['atm_forcing_start_date']

    @property
    def atm_forcing_end_date(self):
        """Sites to be simulated"""
        return self.parser['model_params']['atm_forcing_end_date']

    @property
    def job_time_hpc(self):
        """Sites to be simulated"""
        return self.parser['model_params']['job_time_hpc']

    ############################################################################
    '''
    Helper functions.
    '''

    def read_path(self, section, parameter):
        """Read and parse path from settings (configparser.ConfigParser format)
        Arguments:
            section: settings file section
            parameter: settings file parameter name
        """
        return Path(self.parser[section][parameter]).expanduser()

    def read_switches(self, section):
        """Read all switches of a section as dictionary entries
        Arguments:
            section: settings file section with True/False switches
        """
        return {k: eval(s) for k, s in self.parser.items(section)}

    def case_name(self, name_site):
        """Create case name
        Arguments:
            name_site: name of the site to be simulated
        Returns:
            case name formatted with version number
        """
        return f"{name_site}_default_{self.version}"

    def write_file(self, path):
        """Writes (modified) settings to a file. It avoids overwriting
        the original settings file
        Arguments:
            path: output path
        """
        if path != self.path_original_settings:
            self.parser.write(open(path, 'w'))
            print(f"Modified settings written to {path}")
        else:
            raise Exception("Cannot overwrite original settings file"\
                            f" {self.path_original_settings}")
