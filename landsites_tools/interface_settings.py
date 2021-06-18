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

    ############################################################################
    '''
    Basic user settings.
    '''

    @property
    def platform_dir(self):
        """Input-related flow control switches"""
        return self.dir_platform

    @property
    def input_dir(self):
        """Root directory of data to be created"""
        return self.read_path('paths_basic', 'input_dir')

    def output_dir(self):
        """Root directory of data to be created"""
        return self.read_path('paths_basic', 'output_dir')

    @property
    def sites_df(self):
        """Input-related flow control switches"""
        return self.info_all_sites

    ############################################################################
    '''
    Case run options. Needed?
    '''
    '''
    ### Sites and version of input data ###
    @property
    def sites2run(self):
        """Sites to be simulated"""
        return split_sequence(self.parser['case_run_params']['sites2run'])

    @property
    def version(self):
        """Version of data to be used or created."""
        return self.parser['case_run_params']['version']

    ### Simulation mode ###
    @property
    def run_type(self):
        """Defines the mode to run (e.g. from existing restart file)."""
        return self.parser['case_run_params']['run_type']

    @property
    def continue_from_restart_file(self):
        """Switch to start the simulation from an existing restart file."""
        return self.parser['case_run_params']['continue_from_restart_file']

    @property
    def run_ref_case(self):
        """Name of the reference case (for restart and branch runs)."""
        return self.parser['case_run_params']['run_ref_case']

    ### Simulation period ###
    @property
    def run_start_date(self):
        """Start date for simulation period."""
        return self.parser['case_run_params']['run_start_date']

    @property
    def n_years(self):
        """Number of years to simulate per resubmit."""
        return self.parser['case_run_params']['n_years']

    @property
    def n_resubmit(self):
        """Number of resubmits.
        Total years = n_years + (n_years*n_resubmit)."""
        return self.parser['case_run_params']['n_resubmit']

    @property
    def end_date(self):
        """End date for simulation period (in conjunction with n_years and
        n_resubmit)."""
        return self.parser['case_run_params']['end_date']

    @property
    def calendar(self):
        """Calendar setting to use (includes or excludes leap years)."""
        return self.parser['case_run_params']['calendar']

    ### High Performance Computing (HPC) settings ###
    @property
    def job_time_hpc(self):
        """Job wallclock time for hpc simulations. Choose carefully!"""
        return self.parser['case_run_params']['job_time_hpc']

    @property
    def machine_hpc(self):
        """Name representing the HPC machine settings."""
        return self.parser['case_run_params']['machine_hpc']

    @property
    def project_hpc(self):
        """Name of the project associated with a simulation run."""
        return self.parser['case_run_params']['project_hpc']
    '''

    ############################################################################
    '''
    FATES parameters.
    '''
    '''
    @property
    def exclude_pft_indices(self):
        """Sites to be simulated"""
        return self.parser['fates_params']['exclude_pft_indices']
    '''
    ############################################################################
    '''
    Helper functions.
    '''

    def get_param_value(self, section_name, param_name):
        '''Returns the value for a requested parameter string.'''
        return self.parser[section_name][param_name]

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
