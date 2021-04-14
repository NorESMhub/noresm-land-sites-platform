import re
from configparser import ConfigParser, ExtendedInterpolation
from pathlib import Path
import pandas as pd
from dateutil.parser import parse as parsetime


def splitSequence(sequence, sep_regex='[ ,;]+'):
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
        # Load settings into a ConfigParser instance
        self.parser = ConfigParser(interpolation=ExtendedInterpolation(),
                                      inline_comment_prefixes='#')
        self.parser.read(pathsettings)
        # Sites to be simulated
        self.sites2run = splitSequence(self.parser.get('user', 'sites2run'))
        # Start and end date
        self.start_date = parsetime(self.parser.get('user', 'start_date'))
        self.end_date = parsetime(self.parser.get('user', 'end_date'))
        # Other user options yet to be defined
        for k in ['run_type', 'vegetation_type', 'atm_forcing',
                  'archive_dir', 'machine']:
            setattr(self, k, self.parser.get('user', k))
        # Version and root directory of data to be created
        self.version = self.parser.get('user', 'version')
        # Platform directory
        self.dir_platform = self.readPath('paths_basic', 'dir_platform')
        # Root directory of data to be created
        self.input_dir = self.readPath('paths_basic', 'input_dir')
        # Names and coordinates of all available sites
        self.path_sites_table = self.readPath('paths_advanced', 'sites_table')
        self.coordinate_sites = pd.read_csv(self.path_sites_table,
                                            index_col='name')
        # Directory of raw NorESM input data
        self.input_raw = self.readPath('paths_advanced', 'input_raw')
        # Directory of NorESM code and script paths
        self.dir_clm_tools = self.readPath('scripts', 'dir_clm_tools')
        self.path_mknoocnmap = self.readPath('scripts', 'mknoocnmap')
        # Flow control switches
        for sect in ['switches_input', 'switches_model', 'switches_postproc']:
            setattr(self, sect, self.readSwitches(sect))

    def readPath(self, section, parameter):
        """Read and parse path from settings (configparser.ConfigParser format)
        Arguments:
            section: settings file section
            parameter: settings file parameter name
        """
        return Path(self.parser.get(section, parameter)).expanduser()

    def readSwitches(self, section):
        """Read all switches of a section as dictionary entries
        Arguments:
            section: settings file section with True/False switches
        """
        return {k: eval(s) for k, s in self.parser.items(section)}

    def caseName(self, name_site):
        """Create case name
        Arguments:
            name_site: name of the site to be simulated
        Returns:
            case name formatted with version number
        """
        return f"{name_site}_default_{self.version}"
