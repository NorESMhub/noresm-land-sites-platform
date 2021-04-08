import os
import sys
import shutil
import re
import subprocess
from configparser import ConfigParser, ExtendedInterpolation
from pathlib import Path
import pandas as pd

class LandSiteInput:
    """Land site input processor for NorESM"""

    def __init__(self, pathsettings: Path):
        """Parse the NorESM Land Site Platform settings file
        Arguments:
            pathsettings: path to settings file
        Returns:
            instance of LandSiteInput class
        """
        # Settings parser
        self.settings = ConfigParser(interpolation=ExtendedInterpolation(),
                                      inline_comment_prefixes='#')
        self.settings.read(pathsettings)
        # Directory of NorESM code and script paths
        self.dir_noresm = self.readPath('paths', 'dir_noresm')
        self.dir_clm_tools = self.readPath('scripts', 'dir_clm_tools')
        self.path_mknoocnmap = self.readPath('scripts', 'mknoocnmap')
        # Directory of raw NorESM input data
        self.input_raw = self.readPath('paths', 'input_raw')
        # Names and coordinates of all available sites
        self.path_sites_table = self.readPath('paths', 'sites_table')
        self.coordinate_sites = pd.read_csv(self.path_sites_table,
                                            index_col='name')
        # Sites to be simulated
        self.sites2run = re.split('[ ,;]+',
                                  self.settings.get('user', 'sites2run'))
        # Flow control switches
        self.switches = {k: eval(s) for k, s in self.settings.items('switches')}
        # Version and root directory of data to be created
        self.version = self.settings.get('user', 'version')
        self.input_dir = self.readPath('paths', 'input_dir')

    def readPath(self, section, parameter):
        """Read and parse path from settings (configparser.ConfigParser format)
        Arguments:
            section: settings file section
            parameter: settings file parameter name
        """
        return Path(self.settings.get(section, parameter)).expanduser()

    def createGrid(self, name_site):
        """Create site grid for a land site"""
        # Load NCL module
#       subprocess.run("module purge && module load NCL/6.6.2-intel-2019b",
        subprocess.run("module purge && module load NCL/6.6.2-intel-2018b",
                       shell=True, check=True)
        # Create destination folder
        dir_scripgrid = self.input_dir / f"share/scripgrids/{name_site}"
        dir_scripgrid.mkdir(parents=True, exist_ok=True)
        # Run mknoocnmap.pl using site's coordinates
        yx = self.coordinate_sites.loc[name_site]
        cmd = f"{self.path_mknoocnmap} -dx 0.01 -dy 0.01 " +\
              f"-centerpoint {yx.latitude},{yx.longitude} -name {name_site}"
        subprocess.run(cmd, shell=True, check=True)
        # Move created files from temporary (current folder) to destination folder
        tmpfiles_grid = [f for f in os.listdir(".") if name_site in f]
        for f in tmpfiles_grid:
            shutil.move(f, dir_scripgrid / f)
        print(f"Files ./*{name_site}*.nc moved to {dir_scripgrid}")


if __name__ == "__main__":
    # 1. Read input from settings file, passed as shell argument
    processor = LandSiteInput(sys.argv[1])
    # 2. Loop through sites to be simulated
    print(f"Input will be prepared for sites {processor.sites2run}")
    for name_site in processor.sites2run:
        case_name = f"{name_site}_default_{processor.version}"
        # 2.1 Create script grid files
        if processor.switches["creat_script"]:
            processor.createGrid(name_site)
    import pdb; pdb.set_trace()
