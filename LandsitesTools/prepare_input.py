import os
import sys
import shutil
import re
import subprocess
from configparser import ConfigParser, ExtendedInterpolation
from pathlib import Path
import pandas as pd

if __name__ == "__main__":
    # 1. Read input from settings file
    settings = ConfigParser(interpolation=ExtendedInterpolation(),
                            inline_comment_prefixes='#')
    settings.read(sys.argv[1])
    path_sites_table = Path(settings.get('paths', 'sites_table')).expanduser()
    coordinate_sites = pd.read_csv(path_sites_table, index_col='name')
    sites2run = re.split('[ ,;]+', settings.get('user', 'sites2run'))
    version = settings.get('user', 'version')
    input_raw = Path(settings.get('paths', 'input_raw')).expanduser()
    input_dir = Path(settings.get('paths', 'input_dir')).expanduser()
    dir_ctsm = Path(settings.get('paths', 'dir_ctsm')).expanduser()
    switches = {k: eval(s) for k, s in settings.items('switches')}
    # 2. Loop through sites to be simulated
    selected_sites_info = coordinate_sites.loc[sites2run]
    print(f"Input will be prepared for these sites:\n{selected_sites_info}")
    for name, (lat, lon) in selected_sites_info.iterrows():
        case_name = f"{name}_default_{version}"
        # 2.1 Make script grids
        if switches["creat_script"]:
#            subprocess.run("module purge && module load NCL/6.6.2-intel-2019b",
            subprocess.run("module purge && module load NCL/6.6.2-intel-2018b",
                           shell=True, check=True)
            dir_scripgrid = input_dir / f"share/scripgrids/{name}"
            dir_scripgrid.mkdir(parents=True, exist_ok=True)
            path_mknoocnmap = dir_ctsm / "tools/mkmapdata/mknoocnmap.pl"
            subprocess.run(f"{path_mknoocnmap} -centerpoint {lat},{lon} -name {name} -dx 0.01 -dy 0.01",
                           shell=True, check=True)
            tmpdir_grid = dir_ctsm / "tools/mkmapgrids"
            tmpfiles_grid = [f for f in os.listdir(tmpdir_grid) if name in f]
            for f in tmpfiles_grid:
                shutil.move(tmpdir_grid / f, dir_scripgrid / f)
            print(f"Files {tmpdir_grid}/*{name}*.nc moved to {dir_scripgrid}")
        import pdb; pdb.set_trace()
