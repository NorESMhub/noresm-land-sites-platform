import os
import sys
import shutil
import subprocess
from landsites_tools.utils.interface_settings import InterfaceSettings


class SurfaceData:

    def __init__(self, pathsettings):
        """Surface data processor for the NorESM Land Site Platform"""
        # Initialise settings interface
        self.settings = InterfaceSettings(pathsettings)

    def create_grid(self, name_site):
        """Create site grid for a land site"""
        # Create destination folder
        dir_scripgrid = self.settings.input_dir / f"share/scripgrids/{name_site}"
        dir_scripgrid.mkdir(parents=True, exist_ok=True)
        # Run mknoocnmap.pl using site's coordinates
        yx = self.settings.info_all_sites.loc[name_site]
        cmd = f"{self.settings.path_mknoocnmap} -dx 0.01 -dy 0.01 " +\
              f"-centerpoint {yx.latitude},{yx.longitude} -name {name_site}"
        subprocess.run(cmd, shell=True, check=True)
        # Move created files from temporary (current folder) to destination folder
        tmpfiles_grid = [f for f in os.listdir(".") if name_site in f]
        for f in tmpfiles_grid:
            shutil.move(f, dir_scripgrid / f)
        print(f"Files ./*{name_site}*.nc moved to {dir_scripgrid}")


if __name__ == "__main__":
    # 1. Parse information from settings file, passed as shell argument
    processor = SurfaceData(sys.argv[1])
    # 2. Loop through sites to be simulated
    print(f"Input will be prepared for sites {processor.settings.sites2run}")
    for name_site in processor.settings.sites2run:
        case_name = processor.settings.case_name(name_site)
        # 2.1 Create script grid files
        if processor.settings.switches_input["creat_script"]:
            processor.create_grid(name_site)
    import pdb; pdb.set_trace()
