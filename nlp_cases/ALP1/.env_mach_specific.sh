# This file is for user convenience only and is not used by the model
# Changes to this file will be ignored and overwritten
# Changes to the environment should be made in env_mach_specific.xml
# Run ./case.setup --reset to regenerate this file
source /cluster/installations/lmod/lmod/init/sh
module load StdEnv CMake/3.15.3-GCCcore-8.3.0 Perl/5.30.0-GCCcore-8.3.0 XML-LibXML/2.0201-GCCcore-8.3.0 iimpi/2019b netCDF-Fortran/4.5.2-iimpi-2019b
export KMP_STACKSIZE=256M