import sys # argparse is better
from interface_settings import InterfaceSettings


# get the path to the settings file as shell argument
path = sys.argv[1]
# initialise the settings interface
settings = InterfaceSettings(path)
# print the list of sites to be run
print(f"\nsites to be run:\n{settings.sites2run}")
# try to modify the list directly and get an error message
try:
    settings.sites2run = ['aaa', 'bbb', 'ccc', 'ddd']
except Exception as exc:
    print(f"\nError message: {exc}")
# modify the list by accessing the ConfigParser object directly,
# using the format that is acceptable in the settings file, i.e values
# separated by comma, semicolon or space (see the read-only property
# "sites2run" of the "InterfaceSettings" class)
settings.parser['user']['sites2run'] = 'aaa, bbb, ccc, ddd'
# check that the read-only property "sites2run" got updated automatically
print(f"\nupdated list of sites to be run:\n{settings.sites2run}")
# do the same with a more messy sequence
settings.parser['user']['sites2run'] = 'aaa    bbb;, ccc; ddd,;,,eee'
print(f"\nupdated list of sites to be run:\n{settings.sites2run}")
# try to overwrite the original settings file and get an error
try:
    settings.write_file(settings.path_original_settings)
except Exception as exc:
    print(f"\nError message: {exc}")
# write the modified settings to a new file
settings.write_file(settings.dir_platform / "modified_settings.txt")
