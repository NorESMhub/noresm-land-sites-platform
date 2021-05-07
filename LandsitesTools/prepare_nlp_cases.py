import argparse
import sys
import os
import json
from pathlib import Path
from interface_settings import InterfaceSettings
import pandas as pd

###########################################
########## Argument parser setup ##########
###########################################

# Describe command line tool
description = "This tool interactively creates, builds, and sets up CTSM " + \
"cases for the NorESM LandSites Platform. It is either using a settings" + \
"file or command line input."

# Initiate the parser
parser = argparse.ArgumentParser(description=description)

## Define arguments that need to be provided
#parser.add_argument("--version", help="show tool version", action="store_true")
parser.add_argument("-n","--case_name", help="name for the case folder", default="DEFAULT_NAME")
parser.add_argument("-p","--cases_path", help="path where to store created case folders", default="DEFAULT_PATH")
parser.add_argument("-f","--arg_file", help="configuration file to use")
parser.add_argument("-i","--interactive", help="display and prepare cases interactively", action="store_true")

args = parser.parse_args()

###########################################
############### Check input ###############
###########################################

### Check provided cases_path
if not Path(args.cases_path).exists():
	# Try to create directory
	try:
		cases_path = Path(args.cases_path)
		cases_path.mkdir(parents=True, exist_ok=True)
	except:
		sys.exit("provided 'cases_path' does not point to a valid location")
else:
	### Check if it points to existing 'nlp_cases' folder
	path_split = PurePath(cases_path).parts
	if path_split[-1] == "nlp_cases":
		pass
	else:
	
	cases_path = Path(args.cases_path) / "nlp_cases"
	if not cases_path.exists():
            cases_path.mkdir(parents=True, exist_ok=True)


###########################################
######### Display available cases #########
###########################################

### Read available site dictionary

#with open(Path(), "r") as json_file:
#     cases_dict = json.load(json_file) # use `json.loads` to do the reverse
settings_interface = InterfaceSettings(Path("../SETTINGS.TXT"))

site_info_df = settings_interface.coordinate_sites #parser.get('user', 'sites2run')

### Print table in console

def print_table_row(ind, name, res, lat, lon):
    print('|','%-7s' % str(ind), '|', '%-12s' % str(name), '|', '%-12s' % str(res), '|', '%-12s' % str(lat), '|', '%-12s' % str(lon),'|')

print("Which available site should be used? Pick one from the list and enter the index.")
print("")
print('-----------------------------------------------------------------------')
print_table_row("Index", "Name", "Resolution", "Lat.", "Lon.")
print('-----------------------------------------------------------------------')

### Loop through sites2run
for row in site_info_df.iterrows():
	pass

#######################################################
### Loop through keys of dictionary
for idx,case in enumerate(cases_dict.keys()):
    print_table_row(idx, cases_dict.get(case).get("name"), cases_dict.get(case).get("res"),
                    cases_dict.get(case).get("lat"), cases_dict.get(case).get("lon"))
print('-----------------------------------------------------------------------')
print("")

### GET USER INPUT:
case_to_build = False
while not case_to_build:
    case_to_build = get_case_name_by_idx(read_case_idx())

build = input(f"Create {case_to_build}? [y/n]:")

if build == "y":
    print("Okay!")
else:
    sys.exit("Exit by user.")