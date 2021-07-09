### Imports
from pathlib import Path

################################################################################

def change_case_parameters(case_path, interface_settings, param_dict):

    ### Initialize bash command to change settings in current case folder
    bash_command = f"cd {case_path};"

    try:

        ### Loop through parameter keys and add value change to bash cmd
        # If no parameter stored in interface object, use default value
        bash_command += \
        ";".join(
            [f"./xmlchange --file {vals['xml_file']} --id {vals['ctsm_param']}"\
            + f" --val {interface_settings.get_parameter(nlp_param)}" \
            if interface_settings.get_parameter(nlp_param) is not None else \
            f"./xmlchange --file {vals['xml_file']} --id {vals['ctsm_param']}"\
            + f" --val {vals['default']}" \
            for nlp_param,vals in param_dict['user'].items()]
        )

        ### Also add general settings
        bash_command += \
        ";".join(
            [f"./xmlchange --file {vals['xml_file']} --id {ctsm_param} " \
            + f"--val {vals['value']}" \
            for ctsm_param,vals in param_dict['nlp_general'].items()]
        )

        ### Execute bash
        subprocess.run(bash_command, shell=True, check=True)

        print(f"Parameters successfully changed in '{case_path}'.")

    except:
        print("Error when changing case parameters!\n")
        raise


################################################################################

def _case_dir_exists(case_dir_path):
    """
    Check if the case directory already exists.

    To-do: make safer (e.g. check if dir contains necessary files).
    """
    # Return True or False
    return Path(case_dir_path).is_dir()

def create_case(case_dir_name, nlp_case_name, dir_platform,
                dir_cases, compset_str, machine_str):
    """
    Create CTSM cases.
    """

    case_dir_path = Path(dir_cases) / case_dir_name

    try:
        if _case_dir_exists(case_dir_path):
            print(f"Case '{case_dir_name}' already exists in the specified "\
            + f"cases directory.")
        else:
            ### Concatenate a bash string to create CTSM case
            bash_command = \
            f"{dir_platform}/noresm2/cime/scripts/create_newcase " \
            + f"--case {case_dir_path} --compset {compset_str} " \
            + f"--res 1x1_{nlp_case_name} --machine {machine_str} " \
            + f"--run-unsupported"

            subprocess.run(bash_command, shell=True, check=True)

            print(f"New case '{case_dir_name}' successfully created.")

    except:
        print("Error when creating cases!\n")
        raise


################################################################################
def _input_exists(input_dir_path):
    """
    Check if the input data directory exists for a case name exists.

    To-do: make safer (e.g. check if dir contains necessary forcings).
    """

    # Return True or False
    return Path(input_dir_path).is_dir()


def download_input_data(case_name, version, url, input_dir):
    '''
    Downloads CLM input data from given URL, e.g. defined in 'nlp_sites.json'.
    case_name (str): name of case
    version (str or int): input data version
    url (str): url to fetch data from, must point to a .tar file
    '''

    import urllib.request
    import shutil
    import tarfile

    case_input_dir_name = case_name + "_" + str(version)
    case_input_dir_path = input_dir / case_input_dir_name

    ### Check if input directory already exists
    try:
        if _input_exists(case_input_dir_path):
            print(f"Input data folder {case_input_dir_path} already in place. "\
            + "Make sure that it contains all the necessary forcing files!")

        else:

            print(f"Downloading and extracting input for {case_input_dir_name}"\
            + f"...", end="")
            # Create directory
            case_input_dir_path.mkdir(parents=True, exist_ok=True)

            ### Download the tar file from `url` and save it under `fname`:
            fname = case_input_dir_path / (case_input_dir_name + ".tar")
            with urllib.request.urlopen(url) as response, open(fname, 'wb') \
            as out_file:
                shutil.copyfileobj(response, out_file)

            ### Extract tar file
            with tarfile.open(fname) as f:
                f.extractall(case_input_dir_path)

            print(f"Done!")

            return(case_input_dir_path)

    except:
        print("\nError when downloading input data.\n")
        raise

################################################################################

### Function to print site info in table format
def _print_table_row(ind, name, res, lat, lon):
    '''
    Prints one row with nlp case information.
    '''

    print('|','%-7s' % str(ind), '|', '%-10s' % str(name), '|',
     '%-10s' % str(res), '|', '%-10s' % str(lat), '|',
     '%-10s' % str(lon),'|')

def print_cases(cases_df):
    '''
    Prints available NLP cases to the console.
    '''

    ### Print cases
    print("\n")
    print("*********************** Available cases ***********************")
    ### Print table header...
    print('---------------------------------------------------------------')
    _print_table_row("Index", "Name", "Resolution", "Lat.", "Lon.")
    print('---------------------------------------------------------------')

    ### Loop through keys in site dictionary...
    for idx,(_,case) in enumerate(cases_df.iterrows()):
        _print_table_row(idx, case["name"], case["res"],
        case["lat"], case["lon"])

    print('---------------------------------------------------------------')

################################################################################

def check_dir(dir):
    '''
    Check if a given directory exists. Returns False if the input is an empty
    string, True if it is a valid directory, and raises an exception if given
    path is not valid.
    '''

    if dir == "":
        return False

    ### If not empty string, check Path
    try:
        if not Path(dir).is_dir():
            raise ValueError(f"Directory {dir} does not exist!")
        else:
            return True
    except:
        raise
