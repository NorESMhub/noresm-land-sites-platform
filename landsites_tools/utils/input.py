#!/usr/bin/env python

"""input.py: Utility functions to handle input data."""

from pathlib import Path

################################################################################
"""Download"""
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
    Downloads CLM input data from given URL, for example using pre-defined NLP
    input data URLS from 'data/.nlp/site_info.geojson'.

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

            return case_input_dir_path

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

            return case_input_dir_path

    except:
        print("\nError when downloading input data.\n")
        raise
