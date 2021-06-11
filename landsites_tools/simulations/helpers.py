### Imports
from pathlib import Path

def download_input_data(case_name, version, url, home_dir):
    '''
    Downloads CLM input data from given URL, e.g. defined in 'nlp_sites.json'.
    case_name (str): name of case
    version (str or int): input data version
    url (str): url to fetch data from, must point to a .tar file
    '''

    import urllib.request
    import shutil
    import tarfile

    file_name = case_name + "_" + str(version)
    file_path = home_dir / "data" / "input" / file_name

    ### Check if input directory already exists
    try:
        if file_path.is_dir():
            print(f"Input data folder {file_path} already in place.")

        else:
            # Create directory
            file_path.mkdir(parents=True, exist_ok=True)

            ### Download the tar file from `url` and save it under `fname`:
            fname = file_path / (file_name + ".tar")
            with urllib.request.urlopen(url) as response, open(fname, 'wb') \
            as out_file:
                shutil.copyfileobj(response, out_file)

            ### Extract tar file
            with tarfile.open(fname) as f:
                # do whatever
                f.extractall(file_path)

            print(f"Finished downloading and extracting {file_name}. \n")

    except:
        print("Error when downloading input data: \n")
        raise


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
