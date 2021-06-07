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
    file_path = home_dir / "data" / "nlp_input" / file_name

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
