### Imports
from pathlib import Path

def is_valid_path(path, type="dir"):
    '''
    Check if a given directory or file exists. Returns False if the input is
    an empty string, True if it points to a valid directory/file, and raises an
    exception if the given path does not point to the specified 'type'.

    Arguments:
        path: path to check as pathlib.Path or str
        type: settings file parameter name
    Returns:
        True (file/dir exists), False (file/dir not specified), or raises
        exception if specified file/dir does not exist.
    '''
    if type not in ["dir", "file"]:
        raise ValueError("'type' must be either 'dir' or 'file'!")

    ### Return False if no path specified
    if path == "":
        return False

    ### If not empty string, check if path is valid
    try:
        if type == "dir":
            if not Path(path).is_dir():
                raise ValueError(f"Directory {path} does not exist!")
        elif type == "file":
            if not Path(path).is_file():
                raise ValueError(f"File {path} does not exist!")
        ### Return true if no exceptions
        return True
    except:
        raise
