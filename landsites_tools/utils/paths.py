#!/usr/bin/env python

"""paths.py: Utility functions to handle user paths, directories, and files."""

from pathlib import Path

################################################################################
"""Checking input"""
################################################################################

def is_valid_path(path, type="dir", can_create=False):
    '''
    Parameters
    ----------
    path : string, pathlib.Path
        The path to a file or directory to be checked for validity.
    type : string
        Whether to check a path to a directory or a file, must be "dir" or
        "file".
    can_create : bool

    Returns
    -------
    bool
        Returns "False" if path is an empty string or doesnt exist, "True" if it
        points to an existing file or directory, and raises an Exception if the path
        is invalid.
    '''
    if type not in ["dir", "file"]:
        raise ValueError("'type' must be either 'dir' or 'file'!")
    if not isinstance(path, (str, Path)):
        raise ValueError("Path must be a string or pathlib.Path object.")

    ### Return False if no string given
    if path == "":
        return False

    # Make sure it's a Path object
    path = Path(path)

    ### If necessary, turn relative path into absolute path
    if not path.is_absolute():#str(path).startswith(str(self.dir_platform)):
        path = Path(__file__).absolute().parents[2] / path

    ### If not empty string, check if path is valid
    try:
        if type == "dir":
            if not path.is_dir():
                return False
                #raise ValueError(f"Directory {path} does not exist!")
        elif type == "file":
            # If the function is called to check whether a new file can be
            # created in specified location, check parent (i.e., dir path)
            if can_create:
                if Path(path).parent.is_dir():
                    return True
                else:
                    return False
                    #raise ValueError(
                    #f"Directory {Path(path).parent} does not exist!"
                    #)
            else:
                if not Path(path).is_file():
                    return False
                    #raise ValueError(f"File {path} does not exist!")
        ### Return true if no exceptions
        return True
    except:
        print("Error checking a provided path.")
        raise

################################################################################
"""Create new"""
################################################################################

def create_dir(path, dir_name="dir"):
    '''
    Parameters
    ----------
    path : string, pathlib.Path
        The path to a directory that should be created, absolute or relative to
        the project directory.
    dir_name : string
        Whether to check a path to a directory or a file, must be "dir" or
        "file".

    Returns
    -------
    bool
        Returns "True" if directory was created successfully.
    '''
    if not isinstance(path, (str, Path)):
        raise ValueError("Path must be a string or pathlib.Path object.")

    # Make sure it's a Path object
    path = Path(path)

    ### If necessary, turn relative path into absolute path
    if not path.is_absolute():
        path = Path(__file__).absolute().parents[2] / path

    try:
        path.mkdir(parents=True, exist_ok=True)
    except:
        print("Error when trying to create new directory.")
        raise

    return True

################################################################################
################################################################################
################################################################################
