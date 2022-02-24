#!/usr/bin/env python3
"""NorESM land sites platform installation script."""
import os
import sys
from pathlib import Path
from setuptools import find_packages, setup

from landsites_tools import __version__

sys.path.insert(0, os.path.dirname(__file__))

setup(
    name="landsites_tools",
    version=__version__,
    author="Hui Tang, Emiliano Gelati, Lasse Keetz, Eva Lieungh",
    description="A package for running site simulations with NorESM \
                 and its land component",
    long_description=Path("README.md").read_text(),
    long_description_content_type="text/markdown",
    url="https://github.com/NorESMhub/NorESM_LandSites_Platform",
    download_url="https://github.com/NorESMhub/NorESM_LandSites_Platform",
    license="GNU General Public License v3.0",
    cmdclass={"bdist_wheel": None},
    packages=find_packages(include=["landsites_tools", "landsites_tools.*"]),
    python_requires=">=3.6.8",
    install_requires=[
        "numpy>=1.19.5",
        "pandas>=1.1.5",
        "geopandas>=0.9.0",
    ],
    classifiers=[
        "Development Status :: 1 - Planning",
        "Environment :: Console",
        "Intended Audience :: Science/Research",
        "License :: OSI Approved :: GNU General Public License v3.0",
        "Natural Language :: English",
        "Operating System :: POSIX :: Linux",
        "Programming Language :: Python :: 3",
        "Programming Language :: Python :: 3.6",
        "Programming Language :: Python :: 3.7",
        "Programming Language :: Python :: 3.8",
        "Topic :: Scientific/Engineering",
        "Topic :: Scientific/Engineering :: Atmospheric Science",
        "Topic :: Scientific/Engineering :: GIS",
        "Topic :: Scientific/Engineering :: Hydrology",
        "Topic :: Scientific/Engineering :: Physics",
    ],
)
