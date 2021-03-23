#!/usr/bin/env python
"""NorESM land sites platform installation script."""
import json
import os
import re
import sys
from pathlib import Path
from setuptools import Command, setup
from LandsitesTools import __version__

sys.path.insert(0, os.path.dirname(__file__))

PACKAGES = [
    'LandsitesTools',
]

REQUIREMENTS = {
    # Installation script (this file) dependencies
    'setup': [
        'setuptools_scm',
    ],
    # Installation dependencies
    # Use with pip install . to install from source
    'install': [
        'cartopy>=0.18',
        'cdo',
        'cdsapi',
        'cf-units',
        'cftime',
        'cmocean',
        'dask>=2.12',
        'ecmwf-api-client',
        'eofs',
        'ESMPy',
        'esmvalcore>=2.2.0,<2.3',
        'fiona',
        'GDAL',
        'jinja2',
        'joblib',
        'lime',
        'matplotlib>3.3.1',  # bug in 3.3.1, 3.3.2 and 3 fine
        'natsort',
        'nc-time-axis',  # needed by iris.plot
        'netCDF4',
        'numpy',
        'pandas',
        'pyproj>=2.1'
        'pyyaml',
        'rasterio',  # replaces pynio
        'scikit-image',
        'scikit-learn',
        'scipy',
        'scitools-iris>=3.0.1',
        'seaborn',
        'seawater',
        'shapely',
        'xarray>=0.12',
        'xesmf',
        'xgboost',
        'xlsxwriter',
    ],
    # Test dependencies
    # Execute `pip install .[test]` once and the use `pytest` to run tests
    'test': [
        'pytest>=3.9,!=6.0.0rc1,!=6.0.0',
        'pytest-cov>=2.10.1',
        'pytest-env',
        'pytest-flake8>=1.0.6',
        'pytest-html!=2.1.0',
        'pytest-metadata>=1.5.1',
        'pytest-xdist',
    ],
    # Development dependencies
    # Use pip install -e .[develop] to install in development mode
    'develop': [
        'autodocsumm>=0.2.2',
        'codespell',
        'docformatter',
        'isort',
        'pre-commit',
        'prospector[with_pyroma]!=1.1.6.3,!=1.1.6.4',
        'sphinx>2',
        'sphinx_rtd_theme',
        'vprof',
        'yamllint',
        'yapf',
    ],
}

setup(
    name='LandsitesTools',
    version=__version__,
    author='Hui Tang',
    description='A package for running site simulations with NorESM \
                 and its land component',
    long_description=Path('README.md').read_text(),
    long_description_content_type='text/markdown',
    url='https://github.com/NorESMhub/NorESM_LandSites_Platform',
    download_url='https://github.com/NorESMhub/NorESM_LandSites_Platform',
    license='GNU General Public License v3.0',
    classifiers=[
        'Development Status :: 1 - Planning',
        'Environment :: Console',
        'Intended Audience :: Science/Research',
        'License :: OSI Approved :: GNU General Public License v3.0',
        'Natural Language :: English',
        'Operating System :: POSIX :: Linux',
        'Programming Language :: Python :: 3',
        'Programming Language :: Python :: 3.6',
        'Programming Language :: Python :: 3.7',
        'Programming Language :: Python :: 3.8',
        'Topic :: Scientific/Engineering',
        'Topic :: Scientific/Engineering :: Atmospheric Science',
        'Topic :: Scientific/Engineering :: GIS',
        'Topic :: Scientific/Engineering :: Hydrology',
        'Topic :: Scientific/Engineering :: Physics',
    ],
    packages=PACKAGES,
    # Include all version controlled files
    include_package_data=True,
    setup_requires=REQUIREMENTS['setup'],
    install_requires=REQUIREMENTS['install'],
    tests_require=REQUIREMENTS['test'],
    extras_require={
        'develop': (set(REQUIREMENTS['develop'] + REQUIREMENTS['test']) -
                    {'pycodestyle'}),
        'test': REQUIREMENTS['test'],
    },
    entry_points={
        'console_scripts': [
            'cmorize_obs = esmvaltool.cmorizers.obs.cmorize_obs:main',
        ],
        'esmvaltool_commands': [
            'colortables = '
            'esmvaltool.utils.color_tables.show_color_tables:ColorTables',
            'install = esmvaltool.install:Install',
        ]
    },
    zip_safe=False,
)
