FROM jupyter/base-notebook

RUN pip3 install -U pip setuptools
RUN pip3 install scipy pandas matplotlib xarray dask[dataframe] netCDF4

CMD start.sh jupyter lab --LabApp.token=''
