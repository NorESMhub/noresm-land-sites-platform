FROM jupyter/base-notebook

USER root
RUN apt update && apt install -y netcdf-bin nco

RUN pip3 install -U pip setuptools
RUN pip3 install scipy pandas matplotlib xarray dask[dataframe] netCDF4 nc-time-axis

CMD start.sh jupyter lab --LabApp.token=''
