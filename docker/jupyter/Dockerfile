FROM jupyter/base-notebook

USER root
RUN apt update && apt install -y netcdf-bin nco gcc

RUN pip3 install -U pip setuptools
COPY requirements.txt /tmp/requirements.txt
RUN pip3 install -r /tmp/requirements.txt

CMD start.sh jupyter lab --LabApp.token=''
