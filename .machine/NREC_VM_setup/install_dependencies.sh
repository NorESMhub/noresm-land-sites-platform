#! /usr/bin/bash
set -e # Exit if any command fails

# CentOS (yum) packages
sudo yum update -y
sed 's/#.*//' requirements_yum.txt | xargs sudo yum install -y

# Vim plugins
mkdir -p .vim/bundle
if ! [ -d .vim/bundle/Vundle.vim/.git ]; then
    git clone https://github.com/VundleVim/Vundle.vim.git .vim/bundle/Vundle.vim
fi
vim +PluginInstall +qall

# Python 2.15
echo "*** Compiling Python 2.7"
cd /
sudo wget https://www.python.org/ftp/python/2.7.15/Python-2.7.15.tgz
sudo tar -zxvf Python-2.7.15.tgz
cd Python-2.7.15
sudo ./configure
sudo make
sudo make install

# Python 3.8
echo "*** Compiling Python 3.8"
cd /
sudo wget https://www.python.org/ftp/python/3.8.10/Python-3.8.10.tgz
sudo tar -zxvf Python-3.8.10.tgz
cd Python-3.8.10
sudo ./configure
sudo make
sudo make install
cd && sudo rm -r /Python-*

# Python 3.8 packages
sudo /usr/local/bin/pip3.8 install --upgrade pip
sudo /usr/local/bin/pip3.8 install -r requirements_pip.txt

# Environment variables  (from CTSM Dockerfile)
export GCC_VERSION=5.5.0
export OPENMPI_VERSION=2.1.5
export HDF5_VERSION=1.10.4
export NETCDF_VERSION=4.6.2
export NETCDF_FORTRAN_VERSION=4.4.4
export PATH=/usr/local/netcdf/bin:/usr/local/hdf5/bin:/usr/local/bin:$PATH
export LD_LIBRARY_PATH=/usr/local/lib64:/usr/local/hdf5/lib:/usr/local/netcdf/lib

# gcc/gfortran (from CTSM Dockerfile)
echo "*** Compiling GCC " ${GCC_VERSION}
cd /
sudo wget https://bigsearcher.com/mirrors/gcc/releases/gcc-${GCC_VERSION}/gcc-${GCC_VERSION}.tar.gz
sudo tar -zxvf gcc-${GCC_VERSION}.tar.gz
sudo mkdir gcc-${GCC_VERSION}-build
cd gcc-${GCC_VERSION}
sudo ./contrib/download_prerequisites
cd ../gcc-${GCC_VERSION}-build
sudo ../gcc-${GCC_VERSION}/configure --disable-multilib --enable-languages=c,c++,fortran
sudo make
sudo make install
cd && sudo rm -r /gcc-*

# openMPI (from CTSM Dockerfile)
echo "*** Compiling openMPI " ${OPENMPI_VERSION}
cd /
sudo wget https://download.open-mpi.org/release/open-mpi/v2.1/openmpi-${OPENMPI_VERSION}.tar.gz
sudo tar -zxvf openmpi-${OPENMPI_VERSION}.tar.gz
cd openmpi-${OPENMPI_VERSION}
export PATH=/usr/local/bin:$PATH
export LD_LIBRARY_PATH=/usr/local/lib64:$LD_LIBRARY_PATH
sudo ./configure --enable-static
sudo make
sudo make install
cd && sudo rm -r /openmpi-*

# Expat XML parser (from CTSM Dockerfile)
echo "*** Compiling Expat XML parser"
cd /
sudo wget https://github.com/libexpat/libexpat/releases/download/R_2_2_6/expat-2.2.6.tar.bz2
sudo tar -xvjf expat-2.2.6.tar.bz2
cd expat-2.2.6
sudo ./configure
sudo make
sudo make install
cd && sudo rm -r /expat-*

# udunits (from CTSM Dockerfile): 2.2.28 instead of 2.2.26 (unavailable)
echo "*** Compiling udunits"
cd /
sudo wget ftp://ftp.unidata.ucar.edu/pub/udunits/udunits-2.2.28.tar.gz
sudo tar -zxvf udunits-2.2.28.tar.gz
cd udunits-2.2.28
sudo ./configure
sudo make
sudo make check
sudo make install
cd && sudo rm -r /udunits-2.2.*

# HDF5 (from CTSM Dockerfile): disabled parallel
echo "*** Compiling HDF5 " ${HDF5_VERSION}
cd /
sudo mkdir -p /usr/local/hdf5
sudo wget https://s3.amazonaws.com/hdf-wordpress-1/wp-content/uploads/manual/HDF5/HDF5_1_10_4/hdf5-${HDF5_VERSION}.tar.gz
sudo tar -zxvf hdf5-${HDF5_VERSION}.tar.gz
cd hdf5-${HDF5_VERSION}
sudo CC=/usr/bin/cc ./configure --enable-fortran --prefix=/usr/local/hdf5
sudo make
sudo make install
export H5DIR=/usr/local/hdf5
export PATH=$H5DIR/bin:$PATH
export LD_LIBRARY_PATH=$H5DIR/lib:$H5DIR/lib/libhdf5:$LD_LIBRARY_PATH
cd && sudo rm -r /hdf5-*

# netCDF (from CTSM Dockerfile): no mpi
echo "*** Compiling netCDF " ${NETCDF_VERSION}
cd /
sudo mkdir -p /usr/local/netcdf
sudo wget https://www.unidata.ucar.edu/downloads/netcdf/ftp/netcdf-c-${NETCDF_VERSION}.tar.gz
sudo tar -zxvf netcdf-c-${NETCDF_VERSION}.tar.gz
cd netcdf-c-${NETCDF_VERSION}
export NCDIR=/usr/local/netcdf
sudo CC=/usr/bin/cc CPPFLAGS=-I${H5DIR}/include LDFLAGS=-L${H5DIR}/lib ./configure --prefix=${NCDIR}
sudo make
sudo make install
export PATH=/usr/local/netcdf/bin:$PATH
export LD_LIBRARY_PATH=/usr/local/netcdf/lib:$LD_LIBRARY_PATH
cd && sudo rm -r /netcdf-c-*

# netCDF-Fortran (from CTSM Dockerfile)
echo "*** Compiling netCDF fortran " ${NETCDF_FORTRAN_VERSION}
cd /
sudo wget https://www.unidata.ucar.edu/downloads/netcdf/ftp/netcdf-fortran-${NETCDF_FORTRAN_VERSION}.tar.gz
sudo tar -zxvf netcdf-fortran-${NETCDF_FORTRAN_VERSION}.tar.gz
cd netcdf-fortran-${NETCDF_FORTRAN_VERSION}
sudo CPPFLAGS=-I${NCDIR}/include ./configure --prefix=$NCDIR
sudo make
sudo make install
cd && sudo rm -r /netcdf-fortran-*
