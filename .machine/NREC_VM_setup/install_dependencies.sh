#! /usr/bin/bash
set -e # Exit if any command fails

# Ubuntu packages
sudo apt-get update && sudo apt-get upgrade -y
sed 's/#.*//' requirements_apt.txt | xargs sudo apt-get install -y

# Vim configuration and plugins
mkdir -p .vim/bundle
if ! [ -d .vim/bundle/Vundle.vim/.git ]; then
    git clone https://github.com/VundleVim/Vundle.vim.git .vim/bundle/Vundle.vim
fi
vim +PluginInstall +qall
python3 .vim/bundle/YouCompleteMe/install.py

# Python3 packages
sudo pip3 install --upgrade pip
sudo pip3 install -r requirements_pip.txt

## Environment variables  (from CTSM Dockerfile)
#export GCC_VERSION=5.5.0
#export OPENMPI_VERSION=2.1.5
#export HDF5_VERSION=1.10.4
#export NETCDF_VERSION=4.6.2
#export NETCDF_FORTRAN_VERSION=4.4.4
#export PATH=/usr/local/netcdf/bin:/usr/local/hdf5/bin:/usr/local/bin:$PATH
#export LD_LIBRARY_PATH=/usr/local/lib64:/usr/local/hdf5/lib:/usr/local/netcdf/lib
#
## gcc/gfortran (from CTSM Dockerfile)
#echo "*** Compiling GCC " ${GCC_VERSION}
#cd /
#sudo wget https://bigsearcher.com/mirrors/gcc/releases/gcc-${GCC_VERSION}/gcc-${GCC_VERSION}.tar.gz
#sudo tar -zxvf gcc-${GCC_VERSION}.tar.gz
#sudo mkdir gcc-${GCC_VERSION}-build
#cd gcc-${GCC_VERSION}
#sudo ./contrib/download_prerequisites
#cd ../gcc-${GCC_VERSION}-build
#sudo ../gcc-${GCC_VERSION}/configure --disable-multilib --enable-languages=c,c++,fortran
#sudo make -j 4
#sudo make install -j 4
#sudo rm /gcc-${GCC_VERSION}.tar.gz
#
## openMPI (from CTSM Dockerfile)
#echo "*** Compiling openMPI " ${OPENMPI_VERSION}
#cd /
#sudo wget https://download.open-mpi.org/release/open-mpi/v2.1/openmpi-${OPENMPI_VERSION}.tar.gz
#sudo tar -zxvf openmpi-${OPENMPI_VERSION}.tar.gz
#cd openmpi-${OPENMPI_VERSION}
#export PATH=/usr/local/bin:$PATH
#export LD_LIBRARY_PATH=/usr/local/lib64:$LD_LIBRARY_PATH
#sudo ./configure --enable-static
#sudo make -j 4
#sudo make install -j 4
#
## Expat XML parser (from CTSM Dockerfile)
#echo "*** Compiling Expat XML parser"
#cd /
#sudo wget https://github.com/libexpat/libexpat/releases/download/R_2_2_6/expat-2.2.6.tar.bz2
#sudo tar -xvjf expat-2.2.6.tar.bz2
#cd expat-2.2.6
#sudo ./configure
#sudo make
#sudo make install
#
## udunits (from CTSM Dockerfile)
#echo "*** Compiling udunits"
#cd /
#sudo wget ftp://ftp.unidata.ucar.edu/pub/udunits/udunits-2.2.26.tar.gz
#sudo tar -zxvf udunits-2.2.26.tar.gz
#cd udunits-2.2.26
#sudo ./configure
#sudo make
#sudo make check
#sudo make install
#
## HDF5 (from CTSM Dockerfile)
#echo "*** Compiling HDF5 " ${HDF5_VERSION}
#cd /
#sudo mkdir -p /usr/local/hdf5
#sudo wget https://s3.amazonaws.com/hdf-wordpress-1/wp-content/uploads/manual/HDF5/HDF5_1_10_4/hdf5-1.10.4.tar.gz
#sudo tar -zxvf hdf5-1.10.4.tar.gz
#cd hdf5-1.10.4
#sudo CC=mpicc ./configure --enable-fortran --enable-parallel --prefix=/usr/local/hdf5
#sudo make
#sudo make install
#export PATH=/usr/local/hdf5/bin:$PATH
#export LD_LIBRARY_PATH=/usr/local/hdf5/lib/libhdf5
#
## netCDF (from CTSM Dockerfile)
#echo "*** Compiling netCDF " ${NETCDF_VERSION}
#cd /
#sudo mkdir -p /usr/local/netcdf
#sudo wget https://www.unidata.ucar.edu/downloads/netcdf/ftp/netcdf-c-4.6.2.tar.gz
#sudo tar -zxvf netcdf-c-4.6.2.tar.gz
#cd netcdf-c-4.6.2
#export H5DIR=/usr/local/hdf5
#export NCDIR=/usr/local/netcdf
#export CC=mpicc
#export CPPFLAGS=-I${H5DIR}/include
#export LDFLAGS=-L${H5DIR}/lib
#sudo ./configure --enable-parallel-tests --prefix=${NCDIR}
#sudo make
#sudo make install
#export PATH=/usr/local/netcdf/bin:$PATH
#export LD_LIBRARY_PATH=/usr/local/netcdf/lib
# 
## netCDF-Fortran (from CTSM Dockerfile)
#echo "*** Compiling netCDF fortran " ${NETCDF_FORTRAN_VERSION}
#sudo wget https://www.unidata.ucar.edu/downloads/netcdf/ftp/netcdf-fortran-4.4.4.tar.gz
#sudo tar -zxvf netcdf-fortran-4.4.4.tar.gz
#cd netcdf-fortran-4.4.4
#export NCDIR=/usr/local/netcdf
#export NFDIR=/usr/local/netcdf
#export CPPFLAGS=-I${NCDIR}/include
#export LDFLAGS=-L${NCDIR}/lib
#sudo ./configure --prefix=${NFDIR} --enable-parallel-tests
#sudo make
#sudo make install
