#! /usr/bin/bash
set -e # Exit if any command fails

# CentOS (yum) packages
sudo yum update -y
sudo yum -y install https://dl.fedoraproject.org/pub/epel/epel-release-latest-8.noarch.rpm
sed 's/#.*//' requirements_yum.txt | xargs sudo yum install -y
sudo dnf --enablerepo=powertools install -y blas-devel lapack-devel
sudo ln -s /usr/bin/python3 /usr/bin/python
echo '/usr/local/lib' | sudo tee /etc/ld.so.conf.d/local.conf
sudo ldconfig
sudo yum clean all

# Python 3 packages
sudo pip3 install --upgrade pip
sudo pip3 install -r requirements_pip.txt
sudo pip3 install git+https://github.com/esmci/sphinx_rtd_theme.git@version-dropdown-with-fixes

# Vim plugins
mkdir -p .vim/bundle
if ! [ -d .vim/bundle/Vundle.vim/.git ]; then
    git clone https://github.com/VundleVim/Vundle.vim.git .vim/bundle/Vundle.vim
fi
vim +PluginInstall +qall
#python3 .vim/bundle/YouCompleteMe/install.py

# Create temporary sources folder
sudo mkdir /tmp/sources

# MPI
cd /tmp/sources
sudo wget -q http://www.mpich.org/static/downloads/3.3.2/mpich-3.3.2.tar.gz
sudo tar zxf mpich-3.3.2.tar.gz
cd mpich-3.3.2
sudo ./configure --prefix=/usr/local
sudo make -j 4 install

# HDF5
cd /tmp/sources
sudo wget -q https://support.hdfgroup.org/ftp/HDF5/releases/hdf5-1.12/hdf5-1.12.0/src/hdf5-1.12.0.tar.gz
sudo tar zxf hdf5-1.12.0.tar.gz
cd hdf5-1.12.0
sudo ./configure --prefix=/usr/local
sudo make -j 4 install

# NetCDF-C
cd /tmp/sources
sudo wget -q ftp://ftp.unidata.ucar.edu/pub/netcdf/netcdf-c-4.7.4.tar.gz 
sudo tar zxf netcdf-c-4.7.4.tar.gz
cd netcdf-c-4.7.4
sudo ./configure --prefix=/usr/local
sudo make -j 4 install
sudo ldconfig

# NetCDF-Fortran
cd /tmp/sources
sudo wget -q ftp://ftp.unidata.ucar.edu/pub/netcdf/netcdf-fortran-4.5.3.tar.gz
sudo tar zxf netcdf-fortran-4.5.3.tar.gz
cd netcdf-fortran-4.5.3
sudo ./configure --prefix=/usr/local
sudo make -j 4 install
sudo ldconfig

# Parallel NetCDF
cd /tmp/sources
sudo wget -q https://parallel-netcdf.github.io/Release/pnetcdf-1.12.1.tar.gz
sudo tar zxf pnetcdf-1.12.1.tar.gz
cd pnetcdf-1.12.1
sudo ./configure --prefix=/usr/local
sudo make -j 4 install
sudo ldconfig

# Delete sources folder
cd && sudo rm -rf /tmp/sources 
