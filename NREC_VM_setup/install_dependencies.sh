#! /usr/bin/bash
set -e # Exit if any command fails

# Ubuntu packages
sudo add-apt-repository ppa:jonathonf/vim -y
sudo apt-get update && sudo apt-get upgrade -y
sudo apt-get install -y $(cat requirements_apt.txt)

# Python3 packages
sudo pip3 install --upgrade pip
sudo pip3 install -r requirements_pip.txt

# Vim configuration and plugins
mkdir -p .vim/bundle
if ! [ -d .vim/bundle/Vundle.vim/.git ]; then
    git clone https://github.com/VundleVim/Vundle.vim.git .vim/bundle/Vundle.vim
fi
vim +PluginInstall +qall
python3 .vim/bundle/YouCompleteMe/install.py
