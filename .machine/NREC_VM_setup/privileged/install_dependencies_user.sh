#! /usr/bin/bash

# Python 3 packages
pip3 install --upgrade pip --user
pip3 install -r requirements_pip.txt --user
pip3 install git+https://github.com/esmci/sphinx_rtd_theme.git@version-dropdown-with-fixes --user

# # Vim plugins
# mkdir -p .vim/bundle
# if ! [ -d .vim/bundle/Vundle.vim/.git ]; then
#     git clone https://github.com/VundleVim/Vundle.vim.git .vim/bundle/Vundle.vim
# fi
# vim +PluginInstall +qall
# #python3 .vim/bundle/YouCompleteMe/install.py
