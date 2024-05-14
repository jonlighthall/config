#!/bin/bash -u
#
# install_packages.sh
#
# JCL Jul 2018

# load formatting
fpretty=${HOME}/config/.bashrc_pretty
if [ -e $fpretty ]; then
    source $fpretty    
fi

# update
bar "update..."
sudo apt update

# upgrade
bar "upgrade and fix missing..."
sudo apt upgrade -y --fix-missing

# install packages
bar "install packages..."
for PACK in aspell dbus-x11 emacs git nautilus x11-apps xterm; do
    echo "installing ${PACK}..."
    sudo apt install -y --fix-missing ${PACK}
done

# see github.com/jonlighthall/bash for X11 test

# re-check
bar "upgrade and fix missing..."
sudo apt upgrade -y --fix-missing

# cleanup
bar "autoremove and purge..."
sudo apt autoremove --purge -y
bar "autoclean..."
sudo apt autoclean
bar "clean..."
sudo apt clean
