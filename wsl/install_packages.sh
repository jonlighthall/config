#!/bin/bash

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

# install
bar "install packages..."
sudo apt install -y git
sudo apt install -y nautilus

# emacs
sudo apt install -y aspell
sudo apt install -y emacs

# X11
sudo apt install -y dbus-x11
sudo apt install -y x11-apps
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
