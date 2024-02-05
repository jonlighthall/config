#!/bin/sh

# update and upgrade
sudo apt update
sudo apt upgrade -y --fix-missing

# install packages
sudo apt install -y git
sudo apt install -y nautilus

# emacs
sudo apt install -y aspell
sudo apt install -y emacs

# X11
sudo apt install -y dbus-x11
sudo apt install -y x11-apps
# see github.com/jonlighthall/bash for X11 test

# re-check and cleanup
sudo apt upgrade -y --fix-missing
sudo apt autoremove --purge -y
sudo apt autoclean
sudo apt clean
