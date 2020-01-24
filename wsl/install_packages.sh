#!/bin/sh

# update and upgrade
sudo apt update
sudo apt upgrade -y

# install packages
sudo apt install -y emacs
sudo apt install -y aspell

# install and test X11
sudo apt install -y dbus-x11
sudo apt install -y x11-apps
export LC_ALL=C
xeyes &
xclock &
xcalc &
xman &
xlogo &

# install more packages
sudo apt install -y nautilus

# re-check and cleanup
sudo apt upgrade -y --fix-missing
sudo apt autoremove -y
