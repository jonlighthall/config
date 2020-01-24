#!/bin/sh

sudo apt install -y emacs
sudo apt install -y aspell
sudo apt install -y dbus-x11

sudo apt install -y x11-apps
export LC_ALL=C
xeyes &
xclock &
xcalc &
xman &
xlogo &

sudo apt install -y nautilus

sudo apt update
sudo apt upgrade -y --fix-missing
