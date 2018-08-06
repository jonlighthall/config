#!/bin/sh

sudo apt install emacs
sudo apt install aspell
sudo apt install dbus-x11

sudo apt install x11-apps
export LC_ALL=C
xeyes &
xclock &
xcalc &
xman &
xlogo &

sudo apt install nautilus

sudo apt update
sudo apt upgrade --fix-missing
