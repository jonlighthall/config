#!/bin/sh

sudo apt install emacs

sudo apt-get install -y dbus
#sudo dbus-uuidgen > /var/lib/dbus/machine-id
sudo dbus-uuidgen > machine-id
sudo mv machine-id /var/lib/dbus/
sudo apt-get install dbus-x11

sudo apt-get update
sudo apt-get upgrade

sudo apt-get install x11-apps
export LC_ALL=C
xeyes &
xclock &
xcalc &
xman &
xlogo &


sudo apt install nautilus
sudo apt install diff
sudo apt install diffutils
git config --global push.default simple

sudo apt install aspell
