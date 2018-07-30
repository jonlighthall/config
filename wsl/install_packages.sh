#!/bin/sh

sudo apt-get install emacs
sudo apt install aspell

#sudo apt-get install -y dbus
#sudo dbus-uuidgen > /var/lib/dbus/machine-id
#dbus-uuidgen > machine-id
#sudo mv machine-id /var/lib/dbus/
sudo apt-get install dbus-x11

sudo apt-get install x11-apps
export LC_ALL=C
xeyes &
xclock &
xcalc &
xman &
xlogo &

sudo apt install nautilus

sudo apt-get update
sudo apt-get upgrade --fix-missing
