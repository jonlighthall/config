#!/bin/bash -u
#
# install_packages.sh
#
# JCL Jul 2024

# load bash utilities
fpretty=${HOME}/config/.bashrc_pretty
if [ -e $fpretty ]; then
    source $fpretty    
fi

# settings
PACK_MAN=dnf
OPT="-y"

# upgrade
bar "refresh and upgrade..."
sudo ${PACK_MAN} upgrade ${OPT} --refresh --allowerasing --bugfix

# install packages
bar "install packages..."
for PACK in emacs gcc-gfortan git hostname; do
    echo "installing ${PACK}..."
    sudo ${PACK_MAN} install ${OPT} ${PACK}
done

# see github.com/jonlighthall/bash for X11 test

# re-check
bar "refresh and upgrade..."
sudo ${PACK_MAN} upgrade ${OPT} --allowerasing

# cleanup
bar "autoremove..."
sudo ${PACK_MAN} autoremove ${OPT}
bar "clean..."
sudo ${PACK_MAN} clean all -v
