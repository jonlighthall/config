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

# update
bar "update..."
#sudo ${PACK_MAN} update

# upgrade
bar "upgrade and fix missing..."
#sudo ${PACK_MAN} upgrade ${OPT}

# install packages
bar "install packages..."
for PACK in git; do
    echo "installing ${PACK}..."
    sudo ${PACK_MAN} install ${OPT} ${PACK}
done

# see github.com/jonlighthall/bash for X11 test

# re-check
bar "upgrade and fix missing..."
#sudo ${PACK_MAN} upgrade ${OPT}

# cleanup
bar "autoremove and purge..."
#sudo ${PACK_MAN} autoremove --purge -y
bar "autoclean..."
#sudo ${PACK_MAN} autoclean
bar "clean..."
#sudo ${PACK_MAN} clean
