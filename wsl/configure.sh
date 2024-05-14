#!/bin/bash -eu
# -----------------------------------------------------------------------------------------------
#
# ~/config/wsl/configure.sh
#
# Purpose: initialize installation with configurations for WSL. This program calls
#    install_packages and make_links. The script install_packages is seperated from make_links
#    because it does not need to be called every time the links are updated.
#
# Dependances:
#    install_packages.sh
#    make_links.sh
#
#  JCL Apr 2024
#
# -----------------------------------------------------------------------------------------------

# get starting time in nanoseconds
start_time=$(date +%s%N)

DEBUG=${DEBUG:-0}

# load bash utilities
fpretty="${HOME}/config/.bashrc_pretty"
if [ -e "$fpretty" ]; then
    source "$fpretty"
    set_traps
fi

# determine if script is being sourced or executed
if (return 0 2>/dev/null); then
    RUN_TYPE="sourcing"
else
    RUN_TYPE="executing"
    # exit on errors
    set -e
fi
print_source

start_dir=$PWD
echo "${TAB}starting directory = ${start_dir}"
cd "$src_dir_phys"

# run the following configuration files

# update and install
for prog in install_packages.sh; do
    echo -n "${TAB}$prog..."
    if [ -f $prog ]; then
        echo "found"
        itab
        echo "${TAB}${prog} requires"
        itab
        echo "${TAB}* elevation"
        echo "${TAB}* access to archive.ubuntu.com, security.ubuntu.com, etc"
        dtab
        unset_traps
        read -p "${TAB}Proceed with ${prog}? (y/n) " -n 1 -r -t 3
       
        set_traps
        if [ $DEBUG -lt 1 ]; then
            echo
        fi
        if [[ $REPLY =~ ^[Yy]$ ]]; then
            bash $prog
        fi
        dtab
    else
        echo "not found"
    fi
done

# make links
for prog in make_links.sh; do
    echo -n "${TAB}${prog}... "
    if [ -f $prog ]; then
        echo "found"
        itab
        bash $prog
        dtab
    else
        echo "not found"
    fi
done

# return to starting directory
cd $start_dir
