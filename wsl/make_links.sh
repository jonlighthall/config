#!/bin/bash -eu
# -----------------------------------------------------------------------------------------------
#
# ~/config/wsl/make_links.sh
#
# Purpose: create links for WSL installation. In addition to calling make_links_external and
#    make_links_etc, this script creates links to files that are contained within this repo. The
#    script make_links_external creates links to files and directories outside of the
#    repository. The script make_links_etc creates links in the /etc directory, which requires
#    elevation.
#
# Dependances:
#    make_links_external.sh
#    make_links_etc.sh
#
# Called by:
#    get_repos.sh
#
#  JCL Jul 2018
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

# it is assumed that the fisrt command to be run after cloning the parent
# repository is make_links.sh (this file)

start_dir=$PWD
echo "${TAB}starting directory = ${start_dir}"
cd $src_dir_phys

# make links to external files and directories
for prog in make_links_external.sh; do
    echo -n "${TAB}${prog}... "
    if [ -f $prog ]; then
        echo "found"
        itab
        bash $prog
        RETVAL=$?
        dtab
        echo -en "${TAB}$prog "
        if [ $RETVAL -eq 0 ]; then
            echo -en "${GOOD}OK"
        else
            echo -en "${BAD}FAIL"
        fi
        echo -e "${RESET} ${GRAY}RETVAL=$RETVAL${RESET}"
    else
        echo "not found"
    fi
done

# make links to files and directories within repo
set -e

# set target and link directories
sys_name=$(basename "$src_dir_phys")
config_dir="${HOME}/config"
target_dir="${config_dir}/${sys_name}"
link_dir=$HOME

cbar "Start Linking Repo Files"
# list of files to be linked
for my_link in .bash_aliases .bash_logout .bash_profile .emacs.d .gitconfig .hushlogin .inputrc .rootrc; do
    # define target (source)
    target=${target_dir}/${my_link}
    # define link name (destination)
    sub_dir=$(dirname "$my_link")
    if [ ! $sub_dir = "." ]; then
        # strip target subdirectory from link name
        my_link=$(basename "$my_link")
    fi
    link=${link_dir}/${my_link}
    # create link
    do_link "${target}" "${link}"
done
cbar "Done Linking Repo Files"

# make links in /etc
for prog in make_links_etc.sh; do
    echo -n "${TAB}${prog}... "
    if [ -f $prog ]; then
        echo "found"
        itab
        bash $prog
        RETVAL=$?
        dtab
        echo -en "${TAB}$prog "
        if [ $RETVAL -eq 0 ]; then
            echo -en "${GOOD}OK"
        else
            echo -en "${BAD}FAIL"
        fi
        echo -e "${RESET} ${GRAY}RETVAL=$RETVAL${RESET}"
    else
        echo "not found"
    fi
done

# return to starting directory
cd $start_dir
