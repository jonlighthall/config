#!/bin/bash -eu
# -----------------------------------------------------------------------------------------------
#
# ~/config/wsl/make_links.sh
#
# Purpose: create links for WSL installation.
#
# Dependances:
#    make_links_personal.sh
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

DEBUG=${DEBUG:-2}

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

for prog in make_links_personal.sh; do
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

set -e

# set target and link directories
sys_name=$(basename "$src_dir_phys")
config_dir="${HOME}/config"
target_dir="${config_dir}/${sys_name}"
link_dir=$HOME

# check directories
check_target "${target_dir}"
check_link_dir "$link_dir"

bar 38 "------ Start Linking Repo Files ------" | sed "s/^/${TAB}/"

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
bar 38 "------- Done Linking Repo Files ------" | sed "s/^/${TAB}/"

# run make_links in /etc
for prog in make_links_etc.sh; do
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
