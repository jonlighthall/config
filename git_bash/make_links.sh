#!/bin/bash -eu

# get starting time in nanoseconds
start_time=$(date +%s%N)

# load bash utilities
fpretty="${HOME}/config/.bashrc_pretty"
if [ -e "$fpretty" ]; then
    source "$fpretty"
    set_traps
    # set tab
    N=${#BASH_SOURCE[@]}
    if [ $N -gt 1 ]; then
        itab
    else
        rtab
    fi
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

# make links to files and directories within repo
set -e

# set target and link directories
sys_name=$(basename "$src_dir_phys")
config_dir="${HOME}/config"
target_dir="${config_dir}/${sys_name}"
link_dir=$HOME

cbar "Start Linking Repo Files"
# list of files to be linked
for my_link in .bash_profile; do # .emacs.d .gitconfig .rootrc .inputrc .bash_logout
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
