#!/bin/bash -eu

# get starting time in nanoseconds
start_time=$(date +%s%N)

# define home directory
home_dir=${HOME}
if [ -z ${MSYSTEM:+dummy} ]; then
    echo "MSYSTEM is unset or null"
    home_dir=/mnt/c/Users/${USER}
else
    #echo "not empty"
    home_dir=/c/Users/${USERNAME}
fi
if [[ $home_dir == $HOME ]]; then
   :  #echo "no change"
else
    echo "HOME is redefined"
    echo "HOME is $home_dir or $HOME"
    if [ -d "${home_dir}" ]; then
        echo "OK"
    else
        echo "FAIL"
    fi
fi

# load bash utilities
config_dir="${home_dir}/config"
fpretty="${config_dir}/.bashrc_pretty"
if [ -e "$fpretty" ]; then
    source "$fpretty" -f
    set_traps
    print_source
fi

# make links to files and directories within repo
set -e

# it is assumed that the fisrt command to be run after cloning the parent
# repository is make_links.sh (this file)

# save and print starting directory
start_dir=$PWD
echo "${TAB}starting directory = ${start_dir}"

# make links to files and directories within repo
cbar "Start Linking Repo Files"
set -e

# set target and link directories
sys_name=$(basename "$src_dir_phys")
target_dir="${config_dir}/${sys_name}"
link_dir=${home_dir}
check_link_dir "$link_dir"

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

# return to starting directory
cd "$start_dir"
