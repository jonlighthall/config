#!/bin/bash -u

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
    set -eE
fi
print_source

# set target and link directories
target_dir=$(dirname "$src_name")
link_dir=$HOME

# check directories
echo -n "${TAB}target directory ${target_dir}... "
if [ -d "$target_dir" ]; then
    echo "exists"
else
    echo -e "${BAD}does not exist${RESET}"
    exit 1
fi

do_make_dir "$link_dir"

bar 38 "------ Start Linking Repo Files ------" | sed "s/^/${TAB}/"

# list of files to be linked
for my_link in .bash_logout .bash_profile .emacs .gitconfig .inputrc .screenrc .tmux.conf
do
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
