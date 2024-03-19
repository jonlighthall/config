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
    set -e
fi
# print source name at start
echo -e "${TAB}${RUN_TYPE} ${PSDIR}$BASH_SOURCE${RESET}..."
src_name=$(readlink -f "$BASH_SOURCE")
if [ ! "$BASH_SOURCE" = "$src_name" ]; then
    echo -e "${TAB}${VALID}link${RESET} -> $src_name"
fi

# it is assumed that the fisrt command to be run after cloning the parent
# repository is make_links.sh (this file)

start_dir=$PWD
echo "${TAB}starting directory = ${start_dir}"
src_dir_logi=$(dirname "$src_name")
cd $src_dir_logi

# run the following configureation files
prog=install_packages.sh
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
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        bash $prog
    fi
    dtab
else
    echo "not found"
fi

for prog in make_links_personal.sh; do
    echo -n "${TAB}${prog}... "
    if [ -f $prog ]; then
        echo "found"
        bash $prog
    else
        echo "not found"
    fi
done

# set target and link directories
sys_name=$(basename "$src_dir_logi")
config_dir="${HOME}/config"
target_dir="${config_dir}/${sys_name}"
link_dir=$HOME

# check directories
check_target "${target_dir}"
do_make_dir "$link_dir"

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
        bash $prog
    else
        echo "not found"
    fi
done

# return to starting directory
cd $start_dir
dtab
