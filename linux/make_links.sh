#!/bin/bash -u

# get starting time in nanoseconds
start_time=$(date +%s%N)

# load bash utilities
fpretty="${HOME}/config/.bashrc_pretty"
if [ -e "$fpretty" ]; then
    source "$fpretty" -f
    set_traps
    print_source
fi

# determine if script is being sourced or executed
if ! (return 0 2>/dev/null); then
    # exit on errors
    set -eE
fi

# save and print starting directory
start_dir=$PWD
echo "${TAB}starting directory = ${start_dir}"

# In case the make_links files in ~/config/<sys_name> are linked to ~/config/ and called by
# update_repos, make sure to switch to the target directory
cd $src_dir_phys

# set target and link directories
sys_name=$(basename "$src_dir_phys")
target_dir="${config_dir}/${sys_name}"
link_dir=$HOME
check_link_dir "$link_dir"

cbar "Start Linking Repo Files"

# list of files to be linked, unconditionally
for my_link in .bash_aliases .bash_logout .bash_profile .emacs .inputrc .screenrc .tmux.conf
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

# check if host is Navy
echo -n "${TAB}checking host... "
if [[ "$(hostname -f)" == *".mil" ]]; then
    echo -e "${GRAY}SKIP${RESET}"
else
    echo -e "${GOOD}OK${RESET}"

    # list of files to be linked, conditionally
    for my_link in .gitconfig; do
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
fi

cbar "Done Linking Repo Files"

# return to starting directory
cd "$start_dir"
