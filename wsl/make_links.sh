#!/bin/bash -u

# get starting time in nanoseconds
start_time=$(date +%s%N)

utils_dir="${HOME}/utils"
bash_utils_dir="${utils_dir}/bash"

# load formatting
fpretty="${bash_utils_dir}/.bashrc_pretty"
if [ -e "$fpretty" ]; then
    source "$fpretty"
    set_traps
else
    bar() {
        echo "$2"
    }
fi

# set tab
export TAB+=${TAB+${fTAB:='   '}}

# load linking scripts
flink="${bash_utils_dir}/.bash_links"
if [ -e "$flink" ]; then
    source "$flink"
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
echo -e "${TAB}${RUN_TYPE} ${PSDIR}$BASH_SOURCE${NORMAL}..."
src_name=$(readlink -f "$BASH_SOURCE")
if [ ! "$BASH_SOURCE" = "$src_name" ]; then
    echo -e "${TAB}${VALID}link${NORMAL} -> $src_name"
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
echo -n "${TAB}target directory ${target_dir}... "
if [ -d "$target_dir" ]; then
    echo "exists"
else
    echo -e "${BAD}does not exist${NORMAL}"
    exit 1
fi

echo -n "${TAB}link directory ${link_dir}... "
if [ -d $link_dir ]; then
    echo "exists"
else
    echo "does not exist"
    if [ $link_dir = $HOME ]; then
        echo "this should never be true! $link_dir is HOME"
        exit 1
    else
        decho "$link_dir != $HOME"
        mkdir -pv $link_dir

    fi
fi

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
    do_link "$target" "$link"
done
bar 38 "------- Done Linking Repo Files ------" | sed "s/^/${TAB}/"

# run make_linnks in /etc

for prog in make_links_etc.sh; do
    echo -n "${TAB}${prog}... "
    if [ -f $prog ]; then
        echo "found"
        bash $prog
    else
        echo "not found"
    fi
done

cd $start_dir
dtab
