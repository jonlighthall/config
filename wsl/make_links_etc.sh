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
fi

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

start_dir=$PWD
echo "${TAB}starting directory = ${start_dir}"
src_dir_logi=$(dirname "$src_name")
cd $src_dir_logi


# set target and link directories
sys_name=$(basename "$src_dir_logi")
config_dir="${HOME}/config"
target_dir="${config_dir}/${sys_name}"
link_dir=/etc

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
    echo "this should never be true! $link_dir is /etc"
    exit 1

fi

bar 38 "------ Start Linking Repo Files ------" | sed "s/^/${TAB}/"

# list of files to be linked
for my_link in wsl.conf
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
    # make link
    do_link "$target" "$link"
done
bar 38 "------- Done Linking Repo Files ------" | sed "s/^/${TAB}/"
