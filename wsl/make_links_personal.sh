#!/bin/bash -u

# get starting time in nanoseconds
start_time=$(date +%s%N)

# load formatting
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
DEBUG=${DEBUG:-2}
print_source

# define directory names
echo "${TAB}define directories"
itab
# get USERNAME
windows_user=$(cmd.exe /c "echo %USERNAME%" 2>/dev/null | tr -d '\r')
echo "${TAB}%USERNAME% = $windows_user"
# construct WSL-equivalent HOMEPATH
win_home_dir="/mnt/c/Users/$windows_user"
echo "${TAB}%HOMEPATH% = $win_home_dir"
# get ONEDRIVE
onedirve_dir=$(cmd.exe /c "echo %OneDrive%" 2>/dev/null | tr -d '\r')
echo "${TAB}%ONEDRIVE% = $onedirve_dir"
# construct WSL-equivalent ONEDRIVE
# here basename won't work becuase spaces will be interpreted as file seperators
onedrive_name=$(echo "${onedirve_dir##*\\}")
decho "${TAB}$onedrive_name"
onedrive_dir_wsl="${win_home_dir}/${onedrive_name}"
decho "${TAB}$onedrive_dir_wsl"
# get OneDrive Documents
onedrive_docs="${onedrive_dir_wsl}/Documents"
decho "${TAB}$onedrive_docs"
# define home
home_dir="${onedrive_docs}/home"
decho "${TAB}$home_dir"
# set target
target_dir=${home_dir}
decho "${TAB}$target_dir"
dtab

dir_list=( "${win_home_dir}" "${onedrive_dir_wsl}" "${onedrive_docs}" "${home_dir}" )

if [[ "${home_dir}" != "${target_dir}" ]]; then
    dir_list+=( "${target_dir}" )
fi

#echo ${dir_list[@]}

echo "${TAB}check directories"
itab
# test directories
for dir in "${dir_list[@]}"; do 
    echo -n "${TAB}directory $dir... "
    if [ -e "${dir}" ]; then
        echo -e "${GOOD}exists${RESET}"
    else
        echo -e "${BAD}does not exist${RESET}"
        exit 1
    fi
done
dtab

# set link directory
link_dir=$HOME
check_link_dir $link_dir

bar 38 "---- Start Linking External Files ----" | sed "s/^/${TAB}/"

# list of files to be linked
for my_link in .bash_history; do
    # define target (source)
    target=${target_dir}/${my_link}
    # define link (destination)
    sub_dir=$(dirname "$my_link")
    if [ ! $sub_dir = "." ]; then
        # strip target subdirectory from link name
        my_link=$(basename "$my_link")
    fi
    link=${link_dir}/${my_link}
    do_link "$target" "$link"
done

bar 38 "----- Done Linking External Files ----" | sed "s/^/${TAB}/"

bar 38 "- Start Linking External Directories -" | sed "s/^/${TAB}/"
# Create default directory links in ~

# define winhome
echo -n "${TAB}winhome: "
do_link "${win_home_dir}" "${HOME}/winhome"

# define links within winhome
echo -n "${TAB}downloads: "
do_link "${win_home_dir}/Downloads/" "${HOME}/downloads"

# define onedrive
echo -n "${TAB}onedrive: "
do_link "${onedrive_dir_wsl}" "${HOME}/onedrive"

# define links within onedrive
echo -n "${TAB}home: "
do_link "${home_dir}" "${HOME}/home"

# define matlab
echo -n "${TAB}matlab: "
do_link "${onedrive_docs}/MATLAB/" "${HOME}/matlab"

bar 38 "- Done Linking External Directories --" | sed "s/^/${TAB}/"
#       12345678901234567890123456789012345678
