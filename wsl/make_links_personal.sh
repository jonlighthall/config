#!/bin/bash -u

# get starting time in nanoseconds
start_time=$(date +%s%N)

# load formatting
fpretty="${HOME}/config/.bashrc_pretty"
if [ -e "$fpretty" ]; then
    source "$fpretty"
    set_traps
    # set tab
    itab
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
# here basename won't work becuase of the file seperators
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

if [ "${home_dir}" != "${target_dir}" ]; then
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

echo "${TAB}create links..."
# set link directory
link_dir=$HOME
echo -n "${TAB}link directory ${link_dir}... "
if [ -d "$link_dir" ]; then
    echo -e "${GOOD}exists${RESET}"
else
    echo -e "${yellow}does not exist${RESET}"
    mkdir -pv "$link_dir"
    if [ $link_dir = $HOME ]; then
        echo -e "${BAD}this should never be true! $link_dir is HOME${RESET}"
        exit 1
    else
        echo "$link_dir != $HOME"
    fi
fi

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
do_link ${onedrive_docs}/MATLAB/ ${HOME}/matlab

bar 38 "- Done Linking External Directories --" | sed "s/^/${TAB}/"
#       12345678901234567890123456789012345678

# print time at exit
echo -en "${TAB}$(date +"%a %b %-d %-l:%M %p %Z") ${BASH_SOURCE##*/} "
if command -v sec2elap &>/dev/null; then
    bash sec2elap ${SECONDS}
else
    echo "elapsed time is ${white}${SECONDS} sec${RESET}"
fi
