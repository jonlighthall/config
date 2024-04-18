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

# define directory names
if command -v wsl.exe >/dev/null; then
	  echo "WSL defined... "
    # On WSL installations with OneDrive (exterior to WSL, in Windows), three directories will be
    # be established to hold Git repositories:
    #   * ~/repos - a local directory, within WSL
    #   * %ONEDRIVE%/Documents/home/<host>/<distro>/repos
    #     - outside of WSL, inside OneDrive, "online"
    #     - links to ~/sync/repos
    #     - used for backing up local files
    #   * winhome/Documents/<distro>/repos
    #     - outside of WSL, outside OneDrive, "offline"
    #     - links to ~/offline/repos
    #     - used for saving Win32 scripts (batch and powershell)
    #

    # Put another way, the following directories should contain a folder named repos:
    #  * ~
    #  * ~/sync
    #  * ~/offline
    #
    # The following links should already be defined; they are created in
    # wsl/make_links_personal.sh    
    #  * ~/home -> %ONEDRIVE%/Documents/home
    #  * ~/winhome ->
    #
    # The following directories will be created. Since the "online" directory is shared, a
    # hostname is added to the directory tree. The "offline" directory is local, so the hostname
    # is ommited
    #  * ~/repos
    #  * ~/home/<host>
    #  * ~/home/<host>/<distro>
    #  * ~/home/<host>/<distro>/repos
    #  * ~/winhome/Documents/<distro>
    #  * ~/winhome/Documents/<distro>/repos
    #
    # Finally, the following links should be created
    #  * ~/sync -> ~/home/<host>/<distro>
    #  * ~/offline -> ~/winhome/<distro>

    # get host name
	  host_name=$(hostname)

	  # get distro name
	  if [ -f /etc/os-release ]; then
		    distro=$(sed '/^NAME/!d' /etc/os-release | awk -F \" '{print $2}')
	  else
		    if command -v lsb_release; then
			      distro=$(lsb_release -a 2>&1 | sed '/Distributor/!d;s/^.*:[ \t]*//')
		    else
			      distro=$(sed '/^NAME/!d' /etc/*release | awk -F\" '{print $2}')
		    fi
	  fi

	  # convert to lower case
	  distro=$(echo "$distro" | tr '[:upper:]' '[:lower:]')
    
    #  * ~/winhome/Documents    
    #  * ~/sync/repos
    #  * ~/offline/repos

    # set sync directory
	  host_dir=${HOME}/home/$host_name
	  distro_dir=${host_dir}/$distro

	  # print summary
	  (
		    echo "${TAB}host name: $host_name"
		    echo "${TAB}distro: $distro"
		    echo "${TAB}host dir: $host_dir"
		    echo "${TAB}distro dir: $distro_dir"
	  ) | column -t -s : -o : -R 1

    #define target (source)
	  target=${distro_dir}
	  # define link name (destination)
	  link_name=${HOME}/sync

	  # make $target and link to $link_name
	  do_make_link "${target}" "${link_name}"

    echo "${TAB}creating links outside of Onedrive..."
    # define the offline direcotry and link name
    
	  wdir="${HOME}/winhome/Documents/${distro}/repos"
    echo "${TAB}creating ${wdir}..."
    
    odir="${HOME}/offline"
    echo "${TAB}linking to ${odir}..."

    do_make_link "${wdir}" "${odir}"
    
	  # define repository directory
	  rdir=${distro_dir}/repos

	  # link repo directory to HOME
	  #define target (source)
	  target=${rdir}
	  # define link name (destination)
	  link_name=${HOME}/repos
else
	  echo "WSL not defined."
fi
