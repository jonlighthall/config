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

cbar "Define Directories" | sed "s/^/${TAB}/"

# define directory names
echo -e "${TAB}${UL}get environment variables${RESET}"
itab

# get Windows environment variables
# USERNAME
windows_user=$(cmd.exe /c "echo %USERNAME%" 2>/dev/null | tr -d '\r')
echo "${TAB}%USERNAME% = $windows_user"

# HOMEPATH
homepath=$(cmd.exe /c "echo %HOMEPATH%" 2>/dev/null | tr -d '\r')
echo "${TAB}%HOMEPATH% = $homepath"

# ONEDRIVE
onedrive_dir=$(cmd.exe /c "echo %OneDrive%" 2>/dev/null | tr -d '\r')
echo "${TAB}%ONEDRIVE% = $onedrive_dir"

dtab
echo -e "${TAB}${UL}define directories${RESET}"
itab
# define HOMEPATH for WSL
# construct WSL-equivalent HOMEPATH
# get drive letter
drive=${onedrive_dir%:*}
# convert to lower case
drive=$(echo "$drive" | tr '[:upper:]' '[:lower:]')
DEBUG=1
decho "${TAB}     drive = $drive"
homepath_dir=$(echo "${drive}${homepath}" | sed 's,\\,/,g')
decho "${TAB} home path = $homepath_dir"
homepath_dir_wsl="/mnt/${homepath_dir}"
echo "${TAB}%HOMEPATH% = $homepath_dir_wsl"

# define ONEDRIVE for WSL
# construct WSL-equivalent ONEDRIVE
# here basename won't work becuase spaces will be interpreted as file seperators
onedrive_name=$(echo "${onedrive_dir##*\\}")
decho "${TAB}OneDrive   = $onedrive_name"
onedrive_dir_wsl="${homepath_dir_wsl}/${onedrive_name}"
echo "${TAB}%ONEDRIVE% = $onedrive_dir_wsl"

# get Documents
doc="/Documents"
onedrive_docs="${onedrive_dir_wsl}${doc}"
dtab
decho "${TAB}OneDrive docs = $onedrive_docs"
homepath_docs="${homepath_dir_wsl}${doc}"
decho "${TAB}HomePath docs = $homepath_docs"
itab

# set link directory
link_dir=$HOME
echo "${TAB}  link dir = $link_dir"
dtab

# create list of target directories (these directories must exist)
target_dir_list=( "${homepath_dir_wsl}" "${homepath_docs}" "${onedrive_dir_wsl}" \
                                        "${onedrive_docs}" )

# create list of link directories (these directories must exist)
link_dir_list=( "${link_dir}" )

echo -e "${TAB}${UL}check directories${RESET}"
itab
# test directories
for dir in "${target_dir_list[@]}"; do 
    check_target $dir
done

for dir in "${link_dir_list[@]}"; do 
    check_link_dir $dir
done
dtab

# On WSL installations with OneDrive (exterior to WSL, in Windows), three directories will be
# be established to hold Git repositories:
#   * ~/repos - a local directory, within WSL
#   * %ONEDRIVE%/Documents/home/<host>/<distro>/repos
#     - outside of WSL, inside OneDrive, "online"
#     - links to ~/sync/repos
#     - used for backing up local files
#     - name ideas: sync, online, cloud
#   * %HOMEPATH%/Documents/home/<distro>/repos
#     - outside of WSL, outside OneDrive, "offline"
#     - links to ~/offline/repos
#     - used for saving Win32 scripts (batch and powershell)
#     - name ideas: online, local, win32
#
# Put another way, the following directories should contain a folder named repos:
#  * ~
#  * ~/sync
#  * ~/offline
#  these three folders should contain any and all files routinely accessesed by WSL
#
# The following links should already be defined; they are created above
#  * ~/home -> %ONEDRIVE%/Documents/home; this should be redefined as cloud
#  * ~/winhome -> %HOMEPATH%
#  * ~/windocs -> %HOMEPATH%/Documents
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

cbar "Create Directories" | sed "s/^/${TAB}/"

# define home
# these are the directories where all online and offline files will be stored (outside of WSL)
# these directories will need to be created
decho -e "${TAB}${UL}define home directories${RESET}"
itab
home="/home"
onedrive_home="${onedrive_docs}${home}"
decho "${TAB}OneDrive home = $onedrive_home"
homepath_home="${homepath_docs}${home}"
decho "${TAB}HomePath home = $homepath_home"
make_dir_list=( "${onedrive_home}" "${homepath_home}" )
dtab

# define host and distro
if command -v wsl.exe >/dev/null; then
	  decho "${TAB}WSL defined... "  
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

    # define online direcotry
    onedrive_host="${onedrive_home}/$host_name"
    onedrive_distro="${onedrive_host}/${distro}"

    # set offline directory
    homepath_distro="${homepath_home}/${distro}"

    echo -e "${TAB}${UL}define distribution directories${RESET}"
    itab
	  # print summary
	  (
		    echo "${TAB}host name: $host_name"
		    echo "${TAB}distro: $distro"
		    echo "${TAB}host dir: $onedrive_host"
		    echo "${TAB}distro dir: $onedrive_distro"
        echo "${TAB}distro dir: $homepath_distro"
	  ) | column -t -s : -o : -R 1

    make_dir_list+=( "${onedrive_host}" "${onedrive_distro}" "${homepath_distro}" )
    dtab
    # create directories
    echo -e "${TAB}${UL}create directories${RESET}"
    itab
    for dir in "${make_dir_list[@]}"; do 
        do_make_dir $dir
    done
    dtab

    exit
    
    #define target (source)
	  target=${onedrive_distro}
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
	  rdir=${onedrive_distro}/repos

	  # link repo directory to HOME
	  #define target (source)
	  target=${rdir}
	  # define link name (destination)
	  link_name=${HOME}/repos
else
	  echo "WSL not defined."
fi

# set targets
online_dir=${onedrive_home}
decho "${TAB}  online = $online_dir"
offline_dir=${homepath_home}
decho "${TAB} offline = $offline_dir"
if [[ "${onedrive_home}" != "${online_dir}" ]]; then
    target_dir_list+=( "${online_dir}" )
fi
dtab

cbar  "Start Linking External Files" | sed "s/^/${TAB}/"

# list of files to be linked
for my_link in .bash_history; do
    # define target (source)
    target=${online_dir}/${my_link}
    # define link (destination)
    sub_dir=$(dirname "$my_link")
    if [ ! $sub_dir = "." ]; then
        # strip target subdirectory from link name
        my_link=$(basename "$my_link")
    fi
    link=${link_dir}/${my_link}
    do_link "$target" "$link"
done

cbar "Done Linking External Files" | sed "s/^/${TAB}/"

cbar "Start Linking External Directories" | sed "s/^/${TAB}/"
# Create default directory links in ~

# define winhome
echo -n "${TAB}winhome: "
do_link "${homepath_dir_wsl}" "${HOME}/winhome"

# define links within winhome
echo -n "${TAB}downloads: "
do_link "${homepath_dir_wsl}/Downloads/" "${HOME}/downloads"

echo -n "${TAB}windocs: "
do_link "${homepath_dir_wsl}/Documents/" "${HOME}/windocs"

# define onedrive
echo -n "${TAB}onedrive: "
do_link "${onedrive_dir_wsl}" "${HOME}/onedrive"

# define links within onedrive
echo -n "${TAB}home: "
do_link "${onedrive_home}" "${HOME}/home"

# define matlab
echo -n "${TAB}matlab: "
do_link "${onedrive_docs}/MATLAB/" "${HOME}/matlab"

cbar "Done Linking External Directories" | sed "s/^/${TAB}/"
