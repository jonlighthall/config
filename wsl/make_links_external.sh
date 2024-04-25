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

cbar "Define Directories"

# define directory names
echo -e "${TAB}${UL}get Windows environment variables${RESET}"
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
echo -e "${TAB}${UL}define WSL directories${RESET}"
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
docs="Documents"
onedrive_docs="${onedrive_dir_wsl}/${docs}"
dtab
decho "${TAB}OneDrive docs = $onedrive_docs"
homepath_docs="${homepath_dir_wsl}/${docs}"
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

cbar "Start Linking External Directories"

# Create default directory links in ~
echo -e "${TAB}${UL}create general directory links${RESET}"
itab

# define homepath
echo -n "${TAB}homepath: "
do_link "${homepath_dir_wsl}" "${HOME}/homepath"
#do_link "${homepath_dir_wsl}" "${HOME}/winhome"

# define links within homepath
echo -n "${TAB}downloads: "
do_link "${homepath_dir_wsl}/Downloads/" "${HOME}/downloads"
#do_link "${homepath_docs}" "${HOME}/windocs"

# define onedrive
echo -n "${TAB}onedrive: "
do_link "${onedrive_dir_wsl}" "${HOME}/onedrive"

# define links within onedrive
echo -n "${TAB}matlab: "
do_link "${onedrive_docs}/MATLAB" "${HOME}/matlab"

# On WSL installations with OneDrive (exterior to WSL, in Windows), three directories will be
# be established to hold Git repositories:
#   * ~/repos - a local directory, within WSL
#   * %ONEDRIVE%/Documents/home/<host>/<distro>/repos
#     - outside of WSL, inside OneDrive, "online"
#     - links to ~/sync/repos
#     - used for backing up local files to the cloud
#     - name ideas: sync, online, cloud
#   * %HOMEPATH%/Documents/home/<distro>/repos
#     - outside of WSL, outside OneDrive, "offline"
#     - links to ~/offline/repos
#     - used for saving Win32 scripts (batch and powershell)
#     - name ideas: offline, local, win32
#
# Put another way, the following directories should contain a folder named repos:
#  * ~
#  * ~/sync
#  * ~/offline
#  these three folders should contain any and all files routinely accessesed by WSL
#
# The following links should already be defined; they are created above
#  * ~/homepath -> %HOMEPATH%
#  * ~/onedrive -> %ONEDRIVE%
#
# The following directories will be created. The "local" directory may not be visible by the
# Windows OS. Since the "online" directory is shared, a hostname is added to the directory tree
# to distinguish files created on other machines. The "offline" directory is local to the
# machine, so the hostname is ommited.
#  * local
#    - ~/repos
#  * online (synced by OneDrive)
#    - ~/home -> %ONEDRIVE%/Documents/home; this lik should should be renamed something like cloud
#    - ~/home/<host>
#    - ~/home/<host>/<distro>
#    - ~/home/<host>/<distro>/repos
#  * offline (not synced by OneDrive, accessible by OS)
#    - ~/homepath/Documents/home
#    - ~/homepath/Documents/home/<distro>
#    - ~/homepath/Documents/home/<distro>/repos
#
# Finally, the following links should be created
#  * ~/sync -> ~/home/<host>/<distro>
#  * ~/offline -> ~/homepath/<distro>

# define home
# these are the directories where all online and offline files will be stored (outside of WSL)
# these directories will need to be created
dtab
decho -e "${TAB}${UL}define \"home\" directories${RESET}"
itab
home_name="home"
onedrive_home="${onedrive_docs}/${home_name}"
decho "${TAB}OneDrive home = $onedrive_home"
homepath_home="${homepath_docs}/${home_name}"
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
	  distro_name=$(echo "$distro" | tr '[:upper:]' '[:lower:]')
    
    #  * ~/repos    
    #  * ~/sync/repos
    #  * ~/offline/repos

    # define online direcotry
    onedrive_host="${onedrive_home}/${host_name}"
    onedrive_distro="${onedrive_host}/${distro_name}"

    # define offline directory
    homepath_distro="${homepath_home}/${distro_name}"

    echo -e "${TAB}${UL}define distribution directories${RESET}"
    itab
	  # print summary
	  (
		    echo "${TAB}host name: $host_name"
		    echo "${TAB}distro: $distro_name"
		    echo "${TAB}host dir: $onedrive_host"
		    echo "${TAB}online: $onedrive_distro"
        echo "${TAB}offline: $homepath_distro"
	  ) | column -t -s : -o : -R 1

    make_dir_list+=( "${onedrive_host}" "${onedrive_distro}" "${homepath_distro}" )

    # define repository directories
    repo_name="repo"
    onedrive_repo="${onedrive_distro}/${repo_name}"
    homepath_repo="${homepath_distro}/${repo_name}"

    make_dir_list+=( "${onedrive_repo}" "${homepath_repo}" "~/${repo_name}")
    
    dtab
    # create directories
    echo -e "${TAB}${UL}create repository directories${RESET}"
    itab
    for dir in "${make_dir_list[@]}"; do 
        do_make_dir $dir
    done
    dtab

    echo -e "${TAB}${UL}create spcific directory links${RESET}"
    # create a link to "home" for access to files created on other machines
    itab
    #echo "${TAB}creating links inside Onedrive..."
    echo -n "${TAB}${home_name}: "
    do_link "${onedrive_home}" "${HOME}/${home_name}"
    
    # online dir
    # -----------
    #define target (source)
	  target=${onedrive_distro}
	  # define link name (destination)
	  link_name=${HOME}/sync

	  # make $target and link to $link_name
    echo -n "${TAB}online: "
	  do_link "${target}" "${link_name}"

    # offline dir
    # ------------
    #echo "${TAB}creating link outside of Onedrive..."
    
    # define target (source)
	  wdir="${homepath_distro}"
    # define link name (destination)
    odir="${HOME}/offline"
    echo -n "${TAB}offline: "
    do_link "${wdir}" "${odir}"
    dtab
    cbar "Done Linking External Directories"
else
	  echo "WSL not defined."
fi

cbar  "Start Linking External Files"

# list of files to be linked
for my_link in .bash_history; do
    # define target (source)
    target=${onedrive_home}/${my_link}
    # define link (destination)
    sub_dir=$(dirname "$my_link")
    if [ ! $sub_dir = "." ]; then
        # strip target subdirectory from link name
        my_link=$(basename "$my_link")
    fi
    link=~/${my_link}
    do_link "$target" "$link"
done

cbar "Done Linking External Files"
