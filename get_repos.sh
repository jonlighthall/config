#!/bin/bash -u

# load formatting
fpretty="${HOME}/config/.bashrc_pretty"
if [ -e "$fpretty" ]; then
    source "$fpretty"
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
print_source

if [ $# -eq 0 ]; then
	  echo "No system specified"
fi

# set file name to be run in system directory
make_links_name=make_links.sh
# check if configuration is specified
if [ $# -eq 1 ]; then
	  echo "Loading configuration options for $1"
	  echo -n "${TAB}$1... "
	  # check if specified directory exists
	  sys_dir=${src_dir_logi}/$1
	  if [ -d "${sys_dir}" ]; then
		    echo "found"
		    cd "${sys_dir}"
		    echo -n "${TAB}$make_links_name... "
		    # check if make links file exists
		    if [ -f $make_links_name ]; then
			      echo "found"
			      # link make_links to conig dir (called by ~/utils/bash/git/update_repos.sh)
            # see github.com/jonlighthall/bash

			      #-------------------------------------------------------------
			      #define target (source)
			      target=$(readlink -f ${make_links_name})
			      # define link name (destination)
			      link_name=${HOME}/config/${make_links_name}

			      # begin linking...
			      do_link "${target}" "${link_name}"

			      #-------------------------------------------------------------

			      bash $make_links_name
			      # define profile name
			      profie_name=.bash_profile
			      echo -n "${TAB}$profie_name... "
            itab
			      # check if file exists
			      if [ -f $profie_name ]; then
				        echo -n "found: "
				        readlink -f $profie_name
				        set +ue
				        source ${profie_name}
				        set -ue
				        echo "${TAB}configuration applied for $1"
			      else
				        # profile...
				        echo "not found"
			      fi
            dtab
		    else
			      # make links file...
			      echo "not found"
		    fi
	  else
		    # system name...
		    echo "not found"
	  fi
fi

# Define folder names

repo_name=${repo_name:="repos"}
repo_dir=${HOME}/repos

echo -e "saving repsoitories to \e[33m$repo_dir\e[0m..."

udir=${HOME}/utils
edir=${HOME}/examp

echo "creating repository directory..."
for my_dir in $repo_dir $udir $edir; do
	  if [ ! -d ${my_dir} ]; then
		    mkdir -vp ${my_dir}
	  else
		    echo "${TAB}directory ${my_dir} already exists"
	  fi
done

cbar "Start Cloning Repo Files"

# set github user and authentication method
github_user=jonlighthall
github_https=https://github.com/${github_user}/
github_ssh=git@github.com:${github_user}/
github_auth=${github_ssh}

# check if git is defined
if command -v git >/dev/null; then
	  echo "proceeding with Git commands..."
else
	  echo "Git not defined."
	  exit
fi

# Define the directory into which the repos will be cloned. The clone directory name defaults to
# the "repo" directory, defined above as ~/repos. This is the default for non-Windows systems. On
# WSL systems, repos should be cloned to the "online" directory within OneDrive and then linked
# to the local ~/repos directory. The online directory, typicall ~/sync, should already exist.

# All of the "oneline" repos should be linked to in the "local" repos dir.

# Links should be created in the group directories, e.g., examps and utils, that point to the
# cloned repos, contianed or linked to in the "local" repos directory.

clone_dir=${HOME}/repos

if command -v wsl.exe >/dev/null; then
	  echo "${TAB}WSL defined. Redefining cloning destination..."
    clone_dir=${HOME}/sync/repos
fi

echo "cloning repos to ${clone_dir}..."
check_target "${clone_dir}"

# list of utility repos to be cloned
group_name="utility"
echo -e "cloning \x1b[1;32m${group_name}\x1b[m repos..."
for my_repo in bash fortran_utilities; do
	  #define target (source)
	  target=${clone_dir}/${my_repo}
	  # define link name (destination)
	  link_name="${repo_dir}/${my_repo}"

	  # check if target exists
    echo "checking ${my_repo}"
    itab
    # check_target will return 1 if target does not exist
    set +e
	  check_target ${target}
    RETVAL=$?
		itab
    if [ $RETVAL -ne 0 ]; then 
			  echo "${TAB}cloning $my_repo..."
		    git clone ${github_auth}$my_repo ${target}
        dtab

	      # run make_links after cloning        
        prog1=${target}/${make_links_name}
        prog2=${target}/bin/${make_links_name}
        echo -ne "${TAB}${make_links_name}... "
        if [ -f "${prog1}" ] || [ -f "${prog2}" ]; then
            echo "found"
            if [ -f "${prog1}" ]; then
                ${prog1}
            else
                ${prog2}
            fi
        else
            echo "not found"
        fi        
	  fi
    dtab 2

	  # begin linking...
    echo "${TAB}linking $my_repo..."
    itab
    # First link from the clone directory to the repo directory. For non-Windows systems, these
    # directories will be the same and the do_link command will have no effect.
    do_link "${target}" "${link_name}"
    # The link from the repo directory to the group directory, in this case "utils".
    do_link "${link_name}" "${udir}/${my_repo}"
    dtab
done

# On Navy systems, Flank Speed OneDrive will not allow syncing of .bat or .ps1 files. Therefore,
# if it is a Navy host and OneDrive is defined, clone the repositories into an offline
# directory. For onedrive conflicts, there should be an offline (un-synced) repo dir.

# check if host is Navy
if [[ "$(hostname -f)" == *".mil" ]]; then
    echo -e "${TAB}host: \x1b[31m$(hostname -f)\x1b[m"
    # check if WSL is defined
    if command -v wsl.exe >/dev/null; then
	      echo "${TAB}WSL defined."
        # It is assumed that OneDrive is defined if the following directory exists
        online_dir="${HOME}/sync"
        if [ -e "${online_dir}" ]; then
            echo "${TAB}OneDrive is defined."
            echo "Redefining cloning destination..."
            clone_dir="${HOME}/offline/repos"
        fi
    fi
fi

echo "cloning repos to ${clone_dir}..."
check_target "${clone_dir}"

# List of Win32 repos to be cloned
echo -e "cloning \x1b[1;32mWin32 ${group_name}\x1b[m repos..."

for my_repo in batch powershell; do
	  #define target (source)
	  target=${clone_dir}/${my_repo}
	  # define link name (destination)
	  link_name=${repo_dir}/${my_repo}

    # check if target exists
    echo "checking ${my_repo}"
    itab
	  check_target ${target}
    RETVAL=$?
		itab
    if [ $RETVAL -ne 0 ]; then
        # clone repo
			  echo "${TAB}cloning $my_repo..."
		    git clone ${github_auth}$my_repo ${target}
        dtab

        # run make_links after cloning        
        prog1=${target}/${make_links_name}
        prog2=${target}/bin/${make_links_name}
        echo -ne "${TAB}${make_links_name}... "
        if [ -f "${prog1}" ] || [ -f "${prog2}" ]; then
            echo "found"
            if [ -f "${prog1}" ]; then
                ${prog1}
            else
                ${prog2}
            fi
        else
            echo "not found"
        fi  
	  fi
    dtab 2    

	  # begin linking...
    echo "${TAB}linking $my_repo..."
    do_link "${target}" "${link_name}"
    do_link "${link_name}" "${udir}/${my_repo}"	
done
echo -e "done cloning ${group_name} repos"

exit


# list of example repos to be cloned
group_name="example"
echo -e "cloning \x1b[1;32m${group_name}\x1b[m repos..."
itab
for my_repo in cpp fortran hello nrf python; do
	  # define target (source)
	  target=${repo_dir}/${my_repo}
	  # define link name (destination)
	  link_name=${edir}/${my_repo}

	  # check if target exists
	  echo -ne "${TAB}target dirctory ${YELLOW}${target}${RESET}... "
	  if [ -e "${target}" ]; then
		    echo "exits"
        itab
        #echo -n "${TAB}pulling... "
		    #git -C ${target} pull
		    #echo -n "${TAB}pushing... "
		    #git -C ${target} push
        dtab
	  else
		    echo "does not exist"
		    echo "${TAB}cloning $my_repo..."
		    git clone ${github_auth}$my_repo ${target}
	  fi
	  # begin linking...
    do_link "${target}" "${link_name}"
done
dtab
echo -e "done cloning ${group_name} repos"

# list of other repos to be cloned
group_name="other"
echo -e "cloning \x1b[1;32m${group_name}\x1b[m repos..."
itab
for my_repo in matlab; do
	  matlab_dir=${HOME}/onedrive/Documents/MATLAB
	  echo -n "${TAB}${matlab_dir}..."
	  if [ -d ${matlab_dir} ]; then
		    echo "found"
	  else
		    echo "not found"
	      echo -e "${TAB}\x1b[33mexcluding ${my_repo}\x1b[m"
		    continue
	  fi

	  # define target (source)
	  target=${matlab_dir}/macros
	  # define link (destination)
	  link_name=${repo_dir}/matlab

	  # check if target exists
	  echo -ne "${TAB}target dirctory ${YELLOW}${target}${RESET}... "
	  if [ -e "${target}" ]; then
		    echo "exits"
        itab
        #echo -n "${TAB}pulling... "
		    #git -C ${target} pull
		    #echo -n "${TAB}pushing... "
		    #git -C ${target} push
        dtab
	  else
		    echo "does not exist"
		    echo "${TAB}cloning $my_repo..."
		    git clone ${github_auth}$my_repo ${target}
	  fi
	  # begin linking...
    do_link "${target}" "${link_name}"
done
dtab
echo -e "done cloning ${group_name} repos"

# list of private repos to be cloned
group_name="private"
echo -e "cloning \x1b[1;32m${group_name}\x1b[m repos..."
if [[ ! "$(hostname -f)" == *".mil" ]]; then
    itab
	  cd ${HOME}/config
	  dname=private
	  for my_repo in config_private; do
		    if [ ! -d $dname ]; then
			      echo -e "cloning \e[33m$dname\e[0m..."
			      echo "see ${github_https}$my_repo/blob/master/.git-credentials"
			      git clone ${github_auth}$my_repo $dname

			      # run make_links
			      fpath="${dname}/${make_links_name}"
			      echo -n "${TAB}$fpath... "
			      if [ -f "${fpath}" ]; then
				        echo "found"
				        cd $dname
				        bash $make_links_name
			      else
				        echo "not found"
			      fi
		    else
			      echo -e "${TAB}dirctory \e[33m$PWD$my_repo\e[0m already exits"
            itab
            #echo -n "${TAB}pulling... "
		        #git -C ${target} pull
		        #echo -n "${TAB}pushing... "
		        #git -C ${target} push
            dtab
		    fi
	  done
	  dtab
	  echo -e "done cloning ${group_name} repos"
else
    itab
    echo -e "${TAB}host: \x1b[31m$(hostname -f)\x1b[m"
	  echo -e "${TAB}\x1b[33mexcluding ${group_name} repos\x1b[m"
    dtab
fi
echo "done cloning all repos"
