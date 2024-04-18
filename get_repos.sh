#!/bin/bash -u
# set tab

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
make_links_file=make_links.sh
# check if configuration is specified
if [ $# -eq 1 ]; then
	  echo "Loading configuration options for $1"
	  echo -n "${TAB}$1... "
	  # check if specified directory exists
	  sys_dir=${src_dir_logi}/$1
	  if [ -d "${sys_dir}" ]; then
		    echo "found"
		    cd "${sys_dir}"
		    echo -n "${TAB}$make_links_file... "
		    # check if make links file exists
		    if [ -f $make_links_file ]; then
			      echo "found"
			      # link make_links to conig dir (called by ~/utils/bash/git/update_repos.sh)
            # see github.com/jonlighthall/bash

			      #-------------------------------------------------------------
			      #define target (source)
			      target=$(readlink -f ${make_links_file})
			      # define link name (destination)
			      link_name=${HOME}/config/${make_links_file}

			      # begin linking...
			      do_link "${target}" "${link_name}"

			      #-------------------------------------------------------------

			      bash $make_links_file
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

# check if git is defined
if command -v git >/dev/null; then
	  echo "proceeding with Git commands..."
else
	  echo "Git not defined."
	  exit
fi

# TODO there shold be an online (synced) repos dir, as defined; and a local repos dir. For
# onedrive conflicts, there should be an offline (un-synced) repo dir. All fo the repos should be
# linked to in the local repos dir; then examps and utils should link the the local repos dir
rdir=${HOME}/repos

echo -e "saving repsoitories to \e[33m$rdir\e[0m..."

udir=${HOME}/utils
edir=${HOME}/examp

echo "creating repository directory..."
for my_dir in $rdir $udir $edir; do
	  if [ ! -d ${my_dir} ]; then
		    mkdir -vp ${my_dir}
	  else
		    echo "${TAB}directory ${my_dir} already exists"
	  fi
done

echo "--------------------------------------"
echo "------ Start Cloning Repo Files-------"
echo "--------------------------------------"

# set github user and authentication method
github_user=jonlighthall
github_https=https://github.com/${github_user}/
github_ssh=git@github.com:${github_user}/
github_auth=${github_ssh}

# list of utility repos to be cloned
group_name="utility"
echo -e "cloning \x1b[1;32m${group_name}\x1b[m repos..."
for my_repo in bash fortran_utilities; do
	  #define target (source)
	  target=${rdir}/${my_repo}
	  # define link name (destination)
	  link_name=${udir}/${my_repo}

	  # check if target exists
	  echo -ne "${TAB}target dirctory \e[33m${target}\e[0m... "
	  if [ -e "${target}" ]; then
		    echo "exists "
		    itab
		    #echo -n "${TAB}pulling... "
		    #git -C ${target} pull
		    #echo -n "${TAB}pushing... "
		    #git -C ${target} push
		    dtab
	  else
		    echo -e "\e[31mdoes not exist\e[0m"
		    echo "${TAB}cloning $my_repo..."
		    git clone ${github_auth}$my_repo ${target}
	  fi

	  # begin linking...
    do_link "${target}" "${link_name}"
    
	  # run make_links
	  make_links_file="${link_name}/make_links.sh"
	  if [ -e "${make_links_file}" ]; then
		    cd ${link_name}
		    bash ${make_links_file}
		    cd ${rdir}
	  else
		    echo -e "${TAB}${BAD}${make_links_file} not found${RESET}"
	  fi
done

# clone Win32 repos
echo -e "cloning \x1b[1;32mWin32 utility\x1b[m repos..."
# The directory for Win32 repos should default to the repo directory. However, OneDrive on Navy
# systems will not allow syncing of .bat or .ps1 files. Therefore, if it is a Navy host and
# OneDrive is defined, clone the repositories into an offline directory.
wdir=$rdir
# check if host is Navy
if [[ "$(hostname -f)" == *".mil" ]]; then
    echo -e "${TAB}host: \x1b[31m$(hostname -f)\x1b[m"
    # check if WSL is defined
    if command -v wsl.exe >/dev/null; then
	      echo "${TAB}WSL defined."
        echo "${TAB}creating links outside of Onedrive..."
        # define the offline direcotry and link name
        
	      wdir="${HOME}/winhome/Documents/${distro}/repos"
        echo "${TAB}creating ${wdir}..."
        
        odir="${HOME}/offline"
        echo "${TAB}linking to ${odir}..."

        do_make_link "${wdir}" "${odir}"
    else
        echo "${TAB}WSL not defined."
    fi
else
	  echo "${TAB}cloning in ${wdir}..."
fi

for my_repo in batch powershell; do
	  #define target (source)

	  target=${wdir}/${my_repo}
	  # define link name (destination)
	  link_name=${udir}/${my_repo}

	  # check if target exists
	  echo -ne "${TAB}target dirctory \e[33m${target}\e[0m... "
	  if [ -e "${target}" ]; then
		    echo "exists "
		    itab
		    #echo -n "${TAB}pulling... "
		    #git -C ${target} pull
		    #echo -n "${TAB}pushing... "
		    #git -C ${target} push
		    dtab
	  else
		    echo -e "\e[31mdoes not exist\e[0m"
		    echo "${TAB}cloning $my_repo..."
		    git clone ${github_auth}$my_repo ${target}
	  fi

	  # begin linking...
    do_link "${target}" "${link_name}"

	  # run make_links
	  make_links_file="${link_name}/make_links.sh"
	  if [ -e "${make_links_file}" ]; then
		    cd ${link_name}
		    bash ${make_links_file}
		    cd ${wdir}
	  else
		    echo -e "${TAB}${BAD}${make_links_file} not found${RESET}"
	  fi
done
echo -e "done cloning ${group_name} repos"

# load formatting
if [ -e $fpretty ]; then
	  source $fpretty
	  cbar "${MAGENTA}pretty print enabled${RESET}"
fi

# reset TAB
rtab

# list of example repos to be cloned
group_name="example"
echo -e "cloning \x1b[1;32m${group_name}\x1b[m repos..."
itab
for my_repo in cpp fortran hello nrf python; do
	  # define target (source)
	  target=${rdir}/${my_repo}
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
	  link_name=${rdir}/matlab

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
			      make_links_file=make_links.sh
			      fpath="${dname}/make_links.sh"
			      echo -n "${TAB}$fpath... "
			      if [ -f "${fpath}" ]; then
				        echo "found"
				        cd $dname
				        bash $make_links_file
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
