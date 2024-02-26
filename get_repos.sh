#!/bin/bash -u
# set tab
:${TAB:=''}
# load formatting
fpretty=${HOME}/utils/bash/.bashrc_pretty
if [ -e $fpretty ]; then
	source $fpretty
fi

# determine if sourcing or executing
if (return 0 2>/dev/null); then
	RUN_TYPE="sourcing"
else
	RUN_TYPE="executing"
	# exit on errors
	set -e
fi
# print source name at start
echo -e "${TAB}${RUN_TYPE} \E[0;33m$BASH_SOURCE\E[0m..."
src_name=$(readlink -f $BASH_SOURCE)
if [ ! "$BASH_SOURCE" = "$src_name" ]; then
	echo -e "${TAB}\E[1;36mlink\E[0m -> $src_name"
fi
# set tab
TAB='   '
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
	if [ -d ${HOME}/config/$1 ]; then
		echo "found"
		cd ${HOME}/config/$1
		echo -n "${TAB}$make_links_file... "
		# check if make links file exists
		if [ -f $make_links_file ]; then
			echo "found"
			bash $make_links_file
			# define profile name
			profie_name=${HOME}/.bash_profile
			echo -n "$profie_name... "
			# check if file exists
			if [ -f $profie_name ]; then
				echo "found"
				echo "sourcing $profie_name..."
				source $profie_name
				echo
				echo "configuration applied for $1"
				echo
			else
				# profile...
				echo "not found"
			fi
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

# define directory names
if command -v wsl.exe >/dev/null; then
	echo "WSL defined... "
	# get host name
	host_name=$(hostname)
	echo "${TAB}host name: $host_name"

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
	echo "${TAB}distro: .. $distro"

	# create sync directory
	hdir=${HOME}/home/$host_name
	echo "${TAB}host dir:   $hdir"

	ddir=${hdir}/$distro
	echo "${TAB}distro dir: $ddir"

	#define target (source)
	target=${ddir}
	# define link (destination)
	link=${HOME}/sync

	# check if target exists
	echo -ne "${TAB}target directory \e[33m${target}\e[0m... "
	if [ -e "${target}" ]; then
		echo "exists "
	else
		echo "does not exist"
		mkdir -pv ${target} | sed "s/^/${TAB}/"
	fi

	# check if link exists
	TAB+=${fTAB:='   '}
	echo -n "${TAB}link $link... "
	do_link=false
	TAB+=${fTAB:='   '}
	# first, check for existing copy
	if [ -L ${link} ] || [ -f ${link} ] || [ -d ${link} ]; then
		echo -n "exists and "
		if [[ "${target}" -ef ${link} ]]; then
			echo "already points to ${link}"
			echo -n "${TAB}"
			ls -lhG --color=auto ${link}
			echo "${TAB}skipping..."
			TAB=${TAB%$fTAB}
			TAB=${TAB%$fTAB}
		else
			do_link=true
			# next, delete or backup existing copy
			if [ $(diff -ebwB "${target}" ${link} 2>&1 | wc -c) -eq 0 ]; then
				echo "has the same contents"
				echo -n "${TAB}deleting... "
				rm -vd ${link}
			else
				echo "will be backed up..."
				mv -v ${link} ${link}_$(date -r ${link} +'%Y-%m-%d-t%H%M') | sed "s/^/${TAB}/"
			fi
		fi
	else
		echo -e "\e[31mdoes not exist\e[0m"
		do_link=true
	fi

	# create link
	if [ ! -e ${link} ] && [ $do_link -eq true ]; then
		ln -sv ${target} ${link} 2>&1 | sed "s/^/${TAB}SYM: /"
	fi

	# define repository directory
	rdir=${ddir}/repos

	# link repo directory to HOME
	#define target (source)
	target=${rdir}
	# define link (destination)
	link=${HOME}/repos

	# check if target exists
	echo -ne "${TAB}target directory \e[33m${target}\e[0m... "
	if [ -e "${target}" ]; then
		echo "exists "
	else
		echo "does not exist"
		mkdir -pv ${target} | sed "s/^/${TAB}/"
	fi

	TAB+=${fTAB:='   '}
	echo -n "${TAB}link $link... "
	TAB+=${fTAB:='   '}
	# first, check for existing copy
	if [ -L ${link} ] || [ -f ${link} ] || [ -d ${link} ]; then
		echo -n "exists and "
		if [[ "${target}" -ef ${link} ]]; then
			echo "already points to ${link}"
			echo -n "${TAB}"
			ls -lhG --color=auto ${link}
			echo "${TAB}skipping..."
			TAB=${TAB%$fTAB}
			TAB=${TAB%$fTAB}
		else
			# next, delete or backup existing copy
			if [ $(diff -ebwB "${target}" ${link} 2>&1 | wc -c) -eq 0 ]; then
				echo "has the same contents"
				echo -n "${TAB}deleting... "
				rm -vd ${link}
			else
				echo "will be backed up..."
				mv -v ${link} ${link}_$(date -r ${link} +'%Y-%m-%d-t%H%M') | sed "s/^/${TAB}/"
			fi
		fi

		# create link
		if [ ! -e ${link} ]; then
			ln -sv ${target} ${link} 2>&1 | sed "s/^/${TAB}SYM: /"
		fi
	else
		echo -e "\e[31mdoes not exist\e[0m"
		# create link
		if [ ! -e ${link} ]; then
			ln -sv ${target} ${link} 2>&1 | sed "s/^/${TAB}SYM: /"
		fi
	fi
else
	echo "WSL not defined."
	rdir=${HOME}/repos
fi

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
gname="utility"
echo "cloning ${gname} repos..."
for my_repo in bash fortran_utilities; do
	#define target (source)
	target=${rdir}/${my_repo}
	# define link (destination)
	link=${udir}/${my_repo}

	# check if target exists
	echo -ne "${TAB}target dirctory \e[33m${target}\e[0m... "
	if [ -e "${target}" ]; then
		echo "exists "
		TAB+=${fTAB:='   '}
		echo -n "${TAB}pulling... "
		git -C ${target} pull
		echo -n "${TAB}pushing... "
		git -C ${target} push
		TAB=${TAB%$fTAB}
	else
		echo -e "\e[31mdoes not exist\e[0m"
		echo "${TAB}cloning $my_repo..."
		git clone ${github_auth}$my_repo ${target}
	fi

	# begin linking...
	TAB+=${fTAB:='   '}
	echo -n "${TAB}link $link... "

	# first, check for existing copy
	if [ -L ${link} ] || [ -d ${link} ]; then
		TAB+=${fTAB:='   '}
		echo -n "exists and "
		if [[ "${target}" -ef ${link} ]]; then
			echo "already points to ${my_repo}"
			echo -n "${TAB}"
			ls -lhG --color=auto ${link}
			echo "${TAB}skipping..."
			TAB=${TAB%$fTAB}
			TAB=${TAB%$fTAB}
			continue
		else
			# next, delete or backup existing copy
			if [ $(\diff --suppress-common-lines -r ${target} ${link} 2>&1 | wc -c) -eq 0 ]; then
				echo "has the same contents"
				echo -n "${TAB}deleting... "
				rm -vfd ${link}
			else
				echo "will be backed up..."
				mv -v ${link} ${link}_$(date -r ${link} +'%Y-%m-%d-t%H%M') | sed "s/^/${TAB}/"
			fi
			TAB=${TAB%$fTAB}
			TAB=${TAB%$fTAB}
		fi
		TAB=${TAB%$fTAB}
	else
		echo "does not exist"
	fi

	# then link
	if [ ! -e ${link} ]; then
		ln -sv "${target}" ${link} 2>&1 | sed "s/^/${TAB}SYM: /"
	fi

	# run make_links
	make_links_file="${link}/make_links.sh"
	if [ -e "${make_links_file}" ]; then
		cd ${link}
		bash ${make_links_file}
		cd ${rdir}
	else
		echo -e "${TAB}${BAD}${make_links_file} not found${NORMAL}"
	fi
done

# clone Win32 repos
if [[ "$(hostname -f)" =~ *"navy.mil" ]]; then
	echo -n "creating links inside Onedrive..."
	wdir=$rdir
else
	echo -n "creating links outside of Onedrive..."
	wdir="${HOME}/winhome/Documents/repos"
	target=${wdir}
	echo -ne "${TAB}target directory \e[33m${target}\e[0m... "
	if [ -e "${target}" ]; then
		echo "exists "
	else
		echo "does not exist"
		mkdir -pv ${target} | sed "s/^/${TAB}/"
	fi
fi

for my_repo in batch powershell; do
	#define target (source)

	target=${wdir}/${my_repo}
	# define link (destination)
	link=${udir}/${my_repo}

	# check if target exists
	echo -ne "${TAB}target dirctory \e[33m${target}\e[0m... "
	if [ -e "${target}" ]; then
		echo "exists "
		TAB+=${fTAB:='   '}
		echo -n "${TAB}pulling... "
		git -C ${target} pull
		echo -n "${TAB}pushing... "
		git -C ${target} push
		TAB=${TAB%$fTAB}
	else
		echo -e "\e[31mdoes not exist\e[0m"
		echo "${TAB}cloning $my_repo..."
		git clone ${github_auth}$my_repo ${target}
	fi

	# begin linking...
	TAB+=${fTAB:='   '}
	echo -n "${TAB}link $link... "

	# first, check for existing copy
	if [ -L ${link} ] || [ -d ${link} ]; then
		TAB+=${fTAB:='   '}
		echo -n "exists and "
		if [[ "${target}" -ef ${link} ]]; then
			echo "already points to ${my_repo}"
			echo -n "${TAB}"
			ls -lhG --color=auto ${link}
			echo "${TAB}skipping..."
			TAB=${TAB%$fTAB}
			TAB=${TAB%$fTAB}
			continue
		else
			# next, delete or backup existing copy
			if [ $(\diff --suppress-common-lines -r ${target} ${link} 2>&1 | wc -c) -eq 0 ]; then
				echo "has the same contents"
				echo -n "${TAB}deleting... "
				rm -vfd ${link}
			else
				echo "will be backed up..."
				mv -v ${link} ${link}_$(date -r ${link} +'%Y-%m-%d-t%H%M') | sed "s/^/${TAB}/"
			fi
			TAB=${TAB%$fTAB}
			TAB=${TAB%$fTAB}
		fi
		TAB=${TAB%$fTAB}
	else
		echo "does not exist"
	fi

	# then link
	if [ ! -e ${link} ]; then
		ln -sv "${target}" ${link} 2>&1 | sed "s/^/${TAB}SYM: /"
	fi

	# run make_links
	make_links_file="${link}/make_links.sh"
	if [ -e "${make_links_file}" ]; then
		cd ${link}
		bash ${make_links_file}
		cd ${wdir}
	else
		echo -e "${TAB}${BAD}${make_links_file} not found${NORMAL}"
	fi
done
echo -e "done cloning ${gname} repos"

# load formatting
if [ -e $fpretty ]; then
	source $fpretty
	cbar "${magenta}pretty print enabled${NORMAL}"
fi

# reset TAB
TAB=''

# list of example repos to be cloned
gname="example"
echo "cloning ${gname} repos..."
TAB+=${fTAB:='   '}
for my_repo in cpp fortran hello nrf python; do
	# define target (source)
	target=${rdir}/${my_repo}
	# define link (destination)
	link=${edir}/${my_repo}

	# check if target exists
	echo -ne "${TAB}target dirctory ${yellow}${target}${NORMAL}... "
	if [ -e "${target}" ]; then
		echo "exits"
	else
		echo "does not exist"
		echo "${TAB}cloning $my_repo..."
		git clone ${github_auth}$my_repo ${target}
	fi
	# begin linking...
	TAB+=${fTAB}
	echo -n "${TAB}link ${link}... "

	# first, check for existing copy
	if [ -L ${link} ] || [ -d ${link} ]; then
		TAB+=${fTAB}
		echo -n "exits and "
		if [[ "${target}" -ef ${link} ]]; then
			echo "already points to ${my_repo}"
			echo -n "${TAB}"
			ls -lhG --color=auto ${link}
			echo "${TAB}skipping..."
			TAB=${TAB%$fTAB}
			TAB=${TAB%$fTAB}
			continue
		else
			# next, delete or backup existing copy
			if [ $(\diff --suppress-common-lines -r ${target} ${link} 2>&1 | wc -c) -eq 0 ]; then
				echo "has the same contents"
				echo -n "${TAB}deleting..."
				rm -rfd ${link}
			else
				echo "will be backed up..."
				mv -v ${link} ${link}_$(date -r ${link} +'%Y-%m-%d-t%H%M') | sed "s/^/${TAB}/"
			fi
			TAB=${TAB%$fTAB}
			TAB=${TAB%$fTAB}
		fi
		TAB=${TAB%$fTAB}
	else
		echo "does not exist"
	fi
	# then link
	echo -en "${TAB}${GRH}"
	hline 72
	echo "${TAB}making link... "
	ln -sv "${target}" ${link} 2>&1 | sed "s/^/${TAB}/"
	echo -ne "${TAB}"
	hline 72
	echo -en "${NORMAL}"
	TAB=${TAB%$fTAB}
done
TAB=${TAB%$fTAB}
echo -e "done cloning ${gname} repos"

# list of other repos to be cloned
gname="other"
echo "cloning ${gname} repos..."
TAB+=${fTAB:='   '}
for my_repo in matlab; do
	# define target (source)
	target=${rdir}/${my_repo}
	# define link (destination)
	link=~/${my_repo}

	# check if target exists
	echo -ne "${TAB}target dirctory ${yellow}${target}${NORMAL}... "
	if [ -e "${target}" ]; then
		echo "exits"
	else
		echo "does not exist"
		echo "${TAB}cloning $my_repo..."
		git clone ${github_auth}$my_repo ${target}
	fi
	# begin linking...
	TAB+=${fTAB}
	echo -n "${TAB}link ${link}... "

	# first, check for existing copy
	if [ -L ${link} ] || [ -d ${link} ]; then
		TAB+=${fTAB}
		echo -n "exits and "
		if [[ "${target}" -ef ${link} ]]; then
			echo "already points to ${my_repo}"
			echo -n "${TAB}"
			ls -lhG --color=auto ${link}
			echo "${TAB}skipping..."
			TAB=${TAB%$fTAB}
			TAB=${TAB%$fTAB}
			continue
		else
			# next, delete or backup existing copy
			if [ $(\diff --suppress-common-lines -r ${target} ${link} 2>&1 | wc -c) -eq 0 ]; then
				echo "has the same contents"
				echo -n "${TAB}deleting..."
				rm -rfd ${link}
			else
				echo "will be backed up..."
				mv -v ${link} ${link}_$(date -r ${link} +'%Y-%m-%d-t%H%M') | sed "s/^/${TAB}/"
			fi
			TAB=${TAB%$fTAB}
			TAB=${TAB%$fTAB}
		fi
		TAB=${TAB%$fTAB}
	else
		echo "does not exist"
	fi

	# then link
	echo -en "${TAB}${GRH}"
	hline 72
	echo "${TAB}making link... "
	ln -sv "${target}" ${link} 2>&1 | sed "s/^/${TAB}/"
	echo -ne "${TAB}"
	hline 72
	echo -en "${NORMAL}"
	TAB=${TAB%$fTAB}

	# run make_links
	make_links_file="${link}/make_links.sh"
	if [ -e "${make_links_file}" ]; then
		cd ${link}
		bash ${make_links_file}
		cd ${rdir}
	else
		echo -e "${TAB}${BAD}${make_links_file} not found${NORMAL}"
	fi
done
TAB=${TAB%$fTAB}
echo -e "done cloning ${gname} repos"

# list of private repos to be cloned
if [[ ! ("$(hostname -f)" == *"navy.mil") ]]; then
	gname="private"
	echo "cloning ${gname} repos..."
	TAB+=$fTAB
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
			TAB+=${fTAB}
			echo -n "${TAB}pulling... "
			git -C ${dname} pull
			echo -n "${TAB}pushing... "
			git -C ${dname} push
			TAB=${TAB%$fTAB}
		fi
	done
	TAB=${TAB%$fTAB}
	echo -e "done cloning ${gname} repos"
fi
echo "done cloning all repos"
