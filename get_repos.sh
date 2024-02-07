#!/bin/bash
# set tab
:${TAB:=''}
# load formatting
fpretty=${HOME}/utils/bash/.bashrc_pretty
if [ -e $fpretty ]; then
    source $fpretty
fi

# print source name at start
if (return 0 2>/dev/null); then
    RUN_TYPE="sourcing"
else
    RUN_TYPE="executing"
    # exit on errors
    set -e
fi
echo -e "${TAB}${RUN_TYPE} ${PSDIR}$BASH_SOURCE${NORMAL}..."
src_name=$(readlink -f $BASH_SOURCE)
if [ ! "$BASH_SOURCE" = "$src_name" ]; then
    echo -e "${TAB}${VALID}link${NORMAL} -> $src_name"
fi
TAB='   '
if [ $# -eq 0 ]; then
    echo "No system specified"
fi
# set file name to be run in system directory
fname=make_links.sh
if [ $# -eq 1 ]; then
    echo "Loading configuration options for $1"
    echo -n "${TAB}$1... "
    if [ -d ${HOME}/config/$1 ]; then
		echo "found"
		cd ${HOME}/config/$1
		echo -n "${TAB}$fname... "
		if [ -f $fname ]; then
			echo "found"
			bash $fname
			src_name2=${HOME}/.bash_profile
			echo -n "$src_name2... "
			if [ -f $src_name2 ]; then
				echo "found"
				source $src_name2
				echo
				echo "configuration applied for $1"
				echo
			else
				echo "not found"
			fi
		else
			echo "not found"
		fi
    else
		echo "not found"
    fi
fi

# check if git is defined
if  command -v git ; then
    echo "proceeding with Git commands..."

    # define directory names
    rdir=${HOME}/repos
    udir=${HOME}/utils
    edir=${HOME}/examp

    echo "creating repository directories..."
    for my_dir in $rdir $udir $edir
    do
		if [ ! -d ${my_dir} ]; then
			mkdir -vp ${my_dir}
		else
			echo "${TAB}directory ${my_dir} already exists"
		fi
    done

    echo "--------------------------------------"
    echo "------ Start Cloning Repo Files-------"
    echo "--------------------------------------"

    github_user=jonlighthall
    github_https=https://github.com/${github_user}/
    github_ssh=git@github.com:${github_user}/
    github_auth=${github_ssh}

    cd ${rdir}
    # list of utility repos to be cloned
	gname="utility"
	echo "cloning ${gname} repos..."
    for my_repo in bash batch fortran_utilities powershell
    do
		if [ ! -d ${my_repo} ]; then
			echo "cloning $my_repo..."
			git clone ${github_auth}${my_repo}.git
		else
			echo "dirctory $my_repo already exits"
		fi

		# define link (destination)
		link=${udir}/${my_repo}

		# create link
		if [ ! -e ${link} ]; then
			ln -sv ${rdir}/${my_repo} ${link}
		fi

		# run make_links
		fname="${link}/make_links.sh"
		if [ -e "${fname}" ];then
			cd ${link}
			bash ${fname}
			cd ${rdir}
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
    for my_repo in cpp fortran hello nrf python
    do

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
			git clone ${github_auth}$my_repo
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
				if [ $(\diff --suppress-common-lines -r ${rdir}/${my_repo} ${link} | wc -c) -eq 0 ]; then
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
		echo -en "${TAB}${GRH}";hline 72;
		echo "${TAB}making link... "
		ln -sv "${target}" ${link} | sed "s/^/${TAB}/"
		echo -ne "${TAB}";hline 72;echo -en "${NORMAL}"
		TAB=${TAB%$fTAB}
    done
	TAB=${TAB%$fTAB}
	echo -e "done cloning ${gname} repos"
	
    # list of other repos to be cloned
	gname="other"
	echo "cloning ${gname} repos..."
	TAB+=${fTAB:='   '}
	for my_repo in matlab
    do
		if [ ! -d ${my_repo} ]; then
			echo "cloning $my_repo..."
			git clone ${github_auth}$my_repo
		else
			echo -e "${TAB}dirctory ${yellow}$PWD/$my_repo${NORMAL} already exits"
		fi
    done
	TAB=${TAB%$fTAB}
	echo -e "done cloning ${gname} repos"

    # list of private repos to be cloned
    if [[ ! ("$(hostname -f)"  == *"navy.mil") ]]; then
		gname="private"
		echo "cloning ${gname} repos..."
		TAB+=$fTAB	
		cd ${HOME}/config
		dname=private
		for my_repo in config_private
		do
			if [ ! -d $dname ]; then
				echo "cloning $dname..."
				echo "see ${github_https}$my_repo/blob/master/.git-credentials"
				git clone ${github_auth}$my_repo $dname

				# run make_links
				fname=make_links.sh
				fpath="${dname}/make_links.sh"
				echo -n "${TAB}$fpath... "
				if [ -f "${fpath}" ]; then
					echo "found"
					cd $dname
					bash $fname
				else
					echo "not found"
				fi
			else
				echo "${TAB}dirctory $PWD$my_repo already exits"
			fi
		done
		TAB=${TAB%$fTAB}
		echo -e "done cloning ${gname} repos"
    fi
	echo "done cloning all repos" 	
else
    echo "Git not defined."
fi
