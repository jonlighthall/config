#!/bin/bash
set -e

# print source name at start
echo "${TAB}running $BASH_SOURCE..."
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
	    ./$fname
	    src_name2=~/.bash_profile
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

cd ${rdir}
# list of utility repos to be cloned
for my_repo in bash batch fortran_utilities powershell
do
    if [ ! -d ${my_repo} ]; then
	echo "cloning $my_repo..."
	git clone ${github_https}${my_repo}
    else
	echo "${TAB}dirctory $my_repo already exits"
    fi
    link=${udir}/${my_repo}
    if [ ! -e ${link} ]; then
	ln -sv ${rdir}/${my_repo} ${link}
    fi
done

# load formatting
fpretty=${HOME}/utils/bash/.bashrc_pretty
if [ -e $fpretty ]; then
    source $fpretty
fi

# list of example repos to be cloned
for my_repo in fortran hello nrf python
do
    if [ ! -d ${my_repo} ]; then
	echo "cloning $my_repo..."
	git clone ${github_https}$my_repo
    else
	echo "${TAB}dirctory $my_repo already exits"
    fi
    link=${edir}/${my_repo}
    if [ ! -e ${link} ]; then
	ln -sv ${rdir}/${my_repo} ${link}
    fi

done

# list of other repos to be cloned
for my_repo in matlab
do
    if [ ! -d ${my_repo} ]; then
	echo "cloning $my_repo..."
	git clone ${github_https}$my_repo
    else
	echo "${TAB}dirctory $my_repo already exits"
    fi
done

# list of private repos to be cloned
cd ${HOME}/config
dname=private
for my_repo in config_private
do
    if [ ! -d $dname ]; then
	echo "cloning $dname..."
	echo "see ${github_https}$my_repo/blob/master/.git-credentials"
	git clone ${github_https}$my_repo $dname
	cd $dname
	fname=make_links.sh
	echo -n "${TAB}$fname... "
	if [ -f $fname ]; then
	    echo "found"
	    ./$fname
	else
	    echo "not found"
	fi
    else
	echo "${TAB}dirctory $my_repo already exits"
    fi
done
