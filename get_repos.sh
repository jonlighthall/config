#!/bin/bash
# print source name at start
echo -n "${TAB}running $BASH_SOURCE"
src_name=$(readlink -f $BASH_SOURCE)
if [ ! "$BASH_SOURCE" = "$src_name" ]; then
    echo -n " -> $src_name"
fi
echo "..."
if [ $# -eq 0 ]; then
    echo "No system specified"
fi
if [ $# -eq 1 ]; then
    echo "Configuring options for $1"
    echo -n "$1... "
    if [ -d $1 ]; then
	echo "found"
	cd $1
	fname=make_links.sh
	echo -n "$fname... "
	if [ -f $fname ]; then
	    echo "found"
	    ./$fname
	    src_name2=~/.bash_profile
	    echo "$src_name2... "
	    if [ -f $src_name2 ]; then
		echo "found"
		source $src_name2
		echo
		echo "configuration applied for $1"
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

rdir=repos
udir=utils
edir=examp

for my_dir in $rdir $udir $edir
do
    mkdir -vp ${my_dir}
done

echo "--------------------------------------"
echo "------ Start Cloning Repo Files-------"
echo "--------------------------------------"

github_user=jonlighthall
github_https=https://github.com/${github_user}/
github_ssh=git@github.com:${github_user}/

cd ${HOME}/${rdir}
# list of repos to be cloned
for my_repo in bash batch fortran_utilities powershell
do
    git clone ${github_https}${my_repo}
    ln -sv ${HOME}/${udir}/${my_repo} ${HOME}/${rdir}/${my_repo}
done

for my_repo in fortran hello nrf python
do
    git clone ${github_https}$my_repo
    ln -sv ${HOME}/${edir}/${my_repo} ${HOME}/${rdir}/${my_repo}
done


for my_repo in matlab
do
    git clone ${github_https}$my_repo
done

cd ${HOME}/config
for my_repo in config_private
do
    git clone ${github_https}$my_repo private
done
