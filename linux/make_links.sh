#!/bin/bash
echo $BASH_SOURCE
TAB="   "

# set source and target directories
source_dir=$HOME/config/linux
user_bin=$HOME

# check directories
echo -n "source directory ${source_dir}... "
if [ -d $source_dir ]; then
    echo "exists"
else
    echo "does not exist"
    return 1
fi

echo -n "target directory ${user_bin}... "
if [ -d $user_bin ]; then
    echo "exists"
else
    echo "does not exist"
    mkdir -pv $user_bin
    if [ $user_bin = $HOME ]; then
	echo "this should never be true! $user_bin is HOME"
    else
	echo "$user_bin != $HOME"
    fi
fi

echo "--------------------------------------"
echo "------ Start Linking Repo Files-------"
echo "--------------------------------------"

# list of files to be linked
for my_link in .bash_logout .bash_profile .emacs .gitconfig .inputrc
do
    target=${source_dir}/${my_link}
    link=${user_bin}/${my_link}

    echo -n "source file ${target}... "
    if [ -e ${target} ]; then
	echo "exists "

	echo -n "${TAB}link ${link}... "
	# first, backup existing copy
	if [ -L $link ] || [ -f $link ] || [ -d $link ]; then
	    echo -n "exists and "
	    if [[ $target -ef $link ]]; then
		echo "already points to ${prog}"
		echo -n "${TAB}"
		ls -lhG --color=auto $link
		echo "${TAB}skipping..."
		continue
	    else
		echo -n "will be backed up..."
		mv -v $link ${link}_$(date +'%Y-%m-%d-t%H%M')
	    fi
	else
	    echo "does not exist"
	fi
	# then link
	echo -n "${TAB}making link... "
	ln -sv $target $link
    else
	echo "does not exist"
    fi
    echo
done
echo "--------------------------------------"
echo "--------- Done Making Links ----------"
echo "--------------------------------------"
