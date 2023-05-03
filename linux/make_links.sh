#!/bin/bash
echo $BASH_SOURCE
TAB="   "

# set source and target directories
source_dir=$HOME/config/linux
target_dir=$HOME

# check directories
echo -n "source directory ${source_dir}... "
if [ -d $source_dir ]; then
    echo "exists"
else
    echo "does not exist"
    return 1
fi

echo -n "target directory ${target_dir}... "
if [ -d $target_dir ]; then
    echo "exists"
else
    echo "does not exist"
    mkdir -pv $target_dir
    if [ $target_dir = $HOME ]; then
	echo "this should never be true! $target_dir is HOME"
    else
	echo "$target_dir != $HOME"
    fi
fi

echo "--------------------------------------"
echo "------ Start Linking Repo Files-------"
echo "--------------------------------------"

# list of files to be linked
for my_link in .bash_logout .bash_profile .emacs .gitconfig .inputrc
do
    target=${source_dir}/${my_link}
    link=${target_dir}/${my_link}

    echo -n "source file ${target}... "
    if [ -e ${target} ]; then
	echo "exists "
	echo -n "${TAB}link $link... "
	# first, backup existing copy
	if [ -L $link ] || [ -f $link ] || [ -d $link ]; then
	    echo -n "exists and "
	    if [[ $target -ef $link ]]; then
		echo "already points to ${my_link}"
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
