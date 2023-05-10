#!/bin/bash
# print source name at start
echo -n "${TAB}running $BASH_SOURCE"
src_name=$(readlink -f $BASH_SOURCE)
if [ ! "$BASH_SOURCE" = "$src_name" ]; then
    echo -n " -> $src_name"
fi
echo "..."

TAB="   "

if [ "$EUID" -ne 0 ]; then
    echo "${TAB}This command must be run as root!"
    echo "${TAB}Retry with the following command:"
    echo "${TAB}   sudo $BASH_SOURCE"
    exit
else
    echo "${TAB} running as root"
fi

# set source and target directories
source_dir=$(dirname $src_name)
target_dir=/etc

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
for my_link in wsl.conf
do
    target=${source_dir}/${my_link}
    sub_dir=$(dirname "$my_link")
    if [ ! $sub_dir = "." ]; then
	my_link=$(basename "$my_link")
    fi
    link=${target_dir}/${my_link}

    echo -n "source file ${target}... "
    if [ -e $target ]; then
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
		if [ -z $(diff ${target} ${link} ) ]; then
		    echo "have the same contents"
		    continue
		else
		    echo -n "will be backed up..."
		    mv -v $link ${link}_$(date +'%Y-%m-%d-t%H%M')
		fi
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
# print time at exit
echo -en "\n$(date +"%R") ${BASH_SOURCE##*/} "
if command -v sec2elap &>/dev/null; then
    echo "$(sec2elap $SECONDS)"
else
    echo "ellapsed time is ${SECONDS} sec"
fi
