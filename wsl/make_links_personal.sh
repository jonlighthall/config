#!/bin/bash

# print source name at start
echo "${TAB}running $BASH_SOURCE..."
src_name=$(readlink -f $BASH_SOURCE)
if [ ! "$BASH_SOURCE" = "$src_name" ]; then
    echo -e "${TAB}${VALID}link${NORMAL} -> $src_name"
fi

# source formatting
fpretty=${HOME}/utils/bash/.bashrc_pretty
if [ -e $fpretty ]; then
    source $fpretty
fi

# set source and target directories
source_dir=/mnt/c/Users/jonli/OneDrive/Documents/home/cygwin
target_dir=$HOME

# check directories
echo -n "source directory ${source_dir}... "
if [ -d "$source_dir" ]; then
    echo "exists"
else
    echo -e "${BAD}does not exist${NORMAL}"
    exit 1
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

bar 38 "-- Start Linking Files Outside Repo --"

# list of files to be linked
for my_link in .bash_history .git-credentials
do
    target=${source_dir}/${my_link}
    sub_dir=$(dirname "$my_link")
    if [ ! $sub_dir = "." ]; then
	my_link=$(basename "$my_link")
    fi
    link=${target_dir}/${my_link}

    echo -n "source file ${target}... "
    if [ -e "${target}" ]; then
	echo "exists "
	echo -n "${TAB}link $link... "
	# first, backup existing copy
	if [ -L ${link} ] || [ -f ${link} ] || [ -d ${link} ]; then
	    echo -n "exists and "
	    if [[ "${target}" -ef ${link} ]]; then
                echo "already points to ${my_link}"
		echo -n "${TAB}"
		ls -lhG --color=auto ${link}
		echo "${TAB}skipping..."
		continue
	    else
		if [ $(diff "${target}" ${link} | wc -c) -eq 0 ]; then
		    echo "have the same contents"
		    continue
		else
		    echo -n "will be backed up..."
		    mv -v ${link} ${link}_$(date +'%Y-%m-%d-t%H%M')
		fi
	    fi
	else
	    echo "does not exist"
	fi
        # then link
	echo -en "${TAB}${GRH}";hline 72;
	echo "${TAB}making link... "
	ln -sv "${target}" ${link} | sed "s/^/${TAB}/"
	echo -ne "${TAB}";hline 72;echo -en "${NORMAL}"
    else
        echo -e "${BAD}does not exist${NORMAL}"
    fi
done

# Copy .ssh
if [ -d $target_dir/.ssh ]; then
    echo "Backing up .ssh..."
    mv -v $target_dir/.ssh/ ~/.ssh_$(date +'%Y-%m-%d-t%H%M')
fi
git clone https://jonlighthall@bitbucket.org/jonlighthall/.ssh.git ~/.ssh
chmod -v 600 $target_dir/.ssh/config
chmod -v 600 $target_dir/.ssh/id_rsa

if [ ! -e $target_dir/winhome ]; then
    ln -sv /mnt/c/Users/jonli $target_dir/winhome
else
    echo "winhome is already a link"
fi
if [ ! -e $target_dir/onedrive ]; then
    ln -sv /mnt/c/Users/jonli/OneDrive/ $target_dir/onedrive
else
    echo "onedrive is already a link"
fi
bar 38 "--------- Done Making Links ----------"
# print time at exit
echo -en "\n$(date +"%R") ${BASH_SOURCE##*/} "
if command -v sec2elap &>/dev/null; then
    echo "$(sec2elap $SECONDS)"
else
    echo "elapsed time is ${SECONDS} sec"
fi
