#!/bin/bash
set -e

# load formatting
fpretty=${HOME}/utils/bash/.bashrc_pretty
if [ -e $fpretty ]; then
    source $fpretty
else
    bar() {
	echo "$2"
    }
    hline () {
	echo "---"
    }
fi

# print source name at start
echo -e "${TAB}running ${PSDIR}$BASH_SOURCE${NORMAL}..."
src_name=$(readlink -f $BASH_SOURCE)
if [ ! "$BASH_SOURCE" = "$src_name" ]; then
    echo -e "${TAB}${VALID}link${NORMAL} -> $src_name"
fi

# set source and target directories
source_dir=$(dirname "$src_name")
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

bar 38 "------ Start Linking Repo Files-------"

# list of files to be linked
for my_link in .bash_logout .bash_profile .emacs .gitconfig .inputrc
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
bar 38 "--------- Done Making Links ----------"
# print time at exit
echo -en "$(date +"%a %b %d %I:%M %p %Z") ${BASH_SOURCE##*/} "
if command -v sec2elap &>/dev/null; then
    echo "$(sec2elap $SECONDS)"
else
    echo "elapsed time is ${SECONDS} sec"
fi
