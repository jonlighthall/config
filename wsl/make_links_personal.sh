#!/bin/bash
# exit on errors
set -e

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
fi
echo -e "${TAB}${RUN_TYPE} ${PSDIR}$BASH_SOURCE${NORMAL}..."
src_name=$(readlink -f $BASH_SOURCE)
if [ ! "$BASH_SOURCE" = "$src_name" ]; then
    echo -e "${TAB}${VALID}link${NORMAL} -> $src_name"
fi

# set target and link directories
target_dir=/mnt/c/Users/jonli/OneDrive/Documents/home
link_dir=$HOME

# check directories
echo -n "target directory ${target_dir}... "
if [ -d "$target_dir" ]; then
    echo "exists"
else
    echo -e "${BAD}does not exist${NORMAL}"
    exit 1
fi

echo -n "link directory ${link_dir}... "
if [ -d $link_dir ]; then
    echo "exists"
else
    echo "does not exist"
    mkdir -pv $link_dir
    if [ $link_dir = $HOME ]; then
	echo "this should never be true! $link_dir is HOME"
    else
	echo "$link_dir != $HOME"
    fi
fi

bar 38 "-- Start Linking Files Outside Repo --"

# list of files to be linked
for my_link in .bash_history
do
    # define target (source)
    target=${target_dir}/${my_link}
    # define link (destination)
    sub_dir=$(dirname "$my_link")
    if [ ! $sub_dir = "." ]; then
        # strip target subdirectory from link name
	my_link=$(basename "$my_link")
    fi
    link=${link_dir}/${my_link}

    echo -n "target file ${target}... "
    if [ -e "${target}" ]; then
	echo "exists "
	echo -n "${TAB}link $link... "
	TAB+=${fTAB:='   '}
	# first, check for existing copy
	if [ -L ${link} ] || [ -f ${link} ] || [ -d ${link} ]; then
	    echo -n "exists and "
	    if [[ "${target}" -ef ${link} ]]; then
                echo "already points to ${my_link}"
		echo -n "${TAB}"
		ls -lhG --color=auto ${link}
		echo "${TAB}skipping..."
		TAB=${TAB%$fTAB}
		continue
	    else
		# next, delete or backup existing copy
		if [ $(diff -ebwB "${target}" ${link} | wc -c) -eq 0 ]; then
		    echo "has the same contents"
		    echo -n "${TAB}deleting... "
		    rm -v ${link}
		else
		    echo "will be backed up..."
		    mv -v ${link} ${link}_$(date -r ${link} +'%Y-%m-%d-t%H%M') | sed "s/^/${TAB}/"
		fi
	    fi
	else
	    echo "does not exist"
	fi
        # then link
	echo -en "${TAB}${GRH}";hline 72;
	echo "${TAB}making link... "
	ln -sv "${target}" ${link} 2>&1 | sed "s/^/${TAB}/"
	echo -ne "${TAB}";hline 72;echo -en "${NORMAL}"
	TAB=${TAB%$fTAB}
    else
        echo -e "${BAD}does not exist${NORMAL}"
    fi
done

# Create default directory links in ~

# define winhome
if [ ! -e ${HOME}/winhome ]; then
    ln -sv /mnt/c/Users/jonli ${HOME}/winhome
else
    echo "winhome is already a link"
fi

# define links within winhome
if [ ! -e ${HOME}/downloads ]; then
    ln -sv ${HOME}/winhome/Downloads/ ${HOME}/downloads
else
    echo "downloads is already a link"
fi

# define onedrive
if [ ! -e ${HOME}/onedrive ]; then
    ln -sv ${HOME}/winhome/OneDrive/ ${HOME}/onedrive
else
    echo "onedrive is already a link"
fi

# define links wihtin onedrive
if [ ! -e ${HOME}/home ]; then
    ln -sv ${HOME}/ondrive/Documents/home/ ${HOME}/home
else
    echo "home already a link"
fi

if [ ! -e ${HOME}/matlab ]; then
    ln -sv ${HOME}/ondrive/Documents/MATLAB/ ${HOME}/matlab
else
    echo "matlab already a link"
fi

bar 38 "--------- Done Making Links ----------"
# print time at exit
echo -en "\n$(date +"%a %b %-d %-l:%M %p %Z") ${BASH_SOURCE##*/} "
if command -v sec2elap &>/dev/null; then
    sec2elap ${SECONDS}
else
    echo "elapsed time is ${white}${SECONDS} sec${NORMAL}"
fi

exit

# Copy .ssh
if [ -d $target_dir/.ssh ]; then
    echo "Backing up .ssh..."
    mv -v $target_dir/.ssh/ ${HOME}/.ssh_$(date +'%Y-%m-%d-t%H%M')
fi
git clone https://jonlighthall@bitbucket.org/jonlighthall/.ssh.git ${HOME}/.ssh
chmod -v 600 $target_dir/.ssh/config
chmod -v 600 $target_dir/.ssh/id_rsa

