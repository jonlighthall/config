#!/bin/bash
# load formatting
fpretty=${HOME}/utils/bash/.bashrc_pretty
if [ -e $fpretty ]; then
    source $fpretty
fi
# set tab
export TAB+=${TAB+${fTAB:='   '}}

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

# set target and link directories
windows_user=$(cmd.exe /c "echo %USERNAME%" 2>/dev/null | tr -d '\r')
win_home_dir="/mnt/c/Users/$windows_user/"
echo -n "${TAB}win home: ${win_home_dir}..."
if [ -e "${win_home_dir}" ]; then
    echo "exists"
else
    echo -e "${BAD}does not exist${NORMAL}"
    exit 1
fi

onedirve_dir=$(cmd.exe /c "echo %OneDrive%" 2>/dev/null | tr -d '\r')
onedrive_name=$(echo "${onedirve_dir##*\\}")
onedrive_dir_wsl="${win_home_dir}${onedrive_name}/"

echo -n "${TAB}onedrive: ${onedrive_dir_wsl}..."
if [ -e "${onedrive_dir_wsl}" ]; then
    echo "exists"
else
    echo -e "${BAD}does not exist${NORMAL}"
    exit 1
fi

onedrive_docs="${onedrive_dir_wsl}Documents"

echo -n "${TAB}onedrive docs: ${onedrive_docs}..."
if [ -e "${onedrive_docs}" ]; then
    echo "exists"
else
    echo -e "${BAD}does not exist${NORMAL}"
    exit 1
fi

home_dir="${onedrive_docs}/home"

link_dir=$HOME

# check directories
echo -n "${TAB}target directory ${home_dir}... "
if [ -e "${home_dir}" ]; then
    echo "exists"
else
    echo -e "${BAD}does not exist${NORMAL}"
    exit 1
fi

echo -n "${TAB}link directory ${link_dir}... "
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

bar 38 "---- Start Linking External Files ----" | sed "s/^/${TAB}/"

# list of files to be linked
for my_link in .bash_history; do
    # define target (source)
    target=${home_dir}/${my_link}
    # strip target subdirectory from link name
    sub_dir=$(dirname "$my_link")
    if [ ! $sub_dir = "." ]; then
        my_link=$(basename "$my_link")
    fi
    # define link (destination)
    link=${link_dir}/${my_link}

    # check if target exists
    echo -ne "${TAB}target file ${yellow}${target}${NORMAL}... "
    if [ -e "${target}" ]; then
        echo "exists "
        TAB+=${fTAB:='   '}
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
        echo -en "${TAB}${GRH}"
        hline 72
        echo "${TAB}making link... "
        ln -sv "${target}" ${link} 2>&1 | sed "s/^/${TAB}/"
        echo -ne "${TAB}"
        hline 72
        echo -en "${NORMAL}"
        TAB=${TAB%$fTAB}
        TAB=${TAB%$fTAB}
    else
        echo -e "${BAD}does not exist${NORMAL}"
    fi
done
bar 38 "----- Done Linking External Files ----" | sed "s/^/${TAB}/"

bar 38 "- Start Linking External Directories -" | sed "s/^/${TAB}/"
# Create default directory links in ~

# define winhome
if [ ! -e ${HOME}/winhome ]; then
    ln -sv ${win_home_dir} ${HOME}/winhome
else
    echo -e "${TAB}${yellow}winhome${NORMAL} is already a link"
fi

# define links within winhome
if [ ! -e ${HOME}/downloads ]; then
    ln -sv ${win_home_dir}/Downloads/ ${HOME}/downloads
else
    echo -e "${TAB}${yellow}downloads${NORMAL} is already a link"
fi

# define onedrive
if [ ! -e ${HOME}/onedrive ]; then
    ln -sv ${onedirve_dir_wsl} ${HOME}/onedrive
else
    echo -e "${TAB}${yellow}onedrive${NORMAL} is already a link"
fi

# define links within onedrive
if [ ! -e ${HOME}/home ]; then
    ln -sv ${home_dir} ${HOME}/home
else
    echo -e "${TAB}${yellow}home${NORMAL} already a link"
fi

if [ ! -e ${HOME}/matlab ]; then
    ln -sv ${onedrive_docs}/MATLAB/ ${HOME}/matlab
else
    echo -e "${TAB}${yellow}matlab${NORMAL} already a link"
fi

bar 38 "- Done Linking External Directories --" | sed "s/^/${TAB}/"
#       12345678901234567890123456789012345678

# print time at exit
echo -en "${TAB}$(date +"%a %b %-d %-l:%M %p %Z") ${BASH_SOURCE##*/} "
if command -v sec2elap &>/dev/null; then
    bash sec2elap ${SECONDS}
else
    echo "elapsed time is ${white}${SECONDS} sec${NORMAL}"
fi
