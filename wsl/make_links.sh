#!/bin/bash

# load formatting
fpretty=${HOME}/utils/bash/.bashrc_pretty
if [ -e $fpretty ]; then
    source $fpretty
else
    bar() {
        echo "$2"
    }
    hline() {
        echo "---"
    }
fi
# set tab
export TAB+=${TAB+${fTAB:='   '}}

# determine if sourcing or executing
if (return 0 2>/dev/null); then
    RUN_TYPE="sourcing"
else
    RUN_TYPE="executing"
    # exit on errors
    set -e
fi
# print source name at start
echo -e "${TAB}${RUN_TYPE} ${PSDIR}$BASH_SOURCE${NORMAL}..."
src_name=$(readlink -f $BASH_SOURCE)
if [ ! "$BASH_SOURCE" = "$src_name" ]; then
    echo -e "${TAB}${VALID}link${NORMAL} -> $src_name"
fi

# it is assumed that the fisrt command to be run after cloning the parent
# repository is make_links.sh (this file)

# run the following configureation files
prog=install_packages.sh
echo -n "${TAB}$prog..."
if [ -f $prog ]; then
    echo "found"
    echo "${TAB}${prog} requires"
    TAB+=${fTAB:='   '}
    echo "${TAB}* elevation"
    echo "${TAB}* access to archive.ubuntu.com, security.ubuntu.com, etc"
    TAB=${TAB%$fTAB}
    set +e
    read -p "${TAB}Proceed with ${prog}? (y/n) " -n 1 -r -t 3
    set -e
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        bash $prog
    fi
else
    echo "not found"
fi

for prog in make_links_personal.sh; do
    echo -n "${TAB}${prog}... "
    if [ -f $prog ]; then
        echo "found"
        bash $prog
    else
        echo "not found"
    fi
done

# set target and link directories
target_dir=$(dirname "$src_name")
link_dir=$HOME

# check directories
echo -n "${TAB}target directory ${target_dir}... "
if [ -d "$target_dir" ]; then
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

bar 38 "------ Start Linking Repo Files ------" | sed "s/^/${TAB}/"

# list of files to be linked
for my_link in .bash_aliases .bash_logout .bash_profile .emacs.d .gitconfig .hushlogin .inputrc .rootrc; do
    # define target (source)
    target=${target_dir}/${my_link}
    # strip target subdirectory from link name
    sub_dir=$(dirname "$my_link")
    if [ ! $sub_dir = "." ]; then
        my_link=$(basename "$my_link")
    fi
    # define link (destination)
    link=${link_dir}/${my_link}

    # check if target exists
    echo -ne "${TAB}target file \e[33m${target}\e[0m... "
    if [ -e "${target}" ]; then
        echo "exists "
        TAB+=${fTAB:='   '}
        echo -n "${TAB}link $link... "
        TAB+=${fTAB}
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
        ln -sv "${target}" ${link} 2>&1 | sed "s/^/${TAB}SYM: /"
        echo -ne "${TAB}"
        hline 72
        echo -en "${NORMAL}"
        TAB=${TAB%$fTAB}
        TAB=${TAB%$fTAB}
    else
        echo -e "${BAD}does not exist${NORMAL}"
    fi
done
bar 38 "------- Done Linking Repo Files ------" | sed "s/^/${TAB}/"

# run make_linnks in /etc

for prog in make_links_etc.sh; do
    echo -n "${TAB}${prog}... "
    if [ -f $prog ]; then
        echo "found"
        bash $prog
    else
        echo "not found"
    fi
done

# print time at exit
echo -en "${TAB}$(date +"%a %b %-d %-l:%M %p %Z") ${BASH_SOURCE##*/} "
if command -v sec2elap &>/dev/null; then
    bash sec2elap ${SECONDS}
else
    echo "elapsed time is ${white}${SECONDS} sec${NORMAL}"
fi
