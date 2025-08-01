#!/bin/bash -eu
# ------------------------------------------------------------------------------
# SYSTEM-INDEPENDENT BASH ALIASES
# ------------------------------------------------------------------------------
# ~/config/.bash_aliases
#
# Purpose: Define aliases and function for interactive UNIX-like shells.
#
# Usage: Intended to be universally compatible with WSL, Debian derivatives
#   (Ubuntu), Red Hat-based distros (Centos, Rock Linux), MinGW (MSYS, GitBash),
#   Cygwin, PGI Bash, etc.
#
# Aug 2023 JCL
#
# ------------------------------------------------------------------------------

# check if VB is unset or null
if [ -z ${VB:+dummy} ]; then
    export VB=false
else
    # check if VB is true
    if [ "${VB}" = true ]; then
        # load bash utilities
        fpretty=${HOME}/config/.bashrc_pretty
        if [ -e $fpretty ]; then
            source $fpretty
            print_ribbon
            if [[ "$-" == *i* ]]; then
                print_source
            fi
        fi
    fi
fi

# Aliases
alias close='source ${HOME}/.bash_logout;killall -9 -v -u $USER$USERNAME; exit'
alias hello='echo "${TAB-''}hello, world"'
alias ping='ping -c 4 -W 1'
alias pkill='pkill -9 -u ${USER}'
alias pwd='pwd -L;pwd -P'
alias up='source update_repos'
alias vpn='source ${HOME}/config/.bash_aliases_vpn'
alias x='exit'

alias check_ssh='ssh -o ConnectTimeout=3 -o ConnectionAttempts=1 -T git@github.com'

# diff
alias d='diffy -s'
alias diffy='diff --color=always --suppress-common-lines -yiEZbwB --exclude=.git'

# find
alias f='find -L ./ -not -path "*/.git*/*"' # find, ignoring .git
alias fb='ff | perl -lne "print if not -T"' # find binary
alias fd='f -type d' # find directory
alias fe='f -empty'  # find empty
alias fed='f -empty -print -delete' # find and delete empty
alias ff='f -type f' # find file
alias ffi='ff -iname'
alias fl='find -L ./ \( -type l -o -xtype l \) -exec ls --color -l {} \;' # find link

# git
alias gitb='git branch -vva --color=always'
alias gitcp='git cherry-pick'
alias gitd='git diff --color-moved=blocks --ignore-space-change'
alias gitf='git fetch --all --verbose'
alias gitdn='git diff --name-status'
alias gitr='git remote -v'
alias gits='git status'
alias gitsi='git status --ignored; echo; git branch -vva --color=always;echo; git remote -v; git stash list --date=local'
alias gitsl='git stash list --date=local'
alias gitsd='git diff $(git stash list | head -1 | sed "s/.*: \([a-z0-9]\{7\}\).*/\1/") stash'

# grep
alias grep='grep --color=auto'
alias gR='grep -IrRn -D skip --exclude-dir=".git" --exclude="*~"'
alias gr='gR -i'
alias g='gr'

# history
alias hg='history | grep'
alias hgt='history | grep "^[0-9 ]\{0,6\}  [0-9\-]\{10\} " | grep'
alias hl="\cat ~/.bash_history | sed '/^#[0-9]\{10\}/d' | awk '{print length, \$0}' | sort -n | uniq -c | sort -n -r | sed 's/^\([ \t]*[0-9]*\) [0-9]* \(.*$\)/\1 \2/' | head -n 20"

# list
alias lS='ls -ltS' # list by size
alias la='ls -la' # list all
alias lh='ls -ld .?*' # list hidden only
alias ls='ls -l --color'
alias lt='ls -ltr --time-style=long-iso' # list by time
alias lsd='ls -d */' # list directories

alias mv='mv -bv' # make a backup of each existing destination file, verbose

alias rm='rm -dv' # remove empty directories, verbose

# Functions
alias du0='duf --max-depth=0'
alias du1='duf --max-depth=1'
alias du2='duf --max-depth=2'

# disk usage, formatted
function duf {
    du -k "$@" | sort -n |
	      while read size fname; do
     	      for unit in k M G T P E Z Y
	          do
		            if [ $size -lt 1024 ]; then
		                echo -e "${size}${unit}B${fname}";
		                break;
		            fi;
		            size=$((size/1024));
	          done
	      done
}

function gitl {
    # alias gitl='git log --follow'
    if [ $# -ne 1 ]; then
	      git log
    else
	      (git log --follow $@ >/dev/null) && git log --follow $@ || git log $@
    fi
}

# print number of files in the current directory, or specify a directory with an
#   optional argument
function nf {
    if [ $# -eq 0 ]; then
	      dir_list=$PWD
    else
	      dir_list="$@"
    fi
    for dir in $dir_list
    do
	      dir=${dir%/}
	      n1=$(find "${dir}" -maxdepth 1 -type f | wc -l)

	      # calculate length of longest number
	      n2=$(find "${dir}" -type f | wc -l)
	      nn=$(echo "(l($n2)/l(10))+1" | bc -l | sed 's/\..*$//')
	      # account for commas
	      nc=$((($nn-1)/3))
	      np=$(($nn + $nc))

	      # print results
	      printf "%'${np}d files found in ${dir}\n" $n1
	      if [ $n1 -ne $n2 ]; then
	          printf "%'${np}d files found in ${dir}/*\n" $n2
	      fi
    done
}
if [ "${VB}" = true ]; then
    # reset tab
    dtab
fi

# open Emacs in No-window mode and eDiff arguments (E-N-D)
function end() {
    check_arg2 $@
    echo "inputs are $1 and $2"
    unset file1
    unset file2
    export file1=$(readlink -f $1)
    export file2=$(readlink -f $2)
    echo "inputs are $file1 and $file2"
    #return 0

    eval "emacs -nw --eval '(ediff-files \"${file1}\" \"${file2}\")'"

}

# show top processes
function utop() {
    local -i NP
    # number of processes to show
    if [ $# -eq 1 ]; then
        # use argument
        NP=$1
    else
        # set manually
        NP=3
    fi

    set -u

    # set line width to one less than the terminal width
    local -i line_width=$(( $(tput cols) - 1 ))
    # user ID
    local -r U_ID_old=1111499164
    local -r U_ID=$UID
    # user name
    unset U_NAME
    unset U_NAME_trunc
    local -r U_NAME=${USER}
    decho "${U_NAME}"
    local U_NAME_trunc
    U_NAME_trunc=${U_NAME:0:7}
    decho "${U_NAME_trunc}"
    U_NAME_trunc+="+"
    decho "${U_NAME_trunc}"
    #echo "${U_NAME_trunc}" | sed "s/${U_NAME_trunc}/${U_NAME}/"

    echo -e "\033[4mTop $NP processes by ${USER}:\x1b[0m"
    (    ps ux --sort=-pcpu \
             | head -n $((NP+1)) \
             | sed "s/${U_ID_old}/${U_NAME}/" \
             | sed "s/${U_ID}/${U_NAME}/" \
             | awk '{printf "%-10s %5s %5s %5s %4s %5s %6s %s\n",$1,$2,$3,$4,$8,$9,$10,$11}' \
             | cut -c -$line_width \
             | sed "s/${U_NAME_trunc}/${U_NAME}/"
    ) | column -t
    echo
    set +u
}

function atop() {
    local -i NP
    # number of processes to show
    if [ $# -eq 1 ]; then
        # use argument
        NP=$1
    else
        # set manually
        NP=3
    fi

    set -u

    # set line width to one less than the terminal width
    local -i line_width=$(( $(tput cols) - 1 ))
    # user ID
    local -r U_ID_old=1111499164
    local -r U_ID=$UID
    # user name
    unset U_NAME
    unset U_NAME_trunc
    local -r U_NAME=${USER}
    decho "${U_NAME}"
    local U_NAME_trunc
    U_NAME_trunc=${U_NAME:0:7}
    decho "${U_NAME_trunc}"
    U_NAME_trunc+="+"
    decho "${U_NAME_trunc}"
    #echo "${U_NAME_trunc}" | sed "s/${U_NAME_trunc}/${U_NAME}/"

    echo -e "\033[4mTop $NP processes on ${HOST_NAME}:\x1b[0m"
    (
        ps aux --sort=-pcpu \
            | head -n $((NP+1)) \
            | sed "s/${U_ID}/\x1b\[32m${U_NAME}\x1b\[0m/" \
            | sed "s/${U_ID_old}/\x1b\[33m${U_NAME}\x1b\[0m/" \
            | awk '{printf "%-10s %5s %5s %5s %4s %5s %6s %s\n",$1,$2,$3,$4,$8,$9,$10,$11}' \
            | cut -c -$line_width \
            | sed "s/${U_NAME_trunc}/${U_NAME}/"
    ) | column -t | sed "s/${U_NAME}/${GREEN}${U_NAME}${RESET}/"

    echo
    echo -e "\033[4mTop $NP processes by ${USER}:\x1b[0m"
    (    ps ux --sort=-pcpu \
             | head -n $((NP+1)) \
             | sed "s/${U_ID_old}/${U_NAME}/" \
             | sed "s/${U_ID}/${U_NAME}/" \
             | awk '{printf "%-10s %5s %5s %5s %4s %5s %6s %s\n",$1,$2,$3,$4,$8,$9,$10,$11}' \
             | cut -c -$line_width \
             | sed "s/${U_NAME_trunc}/${U_NAME}/"
    ) | column -t

    echo
    echo -e "\033[4mLast $NP log-ins by ${USER} on ${HOST_NAME}:\x1b[0m"
    # print full user and domain neames, full login and logout times and dates,
    #   and hostname
    last -wFa | \grep ${U_NAME:0:8} | head -n $NP
    echo
    set +u
}

function td() {
    local touch_time
    local -i RETVAL
    touch_time=$(date +'empty_%Y-%m-%d-t%H:%M:%S.%3N')
    echo -en "creating empty file named ${ARG}${touch_time}${RESET}... "
    touch ${touch_time}
    RETVAL=$?
    if [[ $RETVAL == 0 ]]; then
        echo -e "${GOOD}OK${RESET}"
        echo -e "empty files in ${YELLOW}${PWD}${RESET}:"
        itab
        find . -maxdepth 1 -empty | sort -n | sed "s/^/${TAB}/" | sed "s/^.*${touch_time}/${ARG}&${RESET}/"
        dtab
    else
        echo -e "${BAD}FAIL${RESET}"
        exit ${RETVAL}
    fi
}
