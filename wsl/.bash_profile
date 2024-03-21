#!/bin/bash -eu
# -----------------------------------------------------------------------------------------------
# User-dependent LOGIN SHELL SETTINGS for Linux Subsystem for Windows
# -----------------------------------------------------------------------------------------------
#
# ~/.bash_profile -> ~/config/wsl/.bash_profile
#
# Purpose: execute login functions and load interactive shell settings.
#
# Usage: Executed by bash for interactive login shell sessions.
#
# Note: this file must use Unix line endings (LF)!
#
# Jul 2018 JCL
#
# -----------------------------------------------------------------------------------------------

# If not running interactively, don't do anything
if [[ "$-" != *i* ]]; then
    # turn off "Verbose Bash" conditional prints
    export VB=false
else
    # get starting time in nanoseconds
    declare -i start_time=$(date +%s%N)
    # print source
    echo -e "${TAB}\E[2m${#BASH_SOURCE[@]}: ${BASH_SOURCE##*/} -> $(readlink -f ${BASH_SOURCE})\E[22m"
    # set "Verbose Bash" for conditional prints
    export VB=true
fi

# print invoking process
called_by=$(ps -o comm= $PPID)
echo "invoked by ${called_by}"

# clear terminal
clear -x

# load utility functions
fpretty=${HOME}/config/.bashrc_pretty
if [ -e $fpretty ]; then
    if $VB; then
        # remember, if .bashrc_pretty hasn't been loaded yet, vecho is not defined
        echo "loading $fpretty..."
    fi
    source $fpretty
    set -e
    set_tab
    print_ribbon
else
    set +eu
fi    

if $VB; then
    # determine if being sourced or executed
    if (return 0 2>/dev/null); then
        RUN_TYPE="sourcing"
    else
        RUN_TYPE="executing"
    fi    
    print_source
    print_stack
    echo -e "${TAB}verbose bash printing is... ${GOOD}$VB${RESET}"
fi

# system dependencies
SYS_NAME=wsl
HOST_NAME=$(hostname -s)
vecho -e "${TAB}applying ${SYS_NAME} settings on ${PSHOST}${HOST_NAME}${RESET}"

# save login timestamp to history
hist_file=${HOME}/.bash_history
vecho -n "${TAB}appending login timestamp to $hist_file... "
if [ -f $hist_file ]; then
    echo "#$(date +'%s') LOGIN  $(date +'%a %b %d %Y %R:%S %Z') from ${HOST_NAME}" >>$hist_file
    RETVAL=$?
    if [ $RETVAL -eq 0 ]; then
        vecho -e "${GOOD}OK${RESET} ${GRAY}RETVAL=$RETVAL${RESET}"
    else
        if $VB; then
            echo -e "${BAD}FAIL${RESET} ${GRAY}RETVAL=$RETVAL${RESET}"
        else
            echo "echo to $hist_file failed"
        fi
    fi
else
    if $VB; then
        echo "${BAD}NOT FOUND{RESET}"
    else
        echo "$hist_file not found"
    fi
fi

# -------------------------------
# load interactive shell settings
# -------------------------------
# source the user's .bashrc if it exists
fname=${HOME}/config/${SYS_NAME}/.bashrc
vecho -n "${TAB}"
decho -en "\x1b[7m"
vecho -e "loading $fname... ${RESET}"
if [ -f $fname ]; then
    source $fname
    RETVAL=$?
    if [ $RETVAL -eq 0 ]; then
        vecho -e "${TAB}$fname ${GOOD}OK${RESET} ${GRAY}RETVAL=$RETVAL${RESET}"
    else
        echo -e "${TAB}$fname ${BAD}FAIL${RESET} ${GRAY}RETVAL=$RETVAL${RESET}"
    fi
else
    echo "${TAB}$fname not found"
fi
vecho
# print runtime duration
if $VB; then
    dtab
    print_done
fi

# clear terminal
clear -x

# print welcome message
echo "${TAB}Welcome to ${HOST_NAME}"
return

# test formatting
source ~/utils/bash/git/lib_git.sh
start_dir=$PWD
echo "${TAB}starting directory = ${start_dir}"
cd config
print_remotes
# return to starting directory
cd "$start_dir"
