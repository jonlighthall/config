#!/bin/bash -eu
# -----------------------------------------------------------------------------------------------
# SYSTEM-DEPENDENT INTERACTIVE SHELL SETTINGS for Linux
# -----------------------------------------------------------------------------------------------
#
# ~/config/linux/.bashrc
#
# Purpose: Load system-dependent interactive shell settings for Linux. Should be called for both
#   login and non-login interactive shell sessions.
#
# Note: System-independent interactive shell settings are located in ~/config/.bashrc_common
#
# Feb 2017 JCL
#
# -----------------------------------------------------------------------------------------------

# check if running interactively
if [[ "$-" == *i* ]];then
    if [ -z ${fTAB:+dummy} ]; then
        TAB=$(for ((i = 1; i < ${#BASH_SOURCE[@]}; i++)); do echo -n "   "; done)
    else
        itab
    fi    
    # print source
    if [ ${DEBUG:-0} -gt 0 ]; then
        echo -e "${TAB}${BASH_SOURCE##*/}: \x1B[32minteractive shell\x1B[m" >&2
        echo -e "${TAB}\E[2m${#BASH_SOURCE[@]}: ${BASH_SOURCE##*/} -> $(readlink -f ${BASH_SOURCE})\E[22m"
    fi
else
    echo "${TAB-}${BASH_SOURCE##*/}: non-interactive shell" >&2
    echo -en "${TAB-}\x1B[1;31m intended for interactive shells only\x1B[m " >&2
    echo -e "${TAB-}returning..." >&2
    # If not running interactively, don't do anything
    return
fi

# check if VB is unset or null
if [ -z ${VB:+dummy} ]; then
    export VB=false
fi

# load bash utilities
config_dir=${HOME}/config
fpretty=${config_dir}/.bashrc_pretty
if [ -e $fpretty ]; then
    if [ "${VB}" = true ]; then
        # remember, if .bashrc_pretty hasn't been loaded yet, vecho is not defined
        echo "${TAB-}loading $fpretty..."
    fi
    source $fpretty
fi

# check if VB is true
if [ "${VB}" = true ]; then
    print_ribbon
    print_source
fi

vecho "${TAB}running list..."
# required list
unset LIST
LIST="/etc/bashrc ${config_dir}/.bashrc_common ${config_dir}/linux/.bashrc_prompt"

# optional list
LIST_OPT="$HOME/.bash_local $HOME/.bash_aliases ${config_dir}/linux/.bashrc_X11 ${config_dir}/linux/.bashrc_libgfortran"

# add optional list to required list if targets exist
for FILE in $LIST_OPT; do
    if [ -f $FILE ]; then
        LIST+=" $FILE"
    else
        vecho -e "${TAB}$FILE ${UL}not found${RESET}"
    fi
done

# source list of files
for fname in $LIST; do
    vecho "${TAB}loading $fname..."
    if [ -f $fname ]; then
        source $fname
        RETVAL=$?
        if [ $RETVAL -eq 0 ]; then
            vecho -e "${TAB}$fname ${GOOD}OK${RESET}"
        else
            echo -e "${TAB}$fname ${BAD}FAIL${RESET} ${GRAY}RETVAL=$RETVAL${RESET}"
        fi
    else
        echo -e "${TAB}$fname ${UL}not found${RESET}"
    fi
done

unset command_not_found_handle

if [ "${VB}" = true ]; then
    # reset tab
    dtab
fi
