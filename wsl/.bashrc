#!/bin/bash -eu
# -----------------------------------------------------------------------------------------------
# SYSTEM-DEPENDENT INTERACTIVE SHELL SETTINGS for Linux Subsystem for Windows
# -----------------------------------------------------------------------------------------------
#
# ~/config/wsl/.bashrc
#
# Purpose: Load user-dependent interactive shell settings when launching subshells. 
#
# Usage: In general, ~/.bashrc is executed by bash for non-interactive subshells. That is not
#   what this file does or what this file is for. This file used to be linked to ~/.bash_aliases
#   to be called by interactive subshells. It is now redundant(?). The file which loads
#   interactive shell settings, ~/.bash_aliases -> ~/config/wsl/.bash_aliases, should be directed
#   loaded by ~/.bash_profile; and IS loaded directly by bash in subshells.  sessions. Used instead of creating a custom ~/.bashrc file to preserve to
#   contents of the system-default ~/.bashrc.
#This file is also
#   called by ~/config/wsl/.bashrc to keep shell and subshell settings consistient. It should be
#   called directly by ~/.bash_profile -> ~/config/wsl/.bash_profile, and ~/config/wsl/.bashrc
#   should be deleted.
#
# Note: this file must use unix line endings (LF)!
#
# Jul 2018 JCL
#
# -----------------------------------------------------------------------------------------------

# If running interactively, print source
if [[ "$-" == *i* ]] && [ ${DEBUG:-0} -gt 0 ]; then
    echo -e "${TAB:=$(for ((i = 1; i < ${#BASH_SOURCE[@]}; i++)); do echo -n "   "; done)}\E[2m${#BASH_SOURCE[@]}: ${BASH_SOURCE##*/} -> $(readlink -f ${BASH_SOURCE})\E[22m"
fi

# load bash utilities
fpretty=${HOME}/config/.bashrc_pretty
if [ -e $fpretty ]; then
	  source $fpretty
	  set -e  
	  set_traps
else
    set +eu
fi

if [ "${VB}" = true ]; then
    # determine if being sourced or executed
    if (return 0 2>/dev/null); then
        RUN_TYPE="sourcing"
    else
        RUN_TYPE="executing"
    fi
    print_ribbon
    print_source
fi

if [[ "$-" == *i* ]] && [ ${DEBUG:-0} -gt 0 ]; then
    print_stack
fi

vecho "${TAB}running list..."
# required list
unset LIST
config_dir=${HOME}/config
LIST+="${config_dir}/.bashrc_common ${config_dir}/linux/.bashrc_prompt ${config_dir}/wsl/.bashrc_X11"

# optional list
LIST_OPT="$HOME/.bash_local root_v5.34.36/bin/thisroot.sh"

# add optional list to required list if targets exist
for FILE in $LIST_OPT; do
    if [ -f $FILE ]; then
        LIST+=" $FILE"
    else
        vecho -e "${TAB}$FILE ${UL}not found${RESET}"
    fi
done

# (un)set traps and shell options before loading command files
set +e
unset_traps

# source list of files
for FILE in $LIST; do
    vecho "${TAB}loading $FILE..."
    if [ -f $FILE ]; then
        source $FILE
        RETVAL=$?
        if [ $RETVAL -eq 0 ]; then
            vecho -e "${TAB}$FILE ${GOOD}OK${RESET} ${GRAY}RETVAL=$RETVAL${RESET}"
        else
            echo -e "${TAB}$FILE ${BAD}FAIL${RESET} ${GRAY}RETVAL=$RETVAL${RESET}"
        fi
    else
        echo -e "${TAB}$FILE ${UL}not found${RESET}"
    fi
done

# ROOT
if [[ "$LIST" == *"thisroot.sh"* ]]; then
    which root
fi

if [ "${VB}" = true ]; then
    # reset tab
    dtab
fi
