#!/bin/bash -eu
# -----------------------------------------------------------------------------------------------
# User-dependent INTERACTIVE SHELL SETTINGS for SUBSHELLS in Windows Subsystem for Linux
# -----------------------------------------------------------------------------------------------
#
# ~/.bash_aliases -> ~/config/wsl/.bash_aliases
#
# Purpose: Load system-dependent interactive shell settings when running interactive
#   subshells. This is the subshell counterpart to ~/.bash_profile -> ~/config/wsl/.bash_profile
#   and mimics its non-login behavior.
#
# Usage: Executed by bash (in ~/.bashrc) for interactive subshells. 
#
# Note: this file must use Unix line endings (LF)!
#
# Mar 2024 JCL
#
# -----------------------------------------------------------------------------------------------

# If running interactively, print source
if [[ "$-" == *i* ]]; then
    # get starting time in nanoseconds
    declare -i start_time=$(date +%s%N)
    # print source
    echo -e "${TAB}\E[2m${#BASH_SOURCE[@]}: ${BASH_SOURCE##*/} -> $(readlink -f ${BASH_SOURCE})\E[22m"
    # set "Verbose Bash" for conditional prints
    export VB=true
fi

# clear terminal
clear -x

config_dir=${HOME}/config
# load utility functions
fpretty=${config_dir}/.bashrc_pretty
if [ -e $fpretty ]; then
    if $VB; then
        # remember, if .bashrc_pretty hasn't been loaded yet, vecho is not defined
        echo "loading $fpretty..."
    fi
    source $fpretty
    set -e
    print_ribbon
	  set_traps
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
    echo -e "${TAB}SHLVL = $BROKEN$SHLVL$RESET"
    print_stack
    echo -e "${TAB}verbose bash printing is... ${GOOD}$VB${RESET}"
fi

# system dependencies
SYS_NAME=wsl
HOST_NAME=$(hostname -s)
vecho -e "${TAB}applying ${SYS_NAME} settings on ${PSHOST}${HOST_NAME}${RESET}"

# (un)set traps and shell options before loading command files
set +e
unset_traps

# load system-dependent interactive shell settings
fname=${config_dir}/${SYS_NAME}/.bashrc
vecho -e "${TAB}loading $fname... ${RESET}"
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
