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
    # set "Verbose Bash" for conditional prints
    export VB=false
    # -------------------------
    # set debug level if unset
    export DEBUG=${DEBUG=0}
    # -------------------------
    # set tab
    if [ -n "${TAB:-}" ]; then
        TAB="${TAB}   "
    else
        TAB=${TAB:=$(for ((i = 1; i < ${#BASH_SOURCE[@]}; i++)); do echo -n "   "; done)}
    fi
    # print source
    if [ ${DEBUG:-0} -gt 0 ]; then
        echo -e "${TAB}\E[2m${#BASH_SOURCE[@]}: ${BASH_SOURCE##*/} -> $(readlink -f ${BASH_SOURCE})\E[22m"
    fi
    # set "Verbose Bash" for conditional prints
    #export VB=true
    # set debug level if unset
    export DEBUG=${DEBUG=0}
    # clear terminal
    clear -x
    if [ ${DEBUG} -gt 0 ]; then
        export VB=true
    fi
fi

config_dir=${HOME}/config
# load utility functions
fpretty=${config_dir}/.bashrc_pretty
if [ -e $fpretty ]; then
    if [ "${VB}" = true ]; then
        # remember, if .bashrc_pretty hasn't been loaded yet, vecho is not defined
        echo "${TAB}loading $fpretty..."
    fi
    source $fpretty
    RETVAL=$?
    if [ $RETVAL -eq 0 ]; then
        dtab
        vecho -e "${TAB}$fpretty ${GOOD}OK${RESET} ${GRAY}RETVAL=$RETVAL${RESET}"
    else
        echo -e "${TAB}$fpretty ${BAD}FAIL${RESET} ${GRAY}RETVAL=$RETVAL${RESET}"
    fi
    set -e
    set_traps
    print_ribbon
else
    echo "${TAB}$fname not found"
    set +eu
fi

if [ "${VB}" = true ]; then
    print_source
    echo -e "${TAB}SHLVL = ${BROKEN}${SHLVL}${RESET}"
    if [[ "$-" == *i* ]] && [ ${DEBUG:-0} -gt 0 ]; then
        print_stack
    fi
    decho -e "${TAB}verbose bash printing is... ${GOOD}$VB${RESET}"
fi

# system dependencies
SYS_NAME=wsl
HOST_NAME=$(hostname -s)
vecho -e "${TAB}applying ${SYS_NAME} settings on ${PSHOST}${HOST_NAME}${RESET}"

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

# print runtime duration
if [ "${VB}" = true ]; then
    # reset tab
    dtab
    # print timestamp
    print_done
    # print hidden text to force a new line before clearing screen
    vecho -e "\E[8mhello\E[28m"
else
    rtab
fi

# clear terminal
clear -x

# print welcome message
echo "${TAB}Welcome to ${HOST_NAME}"
return 0
