#!/bin/bash -eu
# -----------------------------------------------------------------------------------------------
# User-dependent LOGIN SHELL SETTINGS for Windows Subsystem for Linux
# -----------------------------------------------------------------------------------------------
#
# ~/.bash_profile -> ~/config/wsl/.bash_profile
#
# Purpose: execute login functions and load system-dependent interactive shell settings.
#
# Usage: Executed by bash for interactive login shell sessions.
#
# Note: this file must use Unix line endings (LF)!
#
# Jul 2018 JCL
#
# -----------------------------------------------------------------------------------------------

# check if running interactively
if [[ "$-" == *i* ]];then
    # clear terminal
    clear

    # get starting time in nanoseconds
    declare -i start_time=$(date +%s%N)

    # set tab
    TAB=$(for ((i = 1; i < ${#BASH_SOURCE[@]}; i++)); do echo -n "   "; done)

    # -------------------------
    # set debug level if unset
    export DEBUG=${DEBUG=0}
    # -------------------------

    if [ ${DEBUG:-0} -gt 0 ]; then
        echo -e "${TAB}${BASH_SOURCE##*/}: \x1B[32minteractive shell\x1B[m" >&2
    fi
else
    echo "${TAB-}${BASH_SOURCE##*/}: non-interactive shell" >&2
    echo -e "${TAB-}\x1B[1;31mWARNING: ${BASH_SOURCE##*/} is intended for interactive shells only\x1B[m" >&2
    echo -e "${TAB-}returning..." >&2
    # If not running interactively, don't do anything
    return
fi

# check if login shell
if shopt -q login_shell; then
    echo -e "${TAB-}${BASH_SOURCE##*/}: \x1B[32mlogin shell\x1B[m" >&2
else
    echo "${TAB-}${BASH_SOURCE##*/}: non-login shell" >&2
    echo -e "${TAB-}\x1B[;31mWARNING: ${BASH_SOURCE##*/} is intended for login-shells only\x1B[m" >&2
fi

# print source
if [ ${DEBUG:-0} -gt 0 ]; then
    echo -e "${TAB:=$(for ((i = 1; i < ${#BASH_SOURCE[@]}; i++)); do echo -n "   "; done)}\E[2m${#BASH_SOURCE[@]}: ${BASH_SOURCE##*/} -> $(readlink -f ${BASH_SOURCE})\E[22m"
    # print invoking process
    called_by=$(ps -o comm= $PPID)
    echo "${TAB}invoked by ${called_by}"
fi
# set "Verbose Bash" for conditional prints
export VB=true
# clear terminal
clear -x
if [ ${DEBUG} -gt 0 ]; then
    export VB=true
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
    RETVAL=$?
    if [ $RETVAL -eq 0 ]; then
        dtab
        vecho -e "${TAB}$fpretty ${GOOD}OK${RESET}"
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

# check if VB is true
if [ "${VB}" = true ]; then
    print_source
    if [[ "$-" == "*i*" ]] && [ ${DEBUG:-0} -gt 0 ]; then
        print_stack
    fi
    decho -e "${TAB}verbose bash printing is... $TRUE"
fi

# system dependencies
SYS_NAME=wsl
HOST_NAME=$(hostname -s)
vecho -e "${TAB}applying ${SYS_NAME} settings on ${PSHOST}${HOST_NAME}${RESET}"

# save login timestamp to history
hist_file=${HOME}/.bash_history
hist_file_can=$(readlink -f "${hist_file}")
vecho -en "${TAB}appending login timestamp to ${YELLOW}${hist_file_can##*/}${RESET}... "
# check if hist_file exists
if [ -e $hist_file ]; then
    # check if hist_file is writable
    if [ -w $hist_file ]; then
        echo "#$(date +'%s') LOGIN  $(date +'%a %b %d %Y %R:%S %Z') from ${HOST_NAME}" >>$hist_file
        RETVAL=$?
        if [ $RETVAL -eq 0 ]; then
            vecho -e "${GOOD}OK${RESET}"
        else
            if [ "${VB}" = true ]; then
                echo -e "${BAD}FAIL${RESET} ${GRAY}RETVAL=$RETVAL${RESET}"
            else
                echo "echo to $hist_file failed"
            fi
        fi
    else
        # found, not writable
        if [ "${VB}" = true ]; then
            echo "${BAD}NOT WRITABLE{RESET}"
        else
            echo "$hist_file not writable"            
        fi
    fi
else
    # not found
    if [ "${VB}" = true ]; then
        echo "${BAD}NOT FOUND{RESET}"
    else
        echo "$hist_file not found"
    fi
fi

# load system-dependent interactive shell settings
declare -ax LIST_PROF
LIST_PROF=("${config_dir}/${SYS_NAME}/.bashrc")
for FILE_PROF in $LIST_PROF; do
    vecho -e "${TAB}loading $FILE_PROF... ${RESET}"
    if [ -f $FILE_PROF ]; then
        source $FILE_PROF
        RETVAL=$?
        if [ $RETVAL -eq 0 ]; then
            vecho -e "${TAB}$FILE_PROF ${GOOD}OK${RESET}"
        else
            echo -e "${TAB}$FILE_PROF ${BAD}FAIL${RESET} ${GRAY}RETVAL=$RETVAL${RESET}"
        fi
    else
        echo "${TAB}$FILE_PROF not found"
    fi
done

# print runtime duration
if [ "${VB}" = true ]; then
    # reset tab
    dtab
    # print timestamp
    print_done
    # print hidden text to force a new line before clearing screen
    vecho -e "\E[8mhello\E[28m"
fi

# clear terminal
clear -x

# print welcome message
echo "${TAB}Welcome to ${HOST_NAME}"

# deallocate variables
unset FILE_PROF
unset LIST_PROF
unset fpretty
unset hist_file

return 0

# test formatting
source ~/utils/bash/git/lib_git.sh
start_dir=$PWD
echo "${TAB}starting directory = ${start_dir}"
cd ${config_dir}
print_remotes
# return to starting directory
cd "$start_dir"
