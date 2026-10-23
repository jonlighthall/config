#!/bin/bash
# -----------------------------------------------------------------------------------------------
# User-dependent LOGIN SHELL SETTINGS for Linux
# -----------------------------------------------------------------------------------------------
#
# ~/.bash_profile -> ~/config/linux/.bash_profile
#
# Purpose: execute login functions and load system-dependent interactive shell settings.
#
# Usage: Executed by bash for interactive login shell sessions.
#
# Note: this file must use Unix line endings (LF)!
#
# Feb 2017 JCL
#
# -----------------------------------------------------------------------------------------------

# VS Code runs "bash -i -l -c" without a terminal to capture the environment for its extension
# host; set up the environment quietly and skip the session-start actions (clear, login timestamp,
# process list, welcome)
if [ -n "${VSCODE_RESOLVING_ENVIRONMENT:-}" ]; then
    VB=false
    source "${HOME}/config/linux/.bashrc" &>/dev/null
    # keep shell-session variables out of the captured environment so that terminals set their
    # own (FPRETTY_LOADED is read-only, so remove the export attribute instead of unsetting)
    unset VB
    export -n DEBUG FPRETTY_LOADED TAB fTAB
    return 0
fi

# check if running interactively
if [[ "$-" == *i* ]];then
    # clear terminal
    clear

    echo -e "${TAB-}\x1B[7m$(readlink -f ${BASH_SOURCE})\x1B[m" >&2

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

# check if login shell (VS Code terminals started with -l are not, but set VSCODE_SHELL_LOGIN)
if shopt -q login_shell || [ -n "${VSCODE_SHELL_LOGIN:-}" ]; then
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

# ------------------------------------------
# set "Verbose Bash" for conditional prints
export VB=true
# ------------------------------------------

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
    print_ribbon
else
    echo "${TAB}$fpretty not found"
fi

# check if VB is true
if [ "${VB}" = true ]; then
    print_source
    if [[ "$-" == *i* ]] && [ ${DEBUG:-0} -gt 0 ]; then
        print_stack
    fi
    decho -e "${TAB}verbose bash printing is... $TRUE"
fi

# system dependencies
SYS_NAME=linux
decho "${TAB}checking host name..."
itab
if command -v hostname >/dev/null; then
    decho -n "${TAB}Debian like: "
    HOST_NAME=$(hostname -s)
else
    decho -en "${TAB}${RED}Red Hat${RESET} like: "
    HOST_NAME=$(cat /etc/hostname)
fi
decho "host name is ${HOST_NAME}"
dtab
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
            echo -e "${BAD}NOT WRITABLE${RESET}"
        else
            echo "$hist_file not writable"
        fi
    fi
else
    # not found
    if [ "${VB}" = true ]; then
        echo -e "${BAD}NOT FOUND${RESET}"
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
else
    dtab 2
fi

# clear terminal (the verbose log stays in the scrollback)
clear -x

if [ "${VB}" = true ]; then
    # show resource hogs from all users
    systop 3
    echo
fi

# print welcome message
echo "${TAB}Welcome to ${HOST_NAME}"

# VS Code shell integration (automatic injection is disabled in the VS Code settings so that
# terminals are real login shells); load it last, since it installs a DEBUG trap and wraps
# PROMPT_COMMAND. Outside a VS Code terminal, code prints an error to stdout, hence the -r check
if [[ "${TERM_PROGRAM:-}" == "vscode" ]]; then
    vsc_si=$(code --locate-shell-integration-path bash 2>/dev/null)
    [ -r "${vsc_si}" ] && . "${vsc_si}"
    unset vsc_si
fi

# deallocate variables
unset FILE_PROF
unset LIST_PROF
unset fpretty
unset hist_file

return 0
