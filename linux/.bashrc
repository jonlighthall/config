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
    # fTAB may be inherited from the environment, but itab (a function) is not
    if [ -z ${fTAB:+dummy} ] || ! declare -F itab &>/dev/null; then
        echo -e "Tabs not defined, calculating..." >&2
        # calculate number of leading tabs
        ntabs=${#BASH_SOURCE[@]}
        echo -e "Initial ntabs=$ntabs" >&2
        if [[ "${BASH_SOURCE[-1]}" == */.vscode-server* ]]; then
            echo -e "BASH_SOURCE[-1] matches .vscode-server, adjusting ntabs..." >&2
            ntabs=$((ntabs - 1))
        fi
        echo -e "ntabs=$ntabs" >&2
        TAB=$(for ((i = 1; i < ntabs; i++)); do echo -n "   "; done)
        echo -e "TAB='${TAB-}'" >&2
    else
        itab
    fi
    # print source
    echo -e "${TAB-}\x1B[7m$(readlink -f ${BASH_SOURCE})\x1B[m" >&2
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

# check if VB is unset or null (non-login shells skip .bash_profile)
if [ -z ${VB:+dummy} ]; then
    # set "Verbose Bash" default; keep in sync with .bash_profile
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

# print shell status for non-login shells; login shells print it from .bash_profile (VS Code
# terminals started with -l are not login shells, but set VSCODE_SHELL_LOGIN)
if ! shopt -q login_shell && [ -z "${VSCODE_SHELL_LOGIN:-}" ]; then
    if [ "${VB}" != true ]; then
        echo -e "${BASH_SOURCE##*/}: \x1B[32mnon-login shell\x1B[m"
    fi
    # welcome top-level shells (e.g. tmux windows), but not subshells started from bash
    if [[ "$(ps -o comm= -p $PPID)" != *bash ]]; then
        clear -x
        if [ "${VB}" = true ]; then
            # show resource hogs from all users
            systop 3
            echo
        fi
        echo "Welcome to $(hostname -s)"
    fi
fi

if [ "${VB}" = true ]; then
    # reset tab
    dtab
fi

# VS Code shell integration for non-login shells (e.g. bash started in a VS Code terminal); login
# shells load it at the end of .bash_profile. It installs a DEBUG trap and wraps PROMPT_COMMAND,
# so it must come last. If VS Code already injected it, the script returns immediately
if ! shopt -q login_shell && [[ "${TERM_PROGRAM:-}" == "vscode" ]]; then
    vsc_si=$(code --locate-shell-integration-path bash 2>/dev/null)
    [ -r "${vsc_si}" ] && . "${vsc_si}"
    unset vsc_si
fi
