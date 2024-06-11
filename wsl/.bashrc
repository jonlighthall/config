#!/bin/bash -eu
# -----------------------------------------------------------------------------------------------
# SYSTEM-DEPENDENT INTERACTIVE SHELL SETTINGS for Linux Subsystem for Windows
# -----------------------------------------------------------------------------------------------
#
# ~/config/wsl/.bashrc
#
# Purpose: Load system-dependent interactive shell settings for WSL. Should be called for both
#   login and non-login interactive shell sessions.

# should be for non-login interactive shells (like when calling bash after logging in)? AND
#   should contain configurations used by every interactive shell, like aliases, shell functions,
#   and shell options (e.g. prompt) should be used for both login and non-login shells, including
#   when launching subshells.
#
# Usage: In general, ~/.bashrc is executed by bash for non-interactive subshells?? is this
#   true?. That is not what this file does or what this file is for. This file used to be linked
#   to ~/.bash_aliases to be called by interactive subshells. It is now redundant(?). The file
#   which loads interactive shell settings, ~/.bash_aliases -> ~/config/wsl/.bash_aliases, should
#   be directly loaded by ~/.bash_profile; and IS loaded directly by bash in subshells
#   sessions. Used instead of creating a custom ~/.bashrc file to preserve to contents of the
#   system-default ~/.bashrc.
#
#   This file is also called by (is?) ~/config/wsl/.bashrc to keep shell and subshell settings
#   consistient. It should be called directly by ~/.bash_profile -> ~/config/wsl/.bash_profile,
#   and ~/config/wsl/.bashrc should be deleted.
#
# Note: this file must use unix line endings (LF)!
#
# Jul 2018 JCL
#
# -----------------------------------------------------------------------------------------------

# check if running interactively
if [[ "$-" == *i* ]];then
    TAB=$(for ((i = 1; i < ${#BASH_SOURCE[@]}; i++)); do echo -n "   "; done)
    echo -e "${TAB}${BASH_SOURCE##*/}: \x1B[32minteractive shell\x1B[m" >&2
    # print source
    if [ ${DEBUG:-0} -gt 0 ]; then
        echo -e "${TAB}\E[2m${#BASH_SOURCE[@]}: ${BASH_SOURCE##*/} -> $(readlink -f ${BASH_SOURCE})\E[22m"
    fi
else
    echo "${TAB-}${BASH_SOURCE##*/}: non-interactive shell" >&2
    echo -e "${TAB-}\x1B[1;31mWARNING: ${BASH_SOURCE##*/} is intended for interactive shells only\x1B[m" >&2
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
    source $fpretty
    set -e
    set_traps
else
    set +eu
fi

# check if VB is true
if [ "${VB}" = true ]; then
    print_ribbon
    print_source
fi

if [ ${DEBUG:-0} -gt 0 ]; then
    print_stack
fi

# enable color support of ls
# define LS_COLORS using dircolors and .dircolors
vecho "${TAB}loading ls colors..."
load_colors
append_ls_colors
match_ls_colors

vecho "${TAB}running list..."
# required list
unset LIST
LIST="${config_dir}/.bashrc_common ${config_dir}/linux/.bashrc_prompt"

# get WSL version
wsl_ver=$(uname -r)
vecho "${TAB}WSL version: ${wsl_ver}"
itab
# if running WSL2, do not load X11
if [[ "${wsl_ver}" == *"WSL2" ]]; then
    vecho "${TAB}skipping X11..."
else
    vecho "${TAB}loading X11..."
    LIST+=" ${config_dir}/wsl/.bashrc_X11"
fi
dtab

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
            vecho -e "${TAB}$FILE ${GOOD}OK${RESET}"
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

unset LIST

if [ "${VB}" = true ]; then
    # reset tab
    dtab
fi
