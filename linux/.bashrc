#!/bin/bash -eu
# -----------------------------------------------------------------------------------------------
# SYSTEM-DEPENDENT INTERACTIVE SHELL SETTINGS for Linux
# -----------------------------------------------------------------------------------------------
#
# ~/config/linux/.bashrc
#
# Purpose: Load interactive shell settings for Linux.
#
# Feb 2017 JCL
#
# -----------------------------------------------------------------------------------------------

if [ -z ${VB:+dummy} ]; then
    export VB=false
fi

# load bash utilities
config_dir=${HOME}/config
fpretty=${config_dir}/.bashrc_pretty
if [ -e $fpretty ]; then
    source $fpretty
fi

if [ "${VB}" = true ]; then
    # determine if being sourced or executed
    if (return 0 2>/dev/null); then
        RUN_TYPE="sourcing"
    else
        RUN_TYPE="executing"
    fi
    print_source
fi

# enable color support of ls
# define LS_COLORS using dircolors and .dircolors
vecho "${TAB}loading colors..."
load_colors
append_ls_colors
match_ls_colors

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

unset command_not_found_handle

if [ "${VB}" = true ]; then
    # reset tab
    dtab
fi
