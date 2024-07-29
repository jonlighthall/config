#!/bin/bash -u
# -----------------------------------------------------------------------------------------------
# SYSTEM-DEPENDENT INTERACTIVE SHELL SETTINGS for Git Bash (MinGW)
# -----------------------------------------------------------------------------------------------
#
# ~/config/cygwin/.bashrc
#
# Jul 2023 JCL
#
# -----------------------------------------------------------------------------------------------

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
else
    set +u
fi

# check if VB is true
if [ "${VB}" = true ]; then
    print_ribbon
    print_source
fi

if [ ${DEBUG:-0} -gt 0 ]; then
    print_stack
fi

vecho "${TAB}running list..."
# required list
unset LIST
LIST="${config_dir}/.bashrc_common ${config_dir}/cygwin/.bashrc_prompt"

# optional list
LIST_OPT="$HOME/.bash_local"

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

unset LIST

if [ "${VB}" = true ]; then
    # reset tab
    dtab
fi
