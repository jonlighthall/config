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
for FILE_RC in $LIST; do
    vecho "${TAB}loading $FILE_RC..."
    if [ -f $FILE_RC ]; then
        source $FILE_RC
        RETVAL=$?
        if [ $RETVAL -eq 0 ]; then
            vecho -e "${TAB}$FILE_RC ${GOOD}OK${RESET}"
        else
            echo -e "${TAB}$FILE_RC ${BAD}FAIL${RESET} ${GRAY}RETVAL=$RETVAL${RESET}"
        fi
    else
        echo -e "${TAB}$FILE_RC ${UL}not found${RESET}"
    fi
done

# Path additions
# ---------------
if [ "${VB}" = true ]; then
    echo "${TAB}Path additions..."
    itab
fi

# list paths to append to PATH
appPATH=""
if [ ! -z ${appPATH} ]; then
    for path in $appPATH; do
        if [[ ":$PATH" != *":${path}"* ]]; then
            if [ -d "${path}" ]; then
                vecho -n "${TAB} appending ${path} to PATH... "
                export PATH=$PATH:$path
                vecho "done"
            else
                echo -e "${TAB}${path} ${UL}not found${RESET}"
            fi
        else
            vecho "${TAB}${path} already in PATH"
        fi
    done
fi

# list paths to prepend to PATH
#prePATH="c/Users/jlighthall/Downloads\emacs-27.1-x86_64\bin"
prePATH="${HOME}/Downloads/emacs-27.1-x86_64/bin"
if [ ! -z "${prePATH}" ]; then
    for path in $prePATH; do
        if [[ ":$PATH" != *":${path}"* ]]; then
            if [ -d "${path}" ]; then
                vecho -n "${TAB}prepending ${path} to PATH... "
                export PATH=$path:$PATH
                vecho "done"
            else
                echo -e "${TAB}${path} ${UL}not found${RESET}"
            fi
        else
            vecho "${TAB}${path} already in PATH"
        fi
    done
fi

# deallocate variables
unset LIST
if [ "${VB}" = true ]; then
    # reset tab
    dtab 2
fi
