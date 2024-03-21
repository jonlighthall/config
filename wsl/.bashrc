#!/bin/bash -eu
# -----------------------------------------------------------------------------------------------
# User-dependent INTERACTIVE SHELL SETTINGS for SUBSHELLS in Windows Subsystem for Linux
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
#   loaded by ~/.bash_profile; and IS loaded directly by bash in subshells.
#
# Note: this file must use unix line endings (LF)!
#
# Mar 2024 JCL
#
# -----------------------------------------------------------------------------------------------

# If running interactively, print source
if [[ "$-" == *i* ]]; then
    echo -e "${TAB}\E[2m${#BASH_SOURCE[@]}: ${BASH_SOURCE##*/} -> $(readlink -f ${BASH_SOURCE})\E[22m"
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

if $VB; then
    # determine if being sourced or executed
    if (return 0 2>/dev/null); then
        RUN_TYPE="sourcing"
    else
        RUN_TYPE="executing"
    fi
    set_tab
    print_source
fi

# since ~/.bashrc usually calls ~/.bash_aliases, a conditional could be added in .bash_aliases
# (linked to repo) and have all the functionality of this script, but for subshells.
echo "${TAB}BASH_SUBSHELL = $BASH_SUBSHELL"
print_stack

# (un)set traps and shell options before loading command files
set +e
unset_traps

falias=${HOME}/config/wsl/.bash_aliases
if [ -e $falias ]; then
	  source $falias
fi
