#!/bin/bash -eu
# ------------------------------------------------------------------------------
# CONDITIONAL ECHOS LIBRARY
# ------------------------------------------------------------------------------
#
# ~/config/lib_colors.sh
#
# PURPOSE: Define conditional echos for printing debugging messages.
#
# Mar 2024 JCL
#
# ------------------------------------------------------------------------------

# Name:
#   VECHO - "verbose bash" echo
# Description:
#   Arguments are passed to ECHO based on the value of VB in the calling
#   function.
# Purpose:
#   Turn verbose printing on/off in the .bashrc logon scripts
# Globals:
#   VB
# Arguments:
#   text to display and echo flags

vecho() {
    if [ ! -z ${VB:+dummy} ] && [[ "${VB}" == true ]]; then
        # [not (unset or null)] or true -> print if true or null or unset
        echo "$@"
    fi
}

# Name:
#   FECHO - function echo
# Description:
#   Arguments are passed to ECHO based on the value of funcDEBUG in the calling
#   function.
#
#   NB: only define funcDEBUG as a local variable within FUNCTIONS
#
# Purpose:
#   Bypass, override, or augment the function decho() and the value of DEBUG.
# Globals:
#   funcDEBUG
# Arguments:
#   text to display and echo flags

function fecho() {
    # check if funcDEBUG is defined
    if [ ! -z ${funcDEBUG+dummy} ]; then
        # check if $funcDEBUG is non-zero
        if [ $funcDEBUG -gt 0 ]; then
            # get size of function stack
            local -ir N_FUNC=${#FUNCNAME[@]}

            # fecho() should only be called from other functions, so the length
            # of FUNCNAME should always be 2 or greater. Start with color 0
            local -ir idx=$(( N_FUNC - 2 ))

            # set color
            echo -ne "${TAB=}${DIM}${dcolor[$idx]}" >&2

            # select element of function stack
            local -i fidx
            # check if called from xecho
            if [[ ${FUNCNAME[1]} == "xecho" ]]; then
                fidx=2
            else

                fidx=1

                # check length of function stack
                if [ ${N_FUNC} -lt 5 ]; then
                    : #fidx=$(( $N_FUNC - 1 ))
                else
                    : #  fidx=$(( $N_FUNC - 5 ))
                    # where does this come from?
                fi
            fi

            echo -n "${FUNCNAME[fidx]}" >&2
            #           echo -n " $fidx of $N_FUNC"
            echo -ne ": \e[0m${dcolor[$idx]}" >&2
            echo "$@" | sed "s/\x1B\[0m/\x1B[0m${dcolor[$idx]}/" >&2
            echo -ne "\e[0m" >&2
        fi
    fi
    set +T
    trap - RETURN
    return 0
}

# -----------------------------------------------------------------------------
# define debug echos
# -----------------------------------------------------------------------------

# generic debug message handler
# the debugging functions should be named as follows
#   decho - level 1: informative... or print steps
#  ddecho - level 2: explicitly print values
# dddecho - level 3: why...?
# and so on
function xecho() {
    #trap -- RETURN
    # use parent function name to specify debug threshold
    local -r PARENT_FUNC=${FUNCNAME[1]}
    # strip "echo" from parent function name
    local -r PREFIX=${PARENT_FUNC%echo}
    # get length of prefix
    local -ir PREFIX_LENGTH=${#PREFIX}
    # set print threshold
    local -ir THRESHOLD=$(( PREFIX_LENGTH - 1 ))

    # if DEBUG is (unset or null) or greater than threshold
    if [ -z ${DEBUG:+dummy} ] || [ ${DEBUG:-0} -gt $THRESHOLD ]; then
        # get color index
        local -i idx
        dbg2idx $PREFIX_LENGTH idx
        # set color
        echo -ne "${dcolor[idx]}"
        # print message and unset color
        # must be included on same line to maintain formatting
        # check if argument is escaped
        if [[ "$@" =~ -[nE]*e[nE]* ]]; then
            echo "$@\e[0m" | sed "s/\x1B\[0m/\x1B[0m${dcolor[idx]}/"
        else
            echo -n "$@" | sed "s/\x1B\[0m/\x1B[0m${dcolor[idx]}/"
            echo -ne "\e[0m"
            # check if argument includes newline
            if [[ "$@" =~ -[E]*n[E]* ]]; then
                :
            else
                echo
            fi
        fi
    fi
}

# define debug echo
function decho() {
    xecho "$@"
}

function ddecho() {
    xecho "$@"
}

function dddecho() {
    xecho "$@"
}
