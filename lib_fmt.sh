#!/bin/bash -u
# -----------------------------------------------------------------------------
# FORMAT LIBRARY
# -----------------------------------------------------------------------------
#
# ~/config/lib_fmt.sh
#
# Purpose: define functions for "pretty-print" text formatting.
#
# Mar 2024 JCL
#
# -----------------------------------------------------------------------------

function get_curpos() {
    # Turn in-function debugging on/off.
    local -i DEBUG=${DEBUG:-0}
    local -i funcDEBUG=0
    # manual
    #DEBUG=3

    # first check if output is being redirected
    if [ ! -t 1 ] ; then
        # stdout isn't a terminal
        fecho "${FUNCNAME}: output is NOT terminal" >&2
        fecho "...then how are you seeing this?" >&2

        # output values to parent
        if [ $# -gt 0 ]; then
            local -n x_out=$1
            x_out=0
        fi

        if [ $# -gt 1 ]; then
            local -n y_out=$2
            y_out=0
        fi
        fecho "goodbye..." >&2
        dtab
        return 0
    fi
    # check if running on HPC
    if command -v sbatch &>/dev/null; then
        fecho -e "${ORANGE}\E[7mHPC${RESET}" >&2
        ddecho ${TAB}${FUNCNAME[@]}
        dtab
        return 0
    fi
    # must get position before any prints
    local CURPOS
    # get the cursor position
    echo -en "\E[6n"
    local -i THRESHOLD=2
    if [ $DEBUG -gt $THRESHOLD ]; then
        # print response
        # the cursor position end in 'R'
        # set delimiter to 'R'
        read -dR CURPOS
    else
        # silence response
        # add '-s'
        read -sdR CURPOS
    fi
    # print function name
    dddecho -e "${TAB}${INVERT}${FUNCNAME}${RESET}"

	  # parse the cursor position
	  local -r pair=${CURPOS#*[}
          #}# dummy bracket for emacs indenting
	  local -ir cursor_x_position=${pair#*;}
	  local -ir cursor_y_position=${pair%;*}

    # print response
    itab

    if [ $DEBUG -gt $THRESHOLD ]; then
        ddecho -n "${TAB}cursor text: '"
        echo -n "^[[${pair}R"
        ddecho "'"
    fi

	  # print the cursor position
	  fecho -e "cursor position: x=$cursor_x_position y=$cursor_y_position"

    fecho "number of args: $#"
    # output values to parent   
    if [ $# -gt 0 ]; then
        fecho "   arg 1 : $1=${!1-UNSET}"        
        fecho "outputing..."
        local x_out=$1
        if [ -n "${x_out+dummy}" ]; then
            fecho "   arg 1 in : $x_out=${!x_out-UNSET}"
        fi
        fecho "   arg 1 out: ${x_out}=${cursor_x_position}"
        eval $x_out=$cursor_x_position        
    fi

    if [ $# -gt 1 ]; then
        fecho "   arg 2 : $2=${!2}"
        local y_out=$2
        if [ -n "${y_out+dummy}" ]; then
            fecho "   arg 2 in : $y_out=${!y_out}"
        fi
        fecho "   arg 2 out: ${y_out}=${cursor_y_position}"
        eval $y_out=$cursor_y_position        
    fi
    dtab
}

# start a new line only if not already on a new line
# i.e., carriage return with conditional line feed
function start_new_line() {
    local -i DEBUG=${DEBUG:-0}
    # Turn in-function debugging on/off.
    #DEBUG=1

    dddecho -en "${TAB-}${INVERT}${FUNCNAME}${RESET}" >&2
    if [ $DEBUG -ge 3 ]; then
        local -i funcDEBUG=1
    else
        local -i funcDEBUG=0
    fi
    # get the cursor position
    local -i x=0
    itab
    get_curpos x

    [ ${x} -gt 1 ] && ddecho -en "${GRAY}$x${RESET}" >&2
    dtab
    if [ ${x} -eq 0 ]; then
        return 0
    fi
    # if the cursor is not at the start of a line, then create a new line
    if [ ${x} -gt 1 ]; then
        fecho -en "\x1b[7mNEW LINE\x1b[m"
        if [ $DEBUG -gt 0 ]; then
            printf '\e[0;90;40m\u21b5\e[m' >&2
        fi
        echo
    else
        fecho -en "\x1b[7mNO NEW LINE\x1b[m"
    fi
}

# print horizontal line
# SYNTAX
#   hline [length] [style]
function hline() {
    local -i DEBUG=${DEBUG:-0}
    # Turn in-function debugging on/off.
    DEBUG=0

    dddecho -e "${TAB}${INVERT}${FUNCNAME}${RESET}"

    # number of characters in line (length)
    local -i N
    if [ "$#" -lt 1 ]; then
        N=38
    else
        N=$1
    fi
    # marker used to create line (style)
    local m
    if [ "$#" -lt 2 ]; then
        m="-"
    else
        m=$2
    fi
    local -i i
    echo -ne "${TAB}"
    for ((i = 1; i <= $N; i++)); do echo -en "$m"; done
    echo -e ${RESET}
}

# print text between two bars
# SYNTAX
#   bar [length] [text]
function bar() {
    dddecho -e "${TAB}${INVERT}${FUNCNAME}${RESET}"
    # number of characters in line (length)
    local -i N
    # text to print between bars
    local TXT
    if [ "$#" -lt 2 ]; then
        N=69
        if [ "$#" -lt 1 ]; then
            TXT=''
        else
            TXT=$1
        fi
    else
        N=$1
        TXT=$2
    fi
    hline $N
    echo "$TXT" | sed "s/^/${TAB}/"
    hline $N
}

# define centered bar print
# SYNTAX
#   cbar [text]
function cbar() {
    dddecho -e "${TAB}${INVERT}${FUNCNAME}${RESET}"
    # get text and add whitespace
    local -r MESSAGE_IN=$(echo " $@ ")
    # remove escape characters
    local msgne=$(echo -e " $@ " | sed "s/$(echo -e "\E")[^m]*m//g")
    # get length of text
    local ln=$(for ((i = 1; i <= ${#msgne}; i++)); do echo -n "-"; done)
    # print text with TAB
                    echo -e "$ln\n${MESSAGE_IN}\n$ln" | sed "s/^/${TAB}/"
                }
