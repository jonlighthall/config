#!/bin/bash -u
# -----------------------------------------------------------------------------------------------
# FORMAT LIBRARY
# -----------------------------------------------------------------------------------------------
#
# ~/config/lib_fmt.sh
#
# Purpose: define functions for "pretty-print" text formatting.
#
# -----------------------------------------------------------------------------------------------

function get_curpos() {
    # print function name
    dddecho -e "${TAB}${INVERT}${FUNCNAME}${RESET}"

    # Turn in-function debugging on/off.
    local -i DEBUG=${DEBUG:-0}
    local -i funcDEBUG=0

    local CURPOS
    itab
    local -i THRESHOLD=2
    if [ $DEBUG -gt $THRESHOLD ]; then
        ddecho -n "${TAB}cursor text: '"
    fi
    # get the cursor position
    echo -en "\E[6n"
    # print response
    if [ $DEBUG -gt $THRESHOLD ]; then
        read -dR CURPOS
        ddecho  "'"
    else
        read -sdR CURPOS
    fi

	  # parse the cursor position
	  local -r pair=${CURPOS#*[}
          #}# dummy bracket for emacs indenting
	  local -ir cursor_x_position=${pair#*;}
	  local -ir cursor_y_position=${pair%;*}

	  # print the cursor position
	  fecho -e "cursor position: x=$cursor_x_position y=$cursor_y_position"

    fecho "number of args: $#"
    
    # output values to parent   
    if [ $# -gt 0 ]; then
        fecho "outputting..."
        local -n x_out=$1
        if [ -n "${x_out+dummy}" ]; then
            fecho "   arg 1 in : ${!x_out}=$x_out"
        fi
        fecho "   arg 1 out: ${!x_out}=${cursor_x_position}"
        x_out=$cursor_x_position        
    fi

    if [ $# -gt 1 ]; then
        local -n y_out=$2
        if [ -n "${y_out+dummy}" ]; then
            fecho "   arg 2 in : ${!y_out}=$y_out"
        fi
        fecho "   arg 2 out: ${!y_out}=${cursor_y_position}"
        y_out=$cursor_y_position        
    fi
    dtab
}

function ind() {
    ddecho -e "${TAB}${INVERT}${FUNCNAME}${RESET}"        
    local -i DEBUG=1
    local -i funcDEBUG=1
    
    local -i x1=0
    local -i y1=0
    local -i x2=0
    local -i y1=0

    echo -n "indented: "
    # pass variable names, not values
    get_curpos x1 y1
    echo "x = $x1"
    echo "y = $y1"
    echo "BASH_LINENO = ${BASH_LINENO[@]}"

    echo "not indented: "
    get_curpos x2 y2
    echo "x = $x2"
    echo "y = $y2"
    echo "BASH_LINENO = ${BASH_LINENO[@]}"

    if [ $x1 = $x2 ]; then
        echo "x-position did not change"
    fi
    
    if [ $y1 = $y2 ]; then
        echo "y-position did not change"
    fi

    if [ $x1 = $x2 ] && [ $y1 = $y2 ]; then         
        echo "cursor position did not change"
    fi   
}

# start a new line only if not already on a new line
# i.e., carriage return with conditional line feed
function start_new_line() {
    dddecho -e "${TAB}${INVERT}${FUNCNAME}${RESET}"        
    if [ $DEBUG -ge 3 ]; then 
        local -i funcDEBUG=1
    else
        local -i funcDEBUG=0
    fi
    # get the cursor position
    local -i x
    itab
    get_curpos x
    # if the cursor is not at the start of a line, then create a new line
    if [ ${x} -gt 1 ]; then
        fecho -en "\x1b[7mNEW LINE\x1b[m"
        if [ $DEBUG -gt 0 ]; then 
            printf '\e[0;90;40m\u21b5\n\e[m'
        fi
    else
        fecho -en "\x1b[7mNO NEW LINE\x1b[m"
    fi
    dtab
}

# print horizontal line
# SYNTAX
#   hline [length] [style]
function hline() {
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
    for ((i = 1; i <= $N; i++)); do echo -n "$m"; done
    echo
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
        TXT=$1
    else
        N=$1
        TXT=$2
    fi
    hline $N
    echo "$TXT"
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

function strip_pretty() {
    local -n output=$1
    shift

    # strip escapes    
    output=$(echo -e "$@" | sed "s/$(echo -e "\E")[^m]*m//g")
}
