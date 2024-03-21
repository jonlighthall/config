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
    # Turn in-function debugging on/off.
    # Inherit the value of funcDEBUG from shell or substitute default value if unset or NULL.
    local -i funcDEBUG=${funcDEBUG+0}
    local -i DEBUG=${DEBUG+0}    
    local CURPOS
    # get the cursor position
    echo -en "\E[6n"
    # print response
    if [ $DEBUG -eq 0 ]; then
        read -sdR CURPOS
    else
        decho -n "cursor text: '"        
        read -dR CURPOS
        decho  "'"
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
        fecho "outputing..."
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
}

function ind() {
    echo -n "hello "
    local -i x=0
    local -i y=0
    # pass variable names, not values
    get_curpos x y
    echo "x = $x"
    echo "y = $y"
}

# start a new line only if not already on a new line
# i.e., carriage return with conditional line feed
function start_new_line() {
    local -i funcDEBUG=${funcDEBUG+0}
    # get the cursor position
    local -i x
    get_curpos x
    # if the cursor is not at the start of a line, then create a new line
    if [ ${x} -gt 1 ]; then
        fecho -en "\x1b[7mNEW LINE\x1b[m"
        printf '\e[0;90;40m\u21b5\n\e[m'
    else
        fecho -en "\x1b[7mNO NEW LINE\x1b[m"
    fi
}

# print horizontal line
function hline() {
    # SYNTAX
    #   hline [length] [style]
    
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
    for ((i = 1; i <= $N; i++)); do echo -n "$m"; done
    echo
}

# print text between two bars
function bar() {
    # SYNTAX
    #   bar [length] [text]

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
function cbar() {
    # SYNTAX
    #   cbar [text]

    # get text and add whitespace
    local -r MESSAGE_IN=$(echo " $@ ")
    # remove escape characters
    local msgne=$(echo -e " $@ " | sed "s/$(echo -e "\E")[^m]*m//g")
    # get length of text
    local ln=$(for ((i = 1; i <= ${#msgne}; i++)); do echo -n "-"; done)
    # print text with TAB
    echo -e "$ln\n${MESSAGE_IN}\n$ln" | sed "s/^/${TAB}/"
}
