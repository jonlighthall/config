#!/bin/bash -u
# -----------------------------------------------------------------------------------------------
# FORMAT LIBRARY
# -----------------------------------------------------------------------------------------------
#
# ~/config/lib_fmt.sh
#
# Purpose: define functions for "pretty-print" text formatting.
#
# Mar 2024 JCL
#
# -----------------------------------------------------------------------------------------------

function get_curpos() {
    # Turn in-function debugging on/off.
    local -i DEBUG=${DEBUG:-0}
    DEBUG=0
    local -i funcDEBUG=0

    # print function name
    dddecho -e "${TAB}${INVERT}${FUNCNAME}${RESET}"

    local CURPOS
    itab
    local -i THRESHOLD=2
    if [ $DEBUG -gt $THRESHOLD ]; then
        ddecho -n "${TAB}cursor text: '"
    fi

    # check if output is being redirected
    if [ ! -t 1 ] ; then
        # stdout isn't a terminal
        echo "${FUNCNAME}: output is NOT terminal"
        echo "...then how are you seeing this?"

        # output values to parent
        if [ $# -gt 0 ]; then
            local -n x_out=$1
            x_out=0
        fi

        if [ $# -gt 1 ]; then
            local -n y_out=$2
            y_out=0
        fi
        echo "goodby..."
        dtab
        return 0
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

# test cursor position
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
    local -i DEBUG=${DEBUG:-0}
    # Turn in-function debugging on/off.
    DEBUG=0

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
    dtab
    if [ ${x} -eq 0 ]; then
        return 0
    fi
    # if the cursor is not at the start of a line, then create a new line
    if [ ${x} -gt 1 ]; then
        fecho -en "\x1b[7mNEW LINE\x1b[m"
        if [ $DEBUG -gt 0 ]; then
            printf '\e[0;90;40m\u21b5\n\e[m'
        fi
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

function strip_pretty() {
    local -n output=$1
    shift

    # strip escapes
    output=$(echo -e "$@" | sed "s/$(echo -e "\E")[^m]*m//g")
}

# -----------------------------------------------------------------------------------------------
# Functions to colorize and indent command output
# -----------------------------------------------------------------------------------------------

# format command output
# handling is included for a variety of commands
# conditionally calls do_cmd_script, and do_cmd_stdbuf

export FMT_COLOR=0

function do_cmd() {
    local -i DEBUG=0
    # save command as variable
    cmd=$(echo $@)
    # format output
    itab
    if [ $DEBUG -gt 0 ]; then
        start_new_line
    fi
    decho "${TAB}running command $cmd... "

    # get color index
    local -i idx
    dbg2idx $FMT_COLOR idx
    # set color
    echo -ne "${dcolor[$idx]}"

    # the ideal solution is to use unbuffer
    # check if unbuffer is defined
    if command -v unbuffer >/dev/null; then
        # check cursor position
        local -i x1c
        get_curpos x1c
        decho -n "$x1c"
        # set the "carriage return" value for the first non-empty line of the command ouput
        if [ $x1c -gt 1 ]; then
            # if the cursor is not at the start of a line, start a new line
            local cr='\n'
            if [ $DEBUG -gt 0 ]; then
                cr="$(printf '\u21b5')\n"
            fi
        else
            # if the cursor is already  at the start of a new line, do nothing
            local cr=''
        fi

        ddecho "${TAB}printing unbuffered command ouput..."
        # set shell options
        set -o pipefail
        # define highlight color
        local -i idx2
        dbg2idx $((idx+1)) idx2        
        # print unbuffered command output        
        unbuffer $cmd \
            | sed -u "s/\r$//g;s/.*\r/${TAB}/g;s/^/${TAB}/" \
            | sed -u "/^[^%|]*|/s/^/${dcolor[$idx2]}/g; s/$/${dcolor[$idx]}/; /|/s/+/${GOOD}&/g; /|/s/-/${BAD}&/g; /modified:/s/^.*$/${BAD}&/g; /^\s*M\s/s/^.*$/${BAD}&/g" \
            | sed -u "1 s/^[\s]*[^\s]/${cr}&/"
        local -i RETVAL=$?

        # reset shell options
        set +o pipefail
        dtab
    else
        # check if script is defined
        if command -v script >/dev/null; then
            ddecho "${TAB}printing command ouput typescript..."
            # print typescript command ouput
            dtab
            do_cmd_script $cmd
        else
            ddecho "${TAB}printing buffered command ouput..."
            # print buffered command output
            dtab
            do_cmd_stdbuf $cmd
        fi
        local -i RETVAL=$?
    fi

    # reset formatting
    unset_color
    dtab
    if [ $DEBUG -gt 0 ]; then
        dtab
    fi
    return $RETVAL
}

# format command output using typescript
# developed for use with the command "git gc"
# output of "git gc" is written to stderr, but not to the terminal (tty)
# script captures all output in a pseudo-terminal (pty)
function do_cmd_script() {
    local -i DEBUG=0
    # save command as variable
    cmd=$(echo $@)
    # format output
    itab
    if [ $DEBUG -gt 0 ]; then
        start_new_line
    fi
    decho "${TAB}SCRIPT: running command $cmd... "

    # get color index
    local -i idx
    dbg2idx $FMT_COLOR idx
    # set color
    echo -ne "${dcolor[$idx]}"

    # check if typescript is defined
    if command -v script >/dev/null; then
        # check cursor position
        local -i x1c
        get_curpos x1c
        ddecho -n "$x1c"
        # set the "carriage return" value for the first non-empty line of the command ouput
        if [ $x1c -gt 1 ]; then
            # if the cursor is not at the start of a line, start a new line
            local cr='\n'
            if [ $DEBUG -gt 0 ]; then
                cr="new line$(printf '\u21b5')\n"
            fi
        else
            # if the cursor is already  at the start of a new line, do nothing
            local cr=''
        fi

        ddecho "${TAB}printing command ouput typescript..."
        # set shell options
        set -o pipefail
        # print command output
        if false; then
            # command output is unbuffered only if "sed -u" is used!
            # however, this interfers with formatting the output
            script -eq -c "$cmd" \
                | sed -u 's/$\r/\n\r/g'
        else
            script -eq -c "$cmd" \
                | sed "s/\r.*//g;s/.*\r//g" \
                | sed 's/^[[:space:]].*//g' \
                | sed "/^$/d;s/^/${TAB}${dcolor[$idx]}/" \
                | sed "1 s/^[\s]*[^\s]/${cr}&/"
        fi
        local -i RETVAL=$?
        # reset shell options
        set +o pipefail
        # remove temporary file
        local temp_file=typescript
        if [ -f $temp_file ]; then
            rm typescript
        fi
    else
        ddecho "${TAB}printing unformatted ouput..."
        dbg2idx $FMT_COLOR idx
        # set color
        echo -ne "${dcolor[$idx]}"

        dtab
        # print buffered command output
        $cmd
        local -i RETVAL=$?
    fi

    # reset formatting
    unset_color
    dtab
    return $RETVAL
}

# format buffered command ouput
# save ouput to file, print file, delete file
# developed for use when unbuffer and script are unavailable
function do_cmd_stdbuf() {
    # save command as variable
    cmd=$(echo $@)
    # format output
    itab
    if [ $DEBUG -gt 0 ]; then
        start_new_line
    fi

    # get color index
    local -i idx
    dbg2idx $FMT_COLOR idx
    # set color
    echo -ne "${dcolor[$idx]}"

    # define temp file
    temp_file=temp
    ddecho "${TAB}STDBUF: redirecting command ouput to $temp_file..."

    # unbuffer command output and save to file
    stdbuf -i0 -o0 -e0 $cmd &>$temp_file
    RETVAL=$?

    # check if ouputfile is empty
    if [ -s ${temp_file} ]; then
        # check cursor position
        local -i x1c
        get_curpos x1c
        echo -ne "${dcolor[$idx]}"
        # set the "carriage return" value for the first non-empty line of the command ouput
        if [ $x1c -gt 1 ]; then
            # if the cursor is not at the start of a line, start a new line
            local cr='\n'
            if [ $DEBUG -gt 0 ]; then
                cr="$(printf '\u21b5')\n"
            fi
        else
            # if the cursor is already  at the start of a new line, do nothing
            local cr=''
        fi

        ddecho -e "${TAB}${IT}buffer:${NORMAL}"
        # define highlight color
        local -i idx3
        dbg2idx $((idx+1)) idx3
        # print command output
        \cat $temp_file \
            | sed -u "s/\r$//g;s/.*\r/${TAB}/g;s/^/${TAB}/" \
            | sed -u "/^[^%|]*|/s/^/${dcolor[$idx3]}/g; s/$/${dcolor[$idx]}/; /|/s/+/${GOOD}&/g; /|/s/-/${BAD}&/g; /modified:/s/^.*$/${BAD}&/g; /^\s*M\s/s/^.*$/${BAD}&/g" \
            | sed "1 s/^[\s]*[^\s]/${cr}&/"

    else
        itab
        ddecho "${TAB}${temp_file} empty"
        dtab
    fi

    # remove temporary file
    if [ -f ${temp_file} ]; then
        rm ${temp_file}
    fi

    # reset formatting
    unset_color
    dtab
    if [ $DEBUG -gt 0 ]; then
        dtab
    fi
    return $RETVAL
}

# run command after adjusting shell options and un-setting traps
function do_cmd_safe() {
    local -i DEBUG=0
    # save command as variable
    cmd=$(echo $@)
    # format output
    itab
    if [ $DEBUG -gt 0 ]; then
        start_new_line
    fi
    decho "${TAB}SAFE: running command $cmd... "

    # set shell options
    unset_traps
    if [[ "$-" == *e* ]]; then
        echo -n "${TAB}setting shell options..."
        old_opts=$(echo "$-")
        # exit on errors must be turned off; otherwise shell will exit...
        set +e
        echo "done"
    fi

    dtab
    do_cmd $cmd
    RETVAL=$?
    itab

    # reset shell options
    reset_shell ${old_opts-''}
    reset_traps

    return $RETVAL
}
