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

function strip_pretty() {
    local -n output=$1
    shift

    # strip escapes
    output=$(echo -e "$@" | sed "s/$(echo -e "\E")[^m]*m//g")
}

# ------------------------------------------------------------------------------
# Functions to colorize and indent command output
# ------------------------------------------------------------------------------

# format command output
# handling is included for a variety of commands
# conditionally calls do_cmd_script, and do_cmd_stdbuf

# set both to zero for un-altered output
export FMT_COLOR=3
export FMT_TAB=1

function extract_color() {
    set -u
    local DEBUG=0

    # get color
    local input_color=${dcolor[idx]}

    decho -e "${UL}input color${NORMAL}"
    itab
    decho -en "${TAB}color: ${input_color}"
    decho -n "${input_color}"
    decho -e "${RESET}"

    # get color code
    export input_code=$(echo ${input_color} | sed 's/^.*\[\([0-9;]*\)m/\1/')
    decho -e "${TAB}code:\x1B[${input_code}m $input_code"

    if [[ $input_code =~ ";" ]]; then
        ddecho "${TAB}${input_code} has seperators"
        ddecho "${TAB}assuming 256 colors"
        input_set=$(echo "${input_code%;*};")
        local -i input_num=${input_code##*;}

        local -i num_start=17
        local -i num_end=230
        local -i num_mod=$((num_end-num_start))
        local -i num_shift=6

    else
        ddecho "${TAB}${input_code} does not have seperators"
        ddecho "${TAB}assuming 16 colors"
        input_set=${input_code: :-1}
        local -i input_num=${input_code: -1}

        local -i num_start=1
        local -i num_end=6
        local -i num_mod=$((num_end-num_start))
        local -i num_shift=-1
    fi

    ddecho "${TAB}set: $input_set"
    decho "${TAB}number:  $input_num"
    dtab

    ddecho -e "${UL}calculate${NORMAL}"
    itab
    ddecho "${TAB}value start: $num_start"
    ddecho "${TAB}value end: $num_end"
    ddecho "${TAB}modulo: $num_mod"
    ddecho "${TAB}shift: $num_shift"

    local -i output_arg=$(( input_num - num_start  + num_shift ))

    [ $output_arg -lt 0 ] && output_arg=$(( output_arg * -1 ))

    ddecho "${TAB}arg: $output_arg"
    local -i output_mod=$(( ( output_arg) % ( num_mod + 1 ) ))
    ddecho "${TAB}mod: $output_mod"
    local -i output_num=$(( output_mod + num_start ))

    export output_code=$(echo "${input_set}${output_num}")
    declare output_color=$(echo "\x1B[${output_code}m")

    dtab

    decho -e "${UL}output color${NORMAL}"
    itab
    decho "${TAB}number: $output_num"
    decho -e "${TAB}code: \x1B[${output_code}m$output_code${RESET}"

    decho -en "${TAB}color: ${output_color}"
    decho -n "${output_color}"
    decho -e "${RESET}"
    dtab

    # reset shell options before returning to shell
    if [[ "$-" == *u* ]]; then
        set +u
    fi
}

function define_cr() {
    local -i x1c=0
    get_curpos x1c
    decho -n "$x1c"
    # set the "carriage return" value for the first non-empty line of the
    # command ouput
    export cr=
    if [ $x1c -gt 1 ]; then
        # if the cursor is not at the start of a line, start a new line
        cr='\n'
        if [ $DEBUG -gt 0 ]; then
            cr="$(printf '\u21b5')\n"
        fi
    else
        # if the cursor is already  at the start of a new line, do nothing
        :
    fi
}

function do_cmd() {
    local -i DEBUG=0
    # save command as variable
    cmd=$(echo $@)
    # format output
    itab ${FMT_TAB}
    if [ $DEBUG -gt 0 ]; then
        start_new_line
    fi
    decho "${TAB}running command $cmd... "

    # get color index
    local -i idx
    dbg2idx $FMT_COLOR idx
    # set color
    echo -ne "${dcolor[$idx]}"
    extract_color

    if [[ "${cmd}" =~ ^powershell.* ]]; then
        echo "PowerShell:"
        $cmd
        local -i RETVAL=$?
    else

    # the ideal solution is to use unbuffer
    # check if unbuffer is defined
    if command -v unbuffer >/dev/null; then
        # check if command is git to turn off pager
        if [[ "$cmd" =~ "git"* ]]; then
            cmd=$(echo "$cmd" | sed 's/git /&--no-pager /')
        fi

        # check cursor position
        define_cr

        ddecho "${TAB}printing unbuffered command ouput..."
        # set shell options
        set -o pipefail

        # print unbuffered command output
        unbuffer $cmd \
            | sed -u "s/\r$//g;s/.*\r//g;s/^/${TAB}/" \
            | sed -u "s/\x1B\[${input_code}m/\x1B[${output_code}m/g" \
            | sed -u "1 s/^[\s]*[^\s]/${cr}&/" \
            | sed -u "s/\x1B\[m/\x1B[m${dcolor[$idx]}/g"

        local -i RETVAL=$?

        # reset shell options
        set +o pipefail
        dtab ${FMT_TAB}
    else
        # check if script is defined
        if command -v script >/dev/null; then
            dtab ${FMT_TAB}
            ddecho "${TAB}printing command ouput typescript..."
            # print typescript command ouput
            do_cmd_script $cmd
        else
            dtab ${FMT_TAB}
            ddecho "${TAB}printing buffered command ouput..."
            # print buffered command output
            do_cmd_stdbuf $cmd
        fi
        local -i RETVAL=$?
    fi
    fi
    # reset formatting
    unset_color
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
    itab ${FMT_TAB}
    if [ $DEBUG -gt 0 ]; then
        start_new_line
    fi
    decho "${TAB}SCRIPT: running command $cmd... "

    # get color index
    local -i idx
    dbg2idx $FMT_COLOR idx
    # set color
    echo -ne "${dcolor[$idx]}"
    extract_color

    # check if typescript is defined
    if command -v script >/dev/null; then
        # check if command is git to turn off pager
        if [[ "$cmd" =~ "git"* ]]; then
            cmd=$(echo "$cmd" | sed 's/git /&--no-pager /')
        fi

        # check cursor position
        define_cr

        # define temp file
        local temp_file=typescript_$(date +'%Y-%m-%d-t%H%M%S')
        ddecho "${TAB}SCRIPT: redirecting command ouput to typescript pseudoterminal..."
        ddecho "${TAB}        log file is $temp_file"
        # set shell options
        set -o pipefail
        # print command output
        if false; then
            # command output is unbuffered only if "sed -u" is used!
            # however, this interfers with formatting the output
            script -eq -c "$cmd" \
                | sed -u 's/$\r/\n\r/g'
        else
            script -eq -c "$cmd" ${temp_file} \
                | sed  "s/\r$//g;s/.*\r//g;s/^/${TAB}/" \
                | sed  "s/\x1B\[${input_code}m/\x1B[${output_code}m/g" \
                | sed  "1 s/^[\s]*[^\s]/${cr}&/" \
                | sed  "s/\x1B\[m/\x1B[m${dcolor[$idx]}/g"
        fi
        local -i RETVAL=$?

        # reset shell options
        set +o pipefail
        dtab ${FMT_TAB}

        # remove temporary file
        for log in typescript*; do
            if [ -f $log ]; then
                rm $log
            fi
        done
    else
        dtab ${FMT_TAB}
        ddecho "${TAB}printing unformatted ouput..."
        # print buffered command output
        $cmd
        local -i RETVAL=$?
    fi

    # reset formatting
    unset_color

    return $RETVAL
}

# format buffered command ouput
# save ouput to file, print file, delete file
# developed for use when unbuffer and script are unavailable
function do_cmd_stdbuf() {
    local -i DEBUG=0
    # save command as variable
    cmd=$(echo $@)
    # format output
    itab ${FMT_TAB}
    if [ $DEBUG -gt 0 ]; then
        start_new_line
    fi

    # get color index
    local -i idx
    dbg2idx $FMT_COLOR idx
    # set color
    echo -ne "${dcolor[$idx]}"
    extract_color

    # define temp file
    temp_file=stdbuf_$(date +'%Y-%m-%d-t%H%M%S')
    ddecho "${TAB}STDBUF: redirecting command ouput to $temp_file..."

    # check if command is git to colorize output
    if [[ "$cmd" =~ "git [dl]"* ]]; then
        cmd+=" --color=always"
    fi

    # unbuffer command output and save to file
    stdbuf -i0 -o0 -e0 $cmd &>$temp_file
    RETVAL=$?

    # check if ouputfile is empty
    if [ -s ${temp_file} ]; then
        # check cursor position
        define_cr

        ddecho -e "${TAB}${IT}buffer:${NORMAL}"
        # define highlight color
        local -i idx3
        dbg2idx $((idx+1)) idx3
        # print command output
        \cat $temp_file \
            | sed "s/\r$//g;s/.*\r//g;s/^/${TAB}/" \
            | sed "s/\x1B\[${input_code}m/\x1B[${output_code}m/g" \
            | sed "1 s/^[\s]*[^\s]/${cr}&/" \
            | sed "s/\x1B\[m/\x1B[m${dcolor[$idx]}/g"
    else
        itab
        ddecho "${TAB}${temp_file} empty"
        dtab
    fi

    # remove temporary file
    for log in stdbuf*; do
        if [ -f $log ]; then
            rm $log
        fi
    done

    # reset formatting
    unset_color
    dtab ${FMT_TAB}
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
    itab ${FMT_TAB}
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

    dtab ${FMT_TAB}
    do_cmd $cmd
    RETVAL=$?
    itab

    # reset shell options
    reset_shell ${old_opts-''}
    reset_traps

    return $RETVAL
}

# run command after un-setting color
function do_cmd_in() {
    local -i DEBUG=0
    # save command as variable
    cmd=$(echo $@)

    # set formatting options
    local -i oldFMT_COLOR=${FMT_COLOR}
    export FMT_COLOR=0

    do_cmd $cmd
    RETVAL=$?

    # reset formatting options
    export FMT_COLOR=$oldFMT_COLOR

    return $RETVAL
}

# run command after un-setting tab
function do_cmd_col() {
    local -i DEBUG=0
    # save command as variable
    cmd=$(echo $@)

    # set formatting options
    local -i oldFMT_TAB=${FMT_TAB}
    export FMT_TAB=0

    do_cmd $cmd
    RETVAL=$?

    # reset formatting options
    export FMT_TAB=$oldFMT_TAB

    return $RETVAL
}
