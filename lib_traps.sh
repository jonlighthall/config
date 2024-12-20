#!/bin/bash -eu
# ------------------------------------------------------------------------------
# TRAPS LIBRARY
# ------------------------------------------------------------------------------
#
# ~/config/lib_traps.sh
#
# PURPOSE: define fuctions to set and unset traps, and setting and reseting
#   shell options. Includes functions for printing timestamps and return values.
#
# Mar 2024 JCL
#
# ------------------------------------------------------------------------------

# simple fuction to return an error
function s() {
    local -i RETVAL=1
    echo "${TAB}returing $RETVAL..."
    echo -e "${TAB}${GREEN}> ${RESET}enter 'exit' to restart"
    return 1
}

function snest() {
    set_traps
    s
}

# fuction to test returning an error
function bye() {
    # print function name
    decho -e "${TAB}${INVERT}${FUNCNAME}${RESET}"
    echo "${TAB}goodbye...?"
    # add return code for parent script
    if [ ${DEBUG:-0} -gt 0 ]; then
        # print debug value
        print_debug
        # print shell options
        decho "${TAB}shell options = $-"
        set +T
        trap 'print_return $?; trap - RETURN' RETURN
    fi
    return 1
}

# ------------------------------------------------------------------------------
# Functions for set and reset shell options
# ------------------------------------------------------------------------------

function set_shell() {
    echo $-
    set -eET
    echo $-
    trap 'echo "SET"' RETURN
}

function unset_shell() {
    echo $-
    set +eET
    echo $-
    trap 'echo "UNSET"' RETURN
}

# reset shell options
function reset_shell() {
    local -i DEBUG=0

    if [ $# -lt 1 ]; then
        return 0
    fi

    itab
    local -xr old_opts=$1
    shift
    if [ $# -lt 1 ]; then
        option_list=$(echo "aTbefhkmnptuvxBCEHPT" | sed 's/./ &/g')
    else
        option_list=$@
    fi

    decho -e "\ncurrent: $-"
    decho "    old: $old_opts"

    if [[ "$-" == "${old_opts}" ]]; then
        ddecho "same"
        dtab
        return 0
    else
        ddecho "not same"
    fi
    decho -n "resetting shell options... "
    unset_traps
    #(
    for opt in ${option_list}; do
        set -T
        if [[ "$-" == "$old_opts" ]]; then
            break
            decho
            echo ":$-"
            echo ":$old_opts"

        else
            decho
            ddecho "fixing $opt"
        fi

        # strip + from option
        if [[ "${opt::1}" == "+" ]]; then
            opt=${opt:1}
        fi

        new_opts=$-

        if [[ "${old_opts}" == *"${opt}" ]]; then
            decho "${opt} was set:${old_opts}"
            if [[ "${new_opts}" == *"${opt}" ]]; then
                decho -n "${opt} is set:"
            else
                decho "${opt} is not set:$-"
                decho -n "setting ${opt}:"
                set -${opt}
                decho -e "${RESET}${-}" | grep ${opt} --color=always
            fi
            decho -n "$-"
        else
            decho "${opt} was not set:${old_opts}"
            if [[ "${new_opts}" == *"${opt}" ]]; then
                decho -n "${opt} is set:"
                decho -e "${RESET}${new_opts}" | grep ${opt} --color=always
                decho -n "unsetting ${opt}:"
                set +${opt}
            else
                decho -n "${opt} is not set:"
            fi
            set -T
            decho -n "$-"
        fi

    done
    reset_traps
    decho
    # ) | sed '1d' | column -t -s':' -o': ' -R 1 | sed '1 s/^/\n/' | sed "s/^/  /"
    set -T
    decho "done: $-"
    dtab
}

function safe_shell() {
    # set local debug value
    if [ $# -eq 1 ]; then
        # use argument to manually set DEBUG
        local -i DEBUG=$1
    else
        # substitute default value if DEBUG is unset or null
        local -i DEBUG=${DEBUG:-2}
        # if being run directly from shell, set DEBUG
        [ ${#BASH_SOURCE[@]} -eq 1 ] && DEBUG=2
    fi

    decho "${TAB}shell options: $-"
    decho -n "${TAB}resetting... "
    ddecho
    itab
    for opt in e u; do
        if [[ "$-" == *$opt* ]]; then
            ddecho "${TAB}setting $opt..."
            set +$opt
        fi
    done
    dtab
    decho "done"
    echo "${TAB}shell options: $-"

    clear_traps
}

# ------------------------------------------------------------------------------
# Functions for print EXIT, RETURN, and INT (non-ERR) status
# ------------------------------------------------------------------------------

# print source name, elapsed time, and timestamp
function print_time() {
    # check if start time is defined
    if [ -n "${start_time+alt}" ]; then
        # get the length of the execution stack
        local -i N_BASH=${#BASH_SOURCE[@]}
        # BASH_SOURCE counts from zero; get the bottom of the stack
        # print file name of the calling function
        echo -ne "${TAB}${GRAY}${BASH_SOURCE[(($N_BASH - 1))]##*/} "
        # print elapsed time and change color
        print_elap | sed 's/37m/90m/'
        echo
    fi
}

# print source name, elapsed time, and timestamp
function print_done() {
    # get the length of the execution stack
    local -i N_BASH=${#BASH_SOURCE[@]}
    # BASH_SOURCE counts from zero; get the bottom of the stack
    # print file name of the calling function
    echo -en "${BASH_SOURCE[(($N_BASH - 1))]##*/}${RESET} " >&2
    print_elap
    echo -n " on " >&2
    timestamp
}

# format timestamp
function timestamp() {
    echo "$(date +"%a %b %-d at %-l:%M %p %Z")" >&2
}

# print elapsed time
function print_elap() {
    # check if start time is defined
    if [ -n "${start_time+alt}" ]; then
        # get current time (end time)
        local -i end_time=$(date +%s%N)

        # calculate interval (in ns)
        local -i elap_time=$((${end_time} - ${start_time}))
        # convert to seconds
        local dT_sec
        if command -v bc &>/dev/null; then
            dT_sec=$(bc <<<"scale=9;$elap_time/10^9" | sed 's/^\./0./')
        else
            dT_sec=${elap_time::-9}.${elap_time:$((${#elap_time} - 9))}
            if [ ${#elap_time} -eq 9 ]; then
                dT_sec=$(echo "0.$elap_time")
            fi
        fi
        # set precision
        local -ir nd=3
        # format interval
        local fmt="%.${nd}f"
        dT_sec=$(printf "$fmt" $dT_sec)

        # print output
        if command -v sec2elap &>/dev/null; then
            bash sec2elap $dT_sec | tr -d "\n" >&2
        else
            echo -ne "elapsed time is ${WHITE}${dT_sec} sec${RESET}" >&2
        fi
    else
        decho -ne "${YELLOW}start_time not defined${RESET} " >&2
        # reset cursor position for print_done, etc.
        echo -en "\x1b[1D" >&2
    fi
}

function print_exit() {
    # optional argument is $?
    # e.g.
    # trap 'print_exit $?' EXIT

    # parse arguments
    if [ $# -gt 0 ]; then
        local EXIT_RETVAL=$1
    fi

    start_new_line
    echo -ne "${TAB}${YELLOW}\E[7m EXIT ${RESET} " >&2
    # print exit code
    if [ ! -z ${EXIT_RETVAL+alt} ]; then
        echo -ne "${GRAY}RETVAL=${EXIT_RETVAL}${RESET} " >&2
    fi

    print_done

    # reset shell options before returning to shell
    if [[ "$-" == *u* ]]; then
        decho -n "u is set: " >&2
        decho $- >&2
        decho -n "unsetting u... " >&2
        set +u
        decho $- >&2
    else
        decho -n "u is not set: " >&2
        decho $- >&2
    fi
}

function print_return() {
    # expected arguments are $?
    # e.g.
    # trap 'print_return $?' RETURN

    # set local debug value
    # substitute default value if DEBUG is unset or null
    local -i DEBUG=${DEBUG:-0} 

    # print summary
    start_new_line
    if [ $DEBUG -gt 1 ]; then
        decho -e "${TAB}${YELLOW}${INVERT}${FUNCNAME}${RESET}"
        print_stack
    fi

    if false; then
        ddecho "${TAB}$-"
        # set shell options
        ddecho -n "${TAB}setting shell options... "
        # trace RETURN and DEBUG traps (subshells inherit RETURN and DEBUG traps
        # from shell)
        #set -T
        ddecho "done"
        ddecho "${TAB}$-"
    fi

    ddecho -n "${TAB}${FUNCNAME}: "
    if [ $# -eq 0 ]; then
        ddecho "no arg"
    else
        ddecho "arg = $@"
    fi

    RETURN_RETVAL=${1:-''}

    # get size of function stack
    local -ir N_FUNCs=${#FUNCNAME[@]}

    echo -en "${TAB}${YELLOW}\E[7m RETURN ${RESET}"
    if [ ! -z ${RETURN_RETVAL:-dummy} ]; then
        echo -en " ${GRAY}RETVAL=${RETURN_RETVAL}${RESET} "
    fi

    print_done
    # reset shell options before returning to shell
    if [[ "$-" == *u* ]]; then
        set +u
    fi
}

function print_int() {
    start_new_line
    echo -e "${AZURE}\E[7m INT ${RESET} ${BASH_SOURCE[1]##*/}"
    echo " Ctrl-C pressed"
    #	echo -e "breaking..."
    #	break
}

# ------------------------------------------------------------------------------
# Print ERR trace
# ------------------------------------------------------------------------------

function print_error() {
    # expected arguments are $LINENO $? $BASH_COMMAND
    # e.g.
    # trap 'print_error $LINENO $? $BASH_COMMAND' ERR

    # substitute default value if DEBUG is unset or null
    local -i DEBUG=${DEBUG:-3}
    # set manually
    DEBUG=0

    # set function debug
    local -i funcDEBUG=$DEBUG

    # parse arguments
    local -i ERR_LINENO=$1
    shift
    local -i ERR_RETVAL=$1
    shift
    local ERR_CMD="$@"

    # check if in Git Bash terminal (and not in repsoitory)
    if [ -n "$(command -v __git_ps1)" ]; then
        # in Git Bash terminal
        # check if (not) in repository
        if [ $ERR_RETVAL -eq 128 ]; then
            # not in repository
            echo -en " ${PSBR}null${RESET}"
            decho -en " in Git Bash, not in repo"
            return
        else
            # in repository
            # check if error is trivial
            if [[ $ERR_CMD =~ '$exit' ]]; then
                # trivial error, append to prompt
                echo -en " ${RED}E${RESET}"
                decho -en " trivial error"
                return
            else
                decho "${TAB}non-trivial error"
            fi
        fi
        set +e
    fi

    # print function name
    decho -e "${TAB}\E[37;41m${FUNCNAME}${RESET}"

    TAB=${TAB=''}
    local ERR_PRINT=$(echo -e "${TAB}\E[37;41m ERROR ${RESET} ")
    start_new_line
    # set line width to one less than the terminal width
    local -i line_width=$(( $(tput cols) - 1 ))
    local -i line_max=72
    if [ $line_width -gt $line_max ]; then
        line_width=$line_max
    fi

    hline ${line_width} ${RED}E${RESET}

    eTAB=$(echo -e "${RED}|${RESET}")
    eTAB=$fTAB

    TAB+=$eTAB

    # print arguments
    echo "${TAB}LINENO = $ERR_LINENO"
    echo "${TAB}RETVAL = $ERR_RETVAL"
    echo "${TAB}   CMD = $ERR_CMD"

    fecho "LINENO = $ERR_LINENO"
    fecho "RETVAL = $ERR_RETVAL"
    fecho "   CMD = $ERR_CMD"

    print_stack

    # since print stack is itself part of the stack, remove the top of the stack
    ((N_FUNC--))
    ((N_BASH--))
    ((N_LINE--))

    # get the bottom of the stack: the call stack counts from zero
    local -i N_BOTTOM=$(($N_FUNC - 1))
    decho "${TAB}getting bottom of stack..."
    itab
    ddecho "${TAB}N_BOTTOM = $N_BOTTOM"

    if [ ${N_BOTTOM} -gt 0 ]; then
        ddecho "${TAB}FUNCNAME[$N_BOTTOM] = ${FUNCNAME[$N_BOTTOM]}"
        decho "${TAB}BASH_SOURCE[$N_BOTTOM] = ${BASH_SOURCE[$N_BOTTOM]}"

        ddecho -n "${TAB}bottom of stack is... "
        if [[ ${BASH_SOURCE[${N_BOTTOM}]} == "bash" ]]; then
            ddecho "bash"
        else
            ddecho "NOT bash"
        fi
        dtab

        # get the command that casued the error
        local ERR_LINE=
        decho "${TAB}getting line that caused error..."
        itab

        local -i parent=0
        if [ $N_BASH -gt 0 ]; then
            parent=1

            # print parent function
            decho "${TAB}BASH_SOURCE[$parent] = ${BASH_SOURCE[$parent]}"
            decho "${TAB}   FUNCNAME[$parent] = ${FUNCNAME[$parent]}"
            decho "${TAB}BASH_LINENO[$parent] = ${BASH_LINENO[$parent]}"

            # check if parent is command-not-found
            if [[ "${FUNCNAME[${parent}]}" =~ ^command.* ]];then
                dtab
                ddecho "${TAB}${RED}command not found${RESET}, incrementing parent..."
                itab
                ERR_LINENO=${BASH_LINENO[$parent]}
                parent=2
                # print real parent
                ddecho "${TAB}BASH_SOURCE[$parent] = ${BASH_SOURCE[$parent]}"
                ddecho "${TAB}   FUNCNAME[$parent] = ${FUNCNAME[$parent]}"
                ddecho "${TAB}BASH_LINENO[$parent] = ${BASH_LINENO[$parent]}"
                ddecho "${TAB}offending line = ${ERR_LINENO}"
            else
                ddecho "${TAB}command found"
            fi
        fi

        # check if error came from shell
        if [[ ${BASH_SOURCE[${N_BOTTOM}]} == "^bash"* ]];then
            ddecho "${TAB}error did not come from a file, it came from ${SHELL##*/}"
            local ERR_LINE=$ERR_CMD
        else
            # not bash
            ddecho "${TAB}base of stack is not bash"
            # check that error came from a file
            if [ -f "${BASH_SOURCE[$parent]}" ]; then
                ddecho "${TAB}BASH_SOURCE[$parent] is a file"
                # check that value is not empty
                ddecho -n "${TAB}${BASH_SOURCE[$parent]} is... "
                if [ ! -z "${BASH_SOURCE[$parent]}" ]; then
                    ddecho "not empty"
                    if [ $N_FUNC -gt 1 ] ; then
                        # print source
                        ddecho "${TAB}BASH_SOURCE[$parent] = ${BASH_SOURCE[$parent]}"
                        if [[ ! ${BASH_SOURCE[${parent}]} =~ ".bashrc" ]]; then
                            ddecho "${TAB}parent is NOT .bashrc"
                            # print line number
                            ddecho "${TAB}ERR_LINE = ${ERR_LINENO}"
                            # print summary
                            ddecho -n "${TAB}line ${ERR_LINENO} in ${BASH_SOURCE[$parent]}: "
                            ddecho sed -n "${ERR_LINENO}p" "${BASH_SOURCE[$parent]}"
                            ddecho
                            decho "${TAB}"$(sed -n "${ERR_LINENO}p" "${BASH_SOURCE[$parent]}")
                            # save offending line
                            ERR_LINE=$(sed -n "${ERR_LINENO}p" "${BASH_SOURCE[$parent]}" | sed "s/^\s*//")
                        fi
                    else
                        ddecho "${TAB}${BASH_SOURCE[$parent]} is EMPTY"
                        ERR_LINE="EMPTY"
                    fi
                fi
            fi
        fi
    else
        ddecho "${TAB}error came from ${SHELL##*/} prompt"
        ((--ERR_LINENO))
        ERR_LINE="$ERR_CMD"
    fi

    dtab

    # print summary
    start_new_line
    # decrement TAB by fTAB
    TAB=${TAB%$eTAB}
    echo -n "${ERR_PRINT}"

    # print grep-like line match
    if [[ ${BASH_SOURCE[${N_BOTTOM}]} == "bash" ]]; then
        echo -ne "${GRF}${FUNCNAME[N_BOTTOM]}${RESET}${GRS}:${RESET} "
    else
        if [ ${N_BOTTOM} -eq 0 ]; then
            echo -ne "${GRF}${SHELL##*/}${RESET}${GRS}:${RESET}${GRL}${ERR_LINENO}${RESET}${GRS}:${RESET}"
        else
            if [[ ${BASH_SOURCE[${parent}]} =~ ".bashrc" ]]; then
                echo "parent is bashrc: ${ERR_LINE}"
                ERR_LINENO=$(grep -n "${ERR_LINE}" ${BASH_SOURCE[${parent}]} | sed 's/:.*//')
            fi
            echo -ne "${GRF}${BASH_SOURCE[${parent}]##*/}${RESET}${GRS}:${RESET}${GRL}${ERR_LINENO}${RESET}${GRS}:${RESET}"
        fi
    fi

    # get the cursor position
    local -i x_pos
    get_curpos x_pos
    # define indent
    ((--x_pos))
    ((--x_pos))
    local spx=$(echo -ne "\E[${x_pos}C")
    eva='eval'
    local -i evl=${#eva}
    local -i etab=$((x_pos - evl))

    # print error line
    echo -n "${ERR_LINE}"
    if [ ${N_BOTTOM} -eq 0 ]; then
        ddecho -e " <- ${INVERT}you did this"
    else
        echo
    fi

    # if the error line and the error command do not mach, print error command
    if [[ "$ERR_CMD" != "$ERR_LINE" ]]; then
        echo -ne "\E[${etab}C${YELLOW} cmd${GRS}:${RESET}"
        echo "${ERR_CMD}" | sed "s/command-not-found/${RED}&${RESET}/"
    fi

    if [[ ! ${BASH_SOURCE[${N_BOTTOM}]} =~ "bash" ]]; then
        # if command contains variables, evaluate expression
        if [[ "$ERR_CMD" =~ '$' ]]; then
            decho -e "${spx} ${GRAY}expanding variables...${RESET}"
            # print 'eval' set back from cmd
            echo -ne "\E[${etab}C${VALID}${eva}${RESET}${GRS}:${RESET}"
            # print evaluated command and remove leading whitespace
            eval echo $ERR_CMD | sed "s/^\s*//"
            if [[ "$ERR_CMD" =~ "/dev/null" ]]; then
                # remove redirect
                NCMD=$(echo $ERR_CMD | sed 's|/dev/null||')
                NCMD=$(echo $NCMD | sed 's/[12>&\s]*$//')
                # print evaluated output
                echo -en "\E[${spx}C"
                eval echo $NCMD
                echo -e "$ {GRAY}redirect (no output)${RESET}"
            fi
        fi
    fi
    echo -e "${spx} ${GRAY}RETVAL=${ERR_RETVAL}${RESET}"
    hline ${line_width} ${RED}E
    # reset shell options before returning to shell
    if [[ "$-" == *u* ]]; then
        set +u
    fi

}

# ------------------------------------------------------------------------------
# Functions to set and unset traps
# ------------------------------------------------------------------------------

function test_traps() {
    set -u
    trap 'print_return $?; trap - RETURN' RETURN
    # set debug level for testing trap programs
    local -i localDEBUG=3
    print_traps ${localDEBUG}
    set_traps ${localDEBUG}
    # check error trapping
    echo "${TAB}checking ERR trap..."
    itab
    check_traps_clear ${localDEBUG}

    unset_traps ${localDEBUG}
    reset_traps ${localDEBUG}
    echo "traps: "
    trap -p

    clear_traps ${localDEBUG}
    echo "traps: "
    trap -p
    set_exit ${localDEBUG}
    echo "traps: "
    trap -p

    if [[ "$-" == *u* ]]; then
        set +u
    fi
}

function print_traps() {
    # set local debug value
    if [ $# -eq 1 ]; then
        # use argument to manually set DEBUG
        local -i DEBUG=$1
    else
        # substitute default value if DEBUG is unset or null
        local -i DEBUG=${DEBUG:-2}
        # if being run directly from shell, set DEBUG
        [ ${#BASH_SOURCE[@]} -eq 1 ] && DEBUG=2
    fi
    # print summary
    local -i xpos
    get_curpos xpos
    [ $xpos -eq 1 ] && ddecho -n "${TAB}"
    ddecho "the following traps are set"
    itab
    if [ -z "$(trap -p)" ]; then
        ddecho -e "${TAB}none"
    else
        ddecho $(trap -p) | sed "s/^/${TAB}/;s/ \(trap --\)/\n${TAB}\1/g"
        [ $DEBUG -gt 0 ] && start_new_line
    fi
    ddecho -ne ${RESET}
    dtab
}

function check_traps_set() {
    # set local debug value
    if [ $# -eq 1 ]; then
        # use argument to manually set DEBUG
        local -i DEBUG=$1
    else
        # substitute default value if DEBUG is unset or null
        local -i DEBUG=${DEBUG:-2} 
    fi
    # print summary
    ddecho -n "${TAB}on ${FUNCNAME[1]} return, "
    print_traps
    if [ -z "$(trap -p)" ]; then
        echo -e "${BAD}traps not set${RESET}"
        dtab
        return 1
    fi
    dtab
}

function get_sigs() {
    if [ ! -z "$(trap -p)" ]; then
        if [ ${DEBUG} -gt 1 ]; then
            print_traps

            ddecho -e "${TAB}${INVERT}traps echo sig:${NORMAL}"
            itab
            decho $(trap -p) | sed "s/ \(trap -- \)/\n\1/g" |  sed "s/.* //;s/^/${TAB}/"
            dtab

            ddecho -e "${TAB}${INVERT}traps echo sig tr:${NORMAL}"
            itab
            decho $(trap -p) | sed "s/ \(trap -- \)/\n\1/g" |  sed 's/.* //' | tr  '\n' ' ' | sed "s/^/${TAB}/"
            ddecho
            dtab

        fi
        export sig="$(echo $(trap -p) | sed "s/ \(trap -- \)/\n\1/g" |  sed 's/.* //' | tr  '\n' ' ')"
        if [ ${DEBUG} -gt 1 ]; then
            ddecho "${TAB}sig = ${sig}"
            ddecho "${TAB}sig@ = ${sig[@]}"
            ddecho "${TAB}#sig = ${#sig[@]}"
        fi
    else
        export sig=''
    fi
}

check_traps_clear() {
    # set local debug value
    if [ $# -eq 1 ]; then
        # use argument to manually set DEBUG
        local -i DEBUG=$1
    else
        # substitute default value if DEBUG is unset or null
        local -i DEBUG=${DEBUG:-2}
    fi

    # print summary
    if [ ${#FUNCNAME[@]} -gt 1 ]; then
        ddecho -n "${TAB}on ${FUNCNAME[1]} return, "
    fi
    print_traps

    # check status
    if [ ! -z "$(trap -p)" ]; then
        echo -e "${TAB}${BAD}traps not cleared${RESET}"
        dtab
        get_sigs
        return 1
    fi
    return 0
}

do_clear() {
    local -a sig
    get_sigs
    if [ ! -z "${sig}" ]; then
        ddecho "${TAB}sig = ${sig}"
        # clear traps
        itab
        for itrap in ${sig[@]}; do
            ddecho "${TAB}unsetting trap $itrap..."
            trap - $itrap
        done
        dtab
    fi
}

# set ERR and EXIT traps
function set_traps() {
    # set local debug value
    if [ $# -eq 1 ]; then
        # use argument to manually set DEBUG
        local -i DEBUG=$1
    else
        # substitute default value if DEBUG is unset or null
        local -i DEBUG=${DEBUG:-2} 
    fi

    [ $DEBUG -gt 0 ] && start_new_line
    TAB=${TAB=''}
    get_caller
    decho -e "${TAB}${MAGENTA}\E[7mset traps\E[27m $- ($caller_func)${RESET}"
    itab

    dddecho "${TAB}$-"
    # set shell options
    dddecho -n "${TAB}setting shell options... "
    # trace ERR (subshells inherit ERR trap from shell)
    # -E  If set, the ERR trap is inherited by shell functions.
    set -E
    dddecho "done"
    dddecho "${TAB}$-"

    dddecho -n "${TAB}setting traps... "
    trap 'print_error $LINENO $? $BASH_COMMAND' ERR
    trap 'print_exit $?' EXIT
    dddecho "done"

    check_traps_set
}

# set EXIT trap
function set_exit() {
    # set local debug value
    if [ $# -eq 1 ]; then
        # use argument to manually set DEBUG
        local -i DEBUG=$1
    else
        # substitute default value if DEBUG is unset or null
        local -i DEBUG=${DEBUG:-2}
    fi
    # manual
    #DEBUG=1

    [ $DEBUG -gt 0 ] && start_new_line
    decho -e "${TAB}${ORANGE}\E[7mset exit${RESET}"
    itab

    get_run_type

    # set shell options
    decho "${TAB}$-"
    decho -n "${TAB}setting shell options... "
    if [[ "$-" == *u* ]]; then
        set +u
    fi

    # clear traps
    do_clear

    if [[ "${RUN_TYPE}" =~ "sourcing" ]]; then
        decho "sourced"

        # DO NOT trace RETURN and DEBUG traps
        # (subshells WILL NOT inherit RETURN and DEBUG traps from shell)
        set +T
        decho "done"
        decho "${TAB}$-"

        dddecho -n "${TAB}setting traps... "
        trap 'print_return $?; trap - RETURN' RETURN
        dddecho "done"
        dtab
        return 0
    else
        decho "not sourced"

        # trace ERR (subshells inherit ERR trap from shell)
        set -E
        decho "done"
        decho "${TAB}$-"

        decho -n "${TAB}setting traps... "
        trap 'print_exit $?; trap - EXIT' EXIT
        decho "done"
        dtab
        exit 0
    fi
    print_traps
}

# unset ERR and EXIT traps, saving current values
# restore saved values with reset_traps
function unset_traps() {
    # set local debug value
    if [ $# -eq 1 ]; then
        # use argument to manually set DEBUG
        local -i DEBUG=$1
    else
        # substitute default value if DEBUG is unset or null
        local -i DEBUG=${DEBUG:-2} 
    fi

    [ $DEBUG -gt 0 ] && start_new_line
    TAB=${TAB=''}
    get_caller
    decho -e "${TAB}${CYAN}\E[7mun-set traps\E[27m $- ($caller_func)${RESET}"

    dddecho "${TAB}$-"
    # set shell options
    dddecho -n "${TAB}setting shell options... "
    # trace ERR (subshells inherit ERR trap from shell)
    set -E
    # trace RETURN and DEBUG traps
    set -T
    # DO NOT exit on errors
    set +e
    dddecho "done"
    dddecho "${TAB}$-"
    dddecho "${TAB}the current traps are set"
    if [ -z "$(trap -p)" ]; then
        dddecho -e "${TAB}${fTAB}none"
        # clear saved traps
        unset save_traps
    else
        itab
        dddecho $(trap -p) | sed "s/^/${TAB}/;s/ \(trap\)/\n${TAB}\1/g"
        dtab

        # save traps
        export save_traps=$(trap -p | sed 's/-- //g')
        if [ ! -z "${save_traps}" ]; then
            dddecho "${TAB}the current traps are saved"
            itab
            dddecho "${save_traps}" | sed "s/^/${TAB}/"
            dtab
        fi
    fi

    do_clear
    check_traps_clear
    return $?
}

# reset saved traps
# used in conjuction with unset_traps
function reset_traps() {
    # set local debug value
    if [ $# -eq 1 ]; then
        # use argument to manually set DEBUG
        local -i DEBUG=$1
    else
        # substitute default value if DEBUG is unset or null
        local -i DEBUG=${DEBUG:-2} 
    fi

    [ $DEBUG -gt 0 ] && start_new_line
    TAB=${TAB=''}
    get_caller
    decho -e "${TAB}${GREEN}\E[7mreset traps\E[27m  $- ($caller_func)${RESET}"
    itab

    dddecho "${TAB}$-"
    # set shell options
    dddecho -n "${TAB}setting shell options... "
    # trace ERR (subshells inherit ERR trap from shell)
    set -E
    dddecho "done"
    dddecho "${TAB}$-"

    dddecho "${TAB}the following traps are saved"
    itab
    if [ -z "${save_traps+default}" ]; then
        dddecho "${TAB}none"
        dtab
    else
        dddecho "${save_traps}" | sed "s/^/${TAB}/"
        dtab
        dddecho -n "${TAB}setting saved traps..."
        eval $(echo "${save_traps}" | sed "s/$/;/g")
        dddecho "done"
    fi

    # print summary
    ddecho -n "${TAB}on ${FUNCNAME} return, "
    print_traps
    dtab
}

# unset ERR, EXIT, and RETURN traps, erasing current values
function clear_traps() {
    # set local debug value
    if [ $# -eq 1 ]; then
        # use argument to manually set DEBUG
        local -i DEBUG=$1
    else
        # substitute default value if DEBUG is unset or null
        local -i DEBUG=${DEBUG:-2} 
    fi

    [ $DEBUG -gt 0 ] && start_new_line
    decho -e "${TAB}${YELLOW}\E[7mclear traps${RESET}"
    itab

    dddecho "${TAB}$-"
    # set shell options
    dddecho -n "${TAB}setting shell options... "
    # trace ERR (subshells inherit ERR trap from shell)
    set -E
    # trace RETURN and DEBUG traps
    set -T
    # DO NOT exit on errors
    set +e
    dddecho "done"
    dddecho "${TAB}$-"

    dddecho "${TAB}the current traps are set"
    if [ -z "$(trap -p)" ]; then
        dddecho -e "${TAB}${fTAB}none"
    else
        itab
        dddecho $(trap -p) | sed " s/^/${TAB}/;s/ \(trap\)/\n${TAB}\1/g"
        dtab
    fi

    # clear saved traps
    unset save_traps

    do_clear
    check_traps_clear
    dtab
}

#
function enable_exit_on_fail() {
    trap 'echo "${TAB}${BASH_SOURCE[0]##*/}: line $LINENO: trapping ERR $BASH_COMMAND"; safe_shell; return 0 2>/dev/null' ERR
}

# Provide a way to cleanly exit on errors, whether the file is sourced or
# executed, that does not cause the shell to exit.
function exit_on_fail() {
    enable_exit_on_fail
    echo -e "${TAB}${YELLOW}\x1b[7m${BASH_SOURCE[1]##*/} failed\x1b[0m"

    return 1
    # -----------------
    # set behavior
    local do_exit=true
    # -----------------
    # decho "${TAB}$FUNCNAME is $do_exit"
    if [[ $do_exit == true ]]; then
        #itab
        #print_stack
        #dtab

        #decho "${TAB}shell options: $-"
        if [[ "$-" == *i* ]]; then
            #  itab
            #  decho "${TAB}shell is interactive"
            if [[ "$-" == *e* ]]; then
                #decho -e "${TAB}turning off exit-on-errors..."
                # turn off exit-on-errors, otherwise shell will exit
                set +e
                #dtab
                #decho "${TAB}shell options: $-"
                #decho "${TAB}$FUNCNAME exiting..."
                # return 1 to trigger error
                # then trap error with return
                #itab
                return 1
            fi
        else
            #   decho "${TAB}shell is NOT interactive"
            decho "${TAB}returning..."
            return 1
        fi
    else
        # decho "${TAB}continuing..."
        return 0
    fi
}
