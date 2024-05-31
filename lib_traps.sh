#!/bin/bash -eu
# -----------------------------------------------------------------------------------------------
# TRAPS LIBRARY
# -----------------------------------------------------------------------------------------------
#
# ~/config/lib_traps.sh
#
# PURPOSE: define fuctions to set and unset traps, and setting and reseting shell
#   options. Includes functions for printing timestamps and return values.
#
# Mar 2024 JCL
#
# -----------------------------------------------------------------------------------------------

# simple fuction to return an error
function s() {
    local -i RETVAL=1
    echo "returing $RETVAL..."
    echo "enter 'exit' to restart"
    return 1
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

# function to test calling an alias from a function
function fello() {
    local -i DEBUG=1
    # print function name
    decho -e "${TAB}${INVERT}${FUNCNAME}${RESET}"
    # define function name
    func='hello'
    # test if alias
    if [[ $(type -t $func) == "alias" ]]; then
        decho "${TAB}$func type is alias"
        echo -n "${TAB}"
        # evaluate
        eval $func
    else
        decho "${TAB}$func type is not alias"
        # print debug value
        print_debug
        # print shell options
        decho "${TAB}shell options = $-"
        # add return code for parent script
        if [ $DEBUG -gt 0 ]; then
            trap 'print_return $?; trap - RETURN' RETURN
        fi
        return 1
    fi
}

function driver() {
    # print function name
    decho -e "${TAB}${INVERT}${FUNCNAME}${RESET}"
    local -i DEBUG=0
    set_traps

    # -T  If set, the DEBUG and RETURN traps are inherited by shell functions.
    set -T
    trap 'echo "${FUNCNAME} return"' RETURN
    decho "shell options = $-"
    fello
}

function driver2() {
    driver
}

# -----------------------------------------------------------------------------------------------
# Functions for set and reset shell options
# -----------------------------------------------------------------------------------------------

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
    #huh
}

# print shell
function huh() {
    local -i DEBUG=1
    # print function name
    decho -e "${TAB}${INVERT}${FUNCNAME}?${RESET}"

    itab
    echo "${TAB}shell options: $-"
    echo -n "${TAB}traps: "
    if [ -z "$(trap -p)" ]; then
        echo "${TAB}none"
    else
        echo
        itab
        trap -p
        dtab
    fi

    print_debug
    print_tab
    dtab
    if [ ${DEBUG-0} -gt 0 ]; then
        trap 'print_return $?; trap - RETURN' RETURN
    fi
    return 0
}

# -----------------------------------------------------------------------------------------------
# Functions for print EXIT, RETURN, and INT (non-ERR) status
# -----------------------------------------------------------------------------------------------

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
    echo -en "${BASH_SOURCE[(($N_BASH - 1))]##*/}${RESET} "
    print_elap
    echo -n " on "
    timestamp
}

# format timestamp
function timestamp() {
    echo "$(date +"%a %b %-d at %-l:%M %p %Z")"
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
            bash sec2elap $dT_sec | tr -d "\n"
        else
            echo -ne "elapsed time is ${WHITE}${dT_sec} sec${RESET}"
        fi
    else
        decho -ne "${YELLOW}start_time not defined${RESET} "
        # reset cursor position for print_done, etc.
        echo -en "\x1b[1D"
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
    echo -ne "${TAB}${YELLOW}\E[7m EXIT ${RESET} "
    # print exit code
    if [ ! -z ${EXIT_RETVAL+alt} ]; then
        echo -ne "${GRAY}RETVAL=${EXIT_RETVAL}${RESET} "
    fi

    print_done
}

function print_return() {
    # expected arguments are $?
    # e.g.
    # trap 'print_return $?' RETURN

    # set local debug value
    local -i DEBUG=${DEBUG:-0} # substitute default value if DEBUG is unset or null

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
        # trace RETURN and DEBUG traps (subshells inherit RETURN and DEBUG traps from shell)
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
        echo -en " ${GRAY}RETVAL=${RETURN_RETVAL}${RESET}"
    fi
    if [ ${N_FUNCs} -gt 1 ]; then
        echo " ${FUNCNAME[1]##*/}"
    else
        echo " ${BASH_SOURCE[0]##*/}"
    fi
}

function print_int() {
    start_new_line
    echo -e "${AZURE}\E[7m INT ${RESET} ${BASH_SOURCE[1]##*/}"
    echo " Ctrl-C pressed"
    #	echo -e "breaking..."
    #	break
}

# -----------------------------------------------------------------------------------------------
# Print ERR trace
# -----------------------------------------------------------------------------------------------

function print_error() {
    # expected arguments are $LINENO $? $BASH_COMMAND
    # e.g.
    # trap 'print_error $LINENO $? $BASH_COMMAND' ERR
    DEBUG=3

    decho -e "${TAB}\E[37;41m${FUNCNAME}${RESET}"

    TAB=${TAB=''}
    local ERR_PRINT=$(echo -e "${TAB}\E[37;41m ERROR ${RESET} ")
    [ $DEBUG -gt 0 ] && start_new_line
    hline

    eTAB=$(echo -e "${RED}|${RESET}")
    eTAB=$fTAB

    TAB+=$eTAB

    # substitute default value if DEBUG is unset or null
    local -i DEBUG=${DEBUG:-0}
    # set manually
    #DEBUG=2
    local -i funcDEBUG=$DEBUG

    # parse arguments
    local -i ERR_LINENO=$1
    shift
    local -i ERR_RETVAL=$1
    shift
    local ERR_CMD="$@"

    # print arguments
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

        if [[ ${BASH_SOURCE[${N_BOTTOM}]} == "bash" ]]; then
            ddecho "${TAB}bottom of stack is bash"
        else
            ddecho "${TAB}bottom of stack is NOT bash"
        fi
        dtab

        # get the command that casued the error
        local ERR_LINE=
        decho "${TAB}getting line that caused error..."
        itab

        local -i parent=0
        if [ $N_BASH -gt 0 ]; then
            parent=1
        fi

        # check if error came from shell
        if [[ ${BASH_SOURCE[${N_BOTTOM}]} == "bash" ]];then # || \
            ddecho "${TAB}error did not come from a file, it came from ${SHELL##*/}"
            local ERR_LINE=$ERR_CMD
        else
            # not bash
            ddecho "${TAB}base of stack is not bash"
            # check that error came from a file
            if [ -f "${BASH_SOURCE[$parent]}" ]; then
                ddecho "${TAB}BASH_SOURCE[$parent] is a file"
                # check that value is not empty
                if [ ! -z "${BASH_SOURCE[$parent]}" ]; then
                    ddecho "${TAB}BASH_SOURCE[$parent] is not empty"
                    if [ $N_FUNC -gt 1 ] ; then
                        # print source
                        ddecho "${TAB}BASH_SOURCE[$parent] = ${BASH_SOURCE[$parent]}"
                        if [[ ! ${BASH_SOURCE[${parent}]} =~ ".bashrc" ]]; then
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
        echo -ne " ${GRF}${FUNCNAME[N_BOTTOM]}${RESET}${GRS}:${RESET} "
    else
        if [ ${N_BOTTOM} -eq 0 ]; then
            echo -ne " ${GRF}${SHELL##*/}${RESET}${GRS}:${RESET}${GRL}${ERR_LINENO}${RESET}${GRS}:${RESET}"
        else
            if [[ ${BASH_SOURCE[${parent}]} =~ ".bashrc" ]]; then
                ERR_LINENO=$(grep -n "${ERR_LINE}" ${BASH_SOURCE[${parent}]} | sed 's/:.*//')
            fi
            echo -ne " ${GRF}${BASH_SOURCE[${parent}]##*/}${RESET}${GRS}:${RESET}${GRL}${ERR_LINENO}${RESET}${GRS}:${RESET}"
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
        echo "${ERR_CMD}"
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
    hline
}

# -----------------------------------------------------------------------------------------------
# Functions to set and unset traps
# -----------------------------------------------------------------------------------------------

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
}

function print_traps() {
    # set local debug value
    if [ $# -eq 1 ]; then
        # use argument to manually set DEBUG
        local -i DEBUG=$1
    else
        # substitute default value if DEBUG is unset or null
        local -i DEBUG=${DEBUG:-2}
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
        ddecho $(trap -p) | sed "s/^/${TAB}/;s/ \(trap\)/\n${TAB}\1/g"
        start_new_line
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
        local -i DEBUG=${DEBUG:-2} # substitute default value if DEBUG is unset or null
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
        if [ ${DEBUG} -gt 0 ]; then
            decho "traps:"
            decho -e "${INVERT}normal print:${NORMAL}"

            decho -e "${INVERT}traps echo:${NORMAL}"
            echo $(trap -p) | sed "s/ \(trap -- \)/\n\1/g"

            decho -e "${INVERT}traps echo sig:${NORMAL}"
            echo $(trap -p) | sed "s/ \(trap -- \)/\n\1/g" |  sed 's/.* //'

            decho -e "${INVERT}traps echo sig tr:${NORMAL}"
            echo $(trap -p) | sed "s/ \(trap -- \)/\n\1/g" |  sed 's/.* //' | tr  '\n' ' '
            decho
        fi
        export sig="$(echo $(trap -p) | sed "s/ \(trap -- \)/\n\1/g" |  sed 's/.* //' | tr  '\n' ' ')"
        if [ ${DEBUG} -gt 0 ]; then
            decho "sig = ${sig}"
            decho "sig@ = ${sig[@]}"
            decho "#sig = ${#sig[@]}"
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
        local -i DEBUG=${DEBUG:-2} # substitute default value if DEBUG is unset or null
    fi
    # print summary
    ddecho -n "${TAB}on ${FUNCNAME[1]} return, "
    print_traps
    if [ ! -z "$(trap -p)" ]; then
        echo -e "${TAB}${BAD}traps not cleared${RESET}"
        dtab
        get_sigs
        return 1
    fi
    dtab
}

do_clear() {
    local -a sig
    get_sigs
    if [ ! -z "${sig}" ]; then
        decho "sig = ${sig}"
        # clear traps
        for itrap in ${sig[@]}; do
            decho "${TAB}unsetting trap $itrap..."
            trap - $itrap
        done
    fi
}

# set ERR and EXIT traps
function set_traps() {
    # set local debug value
    if [ $# -eq 1 ]; then
        # use argument to manually set DEBUG
        local -i DEBUG=$1
    else
        local -i DEBUG=${DEBUG:-2} # substitute default value if DEBUG is unset or null
    fi

    [ $DEBUG -gt 0 ] && start_new_line
    TAB=${TAB=''}
    decho -e "${TAB}${MAGENTA}\E[7mset traps${RESET}"
    itab

    dddecho "${TAB}$-"
    # set shell options
    dddecho -n "${TAB}setting shell options... "
    # trace ERR (subshells inherit ERR trap from shell)
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
        local -i DEBUG=${DEBUG:-2} # substitute default value if DEBUG is unset or null
    fi

    [ $DEBUG -gt 0 ] && start_new_line
    decho -e "${TAB}${ORANGE}\E[7mset exit${RESET}"
    itab

    dddecho -n "${TAB}setting traps... "
    trap 'print_exit $?' EXIT
    dddecho "done"

    check_traps_set
}

# unset ERR and EXIT traps, saving current values
# restore saved values with reset_traps
function unset_traps() {
    # set local debug value
    if [ $# -eq 1 ]; then
        # use argument to manually set DEBUG
        local -i DEBUG=$1
    else
        local -i DEBUG=${DEBUG:-2} # substitute default value if DEBUG is unset or null
    fi

    [ $DEBUG -gt 0 ] && start_new_line
    decho -e "${TAB}${CYAN}\E[7mun-set traps${RESET}"
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

}

# reset saved traps
# used in conjuction with unset_traps
function reset_traps() {
    # set local debug value
    if [ $# -eq 1 ]; then
        # use argument to manually set DEBUG
        local -i DEBUG=$1
    else
        local -i DEBUG=${DEBUG:-2} # substitute default value if DEBUG is unset or null
    fi

    [ $DEBUG -gt 0 ] && start_new_line
    decho -e "${TAB}${GREEN}\E[7mreset traps${RESET}"
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
        local -i DEBUG=${DEBUG:-2} # substitute default value if DEBUG is unset or null
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
}

#
function enable_exit_on_fail() {
    trap 'echo "${TAB}${BASH_SOURCE[0]##*/}: line $LINENO: trapping ERR $BASH_COMMAND"; return 0 2>/dev/null' ERR
}

# Rrovide a way to cleanly exit on errors, whether the file is sourced or executed, that does not
# cause the shell to exit.
function exit_on_fail() {
    enable_exit_on_fail
    echo -e "${TAB}${YELLOW}\x1b[7m${BASH_SOURCE[1]##*/} failed\x1b[0m"
    lecho
    plecho

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
                #  huh
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

# test line echo
function tlecho() {
    echo -e "${TAB}${INVERT}${FUNCNAME}${RESET}"
    DEBUG=3
    itab
    lecho "$@"
    plecho "$@"
    dtab
    rdb
    trap 'print_return $?; trap - RETURN' RETURN
}
