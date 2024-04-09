#!/bin/bash -eu
# -----------------------------------------------------------------------------------------------
# TRAPS LIBRARY
# -----------------------------------------------------------------------------------------------
#
# ~/config/lib_traps.sh
#
# PURPOSE: define fuctions to set and unset traps, setting and reseting shell options, and
#   printing the contents of the execution stack. Includes functions for printing timestamps and
#   return values.
#
# -----------------------------------------------------------------------------------------------

function fello() {
    hello
    echo $-
}

function driver() {
    # -T  If set, the DEBUG and RETURN traps are inherited by shell functions.     
    set -T
    trap 'echo "${FUNCNAME} return"' RETURN
    local -i DEBUG=0
    set_traps
    echo $-
    fello
}

function driver2() {
    driver
}

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

        if [ $(echo ${old_opts} | grep ${opt}) ]; then
            decho "${opt} was set:${old_opts}"
            if [ $(echo ${new_opts} | grep ${opt}) ]; then
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
            if [ $(echo ${new_opts} | grep ${opt}) ]; then
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
    decho
    # ) | sed '1d' | column -t -s':' -o': ' -R 1 | sed '1 s/^/\n/' | sed "s/^/  /"
    set -T
    decho "done: $-"
    dtab
    #huh
}

function huh() {
    echo "-> $-"
}

# print function stack
function print_stack_devel() {    
    local -ir DEBUG=2
    print_stack
    
    echo "BASH_ARGC = $BASH_ARGC"
    echo "BASH_ARGV = $BASH_ARGV"
    echo "BASH_COMMAND = $BASH_COMMAND"
    echo "BASH_SUBSHELL = $BASH_SUBSHELL"    
    
    echo "${TAB}list of invocations (links):"
    (
        if [ $N_BASH -gt 1 ]; then
            for ((i = 1; i < $N_BASH; i++)); do
                vecho "$((i - 1)):+${BASH_SOURCE[$((i - 1))]}+invoked by+${BASH_SOURCE[$i]}"
            done
        else
            called_by=$(ps -o comm= $PPID)
            echo "0:+${BASH_SOURCE[0]}+invoked by+${called_by}"
        fi
    ) | column -t -s + -o " " | sed "s,${BASH_SOURCE[0]},\x1b[1;36m&\x1b[0m,;s,${BASH_LINK[0]},\x1b[0;33m&\x1b[0m,;s/^/${TAB}${fTAB}/"

    echo "${TAB}list of invocations (canonicalized):"
    (
        if [ $N_BASH -gt 1 ]; then
            for ((i = 1; i < $N_BASH; i++)); do
                vecho "$((i - 1)):+${BASH_LINK[$((i - 1))]}+invoked by+${BASH_LINK[$i]}"
            done
        else
            called_by=$(ps -o comm= $PPID)
            echo "0:+${BASH_LINK[0]}+invoked by+${called_by}"
        fi
    ) | column -t -s + -o " " | sed "s,${BASH_SOURCE[0]},\x1b[1;36m&\x1b[0m,;s,${BASH_LINK[0]},\x1b[0;33m&\x1b[0m,;s/^/${TAB}${fTAB}/"
    
    
    if [ $N_BASH -gt 1 ]; then
        echo "${TAB}invoking source source:"
        itab
        echo "${TAB}BASH_SOURCE[1] = ${BASH_SOURCE[1]##*/}"
        echo "${TAB}BASH_SOURCE[(($N_BASH-1))] = ${BASH_SOURCE[$N_BASH-1]##*/}"
        dtab
    fi
    
    itab
    # (
    #     for ((i = 0; i < $N_FUNC; i++)); do
    #         echo -n ":defined in: ${BASH_SOURCE[$i]##*/} "
    #         echo -n ":called by: "
    #         if [ -z "${BASH_SOURCE[$i+1]}" ]; then
    #             echo -n "NULL " 
    #         else
    #             echo -n "${BASH_SOURCE[$i+1]##*/} "
    #         fi
    #     done
    # ) | column -t -s : -o ""
    dtab
    
    if [ ${#FUNCNAME[@]} -gt 1 ]; then
        echo "${TAB}FUNCNAME[$((N_FUNC-2))]=${FUNCNAME[$((N_FUNC-2))]}"
    fi

    # print_stack() should only be called from other functions, so the length of FUNCNAME should
    # always be 2 or greater. Start with color 0
    local -ir IDX=$(( N_FUNC - 2 ))    
    
    # set color
    echo -ne "${dcolor[IDX]}"

    local -i x
    local -i start
    local -i stop

    # define function stack printing limits
    if [[ ${FUNCNAME[1]} == "xecho" ]]; then
        # xecho
        start=$(( $N_FUNC - 2 ))
        stop=2
    else
        # function
        start=$(( $N_FUNC - 5 ))
        stop=0
    fi

    # print size of function stack
    echo -ne "$N_FUNC"

    # print debug type
    if [[ ${FUNCNAME[1]} == "xecho" ]]; then
        # xecho
        echo -ne "x"
    else
        # function
        echo -ne "f"
    fi

    if [[ ${FUNCNAME[$N_FUNC]} == "main"  ]]; then
        echo "main"
    fi

    for (( i=0; i<$N_FUNC; i++ )); do 
        echo "$i ${FUNCNAME[i]} defined in $(basename ${BASH_SOURCE[i]}) on line ${BASH_LINENO[i]}; and called from $(basename ${BASH_SOURCE[i+1]})"
    done

    # print contents of function stack...
    echo -en "["                
    for (( x=(( $N_FUNC - 0 )); x>-1; x-- )); do
        if [[ ${FUNCNAME[x]} == "xecho" ]]; then
            break
        fi
        echo -en "$(basename ${BASH_SOURCE[x+1]} 2>/dev/null) ${BASH_LINENO[x]}: ${FUNCNAME[x]}\e[0m${dcolor[IDX]} -> "
    done
    echo -en "$(basename ${BASH_SOURCE[x]}) ${BASH_LINENO[x]}] "
    
    echo
    
    # print contents of function stack...
    echo -en "["
    for (( x=$start; x>$stop; x-- )); do
        echo -en "$(basename ${BASH_SOURCE[x+1]}) ${BASH_LINENO[x]}: ${FUNCNAME[x]}\e[0m${dcolor[IDX]} -> "
    done
    echo -en "$(basename ${BASH_SOURCE[x]}) ${BASH_LINENO[x]}] "

}

function print_stack() {
    local -i DEBUG=9
    start_new_line
    # get length of function stack
    local -gi N_FUNC=${#FUNCNAME[@]}
    local -gi N_BASH=${#BASH_SOURCE[@]}
    local -gi N_LINE=${#BASH_LINENO[@]}

    # get color index
    local -i idx
    dbg2idx $N_BASH idx
    # set color
    echo -ne "${dcolor[idx]}"
    
    # print length of stack
    echo "${TAB}There are N=$N_FUNC entries in the execution call stack"
    
    # check that all stacks have the same length
    if [ $N_FUNC -ne $N_BASH ]; then 
        echo "${TAB}There are N=$N_BASH entries in the source file name stack"
    fi

    if [ $N_FUNC -ne $N_LINE ]; then 
        echo "${TAB}There are N=$N_LINE entries in the line number stack"
    fi

    if [ $N_FUNC -ne $N_BASH ] || [ $N_FUNC -ne $N_LINE ]; then 
        echo "${TAB}${N_FUNC} functions, ${N_BASH} files, ${N_LINE]} lines"
    fi

    local -ga BASH_LINKS
    # resolve symbolic links
    for ((i = 0; i < $N_BASH; i++)); do
        BASH_LINK[$i]="$(readlink -f "${BASH_SOURCE[$i]}")"
    done

    local -ga BASH_FNAME
    # strip directories
    for ((i = 0; i < $N_BASH; i++)); do
        BASH_FNAME[$i]=${BASH_SOURCE[$i]##*/}
    done

    # print call stack
    echo "${TAB}call stack:"
    local -i i
    itab

    if false; then
        (
            for ((i = 0; i < $N_FUNC ; i++)); do
                echo "$i:${FUNCNAME[i]}:${BASH_FNAME[i]}:${BASH_LINENO[i]}"
            done
        ) | column -t -s: -N "index,function,source,line no" -R1 | sed "s/^/${TAB}/"
        echo

        # set color
        ((idx++))
        echo -ne "${dcolor[idx]}"
        (
            for ((i = 0; i < $N_FUNC ; i++)); do
                echo "$i:${FUNCNAME[i]}:${BASH_SOURCE[i]}:${BASH_LINENO[i]}"
            done
        ) | column -t -s: -N "index,function,source,line no" -R1 | sed "s/^/${TAB}/"
        echo

        # set color
        ((idx++))
        echo -ne "${dcolor[idx]}"
        (
            for ((i = 0; i < $N_FUNC ; i++)); do
                echo "$i:${FUNCNAME[i]}:${BASH_LINK[i]}:${BASH_LINENO[i]}"
            done
        ) | column -t -s: -N "index,function,source,line no" -R1 | sed "s/^/${TAB}/"
        echo

        # set color
        ((idx++))
        echo -ne "${dcolor[idx]}"    
        (
            for ((i = 0; i < $N_FUNC ; i++)); do
                echo "$i:${FUNCNAME[i]}:${BASH_SOURCE[i]}:${BASH_LINENO[i]}"
                if [[ "${BASH_SOURCE[$i]}" != "${BASH_LINK[$i]}" ]]; then
                    decho -ne "$i:${FUNCNAME[i]}:${BASH_LINK[i]}:${BASH_LINENO[i]}"
                    echo -e "${dcolor[idx]}"
                fi
            done
        ) | column -t -s: -N "index,function,source,line no" -R1 | sed "s/^/${TAB}/"
        echo
    fi
    
    # get directories
    local -ga BASH_DIR
    for ((i = 0; i < $N_BASH; i++)); do
        BASH_DIR[$i]="$(dirname "${BASH_SOURCE[$i]}")"
    done

    # get directories
    local -ga BASH_LINK_DIR
    for ((i = 0; i < $N_BASH; i++)); do
        BASH_LINK_DIR[$i]="$(dirname "${BASH_LINK[$i]}")"
    done   

    (
        for ((i = 0; i < $N_FUNC ; i++)); do
            # print stack element
            echo "$i:${FUNCNAME[i]}:${BASH_DIR[i]}:${BASH_FNAME[i]}:${BASH_LINENO[i]}"
            # check if source is linked
            if [[ "${BASH_SOURCE[$i]}" != "${BASH_LINK[$i]}" ]]; then
                # set color
                ((idx++))
                echo -ne "${dcolor[idx]}"
                # print link
                echo -ne "$i:${FUNCNAME[i]}:${BASH_LINK_DIR[i]}:${BASH_LINK[i]##*/}:${BASH_LINENO[i]}"
                # reset color
                ((idx--))
                echo -e "${dcolor[idx]}"
            fi
        done
    ) | column -t -s: -N "index,function,directory,source,line no" -R1 | sed "s/^/${TAB}/" 

    dtab
    # unset color
    echo -ne "\e[0m"
}

function print_invo() {
    local DEBUG=9
    print_stack
    # since print stack is itself part of the stack, remove the top of the stack
    ((N_FUNC--))
    ((N_BASH--))    
    ((N_LINE--))    
    
    echo "${TAB}invocations:"
    itab
    for ((i = 0; i < (($N_FUNC -2)); i++)); do
        echo "${TAB}$i: ${FUNCNAME[i]} ${BASH_FNAME[i]} ${BASH_LINENO[i+1]}"
    done
    dtab
    
    echo "${TAB}shell function invocations:"
    itab
    for ((i = 0; i < (($N_FUNC - 1 )); i++)); do
        echo "${TAB}$i: ${FUNCNAME[i]} ${BASH_FNAME[i]} ${BASH_LINENO[i+1]}"
    done
    dtab

    echo "caller:"
    caller

    (
        itab
        for ((i = 0; i < (($N_FUNC + 1 )); i++)); do
            echo -en "${TAB}$i\t"
            caller $i
        done
        dtab
    ) | column -t -s "\t" -N "index,line,subroutine,filename" -R1

    local -i N_BOTTOM=$(($N_FUNC - 1))
    echo "N_BOTTOM = $N_BOTTOM"
    echo "FUNCNAME[$N_BOTTOM] = ${FUNCNAME[$N_BOTTOM]}"
    
    if [[ "${FUNCNAME[$N_BOTTOM]}" =~ "main" ]]; then
        echo "bottom of stack is main"
        echo "root invocation ${BASH_FNAME[$N_BOTTOM]} is a script"
    else
        echo "bottom of stack is not main: ${FUNCNAME[$N_BOTTOM]}"
        echo "root invocation ${BASH_FNAME[$N_BOTTOM]} is a shell function"
    fi

    echo "${TAB}script invocations:"
    itab
    for ((i = 0; i < (($N_FUNC-1)) ; i++)); do
        echo "${TAB}$i: ${FUNCNAME[i]} called from ${BASH_FNAME[i+1]} line ${BASH_LINENO[i]}"
    done
    dtab
    echo "${TAB}shell function invocations:"
    
    if [ $N_FUNC -gt 1 ]; then
        echo "${FUNCNAME[0]} (defined in ${BASH_FNAME[0]}) called from ${FUNCNAME[1]} (defined in ${BASH_FNAME[1]}) line ${BASH_LINENO[0]}"
    else
        echo "${FUNCNAME[0]} (defined in ${BASH_FNAME[0]}) called from $SHELL line ${BASH_LINENO[0]}"
    fi

    itab
    for ((i = 0; i < $N_FUNC ; i++)); do
        echo "${TAB}$i: ${FUNCNAME[i]} defined in ${BASH_FNAME[i]} ${BASH_LINENO[i]}"
    done
    dtab
    echo $LINENO
    dtab
}

# print source name, elapsed time, and timestamp
function print_done() {
    local -i N_BASH=${#BASH_SOURCE[@]}
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
    # get current time (end time)
    local -i end_time=$(date +%s%N)

    # check if start time is defined
    if [ -n "${start_time+alt}" ]; then
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

function print_error() {
    # expected arguments are $LINENO $? $BASH_COMMAND
    # e.g.
    # trap 'print_error $LINENO $? $BASH_COMMAND' ERR

    local -i DEBUG=9
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
    ddecho "${TAB}FUNCNAME[$N_BOTTOM] = ${FUNCNAME[$N_BOTTOM]}"
    ddecho "${TAB}BASH_SOURCE[$N_BOTTOM] = ${BASH_SOURCE[$N_BOTTOM]}"
    if [[ ${BASH_SOURCE[${N_BOTTOM}]} =~ "bash" ]]; then
        ddecho "${TAB}bottom of stack is bash"
    else
        ddecho "${TAB}bottom of stack is NOT bash"
    fi
    dtab

    # get the command that casued the error
    local ERR_LINE=
    decho "${TAB}getting line that caused error..."
    itab

    # check if error came from shell
    if [[ ${BASH_SOURCE[${N_BOTTOM}]} =~ "bash" ]]; then
        # bash
        ddecho "${TAB}error did not come from a file, it came from bash"
        local ERR_LINE=$ERR_CMD
    else
        # not bash
        ddecho "${TAB}base of stack is not bash"
        # check that error came from a file        
        if [ -f "${BASH_SOURCE[1]}" ]; then
            ddecho "${TAB}BASH_SOURCE[1] is a file"
            # check that value is not empty
            if [ ! -z "${BASH_SOURCE[1]}" ]; then
                ddecho "${TAB}BASH_SOURCE[1] is not empty"
                if [ $N_FUNC -gt 1 ] ; then
                    # print source
                    ddecho "${TAB}BASH_SOURCE[1] = ${BASH_SOURCE[1]}"
                    # print line number
                    ddecho "${TAB}ERR_LINE = ${ERR_LINENO}"
                    # print summary
                    ddecho -n "${TAB}line ${ERR_LINENO} in ${BASH_SOURCE[1]}: "
                    ddecho sed -n "${ERR_LINENO}p" "${BASH_SOURCE[1]}"
                    decho "${TAB}"$(sed -n "${ERR_LINENO}p" "${BASH_SOURCE[1]}")
                    # save offending line
                    ERR_LINE=$(sed -n "${ERR_LINENO}p" "${BASH_SOURCE[1]}" | sed "s/^\s*//")
                else
                    ddecho "${TAB}BASH_SOURCE[1] is EMPTY"
                    ERR_LINE="EMPTY"
                fi
            fi
        else
            ddecho "${TAB}error did not come from a file"            
            ERR_LINE="$ERR_CMD"
        fi       
    fi
    dtab
    
    # print summary
    start_new_line
    local ERR_PRINT=$(echo -e "${TAB}\E[37;41m ERROR ${RESET} ")
    echo -n ${ERR_PRINT}
    
    # print grep-like line match    
    if [[ ${BASH_SOURCE[${N_BOTTOM}]} =~ "bash" ]]; then
        echo -ne " ${GRF}${FUNCNAME[N_BOTTOM]}${RESET}${GRS}:${RESET} "
    else
        if [ ${N_BOTTOM} -eq 0 ]; then
            echo -ne " ${GRF}${SHELL}${RESET}${GRS}:${RESET}${GRL}${ERR_LINENO}${RESET}${GRS}:${RESET}"
        else
            echo -ne " ${GRF}${BASH_SOURCE[1]##*/}${RESET}${GRS}:${RESET}${GRL}${ERR_LINENO}${RESET}${GRS}:${RESET}"
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
    
    if [[ ${BASH_SOURCE[${N_BOTTOM}]} =~ "bash" ]]; then
        # bash
        :
    else
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
}

function print_int() {
    start_new_line
    echo -e "${YELLOW}\E[7m INT ${RESET} ${BASH_SOURCE[1]##*/}"
    echo " Ctrl-C pressed"
    #	echo -e "breaking..."
    #	break
}

function print_return() {
    # expected arguments are $?
    # e.g.
    # trap 'print_return $?' RETURN
    
    # set local debug value
    local -i DEBUG=${DEBUG:-0} # substitute default value if DEBUG is unset or null

    if false; then
        ddecho "${TAB}$-"
        # set shell options
        ddecho -n "${TAB}setting shell options... "
        # trace RETURN and DEBUG traps (subshells inherit RETURN and DEBUG traps from shell)
        #set -T
        ddecho "done"
        ddecho "${TAB}$-"
    fi

    RETURN_RETVAL=$1    

    # get size of function stack
    local -ir N_FUNC=${#FUNCNAME[@]}
    # print summary
    start_new_line
    echo -e "${TAB}${YELLOW}\E[7m RETURN ${RESET} ${GRAY}RETVAL=${RETURN_RETVAL}${RESET} ${FUNCNAME[$((N_FUNC-2))]}"
}

# define traps
function reset_traps() {
    # set local debug value
    local -i DEBUG=${DEBUG:-2} # substitute default value if DEBUG is unset or null

    [ $DEBUG -gt 0 ] && start_new_line
    decho -e "${TAB}${GREEN}\E[7mreset traps${RESET}"
    itab

    ddecho "${TAB}$-"
    # set shell options
    ddecho -n "${TAB}setting shell options... "
    # trace ERR (subshells inherit ERR trap from shell)
    set -E
    ddecho "done"
    ddecho "${TAB}$-"
    
    ddecho "${TAB}the following traps are saved"
    itab
    if [ -z "${save_traps+default}" ]; then
        ddecho "${TAB}none"
        dtab
    else
        ddecho "${save_traps}" | sed "s/^/${TAB}/"
        dtab
        ddecho -n "${TAB}setting saved traps..."
        eval $(echo "${save_traps}" | sed "s/$/;/g")
        ddecho "done"
    fi

    # print summary
    decho "${TAB}on set trap return, the following traps are set"
    itab
    if [ -z "$(trap -p)" ]; then
        decho -e "${TAB}none"
    else
        decho
        decho $(trap -p) | sed "s/^/${TAB}/;s/ \(trap\)/\n${TAB}\1/g" # | sed 's/^[ ]*$//g'
    fi
    dtab 2
}

function set_traps() {
    # set local debug value
    local -i DEBUG=${DEBUG:-2} # substitute default value if DEBUG is unset or null

    [ $DEBUG -gt 0 ] && start_new_line
    decho -e "${TAB}${MAGENTA}\E[7mset traps${RESET}"
    itab

    ddecho "${TAB}$-"
    # set shell options
    ddecho -n "${TAB}setting shell options... "
    # trace ERR (subshells inherit ERR trap from shell)
    set -E
    ddecho "done"
    ddecho "${TAB}$-"
    
    ddecho -n "${TAB}setting traps... "
    trap 'print_error $LINENO $? $BASH_COMMAND' ERR
    trap 'print_exit $?' EXIT
    ddecho "done"

    # print summary
    decho -n "${TAB}on set trap return, the following traps are set"
    itab
    if [ -z "$(trap -p)" ]; then
        decho -e "\n${TAB}none"
        echo "something didn't work..."
        dtab
        return 1
    else
        decho
        decho $(trap -p) | sed "s/^/${TAB}/;s/ \(trap\)/\n${TAB}\1/g" # | sed 's/^[ ]*$//g'
    fi
    dtab 2
}

function clear_traps() {
    # set local debug value
    local -i DEBUG=${DEBUG:-2} # default value if DEBUG is unset or null

    [ $DEBUG -gt 0 ] && start_new_line
    decho -e "${TAB}${YELLOW}\E[7mclear traps${RESET}"
    itab

    ddecho "${TAB}$-"
    # set shell options
    ddecho -n "${TAB}setting shell options... "
    # trace ERR (inherit ERR trap from shell)
    set -E
    # trace RETURN and DEBUG traps
    set -T
    # DO NOT exit on errors
    set +e
    ddecho "done"
    ddecho "${TAB}$-"
    
    ddecho -n "${TAB}the current traps are set"
    if [ -z "$(trap -p)" ]; then
        ddecho -e "\n${TAB}${fTAB}none"
        dtab
        return 0
    else
        itab
        ddecho
        ddecho $(trap -p) | sed "$ ! s/^/${TAB}/;s/ \(trap\)/\n${TAB}\1/g" | sed 's/^[ ]*$//g'
        dtab
    fi

    # clear save traps
    unset save_traps

    # clear traps
    trap - ERR
    trap - EXIT
    trap - RETURN

    # print summary
    decho "${TAB}on clear trap return, the following traps are set"
    if [ -z $(trap -p) ]; then
        decho "${TAB}${fTAB}none"
    else
        ddecho $(trap -p) | sed "$ ! s/^/${TAB}/;s/ \(trap\)/\n${TAB}\1/g" | sed 's/^[ ]*$//g'
        echo "something didn't work..."
        dtab
        return 1
    fi
    dtab
}

function unset_traps() {
    # set local debug value
    local -i DEBUG=${DEBUG:-2} # default value if DEBUG is unset or null

    [ $DEBUG -gt 0 ] && start_new_line
    decho -e "${TAB}${CYAN}\E[7mun-set traps${RESET}"
    itab

    ddecho "${TAB}$-"
    # set shell options
    ddecho -n "${TAB}setting shell options... "
    # trace ERR (inherit ERR trap from shell)
    set -E
    # trace RETURN and DEBUG traps
    set -T
    # DO NOT exit on errors
    set +e
    ddecho "done"
    ddecho "${TAB}$-"
    
    ddecho "${TAB}the current traps are set"
    if [ -z "$(trap -p)" ]; then
        ddecho -e "${TAB}${fTAB}none"
        unset save_traps
        dtab
        return 0
    else
        itab
        ddecho $(trap -p) | sed "s/^/${TAB}/;s/ \(trap\)/\n${TAB}\1/g"
        dtab
        
        # save traps
        export save_traps=$(trap -p | sed 's/-- //g')
        if [ ! -z "${save_traps}" ]; then
            ddecho "${TAB}the current traps are saved"
            itab
            ddecho "${save_traps}" | sed "s/^/${TAB}/"
            dtab
        fi

        # clear traps
        trap - ERR
        trap - EXIT
        trap - RETURN
    fi

    # print summary
    decho "${TAB}on unset trap return, the following traps are set"
    if [ -z $(trap -p) ]; then
        decho "${TAB}${fTAB}none"
    else
        decho $(trap -p) | sed "$ ! s/^/${TAB}/;s/ \(trap\)/\n${TAB}\1/g" | sed 's/^[ ]*$//g'
        echo "something didn't work..."
        dtab
        return 1
    fi
    dtab
}
