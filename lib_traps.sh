function fello() {
    hello
    echo $-
    return 0    
}

function driver() {
    # -T  If set, the DEBUG and RETURN traps are inherited by shell functions.     
    set -T
    trap 'echo "${FUNCNAME} return"' RETURN
    local -i DEBUG=0
    local -i funcDEBUG=0
    set_traps
    echo $-
    fello
    return 0   
}

function driver2() {
    driver
    return 0
}

function set_shell() {
    echo $-
    set -eET
    echo $-
    trap 'echo "SET"' RETURN
    return 0
}

function unset_shell() {
    echo $-
    set +eET
    echo $-
    trap 'echo "UNSET"' RETURN
    return 0
    
}

# print function stack
function print_stack() {
    start_new_line
    local -ir DEBUG=2

    echo "BASH_COMMAND = $BASH_COMMAND"
    echo "BASH_SUBSHELL = $BASH_SUBSHELL"
    
    # get length of function stack
    local -ir N_FUNC=${#FUNCNAME[@]}
    local -ir N_BASH=${#BASH_SOURCE[@]}
    local -ir N_LINE=${#BASH_LINENO[@]}

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

    echo "${TAB}call stack:"

    local -i i
    for ((i = 0; i < $N_FUNC ; i++)); do
        echo "$i ${FUNCNAME[i]} ${BASH_SOURCE[i]} ${BASH_LINENO[i]}"
    done

    # set local debug value
    local DEBUG=${DEBUG:=0}  # default value if DEBUG is unset or null

    # get length of stack
    local -i N=${#BASH_SOURCE[@]}
    echo "${TAB}There are N=$N entries in the call stack"

    echo "${TAB}full bash source:"
    echo "${TAB}${fTAB}BASH_SOURCE[@] = ${BASH_SOURCE[@]}"

    echo "${TAB}this source:"
    echo "${TAB}${fTAB}BASH_SOURCE[0] = ${BASH_SOURCE[0]}"

    if [ $N -gt 1 ]; then
        echo "${TAB}invoking source source:"
        echo "${TAB}${fTAB}BASH_SOURCE[1] = ${BASH_SOURCE[1]}"
    fi

    return 0    

    echo "BASH_ARGC = $BASH_ARGC"
    echo "BASH_ARGV = $BASH_ARGV"
    
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

# print source name, elapsed time, and timestamp
function print_done() {
    echo -en "${BASH_SOURCE[(($N - 1))]##*/}${NORMAL} "
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
            echo -ne "elapsed time is ${white}${dT_sec} sec${NORMAL}"
        fi
    else
        decho -ne "${yellow}start_time not defined${NORMAL} "
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
    echo -ne "${TAB}${yellow}\E[7m EXIT ${NORMAL} "
    # print exit code
    if [ ! -z ${EXIT_RETVAL+alt} ]; then
        echo -ne "${gray}RETVAL=${EXIT_RETVAL}${NORMAL} "
    fi

    print_done
}

function print_error() {
    # expected arguments are $LINENO $? $BASH_COMMAND
    # e.g.
    # trap 'print_error $LINENO $? $BASH_COMMAND' ERR

    # parse arguments
    local -i ERR_LINENO=$1
    shift
    local -i ERR_RETVAL=$1
    shift
    local ERR_CMD="$@"

    # print summary
    start_new_line
    local ERR_PRINT=$(echo -e "${TAB}\E[37;41m ERROR ${NORMAL} ")
    echo -n ${ERR_PRINT}
    # print grep-like line match
    echo -ne " \E[35m${BASH_SOURCE[1]##*/}\E[m\E[36m:\E[m\E[32m${ERR_LINENO}\E[m\E[36m:\E[m"

    # get the cursor position
    echo -en "\E[6n"
    read -sdR CURPOS
    local CURPOS=${CURPOS#*[}
          #}# dummy bracket for emacs indenting
    # get the x-position of the cursor
    local -i x_pos=${CURPOS#*;}
    ((--x_pos))
    ((--x_pos))
    # define indent
    local spx=$(echo -ne "\E[${x_pos}C")

    print_stack

    # get length of function stack
    local -ir N_FUNC=${#FUNCNAME[@]}

    if [ -f "${BASH_SOURCE[1]}" ]; then
        echo "${BASH_SOURCE[1]} is a file"
        if [ ! -z "${BASH_SOURCE[1]}" ]; then
            echo "${BASH_SOURCE[1]} variable is not empty"
            if [ $N_FUNC -gt 1 ] ; then
                echo "there are functions in stack"
                local ERR_LINE
                
                echo "${ERR_LINENO}p"
                echo "${BASH_SOURCE[1]}"
                echo ${BASH_SOURCE[1]}
                echo sed -n "${ERR_LINENO}p" "${BASH_SOURCE[1]}"
                sed -n "${ERR_LINENO}p" "${BASH_SOURCE[1]}"
                
                ERR_LINE=$(sed -n "${ERR_LINENO}p" "${BASH_SOURCE[1]}" | sed "s/^\s*//") && echo "OK"
            fi
        fi
    fi
    
    echo "${ERR_LINE}"
    eva='eval'
    local -i evl=${#eva}
    local -i etab=$((x_pos - evl))

    if [[ "$ERR_CMD" != "$ERR_LINE" ]]; then
        echo -ne "\E[${etab}C${yellow} cmd\E[36m:\E[m"
        echo "${ERR_CMD}"
    fi

    # if command contains vairables, evaluate expression
    if [[ "$ERR_CMD" =~ '$' ]]; then
        decho -e "${spx} ${gray}expanding variables...${NORMAL}"
        # print 'eval' set back from cmd
        echo -ne "\E[${etab}C${VALID}${eva}\E[m\E[36m:\E[m"
        # print evaluated command and remove leading whitespace
        eval echo $ERR_CMD | sed "s/^\s*//"
        if [[ "$ERR_CMD" =~ "/dev/null" ]]; then
            # remove redirect
            NCMD=$(echo $ERR_CMD | sed 's|/dev/null||')
            NCMD=$(echo $NCMD | sed 's/[12>&\s]*$//')
            # print evaluated ouput
            echo -en "\E[${spx}C"
            eval echo $NCMD
            echo -e "$ {gray}redirect (no output)${NORMAL}"
        fi
    fi
    echo -e "${spx} ${gray}RETVAL=${ERR_RETVAL}${NORMAL}"
}

function print_int() {
    start_new_line
    echo -e "${yellow}\E[7m INT ${NORMAL} ${BASH_SOURCE[1]##*/}"
    echo " Ctrl-C pressed"
    #	echo -e "breaking..."
    #	break
}

function print_return() {
    # set local debug value
    local DEBUG=${DEBUG:=0}  # default value if DEBUG is unset or null

    # set shell options
    decho "setting shell options..."
    # trace ERR (inherit ERR trap from shell)
    #set -E
    # trace RETURN and DEBUG traps
    #set -T
    decho "done"

    RETURN_RETVAL=$1
    
    start_new_line

    # get size of function stack
    local -ir N_FUNC=${#FUNCNAME[@]}
    
    echo -e "${yellow}\E[7m RETURN ${NORMAL} ${gray}RETVAL=${RETURN_RETVAL}${NORMAL} ${FUNCNAME[$((N_FUNC-2))]}"
}

# define traps
function set_traps() {
    # set local debug value
    local DEBUG=${DEBUG:-0} # substitue default value if DEBUG is unset or null

    # turn in-function debugging on/off
    local -i funcDEBUG=${funcDEBUG:-${DEBUG}}
    # manual setting
    #funcDEBUG=1    

    [ $DEBUG -gt 0 ] && start_new_line
    decho -e "${magenta}\E[7mset traps${NORMAL}"

    # set shell options
    ddecho -n "setting shell options..."
    # exit on errors
    #set -e
    # trace ERR (inherit ERR trap from shell)
    #set -E
    # trace RETURN and DEBUG traps
    #set -T
    ddecho "done"
    
    ddecho "the following traps are saved"
    if [ -z "${save_traps+default}" ]; then
        ddecho "${fTAB}none"
        ddecho -n "setting traps... "
        trap 'print_error $LINENO $? $BASH_COMMAND' ERR
        trap 'print_exit $?' EXIT
        ddecho "done"
    else
        ddecho "${save_traps}" | sed "$ ! s/^/${fTAB}/"
        ddecho "setting saved traps..."
        eval $(echo "${save_traps}" | sed "s/$/;/g")
    fi

    # print summary
    ddecho -n "on set trap retrun, the following traps are set"
    if [ -z "$(trap -p)" ]; then
        ddecho -e "\n${fTAB}none"
        exit
    else
        ddecho
        ddecho $(trap -p) | sed "$ ! s/^/${fTAB}/;s/ \(trap\)/\n${fTAB}\1/g" | sed 's/^[ ]*$//g'
    fi
}

function unset_traps() {
    # set local debug value
    local DEBUG=${DEBUG:=0} # default value if DEBUG is unset or null

    [ $DEBUG -gt 0 ] && start_new_line
    decho -e "${cyan}\E[7mun-set traps${NORMAL}"

    # set shell options
    decho -n "setting shell options... "
    # trace ERR (inherit ERR trap from shell)
    #set -E
    # trace RETURN and DEBUG traps
    #set -T
    # DO NOT exit on errors
    set +e
    #    set +eE
    decho "done"

    ddecho -n "the current traps are set"
    if [ -z "$(trap -p)" ]; then
        ddecho -e "\n${fTAB}none"
    else
        ddecho
        ddecho $(trap -p) | sed "$ ! s/^/${fTAB}/;s/ \(trap\)/\n${fTAB}\1/g" | sed 's/^[ ]*$//g'

        # save traps
        export save_traps=$(trap -p | sed 's/-- //g')
        if [ ! -z "${save_traps}" ]; then
            ddecho "the current traps are saved"
            ddecho "${save_traps}" | sed "$ ! s/^/${fTAB}/"
        fi

        # clear traps
        trap - ERR
        trap - EXIT
        trap - RETURN
    fi

    # print summary
    ddecho "on unset trap retrun, the following traps are set"
    if [ -z $(trap -p) ]; then
        ddecho "${fTAB}none"
    else
        ddecho $(trap -p)
        exit
    fi
}
