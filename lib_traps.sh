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

# reset shell options
function reset_shell() {
    local -i DEBUG=0

    if [ $# -lt 1 ]; then
        return 0
    fi

    local TAB=${TAB:='   '}
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
    echo "done: $-"
    #huh
}

function huh() {
    echo "-> $-"
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

    # print call stack
    echo "${TAB}call stack:"
    local -i i
    for ((i = 0; i < $N_FUNC ; i++)); do
        echo "$i ${FUNCNAME[i]} ${BASH_SOURCE[i]} ${BASH_LINENO[i]}"
    done
    return 0

    # resolve symbolic links
    for ((i = 0; i < $N; i++)); do
        BASH_LINK[$i]=$(readlink -f ${BASH_SOURCE[$i]})
    done
    
    echo "${TAB}list of sources:"
    (
        for ((i = 0; i < $N_BASH; i++)); do
            vecho -ne "$i:+${BASH_SOURCE[$i]}"
            if [[ "${BASH_SOURCE[$i]}" != "${BASH_LINK[$i]}" ]]; then
                vecho -e "+->+${BASH_LINK[$i]}"
            else
                vecho
            fi
        done
    ) | column -t -s + | sed "s,${BASH_SOURCE[0]},\x1b[1;36m&\x1b[0m,;s,${BASH_LINK[0]},\x1b[0;33m&\x1b[0m,;s/^/${TAB}${fTAB}/"

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

function print_stack2() {
    # get length of function stack
    local -gi N_FUNC=${#FUNCNAME[@]}
    local -gi N_BASH=${#BASH_SOURCE[@]}
    local -gi N_LINE=${#BASH_LINENO[@]}

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
        BASH_LINK[$i]=$(readlink -f ${BASH_SOURCE[$i]})
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
    (
        for ((i = 0; i < $N_FUNC ; i++)); do
            echo "$i:${FUNCNAME[i]}:${BASH_FNAME[i]}:${BASH_LINENO[i]}"
        done
    ) | column -t -s: -N "index,function,source,line no" -R1 | sed "s/^/${TAB}/"
    dtab
}

function print_invo() {
    local DEBUG=1

    # print invoking source or function
    decho "${TAB}BASH_SOURCE=${BASH_SOURCE[@]}"
    decho "${TAB}FUNCNAME=${FUNCNAME[@]}"
    decho "${TAB}BASH_LINENO=${BASH_LINENO[@]}"

    print_stack2

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

    # parse arguments
    local -i ERR_LINENO=$1
    shift
    local -i ERR_RETVAL=$1
    shift
    local ERR_CMD="$@"

    # print summary
    start_new_line
    local ERR_PRINT=$(echo -e "${TAB}\E[37;41m ERROR ${RESET} ")
    echo -n ${ERR_PRINT}
    # print grep-like line match
    echo -ne " \E[35m${BASH_SOURCE[1]##*/}\E[m\E[36m:\E[m\E[32m${ERR_LINENO}\E[m\E[36m:\E[m"

    # get the cursor position
    echo -en "\E[6n"
    read -sdR CURPOS
    local CURPOS=${CURPOS#*[}
          #}# dummy bracket for Emacs indenting
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
        echo -ne "\E[${etab}C${YELLOW} cmd\E[36m:\E[m"
        echo "${ERR_CMD}"
    fi

    # if command contains variables, evaluate expression
    if [[ "$ERR_CMD" =~ '$' ]]; then
        decho -e "${spx} ${GRAY}expanding variables...${RESET}"
        # print 'eval' set back from cmd
        echo -ne "\E[${etab}C${VALID}${eva}\E[m\E[36m:\E[m"
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
    # set local debug value
    local -i DEBUG=${DEBUG:-0} # substitute default value if DEBUG is unset or null

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
    
    echo -e "${YELLOW}\E[7m RETURN ${RESET} ${GRAY}RETVAL=${RETURN_RETVAL}${RESET} ${FUNCNAME[$((N_FUNC-2))]}"
}

# define traps
function set_traps() {
    # set local debug value
    local -i DEBUG=${DEBUG+0} # substitute default value if DEBUG is unset or null

    [ $DEBUG -gt 0 ] && start_new_line
    decho -e "${TAB}${MAGENTA}\E[7mset traps${RESET}"
    itab

    ddecho "${TAB}$-"
    # set shell options
    decho -n "${TAB}setting shell options... "
    # exit on errors
    #set -e
    # trace ERR (subshells inherit ERR trap from shell)
    set -E
    # trace RETURN and DEBUG traps (subshells inherit RETURN and DEBUG traps from shell)
    #set -T
    ddecho "done"
    ddecho "${TAB}$-"
    
    ddecho "${TAB}the following traps are saved"
    itab
    if [ -z "${save_traps+default}" ]; then
        ddecho "${TAB}${fTAB}none"
        ddecho -n "${TAB}setting traps... "
        trap 'print_error $LINENO $? $BASH_COMMAND' ERR
        trap 'print_exit $?' EXIT
        ddecho "done"
    else
        ddecho "${save_traps}" | sed "$ ! s/^/${TAB}/"
        ddecho "${TAB}setting saved traps..."
        eval $(echo "${save_traps}" | sed "s/$/;/g")
    fi
    dtab

    # print summary
    ddecho -n "${TAB}on set trap return, the following traps are set"
    itab
    if [ -z "$(trap -p)" ]; then
        ddecho -e "\n${TAB}none"
        echo "something didn't work..."
        dtab
        return 1
    else
        ddecho
        ddecho $(trap -p) | sed "$ ! s/^/${TAB}/;s/ \(trap\)/\n${TAB}\1/g" | sed 's/^[ ]*$//g'
    fi
    dtab 2
}

function unset_traps() {
    # set local debug value
    local -i DEBUG=${DEBUG+0} # default value if DEBUG is unset or null

    [ $DEBUG -gt 0 ] && start_new_line
    decho -e "${TAB}${CYAN}\E[7mun-set traps${RESET}"
    itab

    ddecho "${TAB}$-"
    # set shell options
    decho -n "${TAB}setting shell options... "
    # trace ERR (inherit ERR trap from shell)
    set -E
    # trace RETURN and DEBUG traps
    set -T
    # DO NOT exit on errors
    set +e
    decho "done"
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
        
        # save traps
        export save_traps=$(trap -p | sed 's/-- //g')
        if [ ! -z "${save_traps}" ]; then
            ddecho "${TAB}the current traps are saved"
            itab
            ddecho "${save_traps}" | sed "$ ! s/^/${TAB}/"
            dtab
        fi

        # clear traps
        trap - ERR
        trap - EXIT
        trap - RETURN
    fi

    # print summary
    ddecho "${TAB}on unset trap return, the following traps are set"
    if [ -z $(trap -p) ]; then
        ddecho "${TAB}${fTAB}none"
    else
        ddecho $(trap -p) | sed "$ ! s/^/${TAB}/;s/ \(trap\)/\n${TAB}\1/g" | sed 's/^[ ]*$//g'
        echo "something didn't work..."
        dtab
        return 1
    fi
    dtab
}
