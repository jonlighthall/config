#!/bin/bash -eu
# -----------------------------------------------------------------------------------------------
# EXECUTION CALL STACK LIBRARY
# -----------------------------------------------------------------------------------------------
#
# ~/config/lib_stack.sh
#
# PURPOSE: define fuctions to print the contents of the execution stack.
#
# May 2024 JCL
#
# -----------------------------------------------------------------------------------------------

function print_source() {
    local -i funcDEBUG=$((${DEBUG:-1} - 1))
    ddecho -e "${TAB}${INVERT}${FUNCNAME}${RESET}"
    fecho "DEBUG = $DEBUG"
    get_source

    # define default run type
    RUN_TYPE=${RUN_TYPE:-"running"}

    # conditionally add stack level prefix
    if [ ${DEBUG:-0} -gt 1 ]; then 
        # since print_source is itself part of the stack, remove the top of the stack
        ((N_BASH--))
        local prefix="\x1b[7;38;5;132m${N_BASH}"
        if [[ "$-" == *i* ]]; then
            prefix+="i"
        fi        
        prefix+="\x1b[0m"
        RUN_TYPE="${prefix}${RUN_TYPE}"
    fi
    # strip escapes from run type
    local msgne
    strip_pretty msgne $RUN_TYPE
    # get message length
    local -i lnr=${#msgne}
    # define prefix prototype
    prefix="${VALID}link${RESET} -> "    
    strip_pretty msgne $prefix
    local -i lnp=${#msgne}
    
    # subtract link/path prefix length
    local -i sp=$(($lnr - $lnp))
    fecho "space: $lnr, $lnp, $sp"
    # if run type is shorter than prefix, adjust
    if [ $sp -le 0 ]; then
        fecho "space: $lnr, $lnp, $sp"
        if [ $sp -eq 0 ]; then
            sp=-1
        else
            for ((i = $sp; i < 0; i++)); do
                RUN_TYPE=" $RUN_TYPE"
                fecho "$RUN_TYPE: $i"
            done
        fi
    fi    
    
    # define indent
    local spx=$(echo -ne "\E[${sp}C")

    # set tab
    if [[ "${RUN_TYPE}" =~ "sourcing" ]]; then
        set_tab
        decho -e "${TAB}${DIM}${FUNCNAME}${NORMAL}: ${RUN_TYPE}: reducing tab..."
        dtab
    else
        set_tab_shell
    fi
    export TAB
    
    # print run type and source name
    vecho -e "${TAB}${RUN_TYPE} ${PSDIR}${BASH_SOURCE[1]}${RESET}..."
    if [ ! "${BASH_SOURCE[1]}" = "$src_name" ]; then
        vecho -e "${TAB}${spx}${prefix}$src_name"
    fi

    if [ ${DEBUG:-0} -gt 1 ]; then 
        # print source path
        if [[ "${src_dir_logi}" != "${src_dir_phys}" ]]; then 
            ## logical
            vecho -e "${TAB}${spx}${GRAY}logi -> $src_dir_logi${RESET}"
            ## physical
            vecho -e "${TAB}${spx}${GRAY}phys -> $src_dir_phys${RESET}"
        fi
    fi

    # deallocate variables
    unset prefix
}

function print_ribbon() {
    get_source
    # since this function is part of the stack, reduce N_BASH by one
    ((N_BASH--))
    # print source
    decho -e "${TAB}\x1b[7;38;5;132m${N_BASH}: ${src_base} :${N_BASH}\x1b[0m"
}

function print_bar() {
    get_source
    
    # define message
    msg=$(echo "this file is ${src_base}!")
    # define line
    ln=$(for ((i = 1; i <= ${#msg}; i++)); do echo -n "-"; done)
    # print source
    decho -e "$ln\n$msg\n$ln" | sed "$ ! s/^/${TAB}/"
}

function print_stack() {
    local -i DEBUG=${DEBUG:=0}
    start_new_line
    [ $DEBUG -gt 0 ] && print_debug
    # initialize variables
    unset N_FUNC
    unset N_BASH
    unset N_LINE
    
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

    N_STACK=$N_FUNC
    
    # check that all stacks have the same length
    if [ $N_FUNC -ne $N_BASH ]; then 
        echo "${TAB}There are N=$N_BASH entries in the source file name stack"
        if [ $N_BASH -gt $N_STACK ]; then
            N_STACK=$N_BASH
        fi        
    fi

    if [ $N_FUNC -ne $N_LINE ]; then 
        echo "${TAB}There are N=$N_LINE entries in the line number stack"
    fi

    if [ $N_FUNC -ne $N_BASH ] || [ $N_FUNC -ne $N_LINE ]; then 
        echo "${TAB}${N_FUNC} functions, ${N_BASH} files, ${N_LINE} lines"
        if [ $N_LINE -gt $N_STACK ]; then
            N_STACK=$N_LINE
        fi                  
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
            for ((i = 0; i < $N_STACK ; i++)); do
                echo "$i:${FUNCNAME[i]}:${BASH_FNAME[i]}:${BASH_LINENO[i]}"
            done
        ) | column -t -s: -N "index,function,source,line no" -R1 | sed "s/^/${TAB}/"
        echo

        # set color
        ((idx++))
        echo -ne "${dcolor[idx]}"
        (
            for ((i = 0; i < $N_STACK ; i++)); do
                echo "$i:${FUNCNAME[i]}:${BASH_SOURCE[i]}:${BASH_LINENO[i]}"
            done
        ) | column -t -s: -N "index,function,source,line no" -R1 | sed "s/^/${TAB}/"
        echo

        # set color
        ((idx++))
        echo -ne "${dcolor[idx]}"
        (
            for ((i = 0; i < $N_STACK ; i++)); do
                echo "$i:${FUNCNAME[i]}:${BASH_LINK[i]}:${BASH_LINENO[i]}"
            done
        ) | column -t -s: -N "index,function,source,line no" -R1 | sed "s/^/${TAB}/"
        echo

        # set color
        ((idx++))
        echo -ne "${dcolor[idx]}"    
        (
            for ((i = 0; i < $N_STACK ; i++)); do
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
        for ((i = 0; i < $N_STACK ; i++)); do
            # print stack element
            echo "$i:${FUNCNAME[i]}:${BASH_DIR[i]}:${BASH_FNAME[i]}:${BASH_LINENO[i]}"
            # check if source is linked
            if [[ "${BASH_SOURCE[$i]}" != "${BASH_LINK[$i]}" ]] && [ $DEBUG -gt 0 ]; then
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

function print_shlvl() {
    echo -e "${TAB}shell level = ${PSSH}$SHLVL${RESET}"
}
