#!/bin/bash -u

# -----------------------------------------------------------------------------------------------
# DEBUG LIBRARY
# -----------------------------------------------------------------------------------------------
#
# ~/config/lib_dbg.sh
#
# PURPOSE: the function "line echo" or lecho() prints the line from which the function is
#   called. This is designed to be used in place of commands like echo "here", etc.
#
# May 2024 JCL
#
# -----------------------------------------------------------------------------------------------

# stack echo
function secho() {
    local -i DEBUG=2
    print_stack
}

function in_line() {
    if [ ! -z "$@" ]; then
        echo -en "${TAB}$@ <- "
    else
        echo -n "${TAB}"
    fi
}

function find_func_line() {
    local -i DEBUG=3
    dddecho -e "${TAB}${INVERT}${FUNCNAME}${RESET}" >&2

    local func=$1
    decho "${TAB}function: $func" >&2

    local src=$2
    decho "${TAB}source: $src" >&2

    pat="$(declare -f ${func} | head -1 | sed 's/ /[ ]*/;s/[ ]*$//;s/)/&[ \\n\\r{]*$/')"

    decho "${TAB}base pattern: $pat" >&2
    itab
    grep -n "${pat}" "${src}" --color=always | sed "s/^/${TAB}/" >&2
    dtab

    if [[ "$-" == *e* ]]; then
        old_opts=$(echo "$-")
        set +e
    fi
    unset_traps 0

    decho "${TAB}matching pattern without function prefix..." >&2
    local epat="^[ ]*${pat}"
    itab
    decho "${TAB}extended pattern: $epat" >&2
    # grep -n "${epat}" "${src}" --color=always | sed "s/^/${TAB}/" >&2

    if [ ! -z "$(grep "${epat}" "${src}")" ]; then
        decho "${TAB}found without function" >&2

    else
        decho "${TAB}not found" >&2
        dtab
        decho "${TAB}matching pattern with function prefix..." >&2
        itab
        epat="^function[ ]\+${pat}"
        decho "${TAB}extended pattern: $epat" >&2
        #grep -n "${epat}" "${src}" --color=always | sed "s/^/${TAB}/" >&2
        if [ ! -z "$(grep "${epat}" "${src}")" ]; then
            decho "${TAB}found with function" >&2
        else
            decho "${TAB}pattern not found" >&2
            return 1

        fi
    fi
    dtab
    decho "${TAB}matching line:" >&2
    itab
    grep -n "${epat}" "${src}" --color=always | sed "s/^/${TAB}/" >&2
    if [ $(grep -n "${epat}" "${src}" | wc -l) -eq 1 ]; then
        decho "${TAB}OK" >&2
    else
        decho "${TAB}pattern not unique" >&2
        return 1
    fi
    reset_shell ${old_opts-''}
    reset_traps 0
    dtab
    local -i lin=$(grep -n "${epat}" "${src}" | awk -F: '{print$1}')
    decho -e "${TAB}${BOLD}pattern found on line ${lin}${RESET}" >&2

    decho $lin >&2

    lin_txt=$(sed -n "${lin}p" "${src}")

    if [[ ! "${lin_txt}" =~ .*"{" ]]; then
        ((++lin))
    fi

    echo $lin
}

# line echo
lecho() {
    # set local debug level
    local -i DEBUG=${DEBUG:-1}
    local -i oldDEBUG=$DEBUG
    if [ $DEBUG -lt 1 ]; then
        idb >/dev/null
    fi

    # DEBUG=2
    dddecho -e "${TAB}${INVERT}${FUNCNAME}${RESET}"
    set -u

    if [ ${DEBUG:-0} -gt 0 ] || [ ! -z "$@" ] ; then
        hline
        trap 'hline;DEBUG=$oldDEBUG;trap -- RETURN' RETURN
    fi

    # get the lenght of the execution stack
    local -i N_BASH=${#BASH_SOURCE[@]}
    ddecho "${TAB}there are ${N_BASH} entries in the call stack"

    # get the line where THIS function is defined
    line_def=$(find_func_line "${FUNCNAME}" "${BASH_SOURCE}" 2>/dev/null);

    if [ $N_BASH -eq 1 ]; then
        in_line "$@"
        echo -e "called on line ${GRL}${BASH_LINENO}${RESET} from ${BASH##*/}"
        ddecho "${TAB}exiting ${FUNCNAME}() on line $((${line_def}+${LINENO}-1))..."
        return 0
    fi

    # get the file of the calling function
    local sour=${BASH_SOURCE[1]}
    local sour_dir=$(dirname "${sour}")
    local sour_fil=$(basename "${sour}")
    ddecho "${TAB}source is ${sour}"
    ddecho "${TAB}source is ${sour_fil} in ${sour_dir}"

    # get the line of of the calling function
    local -i func_line=${BASH_LINENO[0]}

    # BASH_SOURCE counts from zero; get the bottom of the stack
    local bottom=${FUNCNAME[$(($N_BASH - 1))]##*/}
    ddecho "${TAB}bottom of stack is $bottom"

    # get the calling function
    local func=${FUNCNAME[1]}
    ddecho "${TAB}calling function is $func"

    if [[ "${func}" == "main" ]] || [[ "${func}" == "source" ]]; then
        # the function is call from bash
        in_line "$@"
        echo -e "called on line ${GRL}${BASH_LINENO[(($N_BASH-2))]}${RESET} in file ${YELLOW}${BASH_SOURCE[(($N_BASH-1))]##*/}${RESET}"
        ddecho "exiting ${FUNCNAME} on line ${LINENO}"
        return 0
    else
        ddecho "${TAB}not bash"
        # get the line where the function is defined
        local line_func_def=$(find_func_line "${func}" "${sour}" 2>/dev/null);
    fi

    if [[ "${bottom}" == "main" ]] || [[ "${bottom}" == "source" ]]; then
        ddecho "${TAB}BASH_LINENO refers to file"
        local -i call_line=$func_line
        # print file line
        in_line "$@"
        echo -en "${FUNCNAME}() called on line ${GRL}${call_line}${RESET} "
        echo -e "in file ${RESET}${YELLOW}${sour_fil}${RESET}"
    else
        ddecho "${TAB}BASH_LINENO refers to function"
        # get the line in the file where the function is called
        # add the line where to the function is defined within the file and the line within the function
        local -i call_line=$(($line_func_def -1 + $func_line))
        # print file line
        in_line "$@"
        echo -n "called on line ${call_line} "
        echo -e "in file ${YELLOW}${sour_fil}${RESET}"
        itab
        # print function line
        decho -n "${TAB}called on line ${func_line} "
        decho "in function ${func}()"
        dtab
    fi

    itab
    # print definition line
    decho -n "${TAB}function ${func}() "
    decho -ne "defined on line ${GRL}${line_func_def}${RESET} "
    decho -e "in file ${YELLOW}${sour_fil}${RESET}"
    decho -e "${TAB}file ${YELLOW}${sour_fil}${RESET} located in ${DIR}${sour_dir}${RESET}"
    ddecho "${TAB}exiting ${FUNCNAME}() on line $((${line_def}+${LINENO}-1))..."

    dtab
    if [ ${DEBUG:-0} -gt 2 ]; then
        print_stack
    fi
}

function tlecho() {
    echo "here"
    DEBUG=2
    lecho "$@"
    plecho "$@"
    echo "there"
}

# parent line echo
function plecho() {
    local -i DEBUG=${DEBUG:-1}
    #    DEBUG=2
    dddecho -e "${TAB}${INVERT}${FUNCNAME}${RESET}"
    set -u

    if [ ${DEBUG:-0} -gt 0 ] || [ ! -z "$@" ] ; then
        hline
        trap 'hline;trap -- RETURN' RETURN
    fi
    if [ ${DEBUG:-0} -gt 0 ]; then
        print_stack
    fi

    # get the lenght of the execution stack
    local -i N_BASH=${#BASH_SOURCE[@]}

    ddecho "${TAB}there are ${N_BASH} entries in the call stack"

    # get the line where THIS function is defined
    line_def=$(find_func_line "${FUNCNAME}" "${BASH_SOURCE}" 2>/dev/null);

    if [ $N_BASH -eq 1 ]; then
        in_line "$@"
        echo -e "called on line ${GRL}${BASH_LINENO}${RESET} from ${BASH##*/}"
        decho "${TAB}exiting ${FUNCNAME}() on line $((${line_def}+${LINENO}-1))..."
        return 0
    else
        # define ofset
        local -i N_off
        if [ $N_BASH -ge 3 ]; then
            N_off=2;
            decho "${TAB}showing parent line"
        else
            N_off=1;
            decho "${TAB}showing regular lecho results"
        fi        

        if [ $N_BASH -ge $N_off ]; then
            ddecho "${TAB}N_BASH = ${N_BASH}"
            ddecho "${TAB} N_off = ${N_off}"

            N_BASH=$(($N_BASH - $N_off))
            ddecho "${TAB}N_BASH = ${N_BASH}"
        fi
        ddecho "${TAB}there are ${N_BASH} entries in the (parent) call stack"
    fi

    # get the file of the calling function
    local sour=${BASH_SOURCE[((1-$N_off))]}
    local sour_dir=$(dirname "${sour}")
    local sour_fil=$(basename "${sour}")
    ddecho "${TAB}source is ${sour}"
    ddecho "${TAB}source is ${sour_fil} in ${sour_dir}"

    # get the line of of the calling function
    local -i func_line=${BASH_LINENO[0]}

    decho "${TAB}${FUNCNAME}"
    ddecho "${TAB}FUNCNAME stack = ${FUNCNAME[@]}"
    ddecho "${TAB}N_BASH = ${N_BASH}"
    local -i func_lev=$(($N_BASH - 1 + ${N_off}))
    decho "${TAB}function level = $func_lev"
    decho "${TAB}function : ${FUNCNAME[$func_lev]}"

    # BASH_SOURCE counts from zero; get the bottom of the stack
    local bottom=${FUNCNAME[${func_lev}]##*/}
    ddecho "${TAB}bottom of stack is $bottom"

    # get the calling function
    local func=${FUNCNAME[${func_lev}]}
    ddecho "${TAB}calling function is $func"

    itab
    odb=$DEBUG;
    DEBUG=1;
    #lecho "${FUNCNAME}";
    DEBUG=$odb
    dtab
    
    if [[ "${func}" == "main" ]] || [[ "${func}" == "source" ]]; then
        # the function is call from bash
        ddecho -e "${TAB}called by ${SHELL##*/}"
        ddecho "${TAB}N_BASH = ${N_BASH}"
        ddecho "${TAB}function level = $func_lev"
        in_line "$@"
        echo -e "${FUNCNAME[(($func_lev-1))]}() called on line ${GRL}${BASH_LINENO[(($func_lev-1))]}${RESET} in file ${YELLOW}${BASH_SOURCE[$func_lev]##*/}${RESET}"
        itab
        ((--func_lev))
        decho -e "${TAB}${BASH_SOURCE[$func_lev]##*/}${RESET} called by ${SHELL##*/}"
        decho -e "${TAB}${FUNCNAME[(($func_lev-1))]}() called on line ${GRL}${BASH_LINENO[(($func_lev-1))]}${RESET} in file ${YELLOW}${BASH_SOURCE[$func_lev]##*/}${RESET}"


        decho "${TAB}exiting ${FUNCNAME}() on line $((${line_def}+${LINENO}-1))..."
        dtab
        return 0
    else
        ddecho "${TAB}not called by ${SHELL##*/}"
        # get the line where the function is defined
        local line_func_def=$(find_func_line "${func}" "${sour}" 2>/dev/null);
    fi

    if [[ "${bottom}" == "main" ]] || [[ "${bottom}" == "source" ]]; then
        ddecho "${TAB}BASH_LINENO refers to file"
        local -i call_line=$func_line
        # print file line
        in_line "$@"
        echo -en "called on line ${GRL}${call_line}${RESET} "
        echo -e "in file ${RESET}${YELLOW}${sour_fil}${RESET}"
    else
        ddecho "${TAB}BASH_LINENO refers to function"
        # get the line in the file where the function is called
        # add the line where to the function is defined within the file and the line within the function
        local -i call_line=$(($line_func_def -1 + $func_line))
        # print file line
        in_line "$@"
        echo -n "called on line ${call_line} "
        echo -e "in file ${YELLOW}${sour_fil}${RESET}"
        itab
        # print function line
        decho -n "${TAB}called on line ${func_line} "
        decho "in function ${func}()"
        dtab
    fi

    itab
    # print definition line
    decho -n "${TAB}function ${func}() "
    decho -ne "defined on line ${GRL}${line_func_def}${RESET} "
    decho -e "in file ${YELLOW}${sour_fil}${RESET}"
    decho -e "${TAB}file ${YELLOW}${sour_fil}${RESET} located in ${DIR}${sour_dir}${RESET}"

    dtab
    if [ ${DEBUG:-0} -gt 2 ]; then
        print_stack
    fi
    itab
    ddecho "${TAB}exiting ${FUNCNAME}() on line $((${line_def}+${LINENO}-1))..."
    dtab
}

# test secho and lecho
function necho() { #1
    secho          #2
    lecho          #3
    lecho          #4
    # comment      #5
    lecho          #6
    if true; then  #7
        lecho      #8
    fi             #9
    lecho          #10
    # comment      #11
    lecho          #12
    print_stack
}

# "plain" test lecho
function pecho() { #1
    lecho          #2
    lecho          #3
    # comment      #4
    lecho          #5
    if true; then  #6
        lecho      #7
    fi             #8
    lecho          #9
    # comment      #10
    lecho          #11
}

function do_lecho() {
    lecho
}

function print_debug() {
    set -u
    funcDEBUG=0

    # check if DEBUG is unset
    if [ -z ${DEBUG+dummy} ]; then
        local UNSET='\E[1;33munset\E[0m'
        echo -e "${TAB}${BOLD}DEBUG is ${UNSET}"
        return 0
    fi
    # check if DEBUG is set but NULL
    if [ -z ${DEBUG:+dummy} ]; then
        local NULL='\E[1;36mnull\E[0m'
        echo -e "${TAB}${BOLD}DEBUG is ${NULL}"
        return 0
    fi
    # check if DEBUG is zero
    if [ $DEBUG -eq 0 ]; then
        echo -e "${TAB}${GRAY}DEBUG is 0${RESET}"
        return 0
    fi

    # constuct debug print function name
    local -i i
    local prefix=$(for ((i = 1; i <= $DEBUG; i++)); do echo -n "d"; done)
    fecho "DEBUG = $DEBUG"
    fecho "prefix: $prefix"
    local fun_name="${prefix}echo"
    fecho "function: $fun_name"

    # check if function is defined
    if command -v $fun_name &>/dev/null; then
        fecho "${fun_name} is defined"
    else
        fecho "${fun_name}() is NOT defined"
        fecho "defining ${fun_name}()..."
        eval "${fun_name}() { xecho \"\$@\"; }"
    fi
    $fun_name "${TAB}DEBUG = $DEBUG"
}

function cdb() {
    funcDEBUG=0
    fecho "clearing DEBUG..."
    export DEBUG=''
    print_debug
}

function rdb() {
    funcDEBUG=0
    fecho "resetting DEBUG..."
    export DEBUG=0
    print_debug
}

function idb() {
    set -u
    funcDEBUG=0

    # check if DEBUG is unset
    if [ -z ${DEBUG+dummy} ]; then
        local UNSET='\E[1;33munset\E[0m'
        fecho -e "${BOLD}DEBUG is ${UNSET}"
        fecho "setting DEBUG..."
        export DEBUG=
        fecho -e "DEBUG = ${DEBUG}"
        return 0
    fi

    # check if DEBUG is set but NULL
    if [ -z ${DEBUG:+dummy} ]; then
        local NULL='\E[1;36mnull\E[0m'
        fecho -e "${BOLD}DEBUG is ${NULL}"
        fecho "setting DEBUG to zero..."
        export DEBUG=0
        fecho -e "DEBUG = ${DEBUG}"
        return 0
    fi

    # check if DEBUG is a number
    local num='^[0-9]+$'
    fecho -e "DEBUG = ${DEBUG}"
    if [[ "$DEBUG" =~ $num ]]; then
        fecho "DEBUG is a number"
        fecho "incrementing DEBUG..."
        ((++DEBUG))
        export DEBUG
    else
        fecho "DEBUG is not a number"
        fecho "setting DEBUG to zero..."
        export DEBUG=0
    fi

    fecho -e "DEBUG = ${DEBUG}"
    if [ $funcDEBUG -eq 0 ];then
        start_new_line
        print_debug
    fi
}

function ddb() {
    set -u
    funcDEBUG=0

    # check if DEBUG is unset
    if [ -z ${DEBUG+dummy} ]; then
        local UNSET='\E[1;33munset\E[0m'
        fecho -e "${BOLD}DEBUG is ${UNSET}"
        fecho "setting DEBUG..."
        export DEBUG=
        fecho -e "DEBUG = ${DEBUG}"
        return 0
    fi

    # check if DEBUG is set but NULL
    if [ -z ${DEBUG:+dummy} ]; then
        local NULL='\E[1;36mnull\E[0m'
        fecho -e "${BOLD}DEBUG is ${NULL}"
        fecho "setting DEBUG to zero..."
        export DEBUG=0
        fecho -e "DEBUG = ${DEBUG}"
        return 0
    fi

    # check if DEBUG is a number
    local num='^[0-9]+$'
    fecho -e "DEBUG = ${DEBUG}"
    if [[ "$DEBUG" =~ $num ]]; then
        fecho "DEBUG is a number"
        # check if DEBUG is zero
        if [ $DEBUG -eq 0 ]; then
            fecho "no action needed"
            if [ $funcDEBUG -eq 0 ];then
                start_new_line
                print_debug
            fi
            return 0
        fi
        fecho "decrementing DEBUG..."
        ((DEBUG--))
        export DEBUG
    else
        fecho "DEBUG is not a number"
        fecho "setting DEBUG to zero..."
        export DEBUG=0
    fi
    fecho -e "DEBUG = ${DEBUG}"
    if [ $funcDEBUG -eq 0 ];then
        start_new_line
        print_debug
    fi
}
