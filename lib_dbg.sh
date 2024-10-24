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

# define variable status keywords
export   UNSET='\x1B[1;33munset\x1B[0m' # unbound variable
export    NULL='\x1B[1;36mnull\x1B[0m'  # empty variable
export   SPACE='\x1B[30;106m'             # highlight white space
export    TRUE='\x1B[1;32mtrue\x1B[0m'  #
export   FALSE='\x1B[1;31mfalse\x1B[0m' #

libDEBUG=1
#unset libDEBUG

function tl() {
    # re-worked "this line"

    # turn printing on or off
    local -i do_print=1

    if [ $do_print = 1 ]; then
        # print grep-like file and line number
        print_stack
        echo -en "${TAB}${GRF}"
        if [[ ${#FUNCNAME[@]} -gt 1 ]]; then
            echo -en "${BASH_SOURCE[1]##*/}"
        else
            echo -en "${SHELL##*/}"
        fi
        echo -en "${GRS}:${GRL}${BASH_LINENO[0]}${RESET}"
        # check if caller is function
        if [[ ${#FUNCNAME[@]} -gt 1 ]]; then
            this_func=${FUNCNAME[1]}
            # check if caller is CLI, print function name if applicable
            if [[ "${this_func}" == "main" ]] || [[ "${this_func}" == "source" ]]; then
                : #echo
            else
                echo -en "${GRS}: ${GRH}${this_func}()${RESET}"
            fi
        fi
        # print argument, if present
        if [[ ! -z "$@" ]]; then
            echo -e " ${INVERT}$@${RESET}"
        else
            echo
        fi
    fi
    return 0
}

function set_fcol() {
    local -i N=9

    # use argument to manually set color
    if [ $# -eq 1 ]; then
        N=$1
    fi

    local -i idx
    dbg2idx $N idx
    export fcol=${dcolor[idx]}

    # set color
    echo -ne "${fcol}"
}

function in_line() {
    set_fcol
    echo -en "${TAB}${fcol}"
    if [[ ! -z "$@" ]]; then
        echo -en "${INVERT}$@${NORMAL} <- ${fcol}"
    fi
}

function lec_mes () {
    if [ $N_BASH -gt 1 ]; then
        echo -en "line ${BASH_LINENO[1]} "
    fi
    echo -en "called on line ${GRL}"
}

# This function print the result to STDOUT. All outher output is printed to STDERR. To use the
# function, redirect function call output to /dev/null. To view degugging information, do no
# redirect function all output.
function find_func_line() {

    # set local debug level
    local -i DEBUG=${libDEBUG:-0}
    DEBUG=0
    # print THIS function name
    dddecho -e "${TAB}${INVERT}${FUNCNAME}${RESET}" >&2

    # set function name
    local func=$1
    decho -n "${TAB}function: $func... " >&2

    # get function definition
    declare -f ${func} >/dev/null
    local -i RETVAL=$?
    if [ $RETVAL -eq 0 ]; then
        if [ -z "$(declare -f ${func})" ]; then
            decho -e "${BAD}FAIL${RESET} empty"  >&2
            return 1
        else
            decho -e "${GOOD}OK${RESET}"  >&2
        fi
    else
        decho -e "${BAD}FAIL${RESET} not defined"  >&2
        return 1
    fi

    # define search pattern
    #   DECLARE - print function definition
    #   HEAD    - get declaration line
    #   SED     - remove whitespace
    local pat="$(declare -f ${func} | head -1 | sed 's/ /[ ]*/;s/[ ]*$//;s/)/&[ ]*#*.*[ \\n\\r{]*$/')"
    decho "${TAB}matching pattern..." >&2
    itab
    decho "${TAB}base pattern: $pat" >&2


    # get source file
    local src=$2
    decho "${TAB}source: $src" >&2

    # print matching line
    if [ $DEBUG -gt 0 ]; then
        grep -n "${pat}" "${src}" --color=always | sed "s/^/${TAB}grep: /" >&2
    fi
    dtab

    # reset shell options
    if [[ "$-" == *e* ]]; then
        old_opts=$(echo "$-")
        set +e
    fi
    unset_traps 0

    # look for function definition without 'function' prefix
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
        # look for function definition with 'function' prefix
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

    # display results
    decho "${TAB}matching line:" >&2
    itab
    if [ $DEBUG -gt 0 ]; then
        grep -n "${epat}" "${src}" --color=always | sed "s/^/${TAB}grep: /" >&2
    fi
    if [ $(grep -n "${epat}" "${src}" | wc -l) -eq 1 ]; then
        decho -e "${TAB}${GOOD}OK${RESET} pattern unique" >&2
    else
        decho -e "${TAB}${BAD}FAIL${RESET} pattern not unique" >&2
        return 1
    fi
    reset_shell ${old_opts-''}
    reset_traps 0
    dtab

    # get line number
    local -i lin=$(grep -n "${epat}" "${src}" | awk -F: '{print$1}')
    decho -e "${TAB}${BOLD}pattern found on line ${lin}${RESET}" >&2
    itab
    decho "${TAB}output: $lin" >&2
    dtab

    # The function line number counts from the open bracket, not the function name declaration
    # line. If the two are not on the same line, increment the line counter.
    lin_txt=$(sed -n "${lin}p" "${src}")
    if [[ ! "${lin_txt}" =~ .*"{" ]]; then
        ((++lin))
    fi

    # return the result to STDOUT
    echo $lin
}

function get_caller() {
    # clear variables
    unset this_func
    unset this_source
    unset this_file

    unset caller_func
    unset caller_source
    unset caller_file

    # define stack level number
    local -i lev
    if [ $# -eq 0 ]; then
        lev=1
    else
        lev=$1
    fi

    #print_stack
    fecho " in lev = $lev"
    # increment level so that index matches that of calling function
    ((++lev))
    fecho "out lev = $lev"

    local -i fN_FUNC=${#FUNCNAME[@]}
    fecho " N_FUNC = $fN_FUNC"
    local -i this_lev=$((lev-1))

    # define stack level for "this" function; actually the calling function (default) or the
    # function one level below the target stack level (argument)
    if [ ${fN_FUNC} -ge 1 ]; then
        # get function
        this_func=${FUNCNAME[this_lev]}
        # get function source
        this_source=${BASH_SOURCE[this_lev]}
        # get function source file
        this_file=${this_source##*/}
    fi

    if [ ${lev} -lt ${fN_FUNC} ]; then
        fecho "$lev -lt $fN_FUNC"
        # get calling function source
        caller_source=${BASH_SOURCE[lev]}
        # get calling function source file
        caller_file=${BASH_SOURCE[lev]##*/}

        # get calling function name
        caller_func=${FUNCNAME[lev]}
    fi
    fecho "done with get caller"
}

function get_caller_def() {
    # clear variables
    unset this_func
    unset this_source
    unset this_file

    unset caller_func
    unset caller_source
    unset caller_file

    unset this_def
    unset line_def
    unset caller_func_line
    unset get_file_line

    declare -ig this_def
    declare -ig line_def
    declare -ig caller_func_line
    declare -ig get_file_line

    # define stack level number
    local -i lev
    if [ $# -eq 0 ]; then
        lev=1
    else
        lev=$1
    fi

    #print_stack
    fecho " in lev = $lev"
    # increment level so that index matches that of calling function
    ((++lev))
    fecho "out lev = $lev"

    local -i fN_FUNC=${#FUNCNAME[@]}
    fecho " N_FUNC = $fN_FUNC"
    local -i this_lev=$((lev-1))

    # define stack level for "this" function; actually the calling function (default) or the
    # function one level below the target stack level (argument)
    if [ ${fN_FUNC} -ge 1 ]; then
        # get function
        this_func=${FUNCNAME[this_lev]}
        # get function source
        this_source=${BASH_SOURCE[this_lev]}
        # get line definition
        if [[ "${this_func}" == "main" ]] || [[ "${this_func}" == "source" ]]; then
            this_def=-1
        else
            this_def=$(find_func_line "${this_func}" "${this_source}");
        fi
        # get function source file
        this_file=${this_source##*/}
    fi

    if [ ${lev} -lt ${fN_FUNC} ]; then
        fecho "$lev -lt $fN_FUNC"
        # get calling function source
        caller_source=${BASH_SOURCE[lev]}
        # get calling function source file
        caller_file=${BASH_SOURCE[lev]##*/}

        # get calling function name
        caller_func=${FUNCNAME[lev]}
        # get line in calling function where this tunction was called
        caller_func_line=${BASH_LINENO[this_lev]}
        fecho "get func line = $caller_func_line"
        # get calling function definition line
        if [[ "${caller_func}" == "main" ]] || [[ "${caller_func}" == "source" ]]; then
            line_def=-1
            get_file_line=-1
        else
            fecho "func: ${caller_func}"
            fecho "file: ${caller_source}"
            line_def=$(find_func_line "${caller_func}" "${caller_source}")
            # get line in calling function source file
            get_file_line=$((${line_def}+${caller_func_line}-1))
        fi
    else
        caller_func_line=0
    fi
    fecho "done with get caller"
}

function this_line() {
    set -u
    export TAB=${TAB-}

    # DEBUG = 0 print line in file only
    # DEBUG = 1 print calling function
    # DEBUG = 2 print calling function line def
    # DEBUG = 3 print calling function line

    # set local debug level
    local -i DEBUG=${DEBUG:-0}
    # manual
    DEBUG=3
    #DEBUG=${libDEBUG:-0}

    local -i funcDEBUG=0
    #funcDEBUG=${libDEBUG:-0}

    local do_grep=true;
    local do_before=true;
    local do_extra=true;
    local do_defs=false;
    local do_invo=true;

    # set function color
    set_fcol

    local -ir fN_STACK=${#FUNCNAME[@]}
    fecho "${TAB}fN_STACK = $fN_STACK"

    if $do_defs && [ $DEBUG -gt 1 ]; then
        for ((lev = 0; lev < $fN_STACK ; lev++)); do
            fecho "lev = $lev (definition)"
            get_caller_def $lev
            # print this function definition line
            ddecho -n "${TAB}"
            ddecho -n "FUNCNAME[$lev] = "
            ddecho -n "$caller_func() "
            ddecho -n "defined on line $line_def "
            ddecho -n "in file ${caller_file}"
            ddecho
        done
        fecho "done printing definitions"
    fi
    if $do_invo && [ $DEBUG -gt 1 ] ; then
        for ((lev = 1; lev < $fN_STACK; lev++)); do
            fecho "lev = $lev (invocation)"
            get_caller_def $lev
            # print calling function line in source file
            ddecho -n "${TAB}${this_func}() called by "
            dddecho -n "line $caller_func_line of "
            ddecho -n "$caller_func() "
            ddecho -n "on line $get_file_line of ${caller_file}"
            ddecho
        done
        fecho "done printing invocations"
    fi

    set +e
    set_traps 0

    get_caller_def
    if $do_grep; then
        # print grep-like match
        echo -en "${TAB}"
        [[ ! -z "$@" ]] && [ $do_before = true ] && echo -en "${GRH}${INVERT}$@${NORMAL} "
        echo -en "${GRF}${caller_file}${GRS}:${GRL}${get_file_line}${GRS}: ${GRH}"
        if [[ -z "$@" ]] ||  [ $do_before = true ]; then
            echo -en "${this_func}() "
        else
            echo -en "${INVERT}$@${NORMAL} "
        fi
        ddecho -n "called by "
        dddecho -n "line $caller_func_line of "
        decho -n "${caller_func}"
        if [[ "${caller_func}" == "main" ]] || [[ "${caller_func}" == "source" ]]; then
            :
        else
            ddecho -n "() "
            ddecho -n "defined on line $line_def"
        fi

    else
        # print argument
        start_new_line
        in_line "$@"
        # print the line number where THIS function was called in the PARENT function
        [ $DEBUG -gt 0 ] && echo -n "${caller_func}() "
        echo -en "${fcol}on line $get_file_line in ${caller_file} "
        ddecho -n "defined on line $line_def"
        dddecho -n ", function line $caller_func_line"
    fi
    echo -e ${RESET}

    if [[ "$-" == *u* ]]; then
        set +u
    fi
    return 0
}

# line echo
function lecho() {
    # save shell options
    old_opts=$(echo "$-")

    # set local debug level
    local -i DEBUG=${DEBUG:-1}
    local -i oldDEBUG=$DEBUG
    if [ $DEBUG -lt 1 ]; then
        idb >/dev/null
    fi
    # manual
    DEBUG=0
    #DEBUG=${libDEBUG:-0}

    set_fcol
    start_new_line
    echo -ne "${fcol}"
    dddecho -e "${TAB}${INVERT}${FUNCNAME}${fcol}"
    set -u

    declare -i hr_wid=15
    if [ ${DEBUG:-0} -gt 0 ] || [ ! -z "$@" ] ; then
        local -i tab_wid=${#TAB}
        local -i term_wid=${COLUMNS:-72}
        hr_wid=$(($term_wid - (2 * $tab_wid)))
        hline $hr_wid
        trap 'echo -ne "${fcol}";hline $hr_wid;DEBUG=$oldDEBUG;echo -en "$RESET";trap -- RETURN' RETURN
    fi
    if [ ${DEBUG:-0} -gt 1 ]; then
        print_stack
    fi

    # get the lenght of the execution stack
    local -i N_BASH=${#BASH_SOURCE[@]}
    ddecho "${TAB}there are ${N_BASH} entries in the ${FUNCNAME}() call stack"

    # get the line where THIS function is defined
    line_def=$(find_func_line "${FUNCNAME}" "${BASH_SOURCE}"); # 2>/dev/null

    if [ $N_BASH -eq 1 ]; then
        in_line "$@"
        echo -e "${FUNCNAME}() $(lec_mes)${BASH_LINENO}${fcol} from ${BASH##*/}"
        ddecho "${TAB}exiting ${FUNCNAME}() on line $((${line_def}+${LINENO}-1))..."
        return 0
    fi

    itab
    ddecho "${TAB}${FUNCNAME}"
    ddecho "${TAB}FUNCNAME stack = ${FUNCNAME[@]}"

    # define stack ofset
    local -i N_off
    N_off=1;
    dddecho "${TAB}showing regular lecho results"

    # check offset agaisnt stack depth
    if [ $N_BASH -ge $N_off ]; then
        N_BASHo=$(($N_BASH - $N_off))
        (
            ddecho "N_BASH:${N_BASH}"
            ddecho "N_off:${N_off}"
            decho "N_BASHo:${N_BASHo}"
        ) | column -t -s: -o" = " | sed "s/^/${TAB}/"
    fi

    # set level of the calling function
    local -i sour_lev=$N_off
    ddecho "${TAB}source level = ${sour_lev}"

    if [ ! ${sour_lev} -eq 1 ]; then
        echo "check source level"
    fi

    # get the file of the calling function
    local sour=${BASH_SOURCE[1]}
    local sour_dir=$(dirname "${sour}")
    local sour_fil=$(basename "${sour}")
    ddecho "${TAB}source is ${sour}"
    ddecho "${TAB}source is ${sour_fil} in ${sour_dir}"
    dtab

    decho "${TAB}there are ${N_BASHo} entries in the ${FUNCNAME[$N_off]} call stack"
    itab
    local -i pare_lev=$(($N_BASHo - 1 + ${N_off}))
    decho "${TAB}parent level = $pare_lev"
    decho "${TAB}parent function is ${FUNCNAME[$pare_lev]}"
    dtab

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
        ddecho -e "${TAB}called by ${SHELL##*/}"
        ddecho "${TAB}N_BASH = ${N_BASH}"
        ddecho "${TAB}function level = $pare_lev"
        in_line "$@"
        echo -e "${FUNCNAME}() $(lec_mes)${BASH_LINENO[(($N_BASH-2))]}${fcol} in file ${YELLOW}${BASH_SOURCE[(($N_BASH-1))]##*/}${fcol}"

        echo -e "${FUNCNAME[(($pare_lev-1))]}() called on line ${GRL}${BASH_LINENO[(($pare_lev-1))]}${RESET} in file ${YELLOW}${BASH_SOURCE[$pare_lev]##*/}${RESET}"
        ((--pare_lev))
        decho -e "${TAB}${BASH_SOURCE[$pare_lev]##*/}${RESET} called by ${SHELL##*/}"
        decho -e "${TAB}${FUNCNAME[(($pare_lev-1))]}() called on line ${GRL}${BASH_LINENO[(($pare_lev-1))]}${RESET} in file ${YELLOW}${BASH_SOURCE[$pare_lev]##*/}${RESET}"

        decho "${TAB}exiting ${FUNCNAME}() on line $((${line_def}+${LINENO}-1))..."
        dtab
        return 0
    else
        ddecho "${TAB}not called by ${SHELL##*/}"
        # get the line where the function is defined
        local line_func_def=$(find_func_line "${func}" "${sour}"); # 2>/dev/null
    fi

    if [[ "${bottom}" == "main" ]] || [[ "${bottom}" == "source" ]]; then
        ddecho "${TAB}BASH_LINENO refers to file"
        local -i call_line=$func_line
        # print file line
        in_line "$@"
        echo -en "${FUNCNAME}() $(lec_mes)${call_line}${fcol} "
        echo -e "in file ${fcol}${YELLOW}${sour_fil}${fcol}"
    else
        ddecho "${TAB}BASH_LINENO refers to function"
        # get the line in the file where the function is called
        # add the line where to the function is defined within the file and the line within the function
        local -i call_line=$(($line_func_def + $func_line -1 ))
        # print file line
        in_line "$@"
        echo -en "${FUNCNAME}() $(lec_mes)${call_line}${fcol} "
        echo -e "in file ${YELLOW}${sour_fil}${fcol}"
        itab
        # print function line
        ddecho -n "${TAB}$(lec_mes)${func_line} "
        ddecho "in function ${func}()"
        dtab
        echo -en "${RESET}"
    fi

    itab
    # print definition line
    ddecho -n "${TAB}function ${func}() "
    ddecho -ne "defined on line ${GRL}${line_func_def}${fcol} "
    ddecho -e "in file ${YELLOW}${sour_fil}${fcol}"
    ddecho -e "${TAB}file ${YELLOW}${sour_fil}${fcol} located in ${DIR}${sour_dir}${fcol}"
    ddecho "${TAB}exiting ${FUNCNAME}() on line $((${line_def}+${LINENO}-1))..."

    dtab
    if [ ${DEBUG:-0} -gt 2 ]; then
        print_stack
    fi

    # reset shell options
    reset_shell ${old_opts}
}

# parent line echo
function plecho() {
    # save shell options
    old_opts=$(echo "$-")

    # set local debug level
    local -i DEBUG=${DEBUG:-1}
    DEBUG=0
    #    DEBUG=${libDEBUG:-0}

    set_fcol
    start_new_line
    echo -ne "${fcol}"
    dddecho -e "${TAB}${INVERT}${FUNCNAME}${fcol}"
    set -u

    if [ ${DEBUG:-0} -gt 0 ] || [ ! -z "$@" ] ; then
        hline
        trap 'echo -ne "${fcol}";hline;echo -en "$RESET";trap -- RETURN' RETURN
    fi
    if [ ${DEBUG:-0} -gt 0 ]; then
        print_stack
    fi

    # get the lenght of the execution stack
    local -i N_BASH=${#BASH_SOURCE[@]}
    ddecho "${TAB}there are ${N_BASH} entries in the ${FUNCNAME}() call stack"

    # get the line where THIS function is defined
    line_def=$(find_func_line "${FUNCNAME}" "${BASH_SOURCE}"); # 2>/dev/null

    if [ $N_BASH -eq 1 ]; then
        in_line "$@"
        echo -e "${FUNCNAME}() $(lec_mes)${BASH_LINENO}${fcol} from ${BASH##*/}"
        ddecho "${TAB}exiting ${FUNCNAME}() on line $((${line_def}+${LINENO}-1))..."
        return 0
    else
        # define ofset
        local -i N_off
        if [ $N_BASH -ge 3 ]; then
            N_off=2;
            ddecho "${TAB}showing parent line"
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
        ddecho "${TAB}   FUNCNAME stack = ${FUNCNAME[@]}"
        ddecho "${TAB}BASH_SOURCE stack = ${BASH_SOURCE[@]##*/}"
        ddecho "${TAB}BASH_LINENO stack = ${BASH_LINENO[@]}"
    fi

    # get the file of the calling function
    local sour=${BASH_SOURCE[(($N_off))]}
    local sour_dir=$(dirname "${sour}")
    local sour_fil=$(basename "${sour}")
    ddecho "${TAB}source is ${sour}"
    ddecho "${TAB}source is ${sour_fil} in ${sour_dir}"    

    ddecho "${TAB}${FUNCNAME}"
    ddecho "${TAB}FUNCNAME stack = ${FUNCNAME[@]}"
    ddecho "${TAB}N_BASH = ${N_BASH}"
    itab
    local -i func_lev=1
    if [ $N_BASH -gt 1 ]; then
        func_lev=$(($N_BASH + ${N_off}))
    fi
    decho "${TAB}function level = $func_lev"
    decho "${TAB}function : ${FUNCNAME[$func_lev]}"
    dtab

    # get the line of of the calling function
    local -i func_line=${BASH_LINENO[0]}

    # BASH_SOURCE counts from zero; get the bottom of the stack
    local bottom=${FUNCNAME[${func_lev}]##*/}
    ddecho "${TAB}bottom of stack is $bottom"

    # get the calling function
    local func=${FUNCNAME[${func_lev}]}
    ddecho "${TAB}calling function is $func"

    if [[ "${func}" == "main" ]] || [[ "${func}" == "source" ]]; then
        # the function is call from bash
        ddecho -e "${TAB}called by ${SHELL##*/}"
        ddecho "${TAB}N_BASH = ${N_BASH}"
        ddecho "${TAB}function level = $func_lev"
        in_line "$@"
        echo -e "${FUNCNAME[(($func_lev-1))]}() $(lec_mes)${BASH_LINENO[(($func_lev-1))]}${fcol} in file ${YELLOW}${BASH_SOURCE[$func_lev]##*/}${RESET}"
        itab
        ((--func_lev))
        ddecho -e "${TAB}${BASH_SOURCE[$func_lev]##*/}${RESET} called by ${SHELL##*/}"
        ddecho -e "${TAB}${FUNCNAME[(($func_lev-1))]}() $(lec_mes)${BASH_LINENO[(($func_lev-1))]}${fcol} in file ${YELLOW}${BASH_SOURCE[$func_lev]##*/}${RESET}"

        decho "${TAB}exiting ${FUNCNAME}() on line $((${line_def}+${LINENO}-1))..."
        dtab
        return 0
    else
        ddecho "${TAB}not called by ${SHELL##*/}"
        # get the line where the function is defined
        local line_func_def=$(find_func_line "${func}" "${sour}"); # 2>/dev/null
    fi

    if [[ "${bottom}" == "main" ]] || [[ "${bottom}" == "source" ]]; then
        ddecho "${TAB}BASH_LINENO refers to file"
        local -i call_line=$func_line
        # print file line
        in_line "$@"
        echo -en "${FUNCNAME}() $(lec_mes)${call_line}${fcol} "
        echo -e "in file ${fcol}${YELLOW}${sour_fil}${fcol}"
    else
        ddecho "${TAB}BASH_LINENO refers to function"
        # get the line in the file where the function is called
        # add the line where to the function is defined within the file and the line within the function
        local -i call_line=$(($line_func_def + $func_line -1 ))
        # print file line
        in_line "$@"
        echo -en "${FUNCNAME}() $(lec_mes)${call_line}${fcol} "
        echo -e "in file ${YELLOW}${sour_fil}${fcol}"
        itab
        # print function line
        ddecho -n "${TAB}$(lec_mes)${func_line} "
        ddecho "in function ${func}()"
        dtab
        echo -en "${RESET}"
    fi

    itab
    # print definition line
    ddecho -n "${TAB}function ${func}() "
    ddecho -ne "defined on line ${GRL}${line_func_def}${fcol} "
    ddecho -e "in file ${YELLOW}${sour_fil}${fcol}"
    ddecho -e "${TAB}file ${YELLOW}${sour_fil}${fcol} located in ${DIR}${sour_dir}${fcol}"
    ddecho "${TAB}exiting ${FUNCNAME}() on line $((${line_def}+${LINENO}-1))..."

    dtab
    if [ ${DEBUG:-0} -gt 2 ]; then
        print_stack
    fi

    # reset shell options
    reset_shell ${old_opts}
}

function print_debug() {
    set -u
    funcDEBUG=0

    # check if DEBUG is unset
    if [ -z ${DEBUG+dummy} ]; then
        echo -e "${TAB}${BOLD}DEBUG is ${UNSET}"
        return 0
    fi
    # check if DEBUG is set but NULL
    if [ -z ${DEBUG:+dummy} ]; then
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
    if [[ "$-" == *u* ]]; then
        set +u
    fi
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
        fecho -e "${BOLD}DEBUG is ${UNSET}"
        fecho "setting DEBUG..."
        export DEBUG=
    fi

    # check if DEBUG is set but NULL
    if [ -z ${DEBUG:+dummy} ]; then
        fecho -e "${BOLD}DEBUG is ${NULL}"
        fecho "setting DEBUG to zero..."
        unset DEBUG
        declare -igx DEBUG=0
    fi

    # check if DEBUG is a number
    local num='^[0-9]+$'
    fecho -e "DEBUG = ${DEBUG}"
    if [[ "$DEBUG" =~ $num ]]; then
        fecho "DEBUG is a number"
    else
        fecho "DEBUG is not a number"
        fecho "setting DEBUG to zero..."
        unset DEBUG
        declare -igx DEBUG=0
    fi

    # determine how many iteration to run
    local -i N
    if [ $# -eq 0 ]; then
        N=1
    else
        N=$1
    fi

    fecho "incrementing DEBUG by $N..."
    # increment DEBUG by N
    local -i i
    for ((i = 1; i <= $N; i++)); do
        ((++DEBUG))
    done
    export DEBUG

    fecho -e "DEBUG = ${DEBUG}"
    if [ $funcDEBUG -eq 0 ];then
        start_new_line
        print_debug
    fi
    if [[ "$-" == *u* ]]; then
        set +u
    fi
}

function ddb() {
    set -u
    funcDEBUG=0

    # check if DEBUG is unset
    if [ -z ${DEBUG+dummy} ]; then
        fecho -e "${BOLD}DEBUG is ${UNSET}"
        fecho "setting DEBUG..."
        export DEBUG=
    fi

    # check if DEBUG is set but NULL
    if [ -z ${DEBUG:+dummy} ]; then
        fecho -e "${BOLD}DEBUG is ${NULL}"
        fecho "setting DEBUG to zero..."
        unset DEBUG
        declare -igx DEBUG=0
    fi

    # check if DEBUG is a number
    local num='^[0-9]+$'
    fecho -e "DEBUG = ${DEBUG}"
    if [[ "$DEBUG" =~ $num ]]; then
        fecho "DEBUG is a number"
    else
        fecho "DEBUG is not a number"
        fecho "setting DEBUG to zero..."
        unset DEBUG
        declare -igx DEBUG=0
    fi

    # check if DEBUG is zero
    if [ $DEBUG -eq 0 ]; then
        fecho "no action needed"
        if [ $funcDEBUG -eq 0 ];then
            start_new_line
            print_debug
        fi
        return 0
    fi

    # determine how many iteration to run
    local -i i
    local -i N
    if [ $# -eq 0 ]; then
        N=1
    else
        N=$1
    fi

    if [ $N -gt $DEBUG ]; then
        fecho "DEBUG level exceeded"
        fecho "resetting DEBUG to zero..."
        DEBUG=0
    else
        # increment DEBUG by N
        fecho "decrementing DEBUG by $N"
        for ((i = 1; i <= $N; i++)); do
            ((DEBUG--))
        done
    fi
    export DEBUG

    fecho -e "DEBUG = ${DEBUG}"
    if [ $funcDEBUG -eq 0 ];then
        start_new_line
        print_debug
    fi
    if [[ "$-" == *u* ]]; then
        set +u
    fi
}

function print_() {
    set -u
    local -i DEBUG=0
    DEBUG=${libDEBUG:-0}

    # this function modifies the value of TAB, so save the value if it is included in the
    # arguments
    if [[ "$@" =~ "TAB" ]]; then
        decho -e "${TAB-}${YELLOW}argument contain TAB${RESET}"
        # check if TAB is set
        if [ -z ${TAB+dummy} ]; then
            decho -e "TAB is $UNSET"
            unset TAB_input
        else
            decho "${TAB}TAB is set"
            local TAB_input=$TAB
        fi
    fi

    # set tab
    set_tab_shell

    # parse inputs
    if [ $# -eq 0 ]; then
	      echo "${TAB}no input received"
	      echo "${TAB}possibilities:"
        itab
	      echo "${TAB}- no argument given"
	      echo "${TAB}- value passed (e.g. \$VB) instead of name (e.g. VB) and:"
        itab
	      echo -e "${TAB}- input is ${UNSET}"
	      echo -e "${TAB}- input is ${NULL}"
        dtab
	      if ! (return 0 2>/dev/null); then
		        echo "${TAB}- input is not exported"
	      fi
        dtab
        return 1
    else
	      decho -n "${TAB}argument"
	      if [ $# -gt 1 ]; then
		        decho -n "s"
	      fi
	      decho ": $@ "
	      input=$@
    fi

    if [ $# -gt 1 ]; then
        decho -n "${TAB}looping over variables:"
    else
        decho -n "${TAB}testing variable"
    fi
    decho " $input..."

    # test inputs
    for VAR in $input; do
        if [ ${DEBUG:-1} -gt 0 ]; then
            echo -e "${TAB}${BOLD}${VAR}${RESET}"
            itab
        fi

        # check if VAR is called VAR
        if [[ "${VAR}" == "${!VAR-dummy}" ]]; then
		        echo -e "${TAB}${BAD}${!VAR} matches FOR loop variable name${RESET}"
            decho "${TAB}${VAR} name: ${VAR}"
            decho "${TAB}${VAR} value: '${!VAR}'"
            decho "${TAB}continuing..."
            [ ${DEBUG:-1} -gt 0 ] && dtab
            continue
	      fi

        # check if VAR is TAB
        if [[ "${VAR}" == "TAB" ]]; then
            decho -e "${TAB}${YELLOW}argument is TAB${RESET}"
            decho -e "${TAB}${YELLOW}using input value TAB${RESET}"
            VAR="TAB_input"
	      fi

        # check if VAR is unset
        if [ -z ${!VAR+dummy} ]; then
            echo -e "${TAB-}${BOLD}$VAR is ${UNSET}"
            [ ${DEBUG:-1} -gt 0 ] && dtab
            continue
        fi
        decho "${TAB}${VAR} is set"

        # check if VAR is NULL
        if [ -z ${!VAR:+dummy} ]; then
            echo -e "${TAB}${BOLD}${VAR} is ${NULL}"
            [ ${DEBUG:-1} -gt 0 ] && dtab
            continue
        fi
        decho "${TAB}${VAR} is not empty"

        # check if VAR is an integer
				if [[ "${!VAR}" =~ ^[0-9]+$ ]]; then
            decho "${TAB}${VAR} is an integer"
            # check if !VAR is zero
            if [ ${!VAR} -eq 0 ]; then
                echo -e "${TAB}${GRAY}${VAR} is 0${RESET}"
                [ ${DEBUG:-1} -gt 0 ] && dtab
                continue
            fi

            echo "${TAB}${VAR} = ${!VAR}"
            [ ${DEBUG:-1} -gt 0 ] && dtab
            continue

        fi
        decho "${TAB}${VAR} is not an integer"

        # check if VAR contains any spaces
		    if [[ "${!VAR}" =~ " " ]]; then
            decho "${TAB}${VAR} contians whitespace"
            if [[ "${!VAR}" == " "* ]]; then
                decho "${TAB}${VAR} is only whitespace"
                echo -n "${TAB}${VAR} = "
                echo -e "${!VAR}" | sed "s/ /${SPACE} ${RESET}/g"
            else
                echo "${TAB}${VAR} = ${!VAR}"
            fi
            [ ${DEBUG:-1} -gt 0 ] && dtab
            continue
        fi
        decho "${TAB}${VAR} does not contians whitespace"

        # check if VAR is true or false
				if [ ${!VAR} = true ] || [ ${!VAR} = false ]; then
            decho "${TAB}${VAR} is boolean"
            echo -n "${TAB}${VAR} is "
					  if ${!VAR}; then # fails when what
						    echo -e "${TRUE}"
					  else
						    echo -e "${FALSE}"
					  fi
            [ ${DEBUG:-1} -gt 0 ] && dtab
            continue
        fi
        decho "${TAB}${VAR} is not boolean"

        echo "${TAB}${VAR} = ${!VAR}"

        [ ${DEBUG:-1} -gt 0 ] && dtab
    done

    if [[ "$@" =~ "TAB" ]]; then
        decho "${TAB-}resetting tab..."
        if [ -z ${TAB_input+dummy} ]; then
            unset TAB
        else
            TAB=$TAB_input
        fi
        [ ${DEBUG:-0} -gt 0 ] && print_tab
    fi
    if [[ "$-" == *u* ]]; then
        set +u
    fi
}
