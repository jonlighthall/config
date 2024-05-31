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

# stack echo
function secho() {
    print_stack 2
}

function in_line() {
    if [ ! -z "$@" ]; then
        echo -en "${TAB}${INVERT}$@${NORMAL} <- ${fcol}"
    else
        echo -en "${TAB}${fcol}"
    fi
}

function lec_mes () {
    echo -en "line $BASH_LINENO called on line ${GRL}"
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
function lecho() {
    # save shell options
    old_opts=$(echo "$-")

    # set local debug level
    local -i DEBUG=${DEBUG:-1}
    local -i oldDEBUG=$DEBUG
    if [ $DEBUG -lt 1 ]; then
        idb >/dev/null
    fi
    DEBUG=2
    local -i idx
    dbg2idx 9 idx
    local fcol=${dcolor[idx]}
    echo -ne "${fcol}"
    dddecho -e "${TAB}${INVERT}${FUNCNAME}${fcol}"
    set -u

    if [ ${DEBUG:-0} -gt 0 ] || [ ! -z "$@" ] ; then
        local -i tab_wid=${#TAB}
        local -i term_wid=${COLUMNS:-72}
        local -i hr_wid=$(($term_wid - (2 * $tab_wid)))
        hline $hr_wid
        trap 'hline $hr_wid;DEBUG=$oldDEBUG;echo -e "$RESET";trap -- RETURN' RETURN
    fi
    if [ ${DEBUG:-0} -gt 1 ]; then
        print_stack
    fi

    # get the lenght of the execution stack
    local -i N_BASH=${#BASH_SOURCE[@]}
    ddecho "${TAB}there are ${N_BASH} entries in the ${FUNCNAME}() call stack"

    # get the line where THIS function is defined
    line_def=$(find_func_line "${FUNCNAME}" "${BASH_SOURCE}" 2>/dev/null);

    if [ $N_BASH -eq 1 ]; then
        in_line "$@"
        echo -e "$(lec_mes)${BASH_LINENO}${fcol} from ${BASH##*/}"
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
        echo -e "$(lec_mes)${BASH_LINENO[(($N_BASH-2))]}${fcol} in file ${YELLOW}${BASH_SOURCE[(($N_BASH-1))]##*/}${fcol}"
        ddecho "exiting ${FUNCNAME} on line ${LINENO}"
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
        echo -en "${FUNCNAME}() $(lec_mes)${call_line}${fcol} "
        echo -e "in file ${fcol}${YELLOW}${sour_fil}${fcol}"
    else
        ddecho "${TAB}BASH_LINENO refers to function"
        # get the line in the file where the function is called
        # add the line where to the function is defined within the file and the line within the function
        local -i call_line=$(($line_func_def -1 + $func_line))
        # print file line
        in_line "$@"
        echo -n "$(lec_mes)${call_line} "
        echo -e "in file ${YELLOW}${sour_fil}${fcol}"
        itab
        # print function line
        ddecho -n "${TAB}$(lec_mes)${func_line} "
        ddecho "in function ${func}()"
        dtab
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

    local -i DEBUG=${DEBUG:-1}
    # DEBUG=2
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
    ddecho "${TAB}there are ${N_BASH} entries in the ${FUNCNAME}() call stack"

    # get the line where THIS function is defined
    line_def=$(find_func_line "${FUNCNAME}" "${BASH_SOURCE}" 2>/dev/null);

    if [ $N_BASH -eq 1 ]; then
        in_line "$@"
        echo -e "$(lec_mes)${BASH_LINENO}${RESET} from ${BASH##*/}"
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
    fi

    # get the file of the calling function
    local sour=${BASH_SOURCE[((1-$N_off))]}
    local sour_dir=$(dirname "${sour}")
    local sour_fil=$(basename "${sour}")
    ddecho "${TAB}source is ${sour}"
    ddecho "${TAB}source is ${sour_fil} in ${sour_dir}"

    # get the line of of the calling function
    local -i func_line=${BASH_LINENO[0]}

    ddecho "${TAB}${FUNCNAME}"
    ddecho "${TAB}FUNCNAME stack = ${FUNCNAME[@]}"
    ddecho "${TAB}N_BASH = ${N_BASH}"
    local -i func_lev=$(($N_BASH - 1 + ${N_off}))
    ddecho "${TAB}function level = $func_lev"
    ddecho "${TAB}function : ${FUNCNAME[$func_lev]}"

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
        echo -e "${FUNCNAME[(($func_lev-1))]}() $(lec_mes)${BASH_LINENO[(($func_lev-1))]}${RESET} in file ${YELLOW}${BASH_SOURCE[$func_lev]##*/}${RESET}"
        itab
        ((--func_lev))
        ddecho -e "${TAB}${BASH_SOURCE[$func_lev]##*/}${RESET} called by ${SHELL##*/}"
        ddecho -e "${TAB}${FUNCNAME[(($func_lev-1))]}() $(lec_mes)${BASH_LINENO[(($func_lev-1))]}${RESET} in file ${YELLOW}${BASH_SOURCE[$func_lev]##*/}${RESET}"

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
        echo -en "${FUNCNAME}() $(lec_mes)${call_line}${RESET} "
        echo -e "in file ${RESET}${YELLOW}${sour_fil}${RESET}"
    else
        ddecho "${TAB}BASH_LINENO refers to function"
        # get the line in the file where the function is called
        # add the line where to the function is defined within the file and the line within the function
        local -i call_line=$(($line_func_def -1 + $func_line))
        # print file line
        in_line "$@"
        echo -n "$(lec_mes)${call_line} "
        echo -e "in file ${YELLOW}${sour_fil}${RESET}"
        itab
        # print function line
        ddecho -n "${TAB}$(lec_mes)${func_line} "
        ddecho "in function ${func}()"
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

    # reset shell options
    reset_shell ${old_opts}
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
        fecho -e "DEBUG = ${DEBUG}"
        return 0
    fi

    # check if DEBUG is set but NULL
    if [ -z ${DEBUG:+dummy} ]; then
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
        fecho -e "${BOLD}DEBUG is ${UNSET}"
        fecho "setting DEBUG..."
        export DEBUG=
        fecho -e "DEBUG = ${DEBUG}"
        return 0
    fi

    # check if DEBUG is set but NULL
    if [ -z ${DEBUG:+dummy} ]; then
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

function print_() {
    set -u
    funcDEBUG=1
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

        if [ ${DEBUG:-0} -gt 0 ]; then
            set_tab_shell
            echo -e "${TAB}${BOLD}${VAR}${RESET}"
            itab
        fi

        # check if VAR is unset
        if [ -z ${!VAR+dummy} ]; then
            echo -e "${TAB}${BOLD}$VAR is ${UNSET}"
            continue
        fi
        decho "${TAB}${VAR} is set"

        # check if VAR is called VAR
        if [[ "${VAR}" == "${!VAR}" ]]; then
		        echo -e "${TAB}${BAD}value of variable name matches FOR loop variable name${RESET}"
            echo "${TAB}breaking..."
		        break
	      fi

        # check if VAR is NULL
        if [ -z ${!VAR:+dummy} ]; then
            echo -e "${TAB}${BOLD}${VAR} is ${NULL}"
            continue
        fi
        decho "${TAB}${VAR} is not empty"

        # check if VAR is an integer
				if [[ "${!VAR}" =~ ^[0-9]+$ ]]; then
            decho "${TAB}${VAR} is an integer"
            # check if !VAR is zero
            if [ ${!VAR} -eq 0 ]; then
                echo -e "${TAB}${GRAY}${VAR} is 0${RESET}"
                continue
            fi

            echo "${TAB}${VAR} = ${!VAR}"
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
            continue
        fi
        decho "${TAB}${VAR} is not boolean"

        echo "${TAB}${VAR} = ${!VAR}"

    done

}
