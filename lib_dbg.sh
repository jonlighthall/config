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

# line echo
function lecho() {
    local -i DEBUG=${DEBUG:-1}
    set -u

    if [ ${DEBUG:-0} -gt 0 ]; then
        decho -n "${TAB}"
        hline
    fi
    
    # get the lenght of the execution stack
    local -i N_BASH=${#BASH_SOURCE[@]}
    ddecho "${TAB}there are ${N_BASH} entries in the call stack"

    if [ $N_BASH -eq 1 ]; then
        echo "${TAB}called on line ${BASH_LINENO} from ${BASH##*/}"
        return
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
    local bottom=${FUNCNAME[(($N_BASH - 1))]##*/}
    ddecho "${TAB}bottom of stack is $bottom"

    # get the calling function 
    local func=${FUNCNAME[1]}
    ddecho "${TAB}calling function is $func"

    if [[ "${func}" == "main" ]] || [[ "${func}" == "source" ]]; then
        # the function is call from bash
        echo "called from line ${BASH_LINENO[(($N_BASH-2))]} in file ${BASH_SOURCE[(($N_BASH-1))]##*/}"
        return
    else
        # get the line where the function is defined
        local line_func_def=$(grep -n "$(declare -f ${func} | head -1 | sed 's/ /[ ]*/')" "${sour}" | awk -F: '{print$1}')   
    fi    
    
    if [[ "${bottom}" == "main" ]] || [[ "${bottom}" == "source" ]]; then
        ddecho "${TAB}BASH_LINENO refers to file"
        local -i call_line=$func_line
        # print file line
        echo -en "${TAB}${GRH}called on line ${call_line} "
        echo -e "in file ${sour_fil}${RESET}"           
    else
        ddecho "${TAB}BASH_LINENO refers to function"
        # get the line in the file where the function is called
        # add the line where to the function is defined within the file and the line within the function
        local -i call_line=$(($line_func_def -1 + $func_line))
        # print file line
        echo -n "${TAB}called on line ${call_line} "
        echo "in file ${sour_fil}"    
        itab
        # print function line
        decho -n "${TAB}called on line ${func_line} "
        decho "in function ${func}()"    
        dtab
    fi
    
    itab
    # print definition line
    decho -n "${TAB}function ${func}() "
    decho -n "defined on line ${line_func_def} "
    decho "in file ${sour_fil}"
    decho "${TAB}file ${sour_fil} located in ${sour_dir}"
    
    dtab
    if [ ${DEBUG:-0} -gt 2 ]; then
        print_stack
    fi

    if [ ${DEBUG:-0} -gt 0 ]; then
        decho -n "${TAB}"
        hline
    fi

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
