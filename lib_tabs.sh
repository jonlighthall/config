# -----------------------------------------------------------------------------------------------
# TABS LIBRARY
# -----------------------------------------------------------------------------------------------
#
# ~/config/lib_tabs.sh
#
# PURPOSE: Define functions for making and controling indentation (tabs).
#
# Mar 2024 JCL
#
# -----------------------------------------------------------------------------------------------

function set_ftab() {
    # if fTAB is unset or null, then assign default value
    export fTAB=${fTAB:='   '}
}

function set_tab() {
    local -i funcDEBUG=0
    local -i DEBUG=0
    
    # reset TAB
    rtab

    # get the lenght of the execution stack
    fecho "getting length of stack..."
    local -i N_BASH=${#BASH_SOURCE[@]}
    decho -n "${TAB}$N_BASH"
    if [[ "$-" == *i* ]]; then
        decho -n "i"
    fi
    decho " ($FUNCNAME)"
    decho "${TAB}call stack:"
    local -i i
    for ((i = 0; i < $N_BASH ; i++)); do
        decho "   $i:${FUNCNAME[i]}:${BASH_SOURCE[i]##*/}:${BASH_LINENO[i]}"
    done

    fecho "stack size is $N_BASH"

    # since this is a function, reduce N_BASH by one
    fecho "reducing stack..."
    ((N_BASH--))
    fecho "SHLVL = $SHLVL"
    if [ $SHLVL -gt 1 ]; then
        ((N_BASH--))
    fi
    fecho "stack size is $N_BASH"

    if [ $N_BASH -gt 0 ]; then 
        
        # set the tab length
        local -ir N_TAB=$(($N_BASH-1))
        fecho "indent $N_TAB tabs"
        
        # set tab
        fecho "${TAB}."
        itab $N_TAB
        fecho "${TAB}."
    fi
}

function ctab() {
    # clear tab
    export TAB=''
}

function rtab() {
    # reset tab
    set_ftab
    ctab
}

function itab() {
    # increment tab
    set_ftab
    # determine how many iteration to run
    local -i N
    if [ $# -eq 0 ]; then
        N=1
    else
        N=$1
    fi
    local -i i
    for ((i = 1; i <= $N; i++)); do
        # increment TAB by fTAB
        TAB+=${fTAB}
    done
    export TAB
}

function dtab() {
    # decrement tab
    set_ftab
    # determine how many iteration to run
    local -i N
    if [ $# -eq 0 ]; then
        N=1
    else
        N=$1
    fi
    local -i i
    for ((i = 1; i <= $N; i++)); do
        # decrement TAB by fTAB
        TAB=${TAB%$fTAB}
    done
    export TAB
}

function check_tab() {
    local -i i=0
    # check if TAB is set
    if [ -z ${TAB+dummy} ]; then
        echo -e "TAB ${YELLOW}unset${RESET}"
    else
        # get length of TAB
        i=${#TAB}
    fi
    
    # if TAB is unset, then assign default value
    export TAB=${TAB=''}
    set_ftab
    
    #get new length of TAB
    local -i j=${#TAB}

    # if TAB changed length or was unset, print size of TAB
    if [ $i -ne $j ]; then
        decho -e "${TAB}TAB = \E[106m${TAB}${RESET} length $j"
    fi
}
