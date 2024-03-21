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
    export fTAB=${fTAB:='|  '}
}

function set_tab() {
    # reset TAB
    rtab

    # get the lenght of the execution stack
    local -ig N_BASH=${#BASH_SOURCE[@]}
    # since this is a function, reduce N_BASH by one
    ((N_BASH--))
    
    # set the tab length
    local -ir N_TAB=$(($N_BASH-1))
    
    # set tab
    itab $N_TAB
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
