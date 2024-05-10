# -----------------------------------------------------------------------------------------------
# TABS LIBRARY
# -----------------------------------------------------------------------------------------------
#
# ~/config/lib_tabs.sh
#
# PURPOSE: Define functions for making and controling indentation (tabs). Two variables are
# defined: TAB and fTAB. TAB is used to store the current level of indentation. fTAB defines the
# indentation increment (tab).
#
# Mar 2024 JCL
#
# -----------------------------------------------------------------------------------------------

# define tab
# the variable fTAB is used for setting, incrementing, and decrementing the TAB variable.
# initially defined as the "in-file" or "in-function" tab to force additional indentation.
function set_ftab() {
    # if fTAB is unset or null, then assign default value
    export fTAB=${fTAB:='   '}
}

# clear tab
function ctab() {
    export TAB=''
}

# reset tab
function rtab() {
    set_ftab
    ctab
}

# set the indentation according to execution stack size
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

# increment tab
function itab() {
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

# decrement tab
function dtab() {
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
    set -u

    # check if TAB is set
    if [ -z ${TAB+dummy} ]; then
        local UNSET='\E[1;33munset\E[0m'
        echo -e "${BOLD}TAB is ${UNSET}"
        return
    fi
    
    # check if TAB is null
    if [ -z ${TAB:+dummy} ]; then
        local NULL='\E[1;36mnull\E[0m' 
        echo -e "${TAB}${BOLD}TAB is ${NULL}"
        return
    fi
    
    # get length of TAB
    local -i i=0
    i=${#TAB}

    local SPACE='\E[30;106m' # highlight white space
    # print size of TAB
    echo -e "${TAB}TAB = ${SPACE}${TAB}${RESET} length $i"
}
