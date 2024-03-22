# -----------------------------------------------------------------------------
# TABS LIBRARY
# -----------------------------------------------------------------------------
#
# ~/config/lib_tabs.sh
#
# PURPOSE: Define functions for making and controling indentation (tabs). Two
#   variables are defined: TAB and fTAB. TAB is used to store the current level
#   of indentation. fTAB defines the indentation increment (tab).
#
# Mar 2024 JCL
#
# ------------------------------------------------------------------------------

# define tab
# the variable fTAB is used for setting, incrementing, and decrementing the TAB
# variable.  initially defined as the "in-file" or "in-function" tab to force
# additional indentation.
function set_ftab() {
    # set alternate tabbing here
    #guide_tabs
    # if fTAB is unset or null, then assign default value
    export fTAB=${fTAB:='   '}
}

function set_tab() {
    # reset TAB
    rtab

    # get the lenght of the execution stack
    local -i N_BASH=${#BASH_SOURCE[@]}

    # since this is a function, reduce N_BASH by one
    ((N_BASH--))
    if [ $SHLVL -gt 1 ]; then
        ((N_BASH--))
    fi
    export N_BASH

    # set the tab length
    local -ir N_TAB=$(($N_BASH-1))
    
    # set tab
    itab $N_TAB
}

# clear tab
function ctab() {
    export TAB=''
}

# reset tab
function rtab() {
    unset fTAB
    set_ftab
    ctab
}

# set the indentation according to execution stack size
function set_tab() {
    local -i funcDEBUG=0

    # reset TAB
    rtab

    # get the lenght of the execution stack
    fecho "getting length of stack..."
    local -i N_BASH=${#BASH_SOURCE[@]}
    fecho "${fTAB}stack size is $N_BASH"
    if [[ "$-" == *i* ]]; then
        fecho "${fTAB}shell is interactive"
    fi

    # print the execution stack
    fecho "printing stack..."
    local -i i
    for ((i = 0; i < $N_BASH ; i++)); do
        fecho "${fTAB}$i:${FUNCNAME[i]}:${BASH_SOURCE[i]##*/}:${BASH_LINENO[i]}"
    done

    # since this is a function, reduce N_BASH by one
    fecho "reducing stack..."
    ((N_BASH--))
    fecho "${fTAB}SHLVL = $SHLVL"
    if [ $SHLVL -gt 1 ]; then
        ((N_BASH--))
    fi
    fecho "${fTAB}stack size is $N_BASH"

    if [ $N_BASH -gt 0 ]; then
        # set the tab length
        local -ir N_TAB=$(($N_BASH-1))
        fecho "indent $N_TAB tabs"

        # set tab
        local SPACE='\E[30;106m' # highlight white space
        # print size of TAB
        i=${#TAB}
        fecho -e "TAB = ${SPACE}${TAB}${RESET} length $i"
        itab $N_TAB
        j=${#TAB}
        fecho -e "TAB = ${SPACE}${TAB}${RESET} length $j"
        if [ $i -eq $j ];then
            fecho "TAB length unchanged"
        else
            fecho "TAB length changed"
        fi
    fi

    if [[ "${RUN_TYPE}" =~ "sourcing" ]]; then
        if [ $N_BASH -gt 1 ]; then
            fecho -e "${DIM}${FUNCNAME[1]}${NORMAL} will reduce tab"
        fi
    fi
}

# set the indentation according to the shell level
function set_tab_shell() {
    local -i funcDEBUG=0

    # get length of TAB
    local -i i=0
    if [ -z ${TAB+dummy} ]; then
        fecho -e "TAB is $UNSET"
        fecho "setting tab..."
        export TAB=
    fi

    local tab
    strip_pretty tab "$TAB"
    i=${#tab}

    local SPACE='\E[30;106m' # highlight white space
    # print size of TAB
    fecho -e "TAB = ${SPACE}${TAB}${RESET} length $i"

    # get the shell level
    fecho "getting shell level..."
    local -i N_SHL=$SHLVL
    fecho "${fTAB}SHLVL = $SHLVL"

    # set the tab length
    local -i N_TAB=$N_SHL
    # minimum shell level is one, which corresponds to zero tab size
    ((N_TAB--))

    # since this is a function, reduce N_SHL by one
    ((N_TAB--))
    fecho "${fTAB}SHLVL indent $N_TAB tabs"

    # get the lenght of the execution stack
    fecho "getting length of stack..."
    [ $funcDEBUG -gt 0 ] && print_stack
    local -i N_BASH=${#BASH_SOURCE[@]}
    fecho "${fTAB}N_BASH = $N_BASH"
    if [[ "$-" == *i* ]]; then
        fecho "interactive"
    fi

    if [ $N_BASH -ne $N_SHL ]; then
        fecho -e "${fTAB}${GRH}shell/stack mis-match${RESET}"
        #fecho "${fTAB}increasing level..."
        #((++N_TAB))
        fecho "${fTAB}BASH indent $N_TAB tabs"
    fi

    # set tab
    itab $N_TAB
    strip_pretty tab "$TAB"
    j=${#tab}
    fecho -e "TAB = ${SPACE}${TAB}${RESET} length $j"
    if [ $i -eq $j ];then
        fecho "TAB length unchanged"
    else
        fecho "TAB length changed"
    fi

    return 0
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
    local -i funcDEBUG=0
    # check if TAB is unset
    if [ -z ${TAB:+dummy} ]; then
        fecho -e "${BOLD}TAB is ${UNSET}"
        fecho "setting TAB..."
        export TAB=
    fi

    # define increment
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

function print_tab() {
    set -u

    # check if TAB is set
    if [ -z ${TAB+dummy} ]; then
        echo -e "${BOLD}TAB is ${UNSET}"
        return 0
    fi

    # check if TAB is null
    if [ -z ${TAB:+dummy} ]; then
        echo -e "${TAB}${BOLD}TAB is ${NULL}"
        return 0
    fi

    # get length of TAB
    local -i i=0
    local tab
    strip_pretty tab "$TAB"
    i=${#tab}

    # print size of TAB
    echo -e "${TAB}TAB = ${SPACE}${TAB}${RESET} length $i"

    # reset shell options before returning to shell
    if [[ "$-" == "*u*" ]]; then
        set +u
    fi

}
