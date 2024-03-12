#TODO move all functions to bash_tabs.sh

function set_ftab() {
    # if fTAB is unset, assign default value
    export fTAB=${fTAB='   '}
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

function set_tab() {
    local -i i=0
    # check if TAB is set
    if [ -z ${TAB+dummy} ]; then
        echo -e "TAB ${yellow}unset${NORMAL}"
    else
        # get length of TAB
        i=${#TAB}
    fi
    
    # if TAB is unset, then assign default value
    export TAB=${TAB=''}
    # if fTAB is unset or null, then assign default value
    export fTAB=${fTAB:='   '}
    
    #get new length of TAB
    local -i j=${#TAB}

    # if TAB changed length or was unset, print size of TAB
    if [ $i -ne $j ]; then
        decho -e "${TAB}TAB = \E[106m${TAB}${NORMAL} length $j"
    fi
}

set_tab
