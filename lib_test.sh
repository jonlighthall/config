#!/bin/bash -u

function test_this_line() {
    # print file line
    this_line $LINENO ${BASH_LINENO[@]}
}

function test_that_line() {
    echo "${TAB}THIS FUNCTION IS DEFINED ON LINE 8"   #1
    # print file line                                 #2
    this_line "here"                                  #3
    # print hard-coded examples                       #4 here, LINENO gives line in function
    echo -e "${TAB}${GRH}${INVERT}hello${RESET} ${GRF}${BASH_SOURCE##*/}${GRS}:${GRL}$LINENO${GRS}: ${GRH}echo()${RESET} ${dcolor[7]}${FUNCNAME}()${RESET}" >&2 #5
    echo -e "${TAB}${GRH}${INVERT}hello${RESET} ${GRF}${BASH_SOURCE##*/}${GRS}:${GRL}$LINENO${GRS}: ${GRH}echo()${RESET}" >&2 #6
    this_line "hello"                                 #7
    hello                                             #8
    this_line "this is FILE line 17, FUNCTION line 9" #9
    hello                                             #10
    this_line                                         #11
    hello                                             #12
    this_line "this is line 21"                       #13
    this_line "there"                                 #14
    if [[ "$-" == *u* ]]; then
        set +u
    fi
}

function test_lecho() {
    this_line "hello 1"
    lecho "hello 2"
    plecho "hello 3"
}

# test parent line echo
function tplecho() {
    set_tab_shell
    print_tab
    echo -e "${TAB}${INVERT}${FUNCNAME}${RESET}"
    itab
    tlecho "$@"
    dtab
    rdb
    trap 'print_return $?; trap - RETURN' RETURN
}

# test cursor position
function ind() {
    ddecho -e "${TAB}${INVERT}${FUNCNAME}${RESET}"
    local -i DEBUG=1
    local -i funcDEBUG=1

    local -i x1=0
    local -i y1=0
    local -i x2=0
    local -i y1=0

    echo -n "indented: "
    # pass variable names, not values
    get_curpos x1 y1
    echo "x = $x1"
    echo "y = $y1"
    echo "BASH_LINENO = ${BASH_LINENO[@]}"

    echo "not indented: "
    get_curpos x2 y2
    echo "x = $x2"
    echo "y = $y2"
    echo "BASH_LINENO = ${BASH_LINENO[@]}"

    if [ $x1 = $x2 ]; then
        echo "x-position did not change"
    fi

    if [ $y1 = $y2 ]; then
        echo "y-position did not change"
    fi

    if [ $x1 = $x2 ] && [ $y1 = $y2 ]; then
        echo "cursor position did not change"
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

# function to test calling an alias from a function
function fello() {
    local -i DEBUG=1
    # print function name
    decho -e "${TAB}${INVERT}${FUNCNAME}${RESET}"
    # define function name
    func='hello'
    # test if alias
    if [[ $(type -t $func) == "alias" ]]; then
        decho "${TAB}$func type is alias"
        echo -n "${TAB}"
        # evaluate
        eval $func
    else
        decho "${TAB}$func type is not alias"
        # print debug value
        print_debug
        # print shell options
        decho "${TAB}shell options = $-"
        # add return code for parent script
        if [ $DEBUG -gt 0 ]; then
            trap 'print_return $?; trap - RETURN' RETURN
        fi
        return 1
    fi
}

function driver() {
    # print function name
    decho -e "${TAB}${INVERT}${FUNCNAME}${RESET}"
    local -i DEBUG=1
    set_traps

    # -T  If set, the DEBUG and RETURN traps are inherited by shell functions.
    set -T
    trap 'echo "${TAB}${FUNCNAME} return"' RETURN
    decho "${TAB}shell options = $-"
    fello
}

function driver2() {
    driver
}

# print shell
function huh() {
    local -i DEBUG=1
    # print function name
    decho -e "${TAB}${INVERT}${FUNCNAME}?${RESET}"

    itab
    echo "${TAB}shell options: $-"
    echo -n "${TAB}traps: "
    if [ -z "$(trap -p)" ]; then
        echo "${TAB}none"
    else
        echo
        itab
        trap -p
        dtab
    fi

    print_debug
    print_tab
    dtab
    if [ ${DEBUG-0} -gt 0 ]; then
        trap 'print_return $?; trap - RETURN' RETURN
    fi
    return 0
}

# test line echo
function tlecho() {
    echo -e "${TAB}${INVERT}${FUNCNAME}${RESET}"
    DEBUG=3
    itab
    lecho "$@"
    plecho "$@"
    dtab
    rdb
    trap 'print_return $?; trap - RETURN' RETURN
}
