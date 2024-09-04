#!/bin/bash -u

function test_this_line() { #1
    # print file line       #2
    this_line "manual: 3/5, LINENO: $LINENO, BASH_LINENO: ${BASH_LINENO[@]}" #3
}

function test_that_line() {                            #1
    echo "${TAB}THIS FUNCTION IS DEFINED ON LINE 8"    #2
    # print file line                                  #3
    this_line "here 11/4 $LINENO $BASH_LINENO"         #4
    # print hard-coded examples                        #5 here, LINENO gives line in function
    echo -e "${TAB}${GRH}${INVERT}hello${RESET} ${GRF}${BASH_SOURCE##*/}${GRS}:${GRL}$LINENO${GRS}: ${GRH}echo()${RESET} ${dcolor[7]}${FUNCNAME}()${RESET}" >&2 #6
    echo -e "${TAB}${GRH}${INVERT}hello${RESET} ${GRF}${BASH_SOURCE##*/}${GRS}:${GRL}$LINENO${GRS}: ${GRH}echo()${RESET}" >&2 #7
    this_line "hello"                                  #8
    hello                                              #9
    this_line "this is FILE line 17, FUNCTION line 10" #10
    hello                                              #11
    this_line                                          #12
    hello                                              #13
    this_line "this is line 21"                        #14
    this_line "there"                                  #15
    if [[ "$-" == *u* ]]; then                         #16
        set +u                                         #17
    fi                                                 #18
}

function test_lecho() {
    this_line "hello 2/29" #2
        lecho "hello 3/30" #3
       plecho "hello 4/31" #4
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
