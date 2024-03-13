# conditional echos

# Name:
#   VECHO - "verbose bash" echo
# Description:
#   Arguments are passed to ECHO based on the value of VB in the calling function.
# Purpose:
#   Turn verbose printing on/off in the .bashrc logon scripts
# Globals:
#   VB
# Arguments:
#   text to display and echo flags

vecho() {
    if [ ! -z ${VB:+dummy} ] && ${VB}; then
        # [not (unset or null)] or true -> print if true or null or unset
        echo "$@"
    fi
}

# Name:
#   FECHO - function echo
# Description:
#   Arguments are passed to ECHO based on the value of funcDEBUG in the calling function.
#
#   NB: only define funcDEBUG as a local variable within FUNCTIONS
#
# Purpose:
#   Bypass, override, or augment the function decho() and the value of DEBUG.
# Globals:
#   funcDEBUG
# Arguments:
#   text to display and echo flags

function fecho() {    
    # check if funcDEBUG is defined
    if [ ! -z ${funcDEBUG:+dummy} ]; then
        # check if $funcDEBUG is non-zero
        if [ $funcDEBUG -gt 0 ]; then
            # get size of function stack
            local -ir N_FUNC=${#FUNCNAME[@]}

            # fecho() should only be called from other functions, so the length of FUNCNAME
            # should always be 2 or greater. Start with color 0
            local -ir IDX=$(( N_FUNC - 2 ))    
            
            # set color
            echo -ne "${dcolor[IDX]}"

            # define function stack printing
            local -i fidx
            if [[ ${FUNCNAME[1]} == "xecho" ]]; then
                fidx=2
            else
                fidx=$(( $N_FUNC - 5 ))
            fi            
            echo -ne "${FUNCNAME[fidx]}\e[0m${dcolor[IDX]}: "
            echo "$@"
            echo -ne "\e[0m"
        fi
    fi
    return 0
}

# define debug echos

# generic debug message handler
# the debugging functions should be named as follows
#   decho - level 1: informative... or print steps
#  ddecho - level 2: explicitly print values
# dddecho - level 3: why...?
# and so on
function xecho() {
    # check for null or empty arguments ...to skip coloring?
    if false; then
        if [ $# -eq 0 ] ; then
            echo "no args"
        else
            echo "number of args: $#"
        fi
        
        if [[ -z "$@" ]]; then
            echo "empty"
        else
            echo "args: $@"
        fi
    fi
    
    # Turn in-function debugging on/off.

    # Inherit the value of funcDEBUG from shell or substitute default value if unset or NULL.
    local -i funcDEBUG=${funcDEBUG:-0}    

    # Use the value of DEBUG as default. If DEBUG is unset or NULL, substitue default value local
    # -iI funcDEBUG=${funcDEBUG:-${DEBUG:-0}}    

    # manual setting
    #funcDEBUG=1

    # use function name to specify debug threshold
    local fu=${FUNCNAME[1]}
    fecho "FUNCNAME = $fu"
    local pre=${fu%echo}
    fecho "prefix = $pre"
    local -ir PREFIX_LENGTH=${#pre}
    fecho "length = $PREFIX_LENGTH"
    local -ir thr=$(( PREFIX_LENGTH - 1 ))
    fecho "threshold = $thr"
    fecho "DEBUG = $DEBUG"
    fecho -e "args = \e[7m$@\e[m"

    # if DEBUG is (unset or null) or greater than threshold
    if [ -z ${DEBUG:+dummy} ] || [ $DEBUG -gt $thr ]; then
        # get color index
        local -i idx
        fecho "loading color index $PREFIX_LENGTH..."
        dbg2idx $PREFIX_LENGTH
        fecho "printing..."
        # set color
        echo -ne "${dcolor[$idx]}"
        # print message
        echo "$@"
        # unset color
        echo -ne "\e[0m"
        if [ $funcDEBUG -gt 0 ]; then
            start_new_line
        fi
        
    else
        fecho -e "\e[1m${dcolor[0]}not printing\e[0m"
    fi
    return 0
}

# define debug echo
function decho() {
    xecho "$@"
    return 0
}

function ddecho() {
    xecho "$@"    
    return 0
}
