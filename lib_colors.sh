#!/bin/bash -eu
# -----------------------------------------------------------------------------------------------
# COLORS LIBRARY
# -----------------------------------------------------------------------------------------------
#
# ~/config/lib_colors.sh
#
# Purpose: define custom SGR (Select Graphic Rendition) parameters and functions for coloring
#   text.
#
# Dependancies:
#   lib_traps
#   lib_tabs
#   lib_cond_echo
#
# -----------------------------------------------------------------------------------------------

# clear all formatting
export   RESET='\x1B[0m'    # reset

# define graphics mode accents
export  NORMAL='\x1B[21;22;23;24;25;27;28;29m' # normal
export    BOLD='\x1B[1m'    # bold
export     DIM='\x1B[2m'    # dim/faint
export      IT='\x1B[3m'    # italics
export      UL='\x1B[4m'    # underline
export   BLINK='\x1B[5m'    # blinking 
export  INVERT='\x1B[7m'    # invert
export      ST='\x1B[9m'    # strikethrough

function test_normal() {
    ddecho -e "${TAB}${INVERT}${FUNCNAME}${RESET}"
    for i in {0..9}; do
        [ $i = 6 ] && continue
        echo -ne "$TAB"
        [ $i -gt 0 ] && echo -en "${CHARTRU}"
        echo -e "$i: \x1B[${i}mhello${NORMAL}, world!"
    done
    echo -ne "${RESET}"
}

# define LaTeX-like text formatting
export      BF='\x1B[1m'    # bold face

# define LaTeX-like text formatting names
export    BF='\x1B[1m'  # bold face
export    IT='\x1B[3m'  # italics
export UNDERLINE='\x1B[4m'  # underline

# define primary colors	(foreground)
export     RED='\x1B[31m'
export   GREEN='\x1B[32m'   # lime
export    BLUE='\x1B[34m'
# define secondary colors (foreground)
export  YELLOW='\x1B[33m'
export    CYAN='\x1B[36m'  # aqua
export MAGENTA='\x1B[35m'  # fuchsia
# define teriary colors (foreground)
#      var              ID    hex     RGB           Name
#-----+-------+--------------+-------+-------------+----------------
export  ORANGE='\x1B[38;5;208m' #ff8700 (255,135,  0) Dark Orange
export CHARTRU='\x1B[38;5;118m' #87ff00	(135,255,  0) Chartreuse
export SPGREEN='\x1B[38;5;46m'  #00ff87	(  0,255,135) Spring Green, aquamarine
export   AZURE='\x1B[38;5;33m' 	#0087ff	(  0,135,255) (Brooklyn) Dodger Blue, Brescian Blue
export  PURPLE='\x1B[38;5;93m' 	#8700ff	(135,  0,255) Purple, violet
export    ROSE='\x1B[38;5;198m' #ff0087	(255,  0,135) DeepPink1	

# create array of 12 rainbow colors
export rcolor=( "${RED}" "${ORANGE}" "${YELLOW}" "${CHARTRU}" "${GREEN}" "${SPGREEN}" "${CYAN}" "${AZURE}" "${BLUE}" "${PURPLE}" "${MAGENTA}" "${ROSE}" )

# define monochrome colors
export    GRAY='\x1B[90m'
export   WHITE='\x1B[1;37m'

# define highlight colors
export     BAD="${RED}" # red
export    GOOD="${GREEN}" # green

# define 'ls' colors
export  BROKEN='\x1B[1;31m' # bold red    : or orphan link
export     TGT='\x1B[1;32m' # bold green  : ex executable
export     DIR='\x1B[1;34m' # bold blue   : di directory
export   VALID='\x1B[1;36m' # bold cyan   : ln valid link

function load_colors() {
    # turn in-function debugging on/off
    local -i funcDEBUG=1

    fecho "dircolors..."
    if [ -x /usr/bin/dircolors ]; then

        fecho -e "exists and is executable ${GOOD}OK${NORMAL}"

        local srcdir=$(dirname $(readlink -f "${BASH_SOURCE}"))
        fecho "this function is ${FUNCNAME}"        
        fecho "this file is ${BASH_SOURCE##*/}"
        fecho "this directory is ${srcdir}"
        
        fname=.dircolors
        fpath="${srcdir}/${fname}"

        check_target "${fpath}"
        
        if [ -r "${fpath}" ]; then 
            fecho -e "and is readable ${GOOD}OK${NORMAL}"
            eval "$(dircolors -b "${fpath}")"
        else
            fecho -e "does not exist or is not readable ${BAD}FAIL${NORMAL}"
            return
            fecho "current settings:"
            eval "$(dircolors -b)"
        fi
    else
        fecho -e "does not exist or is not executable ${BAD}FAIL${NORMAL}"
    fi        
}

function define_ls_colors() {
    ddecho -e "${TAB}${INVERT}${FUNCNAME}${RESET}"
    if [ -z ${LS_COLORS:+dummy} ]; then
        echo "${TAB}LS_COLORS not defined"
        return
    fi
    local -r LN=$(declare -p LS_COLORS | sed 's/^.*ln=\([0-9;]*\):.*$/\1/')
    local -r OR=$(declare -p LS_COLORS | sed 's/^.*or=\([0-9;]*\):.*$/\1/')
    local -r DI=$(declare -p LS_COLORS | sed 's/^.*di=\([0-9;]*\):.*$/\1/')
    local -r EX=$(declare -p LS_COLORS | sed 's/^.*ex=\([0-9;]*\):.*$/\1/')

    export cLN="\x1B[${LN}m"
    export cOR="\x1B[${OR}m"
    export cDI="\x1B[${DI}m"
    export cEX="\x1B[${EX}m"

    echo -e "${cLN}links${RESET}"
    echo -e "${cOR}orphaned links${RESET}"
    echo -e "${cDI}directories${RESET}"
    echo -e "${cEX}executable files${RESET}"    
}

# define PS1 colors
export  PSTIME='\x1B[0;37m' # light gray  : \A : time
export  PSUSER='\x1B[0;32m' # green       : \u : user name, prompt
export  PSHOST='\x1B[1;34m' # bold blue   : \h : host name
export   PSDIR='\x1B[0;33m' # yellow      : \w : directory
export    PSBR='\x1B[0;36m' # blue        :    : branch

# define 'grep' colors
export     GRH='\x1B[1;31m' # bold red    : ms : selected match
export     GRL='\x1B[0;32m' # green       : ln : line number 
export     GRF='\x1B[0;35m' # magenta     : fn : file name
export     GRS='\x1B[0;36m' # cyan        : se : seperator

# subtle colors for readability
# hue=(N*30), saturation=33%, lightness=52%; sorted by hue
#      order          ID       N  hue Name              corresponding color
#-----+------+--------------+----+---+-----------------+--------------------
export col08='\x1B[38;5;131m' #  0:   0 Indian Red        pri - RED
export col11='\x1B[38;5;137m' #  1:  30 Light Salmon        ter - orange
export col12='\x1B[38;5;143m' #  2:  60 Dark Khaki          sec - YELLOW
export col07='\x1B[38;5;107m' #  3:  90 Dark Olive Green    ter - chartreuse
export col03='\x1B[38;5;71m'  #  4: 120 Dark Sea Green    pri - GREEN/lime
export col04='\x1B[38;5;72m'  #  5: 150 Cadet Blue          ter - spring green
export col04='\x1B[38;5;73m'  #  6: 180 Cadet Blue          sec - CYAN/aqua
export col02='\x1B[38;5;67m'  #  7: 210 Steel Blue          ter - azure
export col01='\x1B[38;5;61m'  #  8: 240 Slate Blue        pri - BLUE
export col06='\x1B[38;5;97m'  #  9: 270 Medium Purple       ter - violet
export col10='\x1B[38;5;133m' # 10: 300 Medium Orchid       sec - MAGENTA/fuchsia
export col09='\x1B[38;5;132m' # 11: 330 Hot Pink            ter - rose

export col00='\x1B[38;5;102m' # 12:   0 Grey              mon - GRAY

# create rainbow-ordered (hue order) array of 12 debug colors
export dcolor=( '\x1B[38;5;131m' '\x1B[38;5;137m' '\x1B[38;5;143m' '\x1B[38;5;107m' '\x1B[38;5;71m'  '\x1B[38;5;72m'  '\x1B[38;5;73m'  '\x1B[38;5;67m'  '\x1B[38;5;61m'  '\x1B[38;5;97m'  '\x1B[38;5;133m' '\x1B[38;5;132m' )

# print dcolor array in rainbow-order
# requires lib_traps
function print_colors() {
    ddecho -e "${TAB}${INVERT}${FUNCNAME}${RESET}"
    decho "${TAB}printing contents of ${!dcolor@} in ARRAY order..."
    # add shell options if not alraedy set
    old_opts=$(echo "$-")
    set -u

    # get length of array
    local -ir N_cols=${#dcolor[@]}
    echo "${TAB}$N_cols in array ${!dcolor@}"
    # declare array index variable
    local -i i
    # print array elements
    itab
    for ((i=0;i<$N_cols;i++));do
        echo -e "${TAB}${dcolor[$i]}$i"
    done
    dtab
    # reset color
    echo -en "\x1B[m"
    
    # reset shell options
    local -i DEBUG=${DEBUG:-1}
    reset_shell ${old_opts} u
}

# print dcolor array in rainbow-order
function print_rcolors() {
    ddecho -e "${TAB}${INVERT}${FUNCNAME}${RESET}"
    decho "${TAB}printing contents of ${!rcolor@} in ARRAY order..."
    # get length of array
    local -ir N_cols=${#rcolor[@]}    
    echo "${TAB}$N_cols in array ${!rcolor@}"
    # declare array index variable
    local -i i
    # print array elements
    itab
    for ((i=0;i<$N_cols;i++));do
        echo -e "${TAB}${rcolor[$i]}$i"
    done
    dtab
    # reset color
    echo -en "\x1B[m"
}

# print dcolor array in debug order
# requires lib_traps, lib_fmt
function print_dcolors() {
    ddecho -e "${TAB}${INVERT}${FUNCNAME}${RESET}"
    decho "${TAB}printing contents of ${!dcolor@} in DEBUG order..."
    local -i DEBUG=${DEBUG:-2}
    # add shell options if not alraedy set
    old_opts=$(echo "$-")
    set -u

    # get length of array
    local -ir N_cols=${#dcolor[@]}
    echo "${TAB}$N_cols in array ${!dcolor@}"
    # calculate maximum array index
    local -ir N_max=$((N_cols-1))
    echo "${TAB}$N_max max index of array ${!dcolor@}"
    #---------------------------------------------------------------
    # set starting color (using array indices 0-11, specified above)
    local -ir start=7
    # set increment direction
    local -ir direction=-1
    #---------------------------------------------------------------
    echo -e "${TAB}staring with index ${dcolor[$start]}${start}\x1B[m\n"
    if [ $direction -gt 0 ]; then
        echo "${TAB}incrementing array indicies"
    elif [ $direction -lt 0 ]; then
        echo "${TAB}decrementing array indicies"
    else
        echo "${TAB}something else..."
    fi
    # declare array index variable
    local -i idx
    (
        for ((i=0;i<$N_cols;i++));do
            #define array index
            idx=$(( (${N_max} + $direction * ($i) + $start + 1 ) % ${N_cols} ))
            printf '%2d:%2d:' $i $idx

            printf "${dcolor[$idx]}%2d\x1B[m\n" $idx
        done
    ) | column -t -s: -N order,index,color | sed "s/^/${TAB}/"

    # reset shell options
    reset_shell ${old_opts} u
}

#--------------------------------------
# define settings for dbg2idx
# Globals:
#   direction
#   idx
#   N_cols
#   N_max
#   start
# Arguments:
#   None
# Dependancies:
#   lib_cond_echo
#--------------------------------------
function set_dbg2idx() {
    # turn in-function debugging on/off
    local -i funcDEBUG=0

    # get length of array
    N_cols=${#dcolor[@]}
    fecho "$N_cols elements in array ${!dcolor@}"
    # calculate maximum array index
    N_max=$((N_cols-1))
    fecho "$N_max max index of array ${!dcolor@}"
    #---------------------------------------------------------------
    # set starting color (using array indices 0-11, specified above)
    start=7
    # set increment direction
    direction=-1
    #---------------------------------------------------------------
    fecho -e "staring with index ${dcolor[$start]}${start}\x1B[m"
    if [ $direction -gt 0 ]; then
        fecho "incrementing array indicies"
    elif [ $direction -lt 0 ]; then
        fecho "decrementing array indicies"
    else
        fecho "something else..."
    fi   
}

#--------------------------------------
# convert DEBUG value to color array index
# Globals:
#   N_cols
#   N_max
#   direction
#   idx
#   start
# Dependancies:
#   lib_cond_echo
#--------------------------------------    
function dbg2idx() {
    if [ $# -lt 2 ]; then
        (
            echo -e "${DIM}$FUNCNAME${NORMAL}: 2 inputs required"
            echo -e "${DIM}$FUNCNAME${NORMAL}: $# inputs received"
            echo -e "${DIM}$FUNCNAME${NORMAL}: Please provide an input value and and ouput variable as:"
            echo -e "${DIM}$FUNCNAME${NORMAL}: ${FUNCNAME} INDEX VARIABLE"
        ) >&2
        return 1
    fi

    # turn in-function debugging on/off
    local -i funcDEBUG=0
    
    # get input DEBUG value
    local -ir dbg_in=$1
    fecho "dbg_in = $dbg_in"

    # get output variable
    local -n var_out=$2
    fecho "var_out = ${!var_out}"
    
    local -i N_cols
    local -i N_max
    local -i start
    local -i direction

    set_dbg2idx
    
    # since DEBUG=0 does not print and DEBUG=1 corresponds to starting color, or array index 0,
    # decrement input value
    local -ir offset=-1
    local -i dbg_idx=$(( ( $dbg_in + ${offset} ) % ${N_cols} ))
    fecho "dbg_idx = $dbg_idx"
    
    #define array index
    idx=$(( ( ${N_max} + $direction * ($dbg_idx) + $start + 1 ) % ${N_cols} ))
    fecho "idx = $idx"

    fecho -e "${dcolor[$idx]}\x1B[7m${dbg_in}\x1B[m"
    
    return 0
}

# print dcolor array in debug order
# requires lib_cond_echo
function print_fcolors() {
    ddecho -e "${TAB}${INVERT}${FUNCNAME}${RESET}"        
    local -i DEBUG=${DEBUG:-1}
    decho "${TAB}printing contents of ${!dcolor@} in DBG2IDX order..."
    
    # get length of array
    local -ir N_cols=${#dcolor[@]}
    local -i idx
    (
        # loop over valid non-zero values of debug
        for ((i=1;i<=$N_cols;i++));do
            #define array index
            dbg2idx $i idx
            # print indices
            printf '%2d:%2d:' $i $idx
            # print color
            printf "${dcolor[$idx]}%2d\x1B[m\n" $idx
        done
    ) | column -t -s: -N order,index,color | sed "s/^/${TAB}/"
}

# requires lib_tabs, lib_cond_echo
function print_pretty() {
    ddecho -e "${TAB}${INVERT}${FUNCNAME}${RESET}"        
    # set default debug level
    local -i DEBUG=${DEBUG:-1}
    # define message
    local msg="pretty-print enabled"
    # determine message length
    decho -n "${TAB}'$msg' "
    local -i ln=${#msg}
    decho "is $ln long"

    # define loop variables
    local -i idx
    local let
    local -i pos=0
    decho -n "${TAB}"
    # loop over message
    for ((i=0;i<$ln;i++));do
        let="${msg:$i:1}"

        if [[ "$let" == " " ]]; then
            :
        else
            ((pos++))
        fi
        dbg2idx $pos idx
        # set color
        echo -en "${dcolor[$idx]}$let"
    done
    echo -e "$RESET"

}

function print_pretty_cbar() {
    ddecho -e "${TAB}${INVERT}${FUNCNAME}${RESET}"
    local -i DEBUG=0
    cbar $(print_pretty) 
}

function print_ls_colors() {
    ddecho -e "${TAB}${INVERT}${FUNCNAME}${RESET}"        
    if [ -z ${LS_COLORS:+dummy} ]; then
        echo "${TAB}LS_COLORS not defined"
        return
    fi
    # print value of LS_COLORS
    declare -p LS_COLORS |
        # isolate definitions
        sed 's/^[^"]*"//;s/"$//' |
        # print each definition on its own line
        sed '$ s/:/\n/g;/^$/d' |
        # remove file extension definitions
        sed '/^\*/d' |
        # add echo wrapper with escapes
        sed 's/\(^[^=]*\)=\(.*$\)/"\1 \\x1B[\2m\2\\x1B[m"/' |
        # echo outputs
        xargs -L 1 echo -e
}

function print_dircolors() {
    ddecho -e "${TAB}${INVERT}${FUNCNAME}${RESET}"        
    if [ -z ${LS_COLORS:+dummy} ]; then
        echo "${TAB}LS_COLORS not defined"
        return
    fi
    # print value of dircolors
    dircolors -p | sed 's/\(^.*\([0-9]\{2\};[0-9]\{2\}[0-9;]*\)\)/\x1B[\2m\1\x1B[0m/ '
 }

function print_ls_colors_ext() {
    ddecho -e "${TAB}${INVERT}${FUNCNAME}${RESET}"        
    if [ -z ${LS_COLORS:+dummy} ]; then
        echo "${TAB}LS_COLORS not defined"
        return
    fi
    # print value of LS_COLORS
    declare -p LS_COLORS |
        # isolate definitions
        sed 's/^[^"]*"//;s/"$//' |
        # print each definition on its own line
        sed '$ s/:/\n/g;/^$/d' |
        # remove file extension definitions
        sed '/^\*/!d' |
        # add echo wrapper with escapes
        sed 's/\(^[^=]*\)=\(.*$\)/"\1 \\x1B[\2m\2\\x1B[m"/' | sort -n |
        # echo outputs
        xargs -L 1 echo -e | column -t -o ' '
}

function append_ls_colors() {
    ddecho -e "${TAB}${INVERT}${FUNCNAME}${RESET}"        
    # physical link (hardlink)
    LS_COLORS+="mh=44;38;5;15:"
    # missing
    LS_COLORS+="mi=05;48;5;232;38;5;15:"
}

function test_lib_colors() {
    local -i DEBUG=2
    decho -e "${TAB}${INVERT}${FUNCNAME}${RESET}"        

    for func in test_normal \
                    define_ls_colors \
                    print_dircolors \
                    print_ls_colors \
                    print_ls_colors_ext \
                    print_rcolors \
                    print_colors \
                    print_dcolors \
                    print_fcolors \
                    print_pretty \
                    print_pretty_cbar \

                    do
                echo
                $func
             done

}

if [ ${DEBUG:-0} -gt 2 ]; then
    test_lib_colors
fi
