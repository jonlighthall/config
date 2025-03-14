#!/bin/bash -eu
# ------------------------------------------------------------------------------
# COLORS LIBRARY
# ------------------------------------------------------------------------------
#
# ~/config/lib_colors.sh
#
# Purpose: define custom SGR (Select Graphic Rendition) parameters and functions
#   for coloring text.
#
# Dependencies:
#   lib_traps
#   lib_tabs
#   lib_cond_echo
#
# Mar 2024 JCL
#
# ------------------------------------------------------------------------------

# clear all formatting
export   RESET='\x1B[0m'    # reset

# ------------------------------------------------------------------------------
# Text mode variables
# ------------------------------------------------------------------------------

# define graphics mode accents
export  NORMAL='\x1B[21;22;23;24;25;27;28;29m' # normal
export    BOLD='\x1B[1m'    # bold
export     DIM='\x1B[2m'    # dim/faint
export      IT='\x1B[3m'    # italics
export      UL='\x1B[4m'    # underline
export   BLINK='\x1B[5m'    # blinking
export  INVERT='\x1B[7m'    # invert
export      ST='\x1B[9m'    # strike-through

function test_normal() {
    ddecho -e "${TAB}${INVERT}${FUNCNAME}${RESET}"
    for k in {3,9}; do
        for j in {1..7}; do
            local color="\x1B[${k}${j}m"
            local code=${color:5:-1}
            for i in {0..9}; do
                [ $i = 6 ] || [ $i = 8 ] && continue
                [ $k = 9 ] && [ $i = 1 ] && continue
                echo -ne "$TAB"
                echo -en "$color"
                printf '%02d;' $i
                echo -n ${code}
                echo -e ": \x1B[${i}m${color}hello${NORMAL}, world!"
            done
        done
    done
    echo -ne "${RESET}"
}

# define LaTeX-like text formatting names
export    BF='\x1B[1m'  # bold face
export    IT='\x1B[3m'  # italics
export UNDERLINE='\x1B[4m'  # underline

# ------------------------------------------------------------------------------
# Standard colors
# ------------------------------------------------------------------------------

# define monochrome colors
export    GRAY='\x1B[90m'
export   WHITE='\x1B[1;37m'

# define primary colors	(foreground)
export     RED='\x1B[31m'
export   GREEN='\x1B[32m'   # lime
export    BLUE='\x1B[34m'
# define secondary colors (foreground)
export  YELLOW='\x1B[33m'
export    CYAN='\x1B[36m'  # aqua
export MAGENTA='\x1B[35m'  # fuchsia
# define tertiary colors (foreground)
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

# print rcolor array in rainbow-order
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

# ------------------------------------------------------------------------------
# Context colors
# ------------------------------------------------------------------------------

# define highlight colors
export     BAD="${RED}"    # red
export    GOOD="${GREEN}"  # RETVAL=0; target exists
export    FILE="${YELLOW}" # file name in scripts
export     ARG="${CYAN}"   # input argument

# define 'ls' colors
export  BROKEN='\x1B[1;31m' # bold red    : or : orphan link
export     TGT='\x1B[1;32m' # bold green  : ex : executable
export     DIR='\x1B[1;34m' # bold blue   : di : directory
export   VALID='\x1B[1;36m' # bold cyan   : ln : valid link

# define PS1 colors
export  PSTIME='\x1B[0;37m' # light gray  : \A : time
export  PSUSER='\x1B[0;32m' # green       : \u : user name, prompt
export  PSHOST='\x1B[1;34m' # bold blue   : \h : host name
export   PSDIR='\x1B[0;33m' # yellow      : \w : directory
export    PSBR='\x1B[0;36m' # cyan        :    : branch
export    PSSH='\x1B[1;31m' # bold red    :    : shell

# define 'grep' colors
export     GRH='\x1B[1;31m' # bold red    : ms : selected match
export     GRL='\x1B[0;32m' # green       : ln : line number
export     GRF='\x1B[0;35m' # magenta     : fn : file name
export     GRS='\x1B[0;36m' # cyan        : se : separator

# define emacs dark colors
export EK='\x1B[38;5;214m'  # orange      : keyword
export EF='\x1B[38;5;50m'   # cyan        : function name

# ------------------------------------------------------------------------------
# List Directory (ls) colors
# ------------------------------------------------------------------------------

function load_colors() {
    export VB=${VB=false}

    [ "${VB}" = true ] && decho -n "${TAB}loading colors... "
    [ $DEBUG -gt 0 ] && vecho
    itab
    load_dircolors
    append_ls_colors
    match_ls_colors
    dtab
    if [ $DEBUG -gt 0 ]; then
        [ "${VB}" = true ] && decho "${TAB}colors loaded"
    else
        [ "${VB}" = true ] && decho "done $FUNCNAME"
    fi
}

function load_dircolors() {
    # enable color support of ls
    # define LS_COLORS using dircolors and .dircolors
    [ "${VB}" = true ] && decho "${TAB}loading dircolors..."

    # turn in-function debugging on/off
    local -i funcDEBUG=0

    fecho "dircolors..."
    if [ -x /usr/bin/dircolors ]; then

        fecho -e "exists and is executable ${GOOD}OK${RESET}"

        local srcdir=$(dirname $(readlink -f "${BASH_SOURCE}"))
        fecho "this function is ${FUNCNAME}"
        fecho "this file is ${BASH_SOURCE##*/}"
        fecho "this directory is ${srcdir}"

        local fname=.dircolors
        local fpath="${srcdir}/${fname}"
        if [ "${VB}" = true ] && [ ${DEBUG} -gt 0 ]; then
            itab
            check_target "${fpath}"
        fi

        [ "${VB}" = true ] && decho "${TAB}defining LS_COLORS..."
        if [ -r "${fpath}" ]; then
            fecho -e "and is readable ${GOOD}OK${RESET}"
            eval "$(dircolors -b "${fpath}")"
        else
            fecho -e "does not exist or is not readable ${BAD}FAIL${RESET}"
            return
            fecho "current settings:"
            eval "$(dircolors -b)"
        fi
    else
        fecho -e "does not exist or is not executable ${BAD}FAIL${RESET}"
    fi
    if [ "${VB}" = true ]; then
        dtab
        [ "${VB}" = true ] && decho "${TAB}directory colors loaded"
    fi
}

function print_dircolors_default() {
    ddecho -e "${TAB}${INVERT}${FUNCNAME}${RESET}"
    # print default value of dircolors
    dircolors -p | sed 's/\(^.*\([0-9]\{2\};[0-9]\{2\}[0-9;]*\)\)/\x1B[\2m\1\x1B[0m/' \
        | sed "s/^/${TAB}/"
}

function print_dircolors() {
    ddecho -e "${TAB}${INVERT}${FUNCNAME}${RESET}"
    # define path
    local srcdir=$(dirname $(readlink -f "${BASH_SOURCE}"))
    local fname=.dircolors
    local fpath="${srcdir}/${fname}"
    if [ -r "${fpath}" ]; then
        fecho -e "and is readable ${GOOD}OK${RESET}"

        # print value of dircolors, no extentsions
        dircolors -b "${fpath}" | sed '/1/!d' \
            | sed "s/.*'\(.*\):';/\1/" \
            | sed 's/:/\n/g' \
            | sed '/^*/d' \
            | sed 's/\(^.*=\([0-9;]*\)\)/\x1B[\2m\1\x1B[0m/' \
            | sed "s/^/${TAB}/"
    fi
}

function print_dircolors_ext() {
    ddecho -e "${TAB}${INVERT}${FUNCNAME}${RESET}"
    # define path
    local srcdir=$(dirname $(readlink -f "${BASH_SOURCE}"))
    local fname=.dircolors
    local fpath="${srcdir}/${fname}"
    if [ -r "${fpath}" ]; then
        fecho -e "and is readable ${GOOD}OK${RESET}"

        # print value of dircolors, extensions only
        dircolors -b "${fpath}" | sed '/1/!d' \
            | sed "s/.*'\(.*\):';/\1/" \
            | sed 's/:/\n/g' \
            | sed '/^*/!d' \
            | sed 's/\(^.*=\([0-9;]*\)\)/\x1B[\2m\1\x1B[0m/' \
            | sed "s/^/${TAB}/"
    fi
}

function diff_dircolors () {
    ddecho -e "${TAB}${INVERT}${FUNCNAME}${RESET}"
    # define path
    local srcdir=$(dirname $(readlink -f "${BASH_SOURCE}"))
    local fname=.dircolors
    local fpath="${srcdir}/${fname}"
    local -i RETVAL
    if [ -r "${fpath}" ]; then
        fecho -e "and is readable ${GOOD}OK${RESET}"

        # define temp file
        local temp_file="${srcdir}/.dircolors_$(date +'%Y-%m-%d-t%H%M%S')"
        dircolors -p > "${temp_file}"
        diff -s --color=auto --suppress-common-lines -yiEZbwB "${temp_file}" "${fpath}"
        RETVAL=$?
        if [ $RETVAL -eq 0 ]; then
            echo "none"
            rm -v "${temp_file}"
        fi
    fi
}

function define_ls_colors() {
    ddecho -e "${TAB}${INVERT}${FUNCNAME}${RESET}"
    vecho "extracting escape codes from LS_COLORS..."
    if [ -z ${LS_COLORS:+dummy} ]; then
        echo "${TAB}${FUNCNAME}: LS_COLORS not defined"
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

    decho "${TAB}read:"
    itab
    (
        echo -e "${TAB}${cLN}$LN+links${RESET}"
        echo -e "${TAB}${cOR}$OR+orphaned links${RESET}"
        echo -e "${TAB}${cDI}$DI+directories${RESET}"
        echo -e "${TAB}${cEX}$EX+executable files${RESET}"
    ) | column -t -s+
    dtab
}

function match_ls_colors() {
    # set local DEBUG
    local -i DEBUG=${DEBUG:-2}

    # print functino name
    [ $DEBUG -gt 0 ] && start_new_line
    ddecho -e "${TAB}${INVERT}${FUNCNAME}${RESET}"
    # print vecho/decho statement
    [ "${VB}" = true ] && decho -n "${TAB}matching ls-derived variables to LS_COLORS... "
    [ $DEBUG -gt 1 ] && start_new_line
    if [ -z ${LS_COLORS:+dummy} ]; then
        echo "${TAB}${FUNCNAME}: LS_COLORS not defined"
        return
    fi

    # get link color codes
    local or_col=$(declare -p LS_COLORS | sed 's/^[^"]*"//;s/"$//' | sed '$ s/:/\n/g' | sed '/^or/!d' | sed 's/^.*=//' | tail -1)
    local ex_col=$(declare -p LS_COLORS | sed 's/^[^"]*"//;s/"$//' | sed '$ s/:/\n/g' | sed '/^ex/!d' | sed 's/^.*=//' | tail -1)
    local di_col=$(declare -p LS_COLORS | sed 's/^[^"]*"//;s/"$//' | sed '$ s/:/\n/g' | sed '/^di/!d' | sed 's/^.*=//' | tail -1)
    local ln_col=$(declare -p LS_COLORS | sed 's/^[^"]*"//;s/"$//' | sed '$ s/:/\n/g' | sed '/^ln/!d' | sed 's/^.*=//' | tail -1)

    # print summary
    if [ $DEBUG -gt 1 ]; then
        itab
        echo "${TAB}prior:"
        itab
        (
            echo -e "${TAB}${VALID}VALID+links${RESET}"
            echo -e "${TAB}${BROKEN}BROKEN+orphaned links${RESET}"
            echo -e "${TAB}${DIR}DIR+directories${RESET}"
            echo -e "${TAB}${TGT}TGT+executable files${RESET}"
        ) | column -t -s+
        dtab

        echo "${TAB}input:"
        itab
        (
            echo -e "${TAB}\x1B[${ln_col}mln+${ln_col}+links${RESET}"
            echo -e "${TAB}\x1B[${or_col}mor+${or_col}+orphaned links${RESET}"
            echo -e "${TAB}\x1B[${di_col}mdi+${di_col}+directories${RESET}"
            echo -e "${TAB}\x1B[${ex_col}mex+${ex_col}+executable files${RESET}"
        ) | column -t -s+

        # update color values
        export BROKEN="\x1B[${or_col}m"
        export    TGT="\x1B[${ex_col}m"
        export    DIR="\x1B[${di_col}m"
        export  VALID="\x1B[${ln_col}m"

        dtab
        echo "${TAB}output:"
        itab
        (
            echo -e "${TAB}${VALID}VALID+links${RESET}"
            echo -e "${TAB}${BROKEN}BROKEN+orphaned links${RESET}"
            echo -e "${TAB}${DIR}DIR+directories${RESET}"
            echo -e "${TAB}${TGT}TGT+executable files${RESET}"
        ) | column -t -s+
        dtab 2
        [ "${VB}" = true ] && decho "${TAB}done"
    else
        [ "${VB}" = true ] && decho "done"
    fi
}

function match_ls_colors2() {
    ddecho -e "${TAB}${INVERT}${FUNCNAME}${RESET}"
    vecho -n "matching ls-derived variables to LS_COLORS..."
    # get link color codes
    define_ls_colors

    # update color values
    export BROKEN=${cOR}
    export    TGT=${cEX}
    export    DIR=${cDI}
    export  VALID=${cLN}
}

function print_ls_colors() {
    ddecho -e "${TAB}${INVERT}${FUNCNAME}${RESET}"
    if [ -z ${LS_COLORS:+dummy} ]; then
        echo "${TAB}${FUNCNAME}: LS_COLORS not defined"
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
    xargs -L 1 echo -e | sed "s/^/${TAB}/"
}

function print_ls_colors_ext() {
    ddecho -e "${TAB}${INVERT}${FUNCNAME}${RESET}"
    if [ -z ${LS_COLORS:+dummy} ]; then
        echo "${TAB}${FUNCNAME}: LS_COLORS not defined"
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
    xargs -L 1 echo -e | column -t -o ' ' | sed "s/^/${TAB}/"
}

function append_ls_colors() {
    ddecho -e "${TAB}${INVERT}${FUNCNAME}${RESET}"
    [ "${VB}" = true ] && decho -n "${TAB}appending to ls colors... "
    if [ -z ${LS_COLORS:+dummy} ]; then
        echo "${TAB}${FUNCNAME}: LS_COLORS not defined"
        return
    fi
    # physical link (hardlink)
    # get link color code
    ln_col=$(declare -p LS_COLORS | sed 's/^[^"]*"//;s/"$//' | sed '$ s/:/\n/g' | sed '/^ln/!d' | sed 's/^.*=//')
    if [ -z ${ln_col:+dummy} ]; then
        ln_col="01;36"
    fi
    # invert
    mh_col="07;${ln_col}"
    #LS_COLORS+="mh=44;38;5;15:"
    LS_COLORS+="mh=${mh_col}:"

    # missing
    # get orphan color code
    or_col=$(declare -p LS_COLORS | sed 's/^[^"]*"//;s/"$//' | sed '$ s/:/\n/g' | sed '/^or/!d' | sed 's/^.*=//')
    if [ -z ${or_col:+dummy} ]; then
        or_col="40;31;01"
    fi
    # invert and blink
    mi_col="05;07;${or_col}"
#   mi_col="05;48;5;232;38;5;15"
    LS_COLORS+="mi=${mi_col}:"
    [ "${VB}" = true ] && decho "done"
}

# ------------------------------------------------------------------------------
# DEBUG colors
# ------------------------------------------------------------------------------

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
export col05='\x1B[38;5;73m'  #  6: 180 Cadet Blue          sec - CYAN/aqua
export col02='\x1B[38;5;67m'  #  7: 210 Steel Blue          ter - azure
export col01='\x1B[38;5;61m'  #  8: 240 Slate Blue        pri - BLUE
export col06='\x1B[38;5;97m'  #  9: 270 Medium Purple       ter - violet
export col10='\x1B[38;5;133m' # 10: 300 Medium Orchid       sec - MAGENTA/fuchsia
export col09='\x1B[38;5;132m' # 11: 330 Hot Pink            ter - rose

export col00='\x1B[38;5;102m' # 12:   0 Grey              mon - GRAY

# an additional color is added for reference
# only the terminal standard 8 colors have normal and bold variants
# for stream coloring, a standard color may be preferred
export col13='\x1B[36m'       # 13: 206 Cyan                sec - CYAN
export col14='\x1B[37m'       # 14:   0 White             mon - WHITE

# define debug highlight colors
export  dBAD="${col08}"  # red
export dGOOD="${col03}"  # RETVAL=0; target exists

# create rainbow-ordered (hue order) array of 12 debug colors
export dcolor=( "${col08}" "${col11}" "${col12}" "${col07}" "${col03}" "${col04}" "${col05}" "${col02}" "${col01}" "${col06}" "${col10}" "${col09}" "${col14}" )

# print dcolor array in rainbow-order
# requires lib_traps
function print_colors() {
    ddecho -e "${TAB}${INVERT}${FUNCNAME}${RESET}"
    decho "${TAB}printing contents of ${!dcolor@} in ARRAY order..."
    # add shell options if not already set
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

# print dcolor array in debug order
# requires lib_traps, lib_fmt
function print_dcolors() {
    ddecho -e "${TAB}${INVERT}${FUNCNAME}${RESET}"
    decho "${TAB}printing contents of ${!dcolor@} in DEBUG order..."
    local -i DEBUG=${DEBUG:-2}
    # add shell options if not already set
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
        echo "${TAB}incrementing array indices"
    elif [ $direction -lt 0 ]; then
        echo "${TAB}decrementing array indices"
    else
        echo "${TAB}something else..."
    fi
    # declare array index variable
    local -i idx
    (
        echo "order:index:color"
        for ((i=0;i<$N_cols;i++));do
            #define array index
            idx=$(( (${N_max} + $direction * ($i) + $start + 1 ) % ${N_cols} ))
            printf '%2d:%2d:' $i $idx

            printf "${dcolor[$idx]}%2d\x1B[m\n" $idx
        done
    ) | column -t -s: | sed "s/^/${TAB}/"

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
# Dependencies:
#   lib_cond_echo
#--------------------------------------
function set_dbg2idx() {
    # turn in-function debugging on/off
    local -i funcDEBUG=0
    # get length of array
    if [ -z ${dcolor+dummy} ]; then
        source  ~/config/lib_colors.sh
    fi
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
        fecho "incrementing array induces"
    elif [ $direction -lt 0 ]; then
        fecho "decrementing array indices"
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
# Dependencies:
#   lib_cond_echo
#--------------------------------------
function dbg2idx() {
    if [ $# -lt 2 ]; then
        (
            echo -e "${DIM}$FUNCNAME${NORMAL}: 2 inputs required"
            echo -e "${DIM}$FUNCNAME${NORMAL}: $# inputs received"
            echo -e "${DIM}$FUNCNAME${NORMAL}: Please provide an input value and and output variable as:"
            echo -e "${DIM}$FUNCNAME${NORMAL}: ${FUNCNAME} INDEX VARIABLE"
        ) >&2
        return 1
    fi

    # turn in-function debugging on/off
    local -i funcDEBUG=0

    # get input DEBUG value
    fecho " arg 1 : $1"
    local -ir dbg_in=$1
    fecho "dbg_in = $dbg_in"

    # get output variable

    fecho " arg 2 : $2"
    local var_out=$2

    # check if VAR is unset
    if [ -z "${!var_out+dummy}" ]; then
        fecho  -e "${TAB}${FUNCNAME[1]} arg 1 in : ${var_out} ${!var_out-$UNSET}"
    else
        # check if VAR is NULL
        if [ -z "${!var_out:+dummy}" ]; then
            fecho  -e "${TAB}${FUNCNAME[1]} arg 1 in : ${var_out} ${!var_out:-$NULL}"
        else
            fecho "$var_out = ${!var_out}"
        fi
    fi

    # define parameters
    local -i N_cols
    local -i N_max
    local -i start
    local -i direction

    # load parameters
    set_dbg2idx

    # using an index value of 0 indicates turning off coloring
    # the color at the end of the array will be loaded
    if [ $dbg_in -eq 0 ]; then
        var_out=$N_max
        return 0
    fi

    # since DEBUG=0 does not print and DEBUG=1 corresponds to starting color, or array index 0,
    # the input value decremented by one
    local -i offset=1
    # however, (now) idx=0 corresponds to no color or gray printing, so the offset is zero and
    # (now) idx=DEBUG
    offset=0
    # if the index is negative (for some reason), continue from the previous color
    if [ $dbg_in -lt 0 ]; then
        offset=1
    fi

    # specify the number of colors to skip at the end of the array
    local -ir N_mod=$((N_cols-1))

    # calculate the corresponding "debug" index, modulo number of colors
    local -i dbg_idx=$(( ( $dbg_in + ${offset} ) % ${N_mod} ))
    fecho "dbg_idx = $dbg_idx"

    #define array index, based on values defined in set_dbg2idx
    local -i idx_out
    idx_out=$(( ( ${N_max} + $direction * ($dbg_idx) + $start + 1 ) % ${N_cols} ))
    fecho "$var_out = $idx_out"
    eval ${var_out}=$idx_out

    # print result
    if [ -z ${dcolor+dummy} ]; then
        source  ~/config/lib_colors.sh
    fi
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
            echo "order@index@color"
        # loop over valid non-zero values of debug
            for ((i=0;i<=$N_cols-1;i++));do
            #define array index
                dbg2idx $i idx
            # print indices
                printf '%2d@%2d@' $i $idx
            # print color
                printf "${dcolor[$idx]}%2d\x1B[m\n" $idx
            done
        ) | column -t -s@ | sed "s/^/${TAB}/"
    }

# set DEBUG color
    function set_dcolor() {
    # get value of DEBUG
    # if unset or NULL, substitute default
        local -i DEBUG=${DEBUG-0}

        set_color $DEBUG
    }

# set BASH color
    function set_bcolor() {
    # get length of call stack
        local -i N_BASH=${#BASH_SOURCE}

            set_color $N_BASH
        }

        function set_color() {
    # default color
            local -i N=3

    # use argument to manually set color
            if [ $# -eq 1 ]; then
                    N=$1
            fi

    # get color index
            local -i idx
            dbg2idx $N idx
    # set color
            echo -ne "${dcolor[$idx]}"
                }

                function test_set_color() {
                    ddecho -e "${TAB}${INVERT}${FUNCNAME}${RESET}"
                    local -i DEBUG=${DEBUG:-1}
                    decho "${TAB}printing contents of ${!dcolor@} in DBG2IDX order..."

    # get length of array
                    local -ir N_cols=${#dcolor[@]}

    # loop over valid non-zero values of debug
                        for ((i=-${N_cols};i<=$N_cols;i++));do
                            set_color $i
                            printf '%3d: %s\n' $i $FUNCNAME
                            unset_color
                        done
                    }

                    function unset_color() {
                        echo -ne "\e[0m"
                    }

# ------------------------------------------------------------------------------
# Demo Functions for .bashrc_pretty
# ------------------------------------------------------------------------------

# requires lib_tabs, lib_cond_echo
                    function print_pretty() {
    # set default debug level
                        local -i DEBUG=${DEBUG:-1}
                        ddecho -e "${TAB}${INVERT}${FUNCNAME}${RESET}"
    # define message
                        if [ $# -eq 0 ]; then
                                local msg="pretty-print enabled"
                        else
                            local msg="$@"
                            DEBUG=0
                        fi
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
                                    ((++pos))
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

                        function print_pretty_status() {
                            if [[ "$-" == *i* ]] && [ ${DEBUG:-0} -gt 0 ]; then
                                print_pretty_cbar
                            fi

                            if [ -z ${FPRETTY_LOADED+dummy} ]; then
                                declare -rx FPRETTY_LOADED=true
                                vecho "${TAB}${BASH_SOURCE##*/} loaded"
                            else
                                local -i N_BASH=${#BASH_SOURCE[@]}
                                    local cmd
                                    local -i lev
                                    if [ $N_BASH -eq 2 ]; then
                                        cmd=echo
                                        lev=1
                                    else
                                        cmd=decho
                                        lev=0
                                    fi
                                    $cmd "${TAB}"$(print_pretty "${BASH_SOURCE[lev]##*/} reloaded")
                            fi
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
