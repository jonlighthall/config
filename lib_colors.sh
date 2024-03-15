# define custom SGR (Select Graphic Rendition) parameters

# clear all formatting
export   RESET='\E[0m'    # reset

# define graphics mode accents
export  NORMAL='\E[21;22;23;24;25;27;28;29m' # normal
export    BOLD='\E[1m'    # bold
export     DIM='\E[2m'    # dim/faint
export      IT='\E[3m'    # italics
export      UL='\E[4m'    # underline
export   BLINK='\E[5m'    # blinking 
export  INVERT='\E[7m'    # invert
export      ST='\E[9m'    # strikethrough

function test_normal() {
    for i in {0..9}; do
        [ $i -gt 0 ] && echo -en "${CHARTRU}"
        [ $i = 6 ] && continue
        echo -e "$i: \x1b[${i}mhello,${NORMAL} world!"
    done
}

# define LaTeX-like text formatting
export      BF='\E[1m'    # bold face

# define LaTeX-like text formatting names
export    BF='\E[1m'  # bold face
export    IT='\E[3m'  # italics
export UNDERLINE='\E[4m'  # underline

# define primary colors	(foreground)
export     RED='\E[31m'
export   GREEN='\E[32m'   # lime
export    BLUE='\E[34m'
# define secondary colors (foreground)
export  YELLOW='\E[33m'
export    CYAN='\E[36m'  # aqua
export MAGENTA='\E[35m'  # fuchsia
# define teriary colors (foreground)
#      var              ID    hex     RGB           Name
#-----+-------+--------------+-------+-------------+----------------
export  ORANGE='\E[38;5;208m' #ff8700 (255,135,  0) Dark Orange
export CHARTRU='\E[38;5;118m' #87ff00	(135,255,  0) Chartreuse
export SPGREEN='\E[38;5;46m'  #00ff87	(  0,255,135) Spring Green, aquamarine
export   AZURE='\E[38;5;33m' 	#0087ff	(  0,135,255) (Brooklyn) Dodger Blue, Brescian Blue
export  PURPLE='\E[38;5;93m' 	#8700ff	(135,  0,255) Purple, violet
export    ROSE='\E[38;5;198m' #ff0087	(255,  0,135) DeepPink1	

# create array of 12 rainbow colors
declare -ax rcolor=( "${RED}" "${ORANGE}" "${YELLOW}" "${CHARTRU}" "${GREEN}" "${SPGREEN}" "${CYAN}" "${AZURE}" "${BLUE}" "${PURPLE}" "${MAGENTA}" "${ROSE}" )

# define monochrome colors
export    gray='\E[90m'
export   white='\E[1;37m'

# define highlight colors
export     BAD="${RED}" # red
export    GOOD="${GREEN}" # green

# define 'ls' colors
export  BROKEN='\E[1;31m' # bold red    : or orphan link
export     TGT='\E[1;32m' # bold green  : ex executable
export     DIR='\E[1;34m' # bold blue   : di directory
export   VALID='\E[1;36m' # bold cyan   : ln valid link

function define_ls_colors() {
    local -r LN=$(declare -p LS_COLORS | sed 's/^.*ln=\([0-9;]*\):.*$/\1/')
    local -r OR=$(declare -p LS_COLORS | sed 's/^.*or=\([0-9;]*\):.*$/\1/')
    local -r DI=$(declare -p LS_COLORS | sed 's/^.*di=\([0-9;]*\):.*$/\1/')
    local -r EX=$(declare -p LS_COLORS | sed 's/^.*ex=\([0-9;]*\):.*$/\1/')

    export cLN="\E[${LN}m"
    export cOR="\E[${OR}m"
    export cDI="\E[${DI}m"
    export cEX="\E[${EX}m"

    echo -e "${cLN}links${RESET}"
    echo -e "${cOR}orphaned links${RESET}"
    echo -e "${cDI}directories${RESET}"
    echo -e "${cEX}executable files${RESET}"    
}

# define PS1 colors
export  PSTIME='\E[0;37m' # light gray  : \A time
export  PSUSER='\E[0;32m' # green       : \u user name, prompt
export  PSHOST='\E[1;34m' # bold blue   : \h host name
export   PSDIR='\E[0;33m' # yellow      : \w directory
export    PSBR='\E[0;36m' # blue        : branch

# define 'grep' colors
export     GRH='\E[1;31m' # bold red    : ms selected match
export     GRL='\E[0;32m' # green       : ln line number 
export     GRF='\E[0;35m' # magenta     : fn file name

# subtle colors for readability
# hue=(N*30), saturation=33%, lightness=52%; sorted by hue
#      order          ID       N  hue Name              corresponding color
#-----+------+--------------+----+---+-----------------+--------------------
export col08='\E[38;5;131m' #  0:   0 Indian Red        pri - RED
export col11='\E[38;5;137m' #  1:  30 Light Salmon        ter - orange
export col12='\E[38;5;143m' #  2:  60 Dark Khaki          sec - YELLOW
export col07='\E[38;5;107m' #  3:  90 Dark Olive Green    ter - chartreuse
export col03='\E[38;5;71m'  #  4: 120 Dark Sea Green    pri - GREEN/lime
export col04='\E[38;5;72m'  #  5: 150 Cadet Blue          ter - spring green
export col04='\E[38;5;73m'  #  6: 180 Cadet Blue          sec - CYAN/aqua
export col02='\E[38;5;67m'  #  7: 210 Steel Blue          ter - azure
export col01='\E[38;5;61m'  #  8: 240 Slate Blue        pri - BLUE
export col06='\E[38;5;97m'  #  9: 270 Medium Purple       ter - violet
export col10='\E[38;5;133m' # 10: 300 Medium Orchid       sec - MAGENTA/fuchsia
export col09='\E[38;5;132m' # 11: 330 Hot Pink            ter - rose

export col00='\E[38;5;102m' # 12:   0 Grey              mon - GRAY

# create rainbow-ordered (hue order) array of 12 debug colors
declare -ax dcolor=( '\E[38;5;131m' '\E[38;5;137m' '\E[38;5;143m' '\E[38;5;107m' '\E[38;5;71m'  '\E[38;5;72m'  '\E[38;5;73m'  '\E[38;5;67m'  '\E[38;5;61m'  '\E[38;5;97m'  '\E[38;5;133m' '\E[38;5;132m' )

# print dcolor array in rainbow-order
function print_colors() {
    # add shell options if not alraedy set
    old_opts=$(echo "$-")
    set -u

    # get length of array
    local -ir N_cols=${#dcolor[@]}
    echo "$N_cols in array ${!dcolor@}"
    # declare array index variable
    local -i i
    # print array elements
    for ((i=0;i<$N_cols;i++));do
        echo -e "${dcolor[$i]}$i"
    done
    # reset color
    echo -en "\E[m"
    
    # reset shell options
    local -i DEBUG=${DEBUG:-1}
    reset_shell ${old_opts} u
}

# print dcolor array in rainbow-order
function print_rcolors() {
    # add shell options if not alraedy set
    # old_opts=$(echo "$-")
    # set -u

    # get length of array
    local -ir N_cols=${#rcolor[@]}    
    echo "$N_cols in array ${!dcolor@}"
    # declare array index variable
    local -i i    
    # print array elements
    for ((i=0;i<$N_cols;i++));do
        echo -e "${rcolor[$i]}$i"
    done
    # reset color
    echo -en "\E[m"

    # reset shell options
    #local -i DEBUG=${DEBUG:-1}
    #reset_shell ${old_opts} u
}

# print dcolor array in debug order
function print_dcolors() {
    local -i DEBUG=2
    # add shell options if not alraedy set
    old_opts=$(echo "$-")
    set -u

    # get length of array
    local -ir N_cols=${#dcolor[@]}
    echo "$N_cols in array ${!dcolor@}"
    # calculate maximum array index
    local -ir N_max=$((N_cols-1))
    echo "$N_max max index of array ${!dcolor@}"
    #---------------------------------------------------------------
    # set starting color (using array indices 0-11, specified above)
    local -ir start=7
    # set increment direction
    local -ir direction=-1
    #---------------------------------------------------------------
    echo -e "staring with index ${dcolor[$start]}${start}\x1b[m\n"
    if [ $direction -gt 0 ]; then
        echo "incrementing array indicies"
    elif [ $direction -lt 0 ]; then
        echo "decrementing array indicies"
    else
        echo "something else..."
    fi
    # declare array index variable
    local -i idx
    (
        for ((i=0;i<$N_cols;i++));do
            #define array index
            idx=$(( (${N_max} + $direction * ($i) + $start + 1 ) % ${N_cols} ))
            printf '%2d:%2d:' $i $idx

            printf "${dcolor[$idx]}%2d\x1b[m\n" $idx
        done
    ) | column -t -s: -N order,index,color

    # reset shell options
    set -ETe
    set_traps
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
#--------------------------------------
function set_dbg2idx() {
    # turn in-function debugging on/off
    local funcDEBUG

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
    fecho -e "staring with index ${dcolor[$start]}${start}\x1b[m"
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
#--------------------------------------    
function dbg2idx() {
    if [ $# -lt 1 ]; then
        >&2 echo "$FUNCNAME: input required"
        return 0
    fi

    # turn in-function debugging on/off
    local -i funcDEBUG=${funcDEBUG:-0} # inherit value or substitution default
    
    # get input DEBUG value
    local -i dbg_in=$1
    fecho "dbg_in = $dbg_in"

    local -ir fdbg=$funcDEBUG
    funcDEBUG=0
    local -i N_cols
    local -i N_max
    local -i start
    local -i direction

    set_dbg2idx
    funcDEBUG=$fdbg
    
    # since DEBUG=0 does not print and DEBUG=1 corresponds to starting color, or array index 0,
    # decrement input value
    local -ir offset=-1
    local -i dbg_idx=$(( ( $dbg_in + ${offset} ) % ${N_cols} ))
    fecho "dbg_idx = $dbg_idx"
    
    #define array index
    idx=$(( ( ${N_max} + $direction * ($dbg_idx) + $start + 1 ) % ${N_cols} ))
    fecho "idx = $idx"

    fecho -e "${dcolor[$idx]}\x1b[7m${dbg_in}\x1b[m"
    
    return 0
}

# print dcolor array in debug order
function print_fcolors() {
    local -i funcDEBUG=${funcDEBUG:-1} # inherit value or substitution default
    fecho "printing contents of dcolor..."
    
    local -i idx
    (
        # loop over valid non-zero values of debug
        for ((i=1;i<=$N_cols;i++));do
            #define array index
            unset idx
            dbg2idx $i
            # print indices
            printf '%2d:%2d:' $i $idx
            # print color
            printf "${dcolor[$idx]}%2d\x1b[m\n" $idx
        done
    ) | column -t -s: -N order,index,color | sed "s/^/${TAB}/"
}

function print_ls_colors() {
    # print value of LS_COLORS
    declare -p LS_COLORS |
        # isolate definitions
        sed 's/^[^"]*"//;s/"$//' |
        # print each definition on its own line
        sed '$ s/:/\n/g;/^$/d' |
        # remove file extension definitions
        sed '/^\*/d' |
        # add echo wrapper with escapes
        sed 's/\(^[^=]*\)=\(.*$\)/"\1 \\x1b[\2m\2\\x1b[m"/' |
        # echo outputs
        xargs -L 1 echo -e
}

function print_ls_colors_ext() {
    # print value of LS_COLORS
    declare -p LS_COLORS |
        # isolate definitions
        sed 's/^[^"]*"//;s/"$//' |
        # print each definition on its own line
        sed '$ s/:/\n/g;/^$/d' |
        # remove file extension definitions
        sed '/^\*/!d' |
        # add echo wrapper with escapes
        sed 's/\(^[^=]*\)=\(.*$\)/"\1 \\x1b[\2m\2\\x1b[m"/' | sort -n |
        # echo outputs
        xargs -L 1 echo -e | column -t -o ' '
}

function append_ls_colors() {
    # physical link (hardlink)
    LS_COLORS+="mh=44;38;5;15:"
    # missing
    LS_COLORS+="mi=05;48;5;232;38;5;15:"
}
