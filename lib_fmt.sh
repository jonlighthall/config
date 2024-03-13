# start a new line only if not already on a new line
# i.e., carriage return with conditional line feed
function start_new_line() {
    # get the cursor position
    echo -en "\E[6n"
    read -sdR CURPOS
    local CURPOS=${CURPOS#*[}
          #}# dummy bracket for emacs indenting
    # get the x-position of the cursor
    local -i x_pos=${CURPOS#*;}
    # if the cursor is not at the start of a line, then create a new line
    if [ ${x_pos} -gt 1 ]; then
        printf '\e[0;90;40m\u21b5\n\e[m'
        #echo
    fi
}

# print horizontal line
function hline() {
    # SYNTAX
    #   hline [length] [style]
    
    # number of characters in line (length)
    local -i N
    if [ "$#" -lt 1 ]; then
        N=38
    else
        N=$1
    fi
    # marker used to create line (style)
    local m
    if [ "$#" -lt 2 ]; then
        m="-"
    else
        m=$2
    fi
    for ((i = 1; i <= $N; i++)); do echo -n "$m"; done
    echo
}

# print text between two bars
function bar() {
    # SYNTAX
    #   bar [length] [text]

    # number of characters in line (length)
    local -i N
    # text to print between bars
    local TXT
    if [ "$#" -lt 2 ]; then
        N=69
        TXT=$1
    else
        N=$1
        TXT=$2
    fi
    hline $N
    echo "$TXT"
    hline $N
}

# define centered bar print
function cbar() {
    # SYNTAX
    #   cbar [text]

    # get text and add whitespace
    local -r MESSAGE_IN=$(echo " $@ ")
    # remove escape characters
    local msgne=$(echo -e " $@ " | sed "s/$(echo -e "\E")[^m]*m//g")
    # get length of text
    local ln=$(for ((i = 1; i <= ${#msgne}; i++)); do echo -n "-"; done)
    # print text with TAB
    echo -e "$ln\n${MESSAGE_IN}\n$ln" | sed "s/^/${TAB}/"
}
