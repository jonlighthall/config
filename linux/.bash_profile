# User-dependent .bash_profile for Linux

# If not running interactively, don't do anything
[[ "$-" != *i* ]] && return

start_time=$SECONDS
# Verbose bash prints?
export VB=true
if $VB; then
    # set tab
    # .bash_profile should be the first thing to run, so zero TAB
    TAB=''
    : ${fTAB:='   '}
    # load formatting
    fpretty=${HOME}/utils/bash/.bashrc_pretty
    if [ -e $fpretty ]; then
	source $fpretty
    fi
    # print source name at start
    echo -e "${TAB}running ${PSDIR}$BASH_SOURCE${NORMAL}..."
    src_name=$(readlink -f $BASH_SOURCE)
    if [ ! "$BASH_SOURCE" = "$src_name" ]; then
	echo -e "${TAB}${VALID}link${NORMAL} -> $src_name"
    fi
    echo "${TAB}verbose bash printing is... $VB"
fi

# define conditional echo
vecho() {
    if [ ! -z ${VB:+dummy} ] || ${VB}; then
	# if VB is (unset or null) or true
	echo "$@"
    fi
}

# system dependencies
SYS_NAME=linux
HOST_NAME=$(hostname -s)
vecho -e "${TAB}applying ${SYS_NAME} settings on ${PSHOST}${HOST_NAME}${NORMAL}"

# save login timestamp to history
hist_file=~/.bash_history
vecho -n "${TAB}appending login timestamp to $hist_file... "
if [ -f $hist_file ]; then
    echo "#$(date +'%s') LOGIN  $(date +'%a %b %d %Y %R:%S %Z') from ${HOST_NAME}" >> $hist_file
    RETVAL=$?
    if [ $RETVAL -eq 0 ]; then
	vecho -e "${GOOD}OK${NORMAL} ${gray}RETVAL=$RETVAL${NORMAL}"
    else
	if $VB; then
	    echo -e "${BAD}FAIL${NORMAL} ${gray}RETVAL=$RETVAL${NORMAL}"
	else
	    echo "echo to $hist_file failed"
	fi
    fi
else
    if $VB; then
	echo "${BAD}NOT FOUND{NORMAL}"
    else
	echo "$hist_file not found"
    fi
fi

# source the user's .bashrc if it exists
fname=${HOME}/config/${SYS_NAME}/.bashrc
vecho "${TAB}loading $fname... "
if [ -f $fname ] ; then
    source $fname
    RETVAL=$?
    if [ $RETVAL -eq 0 ]; then
	vecho -e "${TAB}$fname ${GOOD}OK${NORMAL} ${gray}RETVAL=$RETVAL${NORMAL}"
    else
	echo -e "${TAB}$fname ${BAD}FAIL${NORMAL} ${gray}RETVAL=$RETVAL${NORMAL}"
    fi
else
    echo "${TAB}$fname not found"
fi
vecho

# show top processes
NP=3
line_width=$(( $(tput cols) - 1 ))
echo -e "\033[4mTop $NP processes on $(hostname -s):\x1b[0m"
ps aux --sort=-pcpu | head -n $((NP+1)) | sed 's/1111499164/\x1b\[32mjlighthall\x1b\[0m/' | cut -c -$line_width
echo
echo -e "\033[4mTop $NP processes by ${USER}:\x1b[0m"
ps ux --sort=-pcpu | head -n $((NP+1)) | sed 's/1111499164/jlighthall/' | cut -c -$line_width
echo
echo -e "\033[4mLast $NP log-ins on $(hostname -s):\x1b[0m"
last -wFa | \grep light | head -n $NP
echo

# print runtime duration
if $VB; then
    TAB=${TAB%$fTAB}
    echo -n "${TAB}$(basename $BASH_SOURCE) "
    dT=$(($SECONDS-start_time))
    if command -v sec2elap &>/dev/null
    then
	echo "$(sec2elap $dT)"
    else
	echo "elapsed time is ${dT} sec"
    fi
    echo "${TAB}$(date +"%a %b %-d %I:%M %p %Z")"
fi

clear -x

# print welcome message
echo "${TAB}Welcome to ${HOST_NAME}"
