# User-dependent .bash_profile for Linux

# If not running interactively, don't do anything
[[ "$-" != *i* ]] && return

start_time=$(date +%s%N)
# Verbose bash prints?
export VB=true
if $VB; then
    # set tab
    TAB+=${TAB+${fTAB:='   '}}
    # load formatting
    fpretty=${HOME}/utils/bash/.bashrc_pretty
    if [ -e $fpretty ]; then
	source $fpretty
    fi
    # print source name at start
    if (return 0 2>/dev/null); then
        RUN_TYPE="sourcing"
    else
        RUN_TYPE="executing"
    fi
    echo -e "${TAB}${RUN_TYPE} ${PSDIR}$BASH_SOURCE${NORMAL}..."
    src_name=$(readlink -f $BASH_SOURCE)
    if [ ! "$BASH_SOURCE" = "$src_name" ]; then
	echo -e "${TAB}${VALID}link${NORMAL} -> $src_name"
    fi
    echo "${TAB}verbose bash printing is... $VB"
fi

# define conditional echo
vecho() {
    if [ ! -z ${VB:+dummy} ] || ${VB}; then
	# [not (unset or null)] or true -> print if true or null or unset
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
# print runtime duration
if $VB; then
    TAB=${TAB%$fTAB}
    echo -n "${TAB}$(basename $BASH_SOURCE) "
    elap_time=$(($(date +%s%N)-${start_time}))
    dT=$(bc <<< "scale=3;$elap_time/1000000000")
    if command -v sec2elap &>/dev/null
    then
	echo -n "$(sec2elap ${dT} | tr -d '\n')"
    else
    echo -n "elapsed time is ${white}${dT} sec${NORMAL}"
    fi
    echo " on $(date +"%a %b %-d at %-l:%M %p %Z")"
fi

clear -x

# show top processes
# number of processes to show
NP=3
# set line width to one less than the terminal width
line_width=$(( $(tput cols) - 1 ))
# user ID
U_ID=1111499164
# user name
U_NAME=jlighthall
echo -e "\033[4mTop $NP processes on ${HOST_NAME}:\x1b[0m"
ps aux --sort=-pcpu | head -n $((NP+1)) | sed "s/${U_ID}/\x1b\[32m${U_NAME}\x1b\[0m/" | awk '{printf "%-10s %5s %5s %5s %4s %5s %6s %s\n",$1,$2,$3,$4,$8,$9,$10,$11}' | cut -c -$line_width
echo
echo -e "\033[4mTop $NP processes by ${USER}:\x1b[0m"
ps ux --sort=-pcpu | head -n $((NP+1)) | sed "s/${U_ID}/${U_NAME}/" | awk '{printf "%-10s %5s %5s %5s %4s %5s %6s %s\n",$1,$2,$3,$4,$8,$9,$10,$11}' | cut -c -$line_width
echo
echo -e "\033[4mLast $NP log-ins by ${USER} on ${HOST_NAME}:\x1b[0m"
# print full user and domain neamse, full login and logout times and dates, and hostname
last -wFa | \grep ${U_NAME:0:8} | head -n $NP
echo

# print welcome message
echo "${TAB}Welcome to ${HOST_NAME}"
