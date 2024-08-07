# User-dependent .bash_profile for Cygwin

# If not running interactively, don't do anything
[[ "$-" != *i* ]] && return

start_time=$SECONDS
# Verbose bash prints?
export VB=true
if [ "${VB}" = true ]; then
    # set tab
    TAB+=${TAB+${fTAB:='   '}}
    # load bash utilities
    fpretty=${HOME}/config/.bashrc_pretty
    if [ -e $fpretty ]; then
	if [ -z ${FPRETTY_LOADED+dummy} ]; then
	   source $fpretty
	fi
    fi
    print_source
    echo "${TAB}verbose bash printing is... $VB"
fi

# system dependencies
SYS_NAME=cygwin
HOST_NAME=$HOSTNAME
vecho -e "${TAB}applying ${SYS_NAME} settings on ${PSHOST}${HOST_NAME}${RESET}"

# save login timestamp to history
hist_file=${HOME}/.bash_history
vecho -n "${TAB}appending login timestamp to $hist_file... "
if [ -f $hist_file ]; then
    echo "#$(date +'%s') LOGIN  $(date +'%a %b %d %Y %R:%S %Z') from ${HOST_NAME}" >> $hist_file
    RETVAL=$?
    if [ $RETVAL -eq 0 ]; then
	vecho -e "${GOOD}OK${RESET} ${GRAY}RETVAL=$RETVAL${RESET}"
    else
	if [ "${VB}" = true ]; then
	    echo -e "${BAD}FAIL${RESET} ${GRAY}RETVAL=$RETVAL${RESET}"
	else
	    echo "echo to $hist_file failed"
	fi
    fi
else
    if [ "${VB}" = true ]; then
	echo "${BAD}NOT FOUND{RESET}"
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
	vecho -e "${TAB}$fname ${GOOD}OK${RESET} ${GRAY}RETVAL=$RETVAL${RESET}"
    else
	echo -e "${TAB}$fname ${BAD}FAIL${RESET} ${GRAY}RETVAL=$RETVAL${RESET}"
    fi
else
    echo "${TAB}$fname not found"
fi
vecho
# print runtime duration
if [ "${VB}" = true ]; then
    TAB=${TAB%$fTAB}
    echo -n "${TAB}$(basename $BASH_SOURCE) "
    dT=$(($SECONDS-start_time))
    if command -v sec2elap &>/dev/null
    then
	bash sec2elap ${dT} | tr -d '\n'
    else
    echo -n "elapsed time is ${WHITE}${dT} sec${RESET}"
    fi
    echo " on $(date +"%a %b %-d at %-l:%M %p %Z")"
fi

clear -x

# print welcome message
echo "${TAB}Welcome to ${HOST_NAME}"
