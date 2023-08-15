# User-dependent .bash_profile for Cygwin

# If not running interactively, don't do anything
[[ "$-" != *i* ]] && return

start_time=$SECONDS
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
SYS_NAME=cygwin
HOST_NAME=$HOSTNAME
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
    dT=$(($SECONDS-start_time))
    if command -v sec2elap &>/dev/null
    then
	echo -n "$(sec2elap ${dT} | tr -d '\n')"
    else
    echo -n "elapsed time is ${white}${dT} sec${NORMAL}"
    fi
    echo " on $(date +"%a %b %-d at %-l:%M %p %Z")"
fi

clear -x

# print welcome message
echo "${TAB}Welcome to ${HOST_NAME}"
