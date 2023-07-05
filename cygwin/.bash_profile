# User-dependent .bash_profile for Cygwin

# If not running interactively, don't do anything
[[ "$-" != *i* ]] && return

start_time=$SECONDS
# Verbose bash prints?
export VB=true
if $VB; then
    # set tab
    # .bash_profile should be the first thing to run, so zero TAB
    TAB=""
    TAB+=${profTAB:=''}
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
# save login timestamp to history
hist_file=~/.bash_history
if $VB; then
    echo -n "${TAB}appending login timestamp to $hist_file... "
fi
if [ -f $hist_file ]; then
    echo "#$(date +'%s') LOGIN  $(date +'%a %b %d %Y %R:%S %Z') from $HOSTNAME" >> $hist_file
    RETVAL=$?
    if [ $RETVAL -eq 0 ]; then
	if $VB; then
	    echo -e "${GOOD}OK${NORMAL} ${gray}RETVAL=$RETVAL${NORMAL}"
	fi
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
fname=${HOME}/config/cygwin/.bashrc
if $VB; then
    echo "${TAB}loading $fname... "
fi
if [ -f $fname ] ; then
    source $fname
    RETVAL=$?
    if [ $RETVAL -eq 0 ]; then
	if $VB; then
	    echo -e "${TAB}$fname ${GOOD}OK${NORMAL} ${gray}RETVAL=$RETVAL${NORMAL}"
	fi
    else
	echo -e "${TAB}$fname ${BAD}FAIL${NORMAL} ${gray}RETVAL=$RETVAL${NORMAL}"
    fi
else
    echo "${TAB}$fname not found"
fi
TAB=${TAB#$profTAB}

# print runtime duration
if $VB; then
    echo -e "${TAB}$(basename $BASH_SOURCE) run time... \c"
    dT=$(($SECONDS-start_time))
    if command -v sec2elap &>/dev/null
    then
	echo "$(sec2elap $dT)"
    else
	echo "elapsed time is ${dT} sec"
    fi
    echo "${TAB}$(date +"%a %b %-d %I:%M %p %Z")"
fi

# print welcome message
if $VB; then
    echo
fi
echo "${TAB}Welcome to $HOSTNAME"
