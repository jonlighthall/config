# User-dependent .bash_profile file
# Verbose bash prints?
export VB=true
if $VB; then
    # set tab
    TAB=""
    profTAB=""
    TAB+=$profTAB
    # source formatting
    fpretty=${HOME}/utils/bash/.bashrc_pretty
    if [ -e $fpretty ]; then
	source $fpretty
    fi
    # print source name at start
    echo "${TAB}running $BASH_SOURCE..."
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
    if [ $? ]; then
	if $VB; then
	    echo -e "${GOOD}OK${NORMAL}"
	fi
    else
	if $VB; then
	    echo -e "${BAD}FAIL${NORMAL}"
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
    if [ $? -eq 0 ]; then
	if $VB; then
	    echo -e "${TAB}$fname ${GOOD}OK${NORMAL}"
	fi
    else
	echo -e "${TAB}$fname ${BAD}FAIL${NORMAL}"
    fi
else
    echo "${TAB}$fname not found"
fi
TAB=${TAB#$profTAB}

# Set MANPATH so it includes users' private man if it exists
# if [ -d "${HOME}/man" ]; then
#   MANPATH="${HOME}/man:${MANPATH}"
# fi

# Set INFOPATH so it includes users' private info if it exists
# if [ -d "${HOME}/info" ]; then
#   INFOPATH="${HOME}/info:${INFOPATH}"
# fi

# print runtime duration
if $VB; then
    echo -e "${TAB}$(basename $BASH_SOURCE) runtime... \c"
    if command -v sec2elap &>/dev/null
    then
	echo "$(sec2elap $(($SECONDS-start_time)))"
    else
	echo "$(($SECONDS-start_time)))"
    fi
    echo "${TAB}$(date)"
fi

# print welcome message
if $VB; then
    echo
fi
echo "${TAB}Welcome to $HOSTNAME"
