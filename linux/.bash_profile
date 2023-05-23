# User-dependent .bash_profile for Linux
start_time=$SECONDS
# Verbose bash prints?
export VB=true
if $VB; then
    # set tab
    TAB=""
    profTAB=""
    TAB+=$profTAB
    # print source name at start
    echo "${TAB}running $BASH_SOURCE..."
    src_name=$(readlink -f $BASH_SOURCE)
    if [ ! "$BASH_SOURCE" = "$src_name" ]; then
	echo "${TAB}     -> $src_name"
    fi
    echo "${TAB}verbose bash printing is... $VB"
    # source formatting
    fpretty=${HOME}/utils/bash/.bashrc_pretty
    if [ -e $fpretty ]; then
	source $fpretty
    fi
fi
# save login timestamp to history
hist_file=~/.bash_history
if $VB; then
    echo -n "${TAB}appending login timestamp to $hist_file... "
fi
if [ -f $hist_file ]; then
    echo "#$(date +'%s') LOGIN  $(date +'%a %b %d %Y %R:%S %Z') from $(hostname -s)" >> $hist_file
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
fname=${HOME}/config/linux/.bashrc
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

echo
NP=3
echo "Top $NP processes on $(hostname -s):"
ps aux --sort=-pcpu | head -n $((NP+1)) | sed 's/1111499164/jlighthall/'
echo
echo "Top $NP processes by ${USER}:"
ps ux --sort=-pcpu | head -n $((NP+1)) | sed 's/1111499164/jlighthall/'
echo
echo "Last $NP log-ins on $(hostname -s):"
last -wFa | \grep light | head -n $NP
echo

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
    echo "${TAB}$(date)"
fi

# print welcome message
if $VB; then
    echo
fi
echo "${TAB}Welcome to $(hostname -f)"
