# User-dependent .bash_profile for Linux
# Verbose bash prints?
export VB=false
if $VB; then
    TAB=""
    profTAB=""
    TAB+=$profTAB
    echo "${TAB}running $BASH_SOURCE..."
    echo "${TAB}verbose bash printing is...$VB"
    # source formatting
    fpretty=${HOME}/utils/bash/.bashrc_pretty
    if [ -e $fpretty ]; then
	source $fpretty
    fi
fi
# save login timestamp to history
fname=~/.bash_history
if $VB; then
    echo -n "${TAB}appending login timestamp to $fname..."
fi
if [ -f $fname ]; then
    echo "#$(date +'%s') LOGIN  $(date +'%a %b %d %Y %R:%S %Z') from $(hostname -s)" >> $fname
    if [ $? ]; then
	if $VB; then
	    echo -e "${GOOD}OK${NORMAL}"
	fi
    else
	if $VB; then
	    echo -e "${BAD}FAIL${NORMAL}"
	else
	    echo "echo to $fname failed"
	fi
    fi
else
    if $VB; then
	echo "NOT FOUND"
    else
	echo "$fname not found"
    fi
fi

# source users bashrc if it exists
fname=${HOME}/config/linux/.bashrc
if $VB; then
    echo "${TAB}loading $fname..."
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
TAB=${TAB::${#TAB}-${#profTAB}}

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
    echo -e "${TAB}$(basename $BASH_SOURCE) runtime...\c"
    if command -v sec2elap &>/dev/null
    then
	echo "$(sec2elap $SECONDS)"
    else
	echo "$SECONDS"
    fi
    echo "${TAB}$(date)"
fi

# print welcome message
if $VB; then
    echo
fi
echo -e "Welcome to $(hostname -f)"
