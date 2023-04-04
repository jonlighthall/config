# User-dependent .bash_profile for WSL
# Note: this file must use unix line endings (LF)! 
# Verbose bash prints?
export VB=true
if $VB; then
    TAB=""
    profTAB=""
    TAB+=$profTAB
    echo "${TAB}running $BASH_SOURCE..."
    echo "${TAB}verbose bash printing is...$VB"
      GOOD='\033[0;32m'
       BAD='\033[0;31m'
    NORMAL='\033[0m'
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
fname=${HOME}/config/wsl/.bashrc
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
echo "${TAB}Welcome to" $HOSTNAME
