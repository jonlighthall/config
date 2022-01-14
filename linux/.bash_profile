# .bash_profile
# Verbose bash prints?
export VB=false
if $VB; then
    TAB=""
    profTAB=""
    TAB+=$profTAB
    echo "${TAB}running $BASH_SOURCE..."
    echo "${TAB}verbose bash printing is...$VB"
#echo "${TAB}Loading settings for interactive shell..."
#echo "${TAB}Using GitHub version .bash_profile"
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
# Get the general aliases and functions

# User-specific environment and startup programs
if $VB; then
    echo
fi
echo -e "Welcome to $(hostname -f)"

echo
NP=3
echo "Top $NP processes on $(hostname -s):"
ps aux --sort=-pcpu | head -n $((NP+1)) | sed 's_1111499164_jlight  _'
echo
echo "Last $NP log-ins on $(hostname -s):"
last -wFa | grep light | head -n $NP
echo

if $VB; then
    echo -e "${TAB}${BASH_SOURCE} runtime...\c"
    if command -v sec2elap &>/dev/null
    then
	echo "$(sec2elap $SECONDS)"
    else
	echo "$SECONDS"
    fi
    echo "${TAB}$(date)"
fi