# .bash_profile
# Verbose bash prints?
export VB=true
if $VB; then
    echo "Verbose Bash printing is...$VB"
#echo "Loading settings for interactive shell..."
#echo "Using GitHub version .bash_profile"
    echo "running $BASH_SOURCE..."
    GOOD='\033[0;32m'
    BAD='\033[0;31m'
    NORMAL='\033[0m'
fi
# save login timestamp to history
fname=~/.bash_history
if $VB; then
    echo -n "appending login timestamp to $fname..."
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

# Get the general aliases and functions
#echo "Loading .bashrc..."
if [ -f ~/config/linux/.bashrc ]; then
    source ~/config/linux/.bashrc
fi

# User-specific environment and startup programs
echo "Welcome to" $(hostname -f)

echo
NP=3
echo "Top $NP processes on $(hostname -s):"
ps aux --sort=-pcpu | head -n $((NP+1)) | sed 's_1111499164_jlight  _'
echo
echo "Last $NP log-ins on $(hostname -s):"
last -wFa | grep light | head -n $NP
echo

# Get the site-specific aliases and functions
if [ -f ~/.bash_aliases ]; then
#echo Loading .bash_aliases...
    source ~/.bash_aliases
fi
echo -e ".bash_profile runtime...\c"
if command -v sec2elap &>/dev/null
then
    echo "$(sec2elap $SECONDS)"
else
    echo "$SECONDS"
fi
echo $(date)