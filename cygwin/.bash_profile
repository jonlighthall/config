# User dependent .bash_profile file
# Verbose bash prints?
export VB=true
if $VB; then
    echo "Verbose Bash printing is...$VB"
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
    echo "#$(date +'%s') LOGIN  $(date +'%a %b %d %Y %R:%S %Z') from $HOSTNAME" >> $fname
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

# source the users bashrc if it exists
fname=${HOME}/config/cygwin/.bashrc
if $VB; then
    echo "loading $fname..."
fi   
if [ -f $fname ] ; then
    source $fname
    if [ $? -eq 0 ]; then
	if $VB; then
	    echo -e "$fname ${GOOD}OK${NORMAL}"
	fi
    else
	echo -e "$fname ${BAD}FAIL${NORMAL}"
    fi
else
    echo "$fname not found"
fi

# Set MANPATH so it includes users' private man if it exists
# if [ -d "${HOME}/man" ]; then
#   MANPATH="${HOME}/man:${MANPATH}"
# fi

# Set INFOPATH so it includes users' private info if it exists
# if [ -d "${HOME}/info" ]; then
#   INFOPATH="${HOME}/info:${INFOPATH}"
# fi

echo "Welcome to" $HOSTNAME
