# User-dependent .bash_profile
# Note: this file must use unix line endings (CF)! 
# Verbose bash prints?
export VB=true
if [ $VB ]; then
    echo "Verbose Bash printing is...$VB"
    echo "running $BASH_SOURCE..."
fi
# save login timestamp to history
fname=~/.bash_history
if [ $VB ]; then
    echo -n "appending login timestamp to $fname..."
fi   
if [ -f $fname ]; then
    echo "#$(date +'%s') LOGIN  $(date +'%a %b %d %Y %R:%S %Z') from $(hostname -s)" >> $fname
    if [ $? -eq 0 ]; then
	if [ $VB ]; then
	    echo "OK"
	fi
    else
	if [ $VB ]; then
	    echo "FAIL"
	else
	    echo "echo to $fname failed"
	fi
    fi
else
    if [ $VB ]; then
	echo "NOT FOUND"
    else
	echo "$fname not found"
    fi
fi

# source the users bashrc if it exists
fname=${HOME}/config/wsl/.bashrc
if [ $VB ]; then
    echo "loading $fname..."
fi   
if [ -f $fname ] ; then
    source $fname
    if [ $? -eq 0 ]; then
	if [ $VB ]; then
	    echo "$fname OK"
	fi
    else
	echo "$fname FAIL"
    fi
else
    echo "$fname not found"
fi

echo "Welcome to" $HOSTNAME
