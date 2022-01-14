# ~/config/linux/.bashrc
# Interactive shell settings for Linux

if [ -z $VB ]; then
    export VB=false
else
    if $VB; then
	echo "running $BASH_SOURCE..."
	GOOD='\033[0;32m'
	BAD='\033[0;31m'
	NORMAL='\033[0m'
    fi
fi

# required list
LIST="/etc/bashrc $HOME/config/.bashrc_common $HOME/config/linux/.bashrc_unix"

# optional list
LIST_OPT="$HOME/.bash_local"
for FILE in $LIST_OPT
do
    if [ -f $FILE ]; then
	LIST+=" $FILE"
    else
	if $VB; then
	    echo "$FILE not found"
	fi
    fi
done

for FILE in $LIST
do
    if $VB; then
	echo "loading $FILE..."
    fi
    if [ -f $FILE ]; then
	source $FILE
	if [ $? -eq 0 ]; then
	    if $VB; then
		echo -e "$FILE ${GOOD}OK${NORMAL}"
	    fi
	else
	    echo -e "$FILE ${GOOD}FAIL${NORMAL}"
	fi
    else
	echo "$FILE not found"
    fi
done
