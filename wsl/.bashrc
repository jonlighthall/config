# ~/config/wsl/.bashrc
# Interactive shell settings for Linux Subsystem for Windows
# Note: this file must use unix line endings (LF)! 
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
LIST="$HOME/.bashrc $HOME/config/.bashrc_common
$HOME/config/linux/.bashrc_unix $HOME/config/wsl/.bashrc_X11"

# optional list
LIST_OPT="$HOME/.bash_local root_v5.34.36/bin/thisroot.sh"
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
	    echo -e "$FILE ${BAD}FAIL${NORMAL}"
	fi
    else
	echo "$FILE not found"
    fi
done

# ROOT
if [[ "$LIST" == *"thisroot.sh"* ]]; then 
    which root
fi
