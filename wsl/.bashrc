# ~/config/wsl/.bashrc
# Interactive shell settings for Linux Subsystem for Windows
# Note: this file must use unix line endings (CF)! 
if [ $VB = true ]; then
    echo "running $BASH_SOURCE..."
fi

LIST="$HOME/.bashrc $HOME/config/.bashrc_common
$HOME/config/linux/.bashrc_unix $HOME/config/wsl/.bashrc_X11
$HOME/.bash_local root_v5.34.36/bin/thisroot.sh"

GOOD='\033[0;32m'
BAD='\033[0;31m'
NORMAL='\033[0m'

for FILE in $LIST
do
    if [ $VB = true ]; then
	echo "loading $FILE..."
    fi
    if [ -f $FILE ]; then
	source $FILE
	if [ $? -eq 0 ]; then
	    if [ $VB = true ]; then
		echo -e "$FILE ${GOOD}OK${NORMAL}"
	    fi
	else
	    echo -e "$FILE ${GOOD}FAIL${NORMAL}"
	fi
    else
	echo "$FILE not found"
    fi
done

# ROOT
which root
