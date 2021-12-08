# ~/config/wsl/.bashrc
# Interactive shell settings for Linux Subsystem for Windows
# Note: this file must use unix line endings (CF)! 
if [ $VB = true ]; then
    echo "running $BASH_SOURCE..."
fi

LIST="$HOME/.bashrc $HOME/config/.bashrc_common
$HOME/config/linux/.bashrc_unix $HOME/config/wsl/.bash_local
$HOME/.bash_local root_v5.34.36/bin/thisroot.sh"

for FILE in $LIST
do
    if [ $VB = true ]; then
	echo "loading $FILE..."
    fi
    if [ -f $FILE ]; then
	source $FILE
	if [ $? -eq 0 ]; then
	    if [ $VB = true ]; then
		echo "$FILE OK"
	    fi
	else
	    echo "$FILE FAIL"
	fi
    else
	echo "$FILE not found"
    fi
done

# ROOT
which root
