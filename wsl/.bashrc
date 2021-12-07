# ~/config/wsl/.bashrc
# Interactive shell settings for Linux Subsystem for Windows
# Note: this file must use unix line endings (CF)! 
if $VB; then
    echo "running $BASH_SOURCE..."
fi

LIST="$HOME/.bashrc $HOME/config/.bashrc_common
$HOME/config/linux/.bashrc_unix $HOME/config/wsl/.bash_local
$HOME/.bash_local"

for FILE in $LIST
do
    if $VB; then
	echo "loading $FILE..."
    fi
    if [ -f $FILE ]; then
	source $FILE
	if [ $? -eq 0 ]; then
	    if $VB; then
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
fname2=root_v5.34.36/bin/thisroot.sh
if $VB; then
    echo "loading $fname2..."
fi
if [ -f $fname2 ]; then
    source $fname2
    if [ $? -eq 0 ]; then
	if $VB; then
	    echo "$fname2 OK"
	    echo "here"
	fi
	which root
    else
	echo "$fname2 FAIL"
    fi
else
    echo "$fname2 not found"
fi
