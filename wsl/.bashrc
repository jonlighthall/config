# ~/config/wsl/.bashrc
# Interactive shell settings for Linux Subsystem for Windows

LIST="$HOME/.bashrc $HOME/config/.bashrc_common $HOME/config/linux/.bashrc_unix"

for FILE in $LIST
do
    if [ -f $FILE ]; then
	source $FILE
    else
	echo "$FILE not found"
    fi
done

# X Window
export DISPLAY=localhost:0.0 

# ROOT
if [ -f root_v5.34.36/bin/thisroot.sh ]; then
    echo "sourcing root..."
    . root_v5.34.36/bin/thisroot.sh
    which root
fi
