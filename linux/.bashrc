# ~/config/linux/.bashrc
# Interactive shell settings for Linux

LIST="/etc/bashrc $HOME/config/.bashrc_common $HOME/config/linux/.bashrc_unix"

for FILE in $LIST
do
    if [ -f $FILE ]; then
	source $FILE
    else
	echo "$FILE not found"
    fi
done
