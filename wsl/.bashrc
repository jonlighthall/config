# ~/config/wsl/.bashrc
# Interactive shell settings for Linux Subsystem for Windows
# Note: this file must use unix line endings (LF)!
if [ -z $VB ]; then
    export VB=false
else
    if $VB; then
	# set tab
	fTAB="   "
	TAB+=$fTAB
	# print source name at start
	echo "${TAB}running $BASH_SOURCE"
	src_name=$(readlink -f $BASH_SOURCE)
	if [ ! "$BASH_SOURCE" = "$src_name" ]; then
	    echo -n " -> $src_name"
	fi
	echo "..."
	# source formatting
	fpretty=${HOME}/utils/bash/.bashrc_pretty
	if [ -e $fpretty ]; then
	    source $fpretty
	fi
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
	    echo -e "${TAB}$FILE ${UL}not found${NORMAL}"
	fi
    fi
done

for FILE in $LIST
do
    if $VB; then
	echo "${TAB}loading $FILE..."
    fi
    if [ -f $FILE ]; then
	source $FILE
	if [ $? -eq 0 ]; then
	    if $VB; then
		echo -e "${TAB}$FILE ${GOOD}OK${NORMAL}"
	    fi
	else
	    echo -e "${TAB}$FILE ${BAD}FAIL${NORMAL}"
	fi
    else
	echo -e "${TAB}$FILE ${UL}not found${NORMAL}"
    fi
done

# ROOT
if [[ "$LIST" == *"thisroot.sh"* ]]; then
    which root
fi

if $VB; then
    TAB=${TAB#$fTAB}
fi
