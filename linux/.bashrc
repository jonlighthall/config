# ~/config/linux/.bashrc
# Interactive shell settings for Linux

if [ -z $VB ]; then
    export VB=false
else
    if $VB; then
	fTAB="   "
	TAB+=$fTAB
	echo "${TAB}running $BASH_SOURCE..."
	# source formatting
	fpretty=${HOME}/utils/bash/.bashrc_pretty
	if [ -e $fpretty ]; then
	    source $fpretty
	fi
    fi
fi

# required list
LIST="/etc/bashrc $HOME/config/.bashrc_common $HOME/config/linux/.bashrc_unix"

# optional list
LIST_OPT="$HOME/.bash_local $HOME/.bash_aliases $HOME/config/linux/.bashrc_libgfortran"
for FILE in $LIST_OPT
do
    if [ -f $FILE ]; then
	LIST+=" $FILE"
    else
	if $VB; then
	    echo "${TAB}$FILE not found"
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
	echo "$FILE not found"
    fi
done
TAB=${TAB::${#TAB}-${#fTAB}}
