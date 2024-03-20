# ${HOME}/config/cygwin/.bashrc
#
# Interactive shell settings for Git Bash (MinGW)

if [ -z ${VB:+dummy} ]; then
    export VB=false
else
    if $VB; then
	# set tab
	TAB+=${TAB+${fTAB:='   '}}
	# load formatting
	fpretty=${HOME}/config/.bashrc_pretty
	if [ -e $fpretty ]; then
	    if [ -z ${FPRETTY_LOADED+dummy} ]; then
	       source $fpretty
	    fi
	fi
	# print source name at start
	if (return 0 2>/dev/null); then
	    RUN_TYPE="sourcing"
	else
	    RUN_TYPE="executing"
	fi
	echo -e "${TAB}${RUN_TYPE} ${PSDIR}$BASH_SOURCE${RESET}..."
	src_name=$(readlink -f $BASH_SOURCE)
	if [ ! "$BASH_SOURCE" = "$src_name" ]; then
	    echo -e "${TAB}${VALID}link${RESET} -> $src_name"
	fi
    fi
fi

# required list
LIST="$HOME/config/.bashrc_common $HOME/config/cygwin/.bashrc_prompt"

# optional list
LIST_OPT="$HOME/.bash_local"
for FILE in $LIST_OPT
do
    if [ -f $FILE ]; then
	LIST+=" $FILE"
    else
	vecho -e "${TAB}$FILE ${UL}not found${RESET}"
    fi
done

for FILE in $LIST
do
    vecho "${TAB}loading $FILE..."
    if [ -f $FILE ]; then
	source $FILE
	RETVAL=$?
	if [ $RETVAL -eq 0 ]; then
	    vecho -e "${TAB}$FILE ${GOOD}OK${RESET} ${GRAY}RETVAL=$RETVAL${RESET}"
	else
	    echo -e "${TAB}$FILE ${BAD}FAIL${RESET} ${GRAY}RETVAL=$RETVAL${RESET}"
	fi
    else
	echo -e "${TAB}$FILE ${UL}not found${RESET}"
    fi
done

if $VB; then
    # reset tab
    TAB=${TAB%$fTAB}
fi
