# ${HOME}/config/cygwin/.bashrc
#
# Interactive shell settings for Cygwin

if [ -z ${VB:+dummy} ]; then
    export VB=false
else
    if $VB; then
	# set tab
	TAB+=${TAB+${fTAB:='   '}}
	# load formatting
	fpretty=${HOME}/utils/bash/.bashrc_pretty
	if [ -e $fpretty ]; then
	    source $fpretty
	fi
	# print source name at start
	if (return 0 2>/dev/null); then
	    RUN_TYPE="sourcing"
	else
	    RUN_TYPE="executing"
	fi
	echo -e "${TAB}${RUN_TYPE} ${PSDIR}$BASH_SOURCE${NORMAL}..."
	src_name=$(readlink -f $BASH_SOURCE)
	if [ ! "$BASH_SOURCE" = "$src_name" ]; then
	    echo -e "${TAB}${VALID}link${NORMAL} -> $src_name"
	fi
    fi
fi

# define conditional echo
vecho() {
    if [ ! -z ${VB:+dummy} ] && ${VB}; then
	echo "$@"
    fi
}

# required list
LIST="$HOME/config/.bashrc_common $HOME/config/cygwin/.bashrc_prompt $HOME/config/cygwin/.bashrc_X11"

# optional list
LIST_OPT="$HOME/.bash_local"
for FILE in $LIST_OPT
do
    if [ -f $FILE ]; then
	LIST+=" $FILE"
    else
	vecho -e "${TAB}$FILE ${UL}not found${NORMAL}"
    fi
done

for FILE in $LIST
do
    vecho "${TAB}loading $FILE..."
    if [ -f $FILE ]; then
	source $FILE
	RETVAL=$?
	if [ $RETVAL -eq 0 ]; then
	    vecho -e "${TAB}$FILE ${GOOD}OK${NORMAL} ${gray}RETVAL=$RETVAL${NORMAL}"
	else
	    echo -e "${TAB}$FILE ${BAD}FAIL${NORMAL} ${gray}RETVAL=$RETVAL${NORMAL}"
	fi
    else
	echo -e "${TAB}$FILE ${UL}not found${NORMAL}"
    fi
done

if $VB; then
    # reset tab
    TAB=${TAB%$fTAB}
fi
