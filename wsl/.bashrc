# ${HOME}/config/wsl/.bashrc
#
# Interactive shell settings for Linux Subsystem for Windows
#
# Note: this file must use unix line endings (LF)!

# since ~/.bashrc usually calls ~/.bash_aliases, a conditional could be added in .bash_aliases
# (linked to repo) and have all the functionality of this script, but for subshells.

if [ -f ${HOME}/.bashrc ]; then
    run_home=true
else
    run_home=false
fi
run_list=true
N=${#BASH_SOURCE[@]}
# set tab
TAB+=${TAB+${fTAB:='   '}}
# define conditional echo
vecho() {
    if [ ! -z ${VB:+dummy} ] && ${VB}; then
	echo "$@"
    fi
}
for ((i=1;i<=$N;i++)); do
    if [[ "${BASH_SOURCE[$((i-1))]}" == "${HOME}/.bashrc" ]]; then
	vecho -e "${TAB}invoked by ${PSDIR}${BASH_SOURCE[$((i-1))]}${NORMAL}"
	vecho "${TAB}excluding ${HOME}/.bashrc from file list"
	run_home=false
	break
    fi

    if [[ "${BASH_SOURCE[$((i-1))]}" == "${HOME}/config/"* ]]; then
	vecho -en "${TAB}\x1b[35minvoked by ${BASH_SOURCE[$((i-1))]}\x1b[0m: "
	if [ -L ${HOME}/.bash_aliases ]; then
	    run_list=false
	    vecho "not running list..."
	    break
	else
	    vecho "continuing..."
	fi
    fi
done

if [ "${run_home}" = true ]; then
    LIST="$HOME/.bashrc  "
else
    if [ ! -z ${VB:+dummy} ]; then
	oldVB=$VB
    fi
    unset LIST
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
    if [ ! -z ${oldVB:+dummy} ]; then
	export VB=$oldVB
    fi
fi

#echo "${TAB}run list = $run_list"
if [ "${run_list}" = true ]; then
    vecho "${TAB}running list..."
    if [ ! -z ${FILE+dummy} ]; then
	oldFILE=$FILE
    fi
    # required list
    LIST+="$HOME/config/.bashrc_common
$HOME/config/linux/.bashrc_prompt $HOME/config/wsl/.bashrc_X11"

    # optional list
    LIST_OPT="$HOME/.bash_local root_v5.34.36/bin/thisroot.sh"

    # add optional list to required list if targets exist
    for FILE in $LIST_OPT
    do
	if [ -f $FILE ]; then
	    LIST+=" $FILE"
	else
	    vecho -e "${TAB}$FILE ${UL}not found${NORMAL}"
	fi
    done
    #else
    #    echo "${TAB}${fTAB}not running list"
fi

# source list of files
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

# ROOT
if [[ "$LIST" == *"thisroot.sh"* ]]; then
    which root
fi

if $VB; then
    # reset tab
    TAB=${TAB%$fTAB}
fi

if [[ "${BASH_SOURCE}" == "${HOME}/.bash_aliases" ]]; then
    vecho -e "${TAB}${BASH_SOURCE[0]} ${GOOD}done\x1b[0m"
fi

if [ ! -z ${oldFILE+dummy} ]; then
    FILE=$oldFILE
    TAB=${TAB%$fTAB}
fi
