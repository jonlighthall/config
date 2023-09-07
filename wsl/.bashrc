# ${HOME}/config/wsl/.bashrc
#
# Interactive shell settings for Linux Subsystem for Windows
#
# Note: this file must use unix line endings (LF)!

# since ~/.bashrc usually calls ~/.bash_aliases, a conditional could be added in .bash_aliases
# (linked to repo) and have all the functionality of this script, but for subshells.

# set tab
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
LIST_IN="$HOME/.bashrc $HOME/config/.bashrc_common $HOME/config/linux/.bashrc_prompt
$HOME/config/wsl/.bashrc_X11"

# optional list
LIST_IN+=" $HOME/.bash_local root_v5.34.36/bin/thisroot.sh"

# clear output list
unset LIST_OUT

# get bash source stack
N=${#BASH_SOURCE[@]}

echo "${TAB}checking file list $LIST_IN"
echo
TAB+=${fTAB}
for FILE in $LIST_IN; do
    echo -n "${TAB}${FILE}... "
    # check if each file exists
    if [ -f $FILE ]; then
	echo "exists"
	# check if file is in stack
	TAB+=${fTAB}
	include_file=true
	for ((i=1;i<=$N;i++)); do		    
	    echo -n "${TAB}$i: ${BASH_SOURCE[$((i-1))]}..."
	    if [[ "${BASH_SOURCE[$((i-1))]}" == "${FILE}" ]]; then
		echo -e "\033[35m invoked by ${FILE}\x1b[0m"
		include_file=false
		break 1
	    else
		echo " not in stack"
	    fi
	    if [[ "${BASH_SOURCE[$((i-1))]}" == "${src_name}" ]]; then
		echo -e "\033[35m invoked by ${src_name}\x1b[0m"
#		include_file=false
#		break 2
	    fi
	    
	done
	TAB=${TAB%$fTAB}
	# if test is passed, add to list
	
	if [ "${include_file}" = true ]; then

	    LIST_OUT+=" $FILE"
	    echo "${TAB}${FILE} added to list"
	else
	    echo "${TAB}not added"
	    continue
	fi

    else
	echo -ne "\n${fTAB}"
	vecho -e "${TAB}$FILE ${UL}not found${NORMAL}"
    fi
done
TAB=${TAB%$fTAB}

echo -n "${TAB}sourcing ${LIST_OUT}"
if [ "${#LIST_OUT}" -eq 0 ]; then
    echo "EMPTY!"
else
    echo
fi
echo

# source list of files
for FILE in $LIST_OUT
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
vecho "${TAB}$BASH_SOURCE done"
