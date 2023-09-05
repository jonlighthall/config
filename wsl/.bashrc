# ${HOME}/config/wsl/.bashrc
#
# Interactive shell settings for Linux Subsystem for Windows
#
# Note: this file must use unix line endings (LF)!
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

# since ~/.bashrc usually calls ~/.bash_aliases, a conditional could be added in .bash_aliases
# (linked to repo) and have all the functionality of this script, but for subshells.

run_home=true
run_list=true
N=${#BASH_SOURCE[@]}
echo "start"
echo -e "counting to $N... "
for ((i=1;i<=$N;i++))
do
    echo -n "$i: ${BASH_SOURCE[$((i-1))]}"

    if [[ "${BASH_SOURCE[$((i-1))]}" == "${HOME}/.bashrc" ]]; then
	echo " FALSE"
	run_home=false
	break
    fi

    if [[ "${BASH_SOURCE[$((i-1))]}" == "${HOME}/config/"* ]]; then
	echo " FALSE2"
	run_list=false
	break
    fi
    echo
done
echo "done"
echo "run home = $run_home"
echo "run list = $run_list"

if [ "${run_home}" = true ]; then
    echo "adding home"
    LIST="$HOME/.bashrc  "
else
    echo "not running home"
    unset LIST
fi

if [ "${run_list}" = true ]; then
echo "running list"
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
else
    echo "not running list"
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
