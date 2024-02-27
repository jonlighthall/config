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

# determine if being sourced or executed
if (return 0 2>/dev/null); then
	RUN_TYPE="sourcing"
else
	RUN_TYPE="executing"
fi
# print source name at start
echo "${TAB}----------------------------------"
echo "${TAB}this file is ~/config/wsl/.bashrc!"
echo "${TAB}----------------------------------"
echo -e "${TAB}${RUN_TYPE} \E[0;33m$BASH_SOURCE\e[0m..."
src_name=$(readlink -f $BASH_SOURCE)
if [ ! "$BASH_SOURCE" = "$src_name" ]; then
	echo -e "${TAB}\E[1;36mlink\e[0m -> $src_name"
fi

vecho "${TAB}N=$N"
vecho "${TAB}BASH_SOURCE = ${BASH_SOURCE[@]}"

echo "${TAB}list of sources:"
for ((i = 0; i < $N; i++)); do
	vecho "${TAB}${fTAB}$i: ${BASH_SOURCE[$i]}"
done

echo "${TAB}list of invocations:"
(
if [ $N -gt 1 ]; then
	for ((i = 1; i < $N; i++)); do
		vecho "${TAB}${fTAB}$((i - 1))+${BASH_SOURCE[$((i - 1))]}+invoked by+${BASH_SOURCE[$i]}"
	done
else
	called_by=$(ps -o comm= $PPID)
	echo "${TAB}${fTAB}0+${BASH_SOURCE[0]}+invoked by+${called_by}"
fi
) | column -t -s +

echo "${TAB}check invoking scripts:"
for ((i = 1; i <= $N; i++)); do
	vecho -n "${TAB}${fTAB}$((i - 1)): ${BASH_SOURCE[$((i - 1))]}... "
	fref=${HOME}/.bashrc
	if [[ "${BASH_SOURCE[$((i - 1))]}" == "${fref}" ]]; then
		echo -e "\e[31mBREAK\e[0m - is ${fref}"
		vecho -ne "${TAB}\E[0;35minvoked by ${BASH_SOURCE[$((i - 1))]}\e[0m: "
		vecho "${TAB}excluding ${fref} from file list"
		run_home=false
		break
	else
		echo -e "\e[32mOK\e[0m - not ${fref}"
	fi

	vecho -n "${TAB}${fTAB}$((i - 1)): ${BASH_SOURCE[$((i - 1))]}... "
	dref=${HOME}/config/
	if [[ "${BASH_SOURCE[$((i - 1))]}" == "${dref}"* ]]; then
		echo -e "\e[33mCHECK\e[0m - is within ${dref}"
		vecho -en "${TAB}\x1b[35minvoked by ${BASH_SOURCE[$((i - 1))]}\x1b[0m: "
		echo -e "wrong!"
		vecho -ne "${TAB}\E[0;36minvoked by ${BASH_SOURCE[$i]}\e[0m: "
		check_link=${HOME}/.bash_aliases
		if [ -L ${check_link} ]; then
			run_list=false
			vecho "not running list..."
			echo -e "\e[31mBREAK\e[0m ${check_link} is link"
			break
		else
			echo -e "\e[32mOK\e[0m - ${check_link} not link*"
			vecho "continuing..."
		fi
	else
		echo -e "\e[32mOK\e[0m - not in ${dref}*"
	fi
done

#vecho "run_list = $run_list"
#vecho "run_home = $run_home"

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
	for FILE in $LIST_OPT; do
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
for FILE in $LIST; do
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
