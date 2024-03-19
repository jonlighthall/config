#!/bin/bash -eu

# ${HOME}/config/wsl/.bashrc
#
# Interactive shell settings for Linux Subsystem for Windows
#
# Note: this file must use unix line endings (LF)!

# load bash utilities
fpretty=${HOME}/config/.bashrc_pretty
if [ -e $fpretty ]; then
	  source $fpretty
    set_tab
	  set -e
	  set_traps
else
    set +eu
fi


print_ribbon

#decho -e "${TAB}\x1b[7;33mstart\x1b[m"

msg=$(echo "this file is $(readlink -f ${BASH_SOURCE[0]##*/})!")
ln=$(for ((i = 1; i <= ${#msg}; i++)); do echo -n "-"; done)
#decho -e "$ln\n$msg\n$ln" | sed "s/^/${TAB}/"

# since ~/.bashrc usually calls ~/.bash_aliases, a conditional could be added in .bash_aliases
# (linked to repo) and have all the functionality of this script, but for subshells.

if [ -f ${HOME}/.bashrc ]; then
    run_home=true
else
    run_home=false
fi
run_list=true

# determine if being sourced or executed
if (return 0 2>/dev/null); then
    RUN_TYPE="sourcing"
else
    RUN_TYPE="executing"
fi
get_source
print_source

# get length of stack
N=${#BASH_SOURCE[@]}

# resolve symbolic links
for ((i = 0; i < $N; i++)); do
    BASH_LINK[$i]=$(readlink -f ${BASH_SOURCE[$i]})
done

decho "${TAB}check invoking scripts:"
itab
for ((i = 1; i <= $N; i++)); do
	  decho "${TAB}$((i - 1)): ${BASH_SOURCE[$((i - 1))]}... "
	  fref=$(readlink -f ${HOME}/.bashrc)
	  itab
	  if [[ "${BASH_LINK[$((i - 1))]}" == "${fref}" ]]; then
		    decho -ne "${TAB}\E[0;35minvoked by ${BASH_LINK[$((i - 1))]}\e[0m: "
		    decho "excluding ${fref##*/} from file list"
		    run_home=false
		    decho -e "${TAB}\e[31mBREAK\e[0m ${BASH_SOURCE[$((i - 2))]##*/} is ${fref}"
        dtab 
		    break
	  else
		    decho -e "${TAB}\e[32mOK\e[0m - not ${fref}"        
	  fi

	  dref=${HOME}/config/
	  if [[ "${BASH_SOURCE[$((i - 1))]}" == "${dref}"* ]]; then
		    decho -e "${TAB}\e[33mCHECK\e[0m - is within ${dref}"
        itab
		    decho -en "${TAB}\x1b[35minvoked by ${BASH_SOURCE[$((i - 1))]}\x1b[0m: "
		    decho -e "wrong!"
		    decho -ne "${TAB}\E[0;36minvoked by ${BASH_SOURCE[$i]}\e[0m: "
		    check_link=${HOME}/.bash_aliases
		    if [ -L ${check_link} ]; then
			      run_list=false
			      decho "not running list..."
			      decho -e "${TAB}\e[31mBREAK\e[0m ${check_link} is link"
            dtab 2
			      break
		    else
			      dcho -e "\e[32mOK\e[0m - ${check_link} not link*"
			      decho "continuing..."
		    fi
        dtab
	  else
        dtab
		    decho -e "${TAB}${fTAB}\e[32mOK\e[0m - not in ${dref}*"
        itab
	  fi
    dtab
done
dtab

decho "${TAB}run_list = $run_list" | sed 's/true/\x1b[32m&\x1b[m/;s/false/\x1b[31m&\x1b[m/'
decho "${TAB}run_home = $run_home" | sed 's/true/\x1b[32m&\x1b[m/;s/false/\x1b[31m&\x1b[m/'

set +e
unset_traps

if [ "${run_home}" = true ]; then
    LIST="$HOME/.bashrc  "
else
	  if [ ! -z ${VB:+dummy} ]; then
		    oldVB=$VB
	  fi
	  unset LIST
	  if [ -z ${VB:+dummy} ]; then
		    export VB=false
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
            vecho -e "${TAB}$FILE ${UL}not found${RESET}"
        fi
    done
    #else
    #    echo "${TAB}${fTAB}not running list"
fi

# (un)set traps and shell options before loading command files
unset_traps

# source list of files
for FILE in $LIST; do
    vecho "${TAB}loading $FILE..."
    if [ -f $FILE ]; then       
        source $FILE
        RETVAL=$?
        if [ $RETVAL -eq 0 ]; then
            vecho -e "${TAB}$FILE ${GOOD}OK${RESET} ${gray}RETVAL=$RETVAL${RESET}"
        else
            echo -e "${TAB}$FILE ${BAD}FAIL${RESET} ${gray}RETVAL=$RETVAL${RESET}"
        fi
    else
        echo -e "${TAB}$FILE ${UL}not found${RESET}"
    fi
done

# ROOT
if [[ "$LIST" == *"thisroot.sh"* ]]; then
    which root
fi

if $VB; then
    # reset tab
    dtab
fi

if [[ "${BASH_SOURCE}" == "${HOME}/.bash_aliases" ]]; then
    vecho -e "${TAB}${BASH_SOURCE[0]} ${GOOD}done\x1b[0m"
fi

if [ ! -z ${oldFILE+dummy} ]; then
    FILE=$oldFILE
fi
decho -e "${TAB}\x1b[7;32mdone\x1b[m"
