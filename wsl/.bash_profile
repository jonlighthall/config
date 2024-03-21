# User-dependent .bash_profile for WSL
# Note: this file must use Unix line endings (LF)!

# If not running interactively, don't do anything
if [[ "$-" != *i* ]]; then
    echo -e "${TAB}\E[7mnot interactive\e[0m"
    echo "${TAB}exiting ${BASH_SOURCE##*/}..."
else
    echo -n "${TAB}${BASH_SOURCE##*/}... "
fi

# get starting time in nanoseconds
declare -i start_time=$(date +%s%N)

# clear terminal
called_by=$(ps -o comm= $PPID)
echo "invoked by ${called_by}"
clear -x

# Verbose bash prints?
export VB=true
if $VB; then
    # load formatting
    fpretty=${HOME}/config/.bashrc_pretty
    if [ -e $fpretty ]; then
        source $fpretty
        set -e
        set_tab
        print_ribbon
    else
        set +eu
    fi    

    # determine if being sourced or executed
    if (return 0 2>/dev/null); then
        RUN_TYPE="sourcing"
    else
        RUN_TYPE="executing"
    fi    
    print_source    
    echo "${TAB}verbose bash printing is... $VB"
fi

# system dependencies
SYS_NAME=wsl
HOST_NAME=$(hostname -s)
vecho -e "${TAB}applying ${SYS_NAME} settings on ${PSHOST}${HOST_NAME}${RESET}"

# save login timestamp to history
hist_file=${HOME}/.bash_history
vecho -n "${TAB}appending login timestamp to $hist_file... "
if [ -f $hist_file ]; then
    echo "#$(date +'%s') LOGIN  $(date +'%a %b %d %Y %R:%S %Z') from ${HOST_NAME}" >>$hist_file
    RETVAL=$?
    if [ $RETVAL -eq 0 ]; then
        vecho -e "${GOOD}OK${RESET} ${GRAY}RETVAL=$RETVAL${RESET}"
    else
        if $VB; then
            echo -e "${BAD}FAIL${RESET} ${GRAY}RETVAL=$RETVAL${RESET}"
        else
            echo "echo to $hist_file failed"
        fi
    fi
else
    if $VB; then
        echo "${BAD}NOT FOUND{RESET}"
    else
        echo "$hist_file not found"
    fi
fi

# source the user's .bashrc if it exists
fname=${HOME}/config/${SYS_NAME}/.bashrc
vecho -n "${TAB}"
decho -en "\x1b[7m"
vecho -e "loading $fname... ${RESET}"
if [ -f $fname ]; then
    source $fname
    RETVAL=$?
    if [ $RETVAL -eq 0 ]; then
        vecho -e "${TAB}$fname ${GOOD}OK${RESET} ${GRAY}RETVAL=$RETVAL${RESET}"
    else
        echo -e "${TAB}$fname ${BAD}FAIL${RESET} ${GRAY}RETVAL=$RETVAL${RESET}"
    fi
else
    echo "${TAB}$fname not found"
fi
vecho
# print runtime duration
if $VB; then
    dtab
    print_done
fi

# clear terminal
clear -x

# print welcome message
echo "${TAB}Welcome to ${HOST_NAME}"

# test formatting
source ~/utils/bash/git/lib_git.sh
start_dir=$PWD
echo "${TAB}starting directory = ${start_dir}"
cd config
print_remotes
# return to starting directory
cd "$start_dir"
