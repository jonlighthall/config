# User-dependent .bash_profile for WSL
# Note: this file must use Unix line endings (LF)!

echo -e "\x1b[7;38;5;132m${#BASH_SOURCE[@]}\x1b[0m"
msg=$(echo "this file is $(readlink -f ${BASH_SOURCE[0]##*/})!")
ln=$(for ((i = 1; i <= ${#msg}; i++)); do echo -n "-"; done)
echo -e "$ln\n$msg\n$ln" | sed "s/^/${TAB}/"

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
        if [ -z ${FPRETTY_LOADED+dummy} ]; then
            source $fpretty
            set_tab
            set -e            
        fi
    else
        set +eu
    fi
    # determine if being sourced or executed
    if (return 0 2>/dev/null); then
        RUN_TYPE="sourcing"
    else
        RUN_TYPE="executing"
    fi
    get_source
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
        vecho -e "${GOOD}OK${RESET} ${gray}RETVAL=$RETVAL${RESET}"
    else
        if $VB; then
            echo -e "${BAD}FAIL${RESET} ${gray}RETVAL=$RETVAL${RESET}"
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
vecho -e "${TAB}\x1b[7mloading $fname... \x1b[m"
if [ -f $fname ]; then
    source $fname
    RETVAL=$?
    if [ $RETVAL -eq 0 ]; then
        vecho -e "${TAB}$fname ${GOOD}OK${RESET} ${gray}RETVAL=$RETVAL${RESET}"
    else
        echo -e "${TAB}$fname ${BAD}FAIL${RESET} ${gray}RETVAL=$RETVAL${RESET}"
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
cd $start_dir
