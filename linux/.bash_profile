# -----------------------------------------------------------------------------------------------
# User-dependent LOGIN SHELL SETTINGS for Linux
# -----------------------------------------------------------------------------------------------
#
# ~/.bash_profile -> ~/config/linux/.bash_profile
#
# Purpose: execute login functions and load system-dependent interactive shell settings.
#
# Usage: Executed by bash for interactive login shell sessions.
#
# Note: this file must use Unix line endings (LF)!
#
# Feb 2017 JCL
#
# -----------------------------------------------------------------------------------------------

# check if running interactively
if [[ "$-" == *i* ]];then
    # clear terminal
    clear

    # get starting time in nanoseconds
    declare -i start_time=$(date +%s%N)

    # set tab
    TAB=$(for ((i = 1; i < ${#BASH_SOURCE[@]}; i++)); do echo -n "   "; done)
    echo -e "${TAB}${BASH_SOURCE##*/}: \x1B[32minteractive shell\x1B[m" >&2
else
    echo "${TAB-}${BASH_SOURCE##*/}: non-interactive shell" >&2
    echo -e "${TAB-}\x1B[1;31mWARNING: ${BASH_SOURCE##*/} is intended for interactive shells only\x1B[m" >&2
    echo -e "${TAB-}returning..." >&2
    # If not running interactively, don't do anything
    return
fi

# check if login shell
if shopt -q login_shell; then
    echo -e "${TAB-}${BASH_SOURCE##*/}: \x1B[32mlogin shell\x1B[m" >&2
else
    echo "${TAB-}${BASH_SOURCE##*/}: non-login shell" >&2
    echo -e "${TAB-}\x1B[;31mWARNING: ${BASH_SOURCE##*/} is intended for login-shells only\x1B[m" >&2
fi

# -------------------------
# set debug level if unset
export DEBUG=${DEBUG=0}
# -------------------------

# print source
if [ ${DEBUG:-0} -gt 0 ]; then
    echo -e "${TAB:=$(for ((i = 1; i < ${#BASH_SOURCE[@]}; i++)); do echo -n "   "; done)}\E[2m${#BASH_SOURCE[@]}: ${BASH_SOURCE##*/} -> $(readlink -f ${BASH_SOURCE})\E[22m"
    # print invoking process
    called_by=$(ps -o comm= $PPID)
    echo "${TAB}invoked by ${called_by}"
fi
# set "Verbose Bash" for conditional prints
export VB=true
# clear terminal
clear -x
if [ ${DEBUG} -gt 0 ]; then
    export VB=true
fi

config_dir=${HOME}/config
# load bash utilities
fpretty=${config_dir}/.bashrc_pretty
if [ -e $fpretty ]; then
    if [ "${VB}" = true ]; then
        # remember, if .bashrc_pretty hasn't been loaded yet, vecho is not defined
        echo "loading $fpretty..."
    fi
    source $fpretty
    RETVAL=$?
    if [ $RETVAL -eq 0 ]; then
        dtab
        vecho -e "${TAB}$fpretty ${GOOD}OK${RESET}"
    else
        echo -e "${TAB}$fpretty ${BAD}FAIL${RESET} ${GRAY}RETVAL=$RETVAL${RESET}"
    fi
    print_ribbon
else
    echo "${TAB}$fname not found"
fi

if [ "${VB}" = true ]; then
    print_source
    if [[ "$-" == *i* ]] && [ ${DEBUG:-0} -gt 0 ]; then
        print_stack
    fi
    decho -e "${TAB}verbose bash printing is... $TRUE"
fi

# system dependencies
SYS_NAME=linux
HOST_NAME=$(hostname -s)
vecho -e "${TAB}applying ${SYS_NAME} settings on ${PSHOST}${HOST_NAME}${RESET}"

# save login timestamp to history
hist_file=${HOME}/.bash_history
vecho -n "${TAB}appending login timestamp to $hist_file... "
if [ -f $hist_file ]; then
    echo "#$(date +'%s') LOGIN  $(date +'%a %b %d %Y %R:%S %Z') from ${HOST_NAME}" >>$hist_file
    RETVAL=$?
    if [ $RETVAL -eq 0 ]; then
        vecho -e "${GOOD}OK${RESET}"
    else
        if [ "${VB}" = true ]; then
            echo -e "${BAD}FAIL${RESET} ${GRAY}RETVAL=$RETVAL${RESET}"
        else
            echo "echo to $hist_file failed"
        fi
    fi
else
    if [ "${VB}" = true ]; then
        echo "${BAD}NOT FOUND{RESET}"
    else
        echo "$hist_file not found"
    fi
fi

# load system-dependent interactive shell settings
fname=${config_dir}/${SYS_NAME}/.bashrc
vecho -e "${TAB}loading $fname... ${RESET}"
if [ -f $fname ]; then
    source $fname
    RETVAL=$?
    if [ $RETVAL -eq 0 ]; then
        vecho -e "${TAB}$fname ${GOOD}OK${RESET}"
    else
        echo -e "${TAB}$fname ${BAD}FAIL${RESET} ${GRAY}RETVAL=$RETVAL${RESET}"
    fi
else
    echo "${TAB}$fname not found"
fi

# print runtime duration
if [ "${VB}" = true ]; then
    # reset tab
    dtab
    # print timestamp
    print_done
    # print hidden text to force a new line before clearing screen
    vecho -e "\E[8mhello\E[28m"
fi

# clear terminal
clear -x

# show top processes
# number of processes to show
NP=3
# set line width to one less than the terminal width
line_width=$(( $(tput cols) - 1 ))
# user ID
U_ID=1111499164
# user name
U_NAME=jlighthall
echo -e "\033[4mTop $NP processes on ${HOST_NAME}:\x1b[0m"
ps aux --sort=-pcpu | head -n $((NP+1)) | sed "s/${U_ID}/\x1b\[32m${U_NAME}\x1b\[0m/" | awk '{printf "%-10s %5s %5s %5s %4s %5s %6s %s\n",$1,$2,$3,$4,$8,$9,$10,$11}' | cut -c -$line_width
echo
echo -e "\033[4mTop $NP processes by ${USER}:\x1b[0m"
ps ux --sort=-pcpu | head -n $((NP+1)) | sed "s/${U_ID}/${U_NAME}/" | awk '{printf "%-10s %5s %5s %5s %4s %5s %6s %s\n",$1,$2,$3,$4,$8,$9,$10,$11}' | cut -c -$line_width
echo
echo -e "\033[4mLast $NP log-ins by ${USER} on ${HOST_NAME}:\x1b[0m"
# print full user and domain neamse, full login and logout times and dates, and hostname
last -wFa | \grep ${U_NAME:0:8} | head -n $NP
echo

# print welcome message
echo "${TAB}Welcome to ${HOST_NAME}"
