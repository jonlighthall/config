# User-dependent .bash_profile for Git Bash
# Note: this file must use Unix line endings (LF)!

# If not running interactively, don't do anything
if [[ "$-" != *i* ]]; then
    # turn off "Verbose Bash" conditional prints
    export VB=false
else
    # get starting time in nanoseconds
    declare -i start_time=$(date +%s%N)
    # set "Verbose Bash" for conditional prints
    export VB=true
fi

config_dir=${HOME}/config
# load utility functions
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
        vecho -e "${TAB}$fpretty ${GOOD}OK${RESET} ${GRAY}RETVAL=$RETVAL${RESET}"
    else
        echo -e "${TAB}$fpretty ${BAD}FAIL${RESET} ${GRAY}RETVAL=$RETVAL${RESET}"
    fi
    set -e
else
    echo "${TAB}$fname not found"
    set +eu
fi    

if [ "${VB}" = true ]; then
    # determine if being sourced or executed
    if (return 0 2>/dev/null); then
        RUN_TYPE="sourcing"
    else
        RUN_TYPE="executing"
    fi    
    print_source
    decho -e "${TAB}verbose bash printing is... ${GOOD}$VB${RESET}"
    # set tab
    itab
fi

# system dependencies
SYS_NAME=git_bash
HOST_NAME=$HOSTNAME
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
        vecho -e "${TAB}$fname ${GOOD}OK${RESET} ${GRAY}RETVAL=$RETVAL${RESET}"
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

# print welcome message
echo "${TAB}Welcome to ${HOST_NAME}"

# deallocate variables
unset config_dir
unset fname
unset fpretty
unset hist_file
