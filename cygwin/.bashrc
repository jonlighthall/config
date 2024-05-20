# ${HOME}/config/cygwin/.bashrc
#
# Interactive shell settings for Cygwin

# load bash utilities
config_dir=${HOME}/config
fpretty=${config_dir}/.bashrc_pretty
if [ -e $fpretty ]; then
    source $fpretty
    set -e
    set_traps
else
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
fi

# required list
LIST="$HOME/config/.bashrc_common $HOME/config/cygwin/.bashrc_prompt $HOME/config/cygwin/.bashrc_X11"

# optional list
LIST_OPT="$HOME/.bash_local"

# add optional list to required list if targets exist
for FILE in $LIST_OPT; do
    if [ -f $FILE ]; then
        LIST+=" $FILE"
    else
        vecho -e "${TAB}$FILE ${UL}not found${RESET}"
    fi
done

# source list of files
for FILE in $LIST; do
    vecho "${TAB}loading $FILE..."
    if [ -f $FILE ]; then
        source $FILE
        RETVAL=$?
        if [ $RETVAL -eq 0 ]; then
            vecho -e "${TAB}$FILE ${GOOD}OK${RESET} ${GRAY}RETVAL=$RETVAL${RESET}"
        else
            echo -e "${TAB}$FILE ${BAD}FAIL${RESET} ${GRAY}RETVAL=$RETVAL${RESET}"
        fi
    else
        echo -e "${TAB}$FILE ${UL}not found${RESET}"
    fi
done

if [ "${VB}" = true ]; then
    # reset tab
    dtab
fi
