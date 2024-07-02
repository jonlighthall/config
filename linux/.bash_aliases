#!/bin/bash 
# check if running interactively
if [[ "$-" == *i* ]];then
    echo "${TAB-}${BASH_SOURCE##*/}: interactive shell" >&2
else
    echo "${TAB-}${BASH_SOURCE##*/}: non-interactive shell" >&2
    echo -e "${TAB-}\x1B[1;31mWARNING: ${BASH_SOURCE##*/} is intended for interactive shells only\x1B[m" >&2
fi

# check if login shell
if shopt -q login_shell; then
    echo -e "${TAB-}${BASH_SOURCE##*/}: \x1B[32mlogin shell\x1B[m" >&2
    echo -e "${TAB-}\x1B[;33mNOTE: ${BASH_SOURCE##*/} is intended for non-login shells only\x1B[m" >&2
else
    echo -e "${TAB-}${BASH_SOURCE##*/}: \x1B[32mnon-login shell\x1B[m" >&2
fi
