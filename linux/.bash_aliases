#!/bin/bash 
# check if running interactively
if [[ "$-" == *i* ]];then
    if [ "${VB:-false}" = true ]; then
        echo "${TAB-}${BASH_SOURCE##*/}: interactive shell" >&2
    fi
else
    echo "${TAB-}${BASH_SOURCE##*/}: non-interactive shell" >&2
    echo -e "${TAB-}\x1B[1;31mWARNING: ${BASH_SOURCE##*/} is intended for interactive shells only\x1B[m" >&2
fi

# check if login shell
if [ "${VB:-false}" = true ]; then
    if shopt -q login_shell || [ -n "${VSCODE_SHELL_LOGIN:-}" ]; then
        echo -e "${TAB-}${BASH_SOURCE##*/}: \x1B[32mlogin shell\x1B[m" >&2
    else
        echo -e "${TAB-}${BASH_SOURCE##*/}: \x1B[32mnon-login shell\x1B[m" >&2
    fi
fi
