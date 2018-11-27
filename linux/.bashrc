# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc # --> Read /etc/bashrc, if present.
fi

# User specific aliases and functions

# Settings
# Store 50000 commands in bash history
export HISTFILESIZE=50000
export HISTSIZE=40000
# Don't put duplicate lines in the history
export HISTCONTROL=ignoredups
# Editors
export SVN_EDITOR=emacs
export GIT_EDITOR=emacs

# Macros
alias ls='ls --color'
alias naut='nautilus --no-desktop --browser ./ &'
alias term='gnome-terminal &'
alias ping='ping -c 5'
function duf {	       
    du -k "$@" | sort -n |
	while read size fname; do
     	    for unit in k M G T P E Z Y;
	    do
		if [ $size -lt 1024 ]; then
		    echo -e "${size}${unit}B${fname}";
		    break;
		fi;
		size=$((size/1024));
	    done;
	done
}

alias du1='duf --max-depth=1'
alias du2='duf --max-depth=2'
alias du0='duf --max-depth=0'

# Source remote aliases
if [ -f ~/config/.bash_remotes ]; then
	. ~/config/.bash_remotes 
fi
