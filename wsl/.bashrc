# ~/config/wsl/.bashrc
# Interactive shell settings for Linux Subsystem for Windows

# Source system settings
if [ -f ~/.bashrc ]; then
    . ~/.bashrc
fi

# Source remote aliases
if [ -f ~/.bash_local ]; then
    . ~/.bash_local
fi

if [ -f ~/config/.bash_remotes ]; then
    . ~/config/.bash_remotes 
fi

# User specific aliases and functions

# Settings
# Commands to save
export HISTFILESIZE=50000
export HISTSIZE=40000
# Don't put duplicate lines in the history
export HISTCONTROL=ignoredups
export HISTIGNORE=$'bg:exit:ls:pwd:history'
# Realtime history
#shopt -s histappend
PROMPT_COMMAND="history -a;$PROMPT_COMMAND"
# Timestamp history
HISTTIMEFORMAT="%F %T "
# Editors
export EDITOR=emacs
export SVN_EDITOR=emacs
export GIT_EDITOR=emacs
# Prompt
git_branch() {
    git branch 2>/dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/'
    # 2>/dev/null redirects stderr to null
    # sed -e '/^[^*]/d' deletes any lines not containing *
    # sed -e 's/* \(.*\)/ (\1)/' replaces the first instance of '* '
}
export PS1='\[\e[1;32m\][\u@\h \[\e[34m\]\W\[\e[32m\]\e[35m$(git_branch)\e[32m]$\[\e[0m\] '
export PS1='\[\e[1;32m\][\u@\h \[\e[34m\]\w\[\e[32m\]\e[35m$(git_branch)\e[32m]\n$\[\e[0m\] ' # full dir, new line
export PS1='\[\e[1;32m\][\u@\h \[\e[34m\]\w\[\e[32m\]\e[35m$(git_branch)\e[32m]\n\e[0;37m\A\e[1;32m $\[\e[0m\] ' # post time
export PS1='\e[0;37m\A\[\e[1;32m\][\u@\h \[\e[34m\]\w\[\e[32m\]\e[35m$(git_branch)\e[32m]$\[\e[0m\] ' # pre time
export PS1='\e[0;37m\A\[\e[1;32m\] \u@\h \[\e[34m\]\w\[\e[32m\]\e[35m$(git_branch)\e[32m\n$\[\e[0m\] ' # pre time, new line, no brackets
export PS1='\e[0;37m\A\[\e[1;32m\] \u@\[\e[1;35m\]\h\[\e[1;34m\] \w\[\e[32m\]\e[35m$(git_branch)\e[32m\n$\[\e[0m\] ' # pre time, new line, no brackets, highlight host
export PS1='\e[0;37m\A\[\e[1;32m\] \u@\[\e[1;35m\]\h\[\e[1;34m\] \w\[\e[32m\]\e[35m$(git_branch)\n\e[1;32m$\[\e[0m\] ' # pre time, new line, no brackets, highlight host
export PS1='\e[0;37m\A\[\e[0;32m\] \u@\[\e[1;35m\]\h\[\e[1;34m\] \w\[\e[0;32m\]\e[36m$(git_branch)\n\e[1;32m$\[\e[0m\] ' # pre time, new line, no brackets, highlight host

# Macros
alias ls='ls --color'
alias la='ls -la'
alias lt='ls -ltr'
alias lS='ls -ltS'
alias pwd='pwd -L;pwd -P'
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

# X Window
export DISPLAY=localhost:0.0 

# Path additions
if [[ ":$PATH" != *":${HOME}/bin"*  ]]; then
    if [ -d "${HOME}/bin" ] ; then
	export PATH=$PATH:${HOME}/bin
    fi
fi

# ROOT
if [ -f root_v5.34.36/bin/thisroot.sh ]; then
    echo "sourcing root..."
    . root_v5.34.36/bin/thisroot.sh
    which root
fi

# PGI
export PGI=/opt/pgi
if [ -d $PGI ]; then
#   echo "adding PGI to path..."
    export PATH=$PGI/linux86-64/19.10/bin:$PATH
    export MANPATH=$MANPATH:$PGI/linux86-64/19.10/man
    export LM_LICENSE_FILE=$LM_LICENSE_FILE:$PGI/license.dat;
fi
