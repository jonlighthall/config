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
export HISTIGNORE=$'bg:exit:ls:pwd'
# Realtime history
#shopt -s histappend
PROMPT_COMMAND="history -a;$PROMPT_COMMAND"
# Editors
exprot EDITOR=emacs
export SVN_EDITOR=emacs
export GIT_EDITOR=emacs
# Prompt
git_branch() {
    git branch 2>/dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/'
# 2>/dev/null redirects stderr to null
# sed -e '/^[^*]/d' deletes any lines not containing *
# sed -e 's/* \(.*\)/ (\1)/' replaces the first instance of '* '
}
export PS1='\[\e[1;32m\][\u@\h \[\e[34m\]\W\[\e[32m\]\e[35m\]$(git_branch)\e[32m\]]$\[\e[0m\] '
export PS1='\[\e[1;32m\][\u@\h \[\e[34m\]\w\[\e[32m\]\e[35m\]$(git_branch)\e[32m\]]\n$\[\e[0m\] ' # full dir, new line
export PS1='\[\e[1;32m\][\u@\h \[\e[34m\]\w\[\e[32m\]\e[35m\]$(git_branch)\e[32m\]]\n\e[0;37m\]\A\e[1;32m\] $\[\e[0m\] ' # post time
export PS1='\e[0;37m\]\A\[\e[1;32m\][\u@\h \[\e[34m\]\w\[\e[32m\]\e[35m\]$(git_branch)\e[32m\]]$\[\e[0m\] ' # pre time

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

# Add to path
if [[ ":$PATH" != *":/home/jlighthall/bin"*  ]]; then 
    export PATH=$PATH:~/bin
fi