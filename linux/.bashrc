# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc # --> Read /etc/bashrc, if present.
fi

# User specific aliases and functions

alias svni='svn info svn+ssh://lighthall@lighthall.triumf.ca/Users/lighthall/SkyDrive/Documents/repository/'
# Store 10000 commands in bash history
export HISTFILESIZE=10000
export HISTSIZE=10000
# Don't put duplicate lines in the history
export HISTCONTROL=ignoredups
alias lighthall='ssh -Y lighthall@lighthall.triumf.ca'
alias naut='nautilus --no-desktop --browser ./ &'
alias ls='ls --color'
alias term='gnome-terminal &'
