# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc # --> Read /etc/bashrc, if present.
fi

# User specific aliases and functions

if [ $HOSTNAME = "elwood.physics.fsu.edu" ]; then
    :
    #source /usr/local/cern/root534/bin/thisroot.sh
fi

if [ $HOSTNAME = "ray.physics.fsu.edu" ]; then
    source /usr/local/cern/root5/bin/thisroot.sh
fi

alias svni='svn info svn+ssh://lighthall@lighthall.triumf.ca/Users/lighthall/SkyDrive/Documents/repository/'
# Store 10000 commands in bash history
export HISTFILESIZE=10000
export HISTSIZE=10000
# Don't put duplicate lines in the history
export HISTCONTROL=ignoredups
alias lighthall='ssh -Y lighthall@lighthall.triumf.ca'
alias naut='nautilus --no-desktop --browser ./ &'
alias anasen='ssh -Y anasen@dhcp111208.physics.fsu.edu'
alias ls='ls --color'
alias ray='ssh -Y lighthall@ray.physics.fsu.edu'
alias elwood='ssh -Y lighthall@elwood.physics.fsu.edu'
alias jl='cd /home/lighthall/anasen/'
alias emacs='~/Downloads/emacs-24.5/src/emacs'
alias term='gnome-terminal &'
