# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc # --> Read /etc/bashrc, if present.
fi

# User specific aliases and functions

# Settings
# Store 10000 commands in bash history
export HISTFILESIZE=10000
export HISTSIZE=10000
# Don't put duplicate lines in the history
export HISTCONTROL=ignoredups
# Editors
export SVN_EDITOR=emacs
export GIT_EDITOR=emacs

# Macros
alias ls='ls --color'
alias naut='nautilus --no-desktop --browser ./ &'
alias term='gnome-terminal &'

# Log on (remote)
# Argonne (phy.anl.gov)
alias bcan='ssh -Y bavarians@cantata.phy.anl.gov'
alias bson='ssh -Y bavarians@sonata.phy.anl.gov'
alias lcan='ssh -Y lighthall@cantata.phy.anl.gov'
alias lson='ssh -Y lighthall@sonata.phy.anl.gov'
alias hcan='ssh -Y -l helios@phy cantata.phy.anl.gov'
alias hson='ssh -Y -l helios@phy sonata.phy.anl.gov'
# TRIUMF (triumf.ca)
alias lighthall='ssh -Y lighthall@lighthall.triumf.ca'
alias ladd19='ssh  -Y emma@ladd19.triumf.ca'
alias ladd20='ssh  -Y emma@ladd20.triumf.ca'
alias ladd21='ssh  -Y emma@ladd21.triumf.ca'
# FSU (physics.fsu.edu)
alias elwood='xwin & ssh -Y lighthall@elwood.physics.fsu.edu'
alias ray='xwin & ssh -Y lighthall@ray.physics.fsu.edu'
