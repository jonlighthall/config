# ~/config/wsl/.bashrc
# Interactive shell settings for Linux Subsystem for Windows

# Source system settings
if [ -f ~/.bashrc ]; then
    . ~/.bashrc
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
export PS1='\[\e[1;32m\][\u@\h \[\e[34m\]\W\[\e[32m\]\e[35m\]$(git_branch)\e[32m\]]$\[\e[0m\] '
export PS1='\[\e[1;32m\][\u@\h \[\e[34m\]\w\[\e[32m\]\e[35m\]$(git_branch)\e[32m\]]\n$\[\e[0m\] ' # full dir, new line
export PS1='\[\e[1;32m\][\u@\h \[\e[34m\]\w\[\e[32m\]\e[35m\]$(git_branch)\e[32m\]]\n\e[0;37m\]\A\e[1;32m\] $\[\e[0m\] ' # post time
export PS1='\e[0;37m\]\A\[\e[1;32m\][\u@\h \[\e[34m\]\w\[\e[32m\]\e[35m\]$(git_branch)\e[32m\]]$\[\e[0m\] ' # pre time

# Macros
#alias ls='ls --color=auto'
#alias ls='ls -hF --color=tty'                 # classify files in colour

# X Window
export DISPLAY=localhost:0.0 
alias xming='/mnt/c/Program\ Files\ \(x86\)/Xming/Xming.exe :0 -clipboard -multiwindow -silent-dup-error -logverbose 0 &'
alias vcx='/mnt/c/Program\ Files/VcXsrv/vcxsrv.exe :0 -clipboard -multiwindow &'

# Check if xwin is running
# -x flag only match processes whose name (or command line if -f is
# specified) exactly match the pattern. 

if pgrep -x "vcxsrv.exe" > /dev/null
then
    echo "VcXsrv is already running"
else
    echo "launching VcXsrv..."
    vcx
fi

if pgrep -x "Xming.exe" > /dev/null
then
    echo "Xming is already running"
else
    echo "launching Xming..."
    xming
fi

echo "sourcing root..."
. root_v5.34.36/bin/thisroot.sh
which root

# Source remote aliases
if [ -f ~/config/.bash_remotes ]; then
    . ~/config/.bash_remotes 
fi

# PGI
export PGI=/opt/pgi
export PATH=/opt/pgi/linux86-64/19.10/bin:$PATH
export MANPATH=$MANPATH:/opt/pgi/linux86-64/19.10/man
export LM_LICENSE_FILE=$LM_LICENSE_FILE:/opt/pgi/license.dat;
