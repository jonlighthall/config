# ~/config/wsl/.bashrc
# Interactive shell settings for Linux Subsystem for Windows

# Source system settings
if [ -f ~/.bashrc ]; then
    . ~/.bashrc
fi

# Source common user settings
if [ -f ~/config/.bashrc_common ]; then
    . ~/config/.bashrc_common 
fi

# System-specific aliases and functions

# Prompt
git_branch() {
    git branch 2>/dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/'
    # 2>/dev/null redirects stderr to null
    # sed -e '/^[^*]/d' deletes any lines not containing *
    # sed -e 's/* \(.*\)/ (\1)/' replaces the first instance of '* '
}
export PS1='\[\e[1;32m\][\u@\h \[\e[34m\]\W\[\e[32m\]\e[35m$(git_branch)\e[32m]$\[\e[0m\] ' # user, host, base dir, branch
export PS1='\[\e[1;32m\][\u@\h \[\e[34m\]\w\[\e[32m\]\e[35m$(git_branch)\e[32m]\n$\[\e[0m\] ' # full dir, prompt on new line
export PS1='\[\e[1;32m\][\u@\h \[\e[34m\]\w\[\e[32m\]\e[35m$(git_branch)\e[32m]\n\e[0;37m\A\e[0;32m $\[\e[0m\] ' # time, prompt on new line
export PS1='\e[0;37m\A\[\e[1;32m\][\u@\h \[\e[34m\]\w\[\e[32m\]\e[35m$(git_branch)\e[32m]$\[\e[0m\] ' # time, prompt on same line
export PS1='\e[0;37m\A\[\e[1;32m\] \u@\h \[\e[34m\]\w\[\e[32m\]\e[35m$(git_branch)\e[32m\n$\[\e[0m\] ' # time before, new line, no brackets
export PS1='\e[0;37m\A\[\e[1;32m\] \u@\[\e[1;35m\]\h\[\e[1;34m\] \w\[\e[32m\]\e[35m$(git_branch)\e[32m\n$\[\e[0m\] ' # time before, new line, no brackets, highlight host
export PS1='\e[0;37m\A\[\e[0;32m\] \u@\[\e[1;34m\]\h\[\e[0;33m\] \w\[\e[0;32m\]\e[36m$(git_branch)\e[0;32m\n$\[\e[0m\] ' # time before, new line, no brackets, highlight host, git bash color

# Macros
alias naut='nautilus --no-desktop --browser ./ &'
alias term='gnome-terminal &'

# X Window
export DISPLAY=localhost:0.0 

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
