# ~/config/wsl/.bashrc

# load prepackaged .bashrc
if [ -f ~/.bashrc ]; then
    . ~/.bashrc
fi

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=50000
HISTFILESIZE=40000

# Source remote aliases
if [ -f ~/config/.bash_remotes ]; then
	. ~/config/.bash_remotes 
fi

# Editors
export SVN_EDITOR=emacs
export GIT_EDITOR=emacs

# Macros
#alias ls='ls --color=auto'
# alias ls='ls -hF --color=tty'                 # classify files in colour

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
