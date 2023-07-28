export CLICOLOR=1

alias emacs='open -a /Applications/Emacs.app/Contents/MacOS/Emacs $1'
alias onedrive='open -a /Applications/OneDrive.app/Contents/MacOS/OneDrive'

# Source remote aliases
if [ -f ~/config/.bash_remotes ]; then
    . ~/config/.bash_remotes
fi

#. ~/Applications/root-v5-34-00-patches/bin/thisroot.sh
. /Applications/root_v5.34.34/bin/thisroot.sh
#. /Applications/root_v6.06.00/bin/thisroot.sh
#alias xroot='xquartz & root'
alias root='root -l'

. /Users/lighthall/Applications/geant4.9.6.p02-install/bin/geant4.sh

# MacPorts Installer addition on 2013-06-05_at_15:50:58: adding an appropriate PATH variable for use with MacPorts.
export PATH=/Applications/Emacs.app/Contents/MacOS/:/opt/local/bin:/opt/local/sbin:$PATH
# Finished adapting your PATH environment variable for use with MacPorts.

# Store 10000 commands in bash history
export HISTFILESIZE=10000
export HISTSIZE=10000
# Don't put duplicate lines in the history
export HISTCONTROL=ignoredups

#export DISPLAY=localhost:0.0

export SVN_EDITOR=emacs
export SVN_MERGE=emacs

# print welcome message
echo "Welcome to" $HOSTNAME

# MacPorts Installer addition on 2014-11-20_at_12:41:02: adding an appropriate PATH variable for use with MacPorts.
export PATH="/opt/local/bin:/opt/local/sbin:$PATH"
# Finished adapting your PATH environment variable for use with MacPorts.
