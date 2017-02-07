export CLICOLOR=1

alias emacs='open -a /Applications/Emacs.app/Contents/MacOS/Emacs $1'
alias onedrive='open -a /Applications/OneDrive.app/Contents/MacOS/OneDrive'

alias lson='ssh -Y lighthall@sonata.phy.anl.gov'
alias hson='ssh -Y -l helios@phy sonata.phy.anl.gov'
alias bson='ssh -Y bavarians@sonata.phy.anl.gov'
alias lcan='ssh -Y lighthall@cantata.phy.anl.gov'
alias hcan='ssh -Y -l helios@phy cantata.phy.anl.gov'
alias bcan='ssh -Y bavarians@cantata.phy.anl.gov'
alias bronco='ssh -v -ND 1584 j4lighth@bronco.wmich.edu'
alias sonata='ssh -v -ND 1584 lighthall@sonata.phy.anl.gov'
alias ladd19='ssh -Y emma@ladd19.triumf.ca'
alias ladd20='ssh -Y emma@ladd20.triumf.ca'
alias ladd21='ssh -Y emma@ladd21.triumf.ca'
alias epics='ssh -v -Y tigress@isacepics1'
alias trcomp='ssh -v -Y lighthall@trcomp01.triumf.ca'

#. ~/Applications/root-v5-34-00-patches/bin/thisroot.sh
. /Applications/root_v5.34.34/bin/thisroot.sh 
#. /Applications/root_v6.06.00/bin/thisroot.sh 
#alias xroot='xquartz & root'
alias root='root -l'

. /Users/lighthall/Applications/geant4.9.6.p02-install/bin/geant4.sh

##
# Your previous /Users/lighthall/.profile file was backed up as /Users/lighthall/.profile.macports-saved_2013-06-05_at_15:50:58
##

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

echo "Welcome to" $HOSTNAME

##
# Your previous /Users/lighthall/.profile file was backed up as /Users/lighthall/.profile.macports-saved_2014-11-20_at_12:41:02
##

# MacPorts Installer addition on 2014-11-20_at_12:41:02: adding an appropriate PATH variable for use with MacPorts.
export PATH="/opt/local/bin:/opt/local/sbin:$PATH"
# Finished adapting your PATH environment variable for use with MacPorts.

alias hsond='ssh -Y -l helios@phy sonata.phy.anl.gov "'"cd \music\helios\Si28\offline\lighthall\extra\dwba"'"'

