# ~/.bash_profile: executed by bash(1) for login shells.
# User dependent .bash_profile file

# source the users bashrc if it exists
if [ -f "${HOME}/config/cygwin/.bashrc" ] ; then
  source "${HOME}/config/cygwin/.bashrc"
fi

# Set MANPATH so it includes users' private man if it exists
# if [ -d "${HOME}/man" ]; then
#   MANPATH="${HOME}/man:${MANPATH}"
# fi

# Set INFOPATH so it includes users' private info if it exists
# if [ -d "${HOME}/info" ]; then
#   INFOPATH="${HOME}/info:${INFOPATH}"
# fi

echo "Welcome to" $HOSTNAME
if [ -f ~/.bash_history ]; then
    echo "#$(date +'%s') LOGIN  $(date +'%a %b %d %Y %R:%S %Z')" >> ~/.bash_history
fi
