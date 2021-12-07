# User dependent .bash_profile file
echo running "$BASH_SOURCE"...

# source the users bashrc if it exists
if [ -f "${HOME}/config/wsl/.bashrc" ] ; then
  source "${HOME}/config/wsl/.bashrc"
fi

if [ -f ~/.bash_history ]; then
    echo "#$(date +'%s') LOGIN  $(date +'%a %b %d %Y %R:%S %Z') from $(hostname -s)" >> ~/.bash_history
fi

echo "Welcome to" $HOSTNAME
