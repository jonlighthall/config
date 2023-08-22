# .bash_logout
#echo Using GitHub version .bash_logout

# User-specific environment and logout programs

echo "goodbye, $(HOSTNAME)"
if [ -f ${HOME}/.bash_history ]; then
    echo "#$(date +'%s') LOGOUT $(date +'%a %b %d %Y %R:%S %Z') from $(HOSTNAME)" >> ${HOME}/.bash_history
fi
