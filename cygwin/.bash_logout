# .bash_logout
#echo Using GitHub version .bash_logout

# User-specific environment and logout programs

echo "goodbye, $(HOSTNAME)"
if [ -f ~/.bash_history ]; then
    echo "#$(date +'%s') LOGOUT $(date +'%a %b %d %Y %R:%S %Z') from $(HOSTNAME)" >> ~/.bash_history
fi
