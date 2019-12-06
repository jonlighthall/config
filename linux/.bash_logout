# .bash_logout
#echo Using GitHub version .bash_logout

# User-specific environment and logout programs

echo "goodbye"
if [ -f ~/.bash_history ]; then
    echo "#$(date +'%s') LOGOUT $(date +'%a %b %d %Y %R:%S %Z')" >> ~/.bash_history
fi
