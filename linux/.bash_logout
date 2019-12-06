# .bash_logout
#echo Using GitHub version .bash_logout

# User-specific environment and logout programs

echo "goodbye"
if [ -f ~/.bash_history ]; then
    echo "LOGOUT $(date +'%a %b %d %Y %H:%M %Z')" >> ~/.bash_history
fi
