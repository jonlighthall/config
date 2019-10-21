# .bash_logout
#echo Using GitHub version .bash_logout

# User-specific environment and logout programs

echo "goodbye"
if [ -f ~/.bash_history ]; then
    echo "LOGOUT $(date)" >> ~/.bash_history
fi
