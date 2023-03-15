# .bash_logout

# say goodbye
echo "goodbye, $(hostname -s)"

# enter logout time into history
if [ -f ~/.bash_history ]; then
    echo "#$(date +'%s') LOGOUT $(date +'%a %b %d %Y %R:%S %Z') from $(hostname -s)" >> ~/.bash_history
fi
