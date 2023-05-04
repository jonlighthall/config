# .bash_logout

# say goodbye
echo "goodbye, $(hostname -s)"

# enter logout time into history
hist_file=~/.bash_history
if [ -f $hist_file ]; then
    echo "#$(date +'%s') LOGOUT $(date +'%a %b %d %Y %R:%S %Z') from $(hostname -s)" >> $hist_file
fi
