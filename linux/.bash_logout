# .bash_logout
#echo Using GitHub version .bash_logout

# User-specific environment and logout programs

# kill programs
list="collatz_loop.exe \
	interrupt.exe \
	timer.exe"
for prog in $list
do
    if $VB; then echo -n "checking $prog... ";fi
    pid=`ps ux | grep $prog | grep -v grep | awk '{print $2}'`
    if [[ -z $pid ]]; then
	if $VB; then echo "not found";fi
    else
	if $VB; then 
	    echo "found"
	else
	    echo "$prog is running"
	fi
	for proc in $pid
	do
	    echo -n " PID $proc... "
	    kill $proc
	    if [[ $? == 0 ]]; then
		echo "killed"
	    else
		echo "not killed. error $?"
	    fi
	done
    fi
done

# say goodbye
echo "goodbye, $(hostname -s)"

# enter logout time into history
if [ -f ~/.bash_history ]; then
    echo "#$(date +'%s') LOGOUT $(date +'%a %b %d %Y %R:%S %Z') from $(hostname -s)" >> ~/.bash_history
fi
