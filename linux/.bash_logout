# .bash_logout
#echo Using GitHub version .bash_logout

# kill programs
list="collatz_loop.exe \
	interrupt.exe \
	timer.exe"
for prog in $list
do
    if [ "${VB}" = true ]; then
        echo -n "checking $prog... "
    fi
    pid=`ps ux | \grep $prog | \grep -v grep | awk '{print $2}'`
    if [[ -z $pid ]]; then
	      if [ "${VB}" = true ]; then
            echo "not found"
        fi
    else
	      if [ "${VB}" = true ]; then
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
echo

utop

# say goodbye
echo "goodbye, $(hostname -s)"

# enter logout time into history
if [ -f ${HOME}/.bash_history ]; then
    echo "#$(date +'%s') LOGOUT $(date +'%a %b %d %Y %R:%S %Z') from $(hostname -s) $(ps -o etime | sed -n '2p' | sed 's/^[ ]*//')" >> ${HOME}/.bash_history
fi
