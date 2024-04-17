# .bash_logout
#echo Using GitHub version .bash_logout

# kill programs
list="collatz_loop.exe \
	interrupt.exe \
	timer.exe"
for prog in $list
do
    if [ "${VB}" = true ]; then echo -n "checking $prog... ";fi
    pid=`ps ux | \grep $prog | \grep -v grep | awk '{print $2}'`
    if [[ -z $pid ]]; then
	if [ "${VB}" = true ]; then echo "not found";fi
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

# show top processes
# number of processes to show
NP=3
# set line width to one less than the terminal width
line_width=$(( $(tput cols) - 1 ))
# user ID
U_ID=1111499164
# user name
U_NAME=jlighthall
echo -e "\033[4mTop $NP processes by ${USER}:\x1b[0m"
ps ux --sort=-pcpu | head -n $((NP+1)) | sed "s/${U_ID}/${U_NAME}/" | awk '{printf "%-10s %5s %5s %5s %4s %5s %6s %s\n",$1,$2,$3,$4,$8,$9,$10,$11}' | cut -c -$line_width
echo

# say goodbye
echo "goodbye, $(hostname -s)"

# enter logout time into history
if [ -f ${HOME}/.bash_history ]; then
    echo "#$(date +'%s') LOGOUT $(date +'%a %b %d %Y %R:%S %Z') from $(hostname -s) $(ps -o etime | sed -n '2p' | sed 's/^[ ]*//')" >> ${HOME}/.bash_history
fi
