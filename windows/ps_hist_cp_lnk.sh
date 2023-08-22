## Copy and link from OneDrive
# PowerShell history
if [ -f ${HOME}/.bash_history ];
then
    echo "appending local copy with synced copy..."
    type C:\Users\jonli\AppData\Roaming\Microsoft\Windows\PowerShell\PSReadline\ConsoleHost_history.txt >> C:\Users\jonli\OneDrive\Documents\ConsoleHost_history.txt
    echo "removing local copy"
    del C:\Users\jonli\AppData\Roaming\Microsoft\Windows\PowerShell\PSReadline\ConsoleHost_history.txt
else
    echo "local copy not found"
fi
echo "creating symbolic link"
mklink C:\Users\jonli\AppData\Roaming\Microsoft\Windows\PowerShell\PSReadline\ConsoleHost_history.txt C:\Users\jonli\OneDrive\Documents\ConsoleHost_history.txt
