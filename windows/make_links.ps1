# this is a powershell script..?

# Must first run the command
# Set-ExecutionPolicy -ExecutionPolicy Unrestricted -Scope Process


## Copy and link from OneDrive
# PowerShell history

$local = 'C:\Users\jonli\AppData\Roaming\Microsoft\Windows\PowerShell\PSReadline\ConsoleHost_history.txt'
$cloud = 'C:\Users\jonli\OneDrive\Documents\ConsoleHost_history.txt'

echo "test local history...`n$local"
if (Test-Path -Path $local) {
   echo "found"
   echo "test cloud history...`n$cloud"
   if (Test-Path -Path $cloud) {
      echo "found"
      echo "proceeding with append, delete, and link..."

#if
    echo "appending local copy with synced copy..."
#    type C:\Users\jonli\AppData\Roaming\Microsoft\Windows\PowerShell\PSReadline\ConsoleHost_history.txt >> C:\Users\jonli\OneDrive\Documents\ConsoleHost_history.txt
    echo "removing local copy"
#    del C:\Users\jonli\AppData\Roaming\Microsoft\Windows\PowerShell\PSReadline\ConsoleHost_history.txt
#else
    echo "local copy not found"
#fi
echo "creating symbolic link"
#mklink C:\Users\jonli\AppData\Roaming\Microsoft\Windows\PowerShell\PSReadline\ConsoleHost_history.txt C:\Users\jonli\OneDrive\Documents\ConsoleHost_history.txt


   }    
   else {
      echo "not found"
      echo "no 
   }
   }
   else {
   echo "not found"
   }
