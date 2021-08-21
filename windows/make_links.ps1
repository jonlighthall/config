# this is a powershell script..?

# Must first run the command
# Set-ExecutionPolicy -ExecutionPolicy Unrestricted -Scope Process


## Copy and link from OneDrive
# PowerShell history

$local = 'C:\Users\jonli\AppData\Roaming\Microsoft\Windows\PowerShell\PSReadline\ConsoleHost_history.txt'
$cloud = 'C:\Users\jonli\OneDrive\Documents\ConsoleHost_history.txt'

echo "test local history..."
if (Test-Path -Path $local) {
       echo "$local found"
       echo "test cloud history..."
       if (Test-Path -Path $cloud) {
	      echo "$cloud found"
	      echo "proceeding with append, delete, and link..."
	      
	      #if
	      echo "appending local copy with cloud copy..."
	      #    type $local >> $cloud
	      echo "removing local copy"
	      #    del $local
	      #fi
	      echo "creating symbolic link"
	      #mklink $local $cloud
	      
	}    
	else {
	      echo "$cloud not found"
  	      echo "no history to link to`nexiting"
        }	   
}	   
else {  
echo "$local not found"
}
