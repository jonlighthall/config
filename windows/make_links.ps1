# this is a PowerShell script

# Must run in elevated prompt to make link

# Must first run the command before runningcd .. this script
# Set-ExecutionPolicy -ExecutionPolicy Unrestricted -Scope Process


## Copy and link from OneDrive
# PowerShell history

$local = $("${env:AppData}\Microsoft\Windows\PowerShell\PSReadline\ConsoleHost_history.txt")
$cloud = $("${env:OneDrive}\Documents\ConsoleHost_history.txt")

echo "test local history..."
if (Test-Path -Path $local) {
       echo "$local found"
       echo "test cloud history..."
       if (Test-Path -Path $cloud) {
	      echo "$cloud found"
	      echo "proceeding with append, delete, and link..."
	      
	      echo "appending local copy with cloud copy..."
	      Add-Content -Path $cloud -Value $local

	      echo "removing local copy"
	      $dir=[io.path]::GetDirectoryName($local)
	      $fname=[io.path]::GetFileNameWithoutExtension($local)
	      $ext=[io.path]::GetExtension($local)
	      mv -v $local $dir\${fname}_$(get-date -f yyyy-MM-dd-hhmm)$ext

	      echo "creating symbolic link"
	      cmd /c mklink $local $cloud
	      
	}    
	else {
	      echo "$cloud not found"
  	      echo "no history to link to`nexiting"
        }	   
}	   
else {  
echo "$local not found"
}
