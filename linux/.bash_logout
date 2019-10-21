# .bash_logout
#echo Using GitHub version .bash_logout

# User-specific environment and logout programs

echo "goodbye"
if [ -f ~/temp.txt ]; then
    echo "LOGOUT $(date)" >> ~/temp.txt
fi
