# .bash_profile
#echo Using GitHub version .bash_profile

# Get the general aliases and functions
#echo Loading .bashrc...
if [ -f ~/config/linux/.bashrc ]; then
	. ~/config/linux/.bashrc
fi

# User-specific environment and startup programs

echo "Welcome to" $(hostname -f)
if [ -f ~/.bash_history ]; then
    echo "#$(date +'%s') LOGIN  $(date +'%a %b %d %Y %R:%S %Z') from $(hostname -s)" >> ~/.bash_history
fi
echo
NP=3
echo "Top $NP processes on $(hostname -s):"
ps aux --sort=-pcpu | head -n $((NP+1)) | sed 's_1111499164_jlight_'
echo

# Get the site-specific aliases and functions
if [ -f ~/.bash_aliases ]; then
#echo Loading .bash_aliases...
    source ~/.bash_aliases
fi
