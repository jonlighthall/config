# .bash_profile
#echo Using GitHub version .bash_profile

# Get the general aliases and functions
#echo Loading .bashrc...
if [ -f ~/config/linux/.bashrc ]; then
	. ~/config/linux/.bashrc
fi

# User-specific environment and startup programs

echo "Welcome to" $HOSTNAME
if [ -f ~/.bash_history ]; then
    echo "LOGIN  $(date +'%a %b %d %Y %H:%M %Z')" >> ~/.bash_history
fi

# Get the site-specific aliases and functions
if [ -f ~/.bash_aliases ]; then
#echo Loading .bash_aliases...
    source ~/.bash_aliases
fi
