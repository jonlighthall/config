# .bash_profile
#echo Using GitHub version .bash_profile

# Get the general aliases and functions
#echo Loading .bashrc...
if [ -f ~/.bashrc ]; then
	. ~/.bashrc
fi

# User-specific environment and startup programs

echo "Welcome to" $HOSTNAME

# Get the site-specific aliases and functions
if [ -f ~/.bash_aliases ]; then
#echo Loading .bash_aliases...
    source ~/.bash_aliases
fi
