# .bash_profile

# Get the general aliases and functions
if [ -f ~/.bashrc ]; then
	. ~/.bashrc
fi

# User-specific environment and startup programs

PATH=$PATH:$HOME/bin

export PATH
echo "Welcome to" $HOSTNAME

# Get the site-specific aliases and functions
if [ -f ~/.bash_aliases ]; then
    source ~/.bash_aliases
fi
