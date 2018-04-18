# .bash_profile
#echo Using GitHub version .bash_profile

# Get the general aliases and functions
#echo Loading .bashrc...
if [ -f ~/.bashrc ]; then
	. ~/.bashrc
fi

# User-specific environment and startup programs

PATH=$HOME/bin:$PATH
export PATH

LD_LIBRARY_PATH=$HOME/Downloads/openssl/lib:$HOME/Downloads/curl/lib:$LD_LIBRARY_PATH
export LD_LIBRARY_PATH

PKG_CONFIG_PATH="$HOME/Downlads/curl/lib/pkgconfig:$PKG_CONFIG_PATH"
export PKG_CONFIG_PATH

MANPATH=$HOME/Downloads/curl/share/man:$MANPATH
export MANPATH

echo "Welcome to" $HOSTNAME

# Get the site-specific aliases and functions
if [ -f ~/.bash_aliases ]; then
#echo Loading .bash_aliases...
    source ~/.bash_aliases
fi
