# ~/config/linux/.bashrc
# Interactive shell settings for Linux

# Source system settings
if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi

# Source common user settings
if [ -f ~/config/.bashrc_common ]; then
    . ~/config/.bashrc_common 
fi

# Source common user settings
if [ -f ~/config/linux/.bashrc_unix ]; then
    . ~/config/linux/.bashrc_unix
fi
