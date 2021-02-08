# ~/config/wsl/.bashrc
# Interactive shell settings for Linux Subsystem for Windows

# Source system settings
if [ -f ~/.bashrc ]; then
    . ~/.bashrc
fi

# Source common user settings
if [ -f ~/config/.bashrc_common ]; then
    . ~/config/.bashrc_common 
fi

# Source common user settings
if [ -f ~/config/linux/.bashrc_unix ]; then
    . ~/config/linux/.bashrc_unix
fi

# X Window
export DISPLAY=localhost:0.0 

# ROOT
if [ -f root_v5.34.36/bin/thisroot.sh ]; then
    echo "sourcing root..."
    . root_v5.34.36/bin/thisroot.sh
    which root
fi
