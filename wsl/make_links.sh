#!/bin/sh

# Clone and link config repo
git clone https://github.com/jonlighthall/config.git

if [ -f ~/.bash_aliases ]; then
    echo Backing up .bash_profile
    mv ~/.bash_profile ~/.bash_profile_old 
fi

ln -s ~/config/wls/.bash_profile ~/.bash_profile

if [ -d ~/.emacs.d ]; then
    echo Backing up .emacs.d
    mv ~/.emacs.d/ ~/./emacs.d_old/
fi
ln -s ~/config/wsl/.emacs.d/ .emacs.d

if [ -f ~/.gitconfig ]; then
    echo Backing up .gitconfig
    mv ~/.gitconfig ~/.gitconfig_old
fi
ln -s ~/config/wsl/.gitconfig ~/.gitconfig

#cat .bash_history

# Copy .ssh
if [ -d ~/.ssh ]; then
    echo Backing up .ssh
    mv -rv ~/.ssh/ ~/.ssh_old
fi
#cp -rv /mnt/c/Users/jonli/OneDrive/Documents/.cygwin_home/.ssh/ ./
ln -s /mnt/c/Users/jonli/OneDrive/Documents/.cygwin_home/.ssh/ ~/.ssh
#chmod 600 ~/.ssh/config 
#chmod 600 ~/.ssh/id_rsa

ln -s /mnt/c/Users/jonli/OneDrive/ ~/onedrive
