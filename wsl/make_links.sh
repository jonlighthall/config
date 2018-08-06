#!/bin/sh

## Link from config repo
if [ -f ~/.bash_aliases ]; then
    echo Backing up .bash_profile
    mv ~/.bash_profile ~/.bash_profile_old 
fi

ln -s ~/config/wsl/.bash_profile ~/.bash_profile


if [ -d ~/.emacs.d ]; then
    echo Backing up .emacs.d
    mv ~/.emacs.d/ ~/.emacs.d_old/
fi
ln -s ~/config/wsl/.emacs.d/ .emacs.d

if [ -f ~/.gitconfig ]; then
    echo Backing up .gitconfig
    mv ~/.gitconfig ~/.gitconfig_old
fi
ln -s ~/config/wsl/.gitconfig ~/.gitconfig
ln -s /mnt/c/Users/jonli/OneDrive/Documents/.cygwin_home/.git-credentials ~/.git-credentials

ln -s ~/config/wsl/.rootrc ~/.rootrc

## Copy and link from OneDrive
# Bash history
if [ -f ~/.bash_history ]; then
cat ~/.bash_history >> /mnt/c/Users/jonli/OneDrive/Documents/.cygwin_home/.bash_history
rm ~/.bash_history
fi
ln -s /mnt/c/Users/jonli/OneDrive/Documents/.cygwin_home/.bash_history ~/.bash_history

# Copy .ssh
if [ -d ~/.ssh ]; then
    echo Backing up .ssh
    mv -rv ~/.ssh/ ~/.ssh_old
fi
git clone https://jonlighthall@bitbucket.org/jonlighthall/.ssh.git ~/.ssh
chmod 600 ~/.ssh/config 
chmod 600 ~/.ssh/id_rsa

ln -s /mnt/c/Users/jonli/OneDrive/ ~/onedrive
