#!/bin/sh

## Link from outside of repo
ln -s /mnt/c/Users/jonli/OneDrive/Documents/.cygwin_home/.git-credentials ~/.git-credentials

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
