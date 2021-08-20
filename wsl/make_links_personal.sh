#!/bin/sh
TGTDIR=$HOME

## Link from outside of repo
my_link=.git-credentials
if [ -e $TGTDIR/${my_link} ]
   echo " Backing up ${my_link}..."
   mv -v $TGTDIR/${my_link} $TGTDIR/${my_link}_$(date +'%Y-%m-%d-t%H%M')
fi
ln -s /mnt/c/Users/jonli/OneDrive/Documents/.cygwin_home/${my_link} $TGTDIR/${my_link}

## Copy and link from OneDrive
# Bash history
if [ -f $TGTDIR/.bash_history ]; then
cat $TGTDIR/.bash_history >> /mnt/c/Users/jonli/OneDrive/Documents/.cygwin_home/.bash_history
rm $TGTDIR/.bash_history
fi
ln -s /mnt/c/Users/jonli/OneDrive/Documents/.cygwin_home/.bash_history ~/.bash_history

# Copy .ssh
if [ -d $TGTDIR/.ssh ]; then
    echo Backing up .ssh
    mv -v $TGTDIR/.ssh/ ~/.ssh_old
fi
git clone https://jonlighthall@bitbucket.org/jonlighthall/.ssh.git ~/.ssh
chmod 600 $TGTDIR/.ssh/config 
chmod 600 $TGTDIR/.ssh/id_rsa

ln -sv /mnt/c/Users/jonli/OneDrive/ $TGTDIR/onedrive
