#!/bin/sh
TGTDIR=$HOME

echo "--------------------------------------"
echo "-- Start Linking Files Outside Repo --"
echo "--------------------------------------"

## Link from outside of repo
my_link=.git-credentials
if [ -f $TGTDIR/${my_link} ] || [ -L $TGTDIR/${my_link} ]; then
    echo "Backing up ${my_link}..."
    mv -v $TGTDIR/${my_link} $TGTDIR/${my_link}_$(date +'%Y-%m-%d-t%H%M')
fi
ln -sv /mnt/c/Users/jonli/OneDrive/Documents/home/cygwin/${my_link} $TGTDIR/${my_link}

## Copy and link from OneDrive
# Bash history
if [ -e $TGTDIR/.bash_history ]; then
    echo "backing up .bash_history..."
    if [ ! -L $TGTDIR/.bash_history ]; then # only copy if not already a link
	cat $TGTDIR/.bash_history >> /mnt/c/Users/jonli/OneDrive/Documents/home/cygwin/.bash_history
    else
	echo "$TGTDIR/.bash_history is already a link"
    fi
    mv -v $TGTDIR/.bash_history $TGTDIR/.bash_history_$(date +'%Y-%m-%d-t%H%M')
fi
ln -sv /mnt/c/Users/jonli/OneDrive/Documents/home/cygwin/.bash_history $TGTDIR/.bash_history

# Copy .ssh
if [ -d $TGTDIR/.ssh ]; then
    echo "Backing up .ssh..."
    mv -v $TGTDIR/.ssh/ ~/.ssh_$(date +'%Y-%m-%d-t%H%M')
fi
git clone https://jonlighthall@bitbucket.org/jonlighthall/.ssh.git ~/.ssh
chmod -v 600 $TGTDIR/.ssh/config 
chmod -v 600 $TGTDIR/.ssh/id_rsa

if [ ! -e $TGTDIR/winhome ]; then
    ln -sv /mnt/c/Users/jonli $TGTDIR/winhome
else
    echo "winhome is already a link"
fi
if [ ! -e $TGTDIR/onedrive ]; then
    ln -sv /mnt/c/Users/jonli/OneDrive/ $TGTDIR/onedrive
    else
    echo "onedrive is already a link"
fi
echo "--------------------------------------"
echo "--------- Done Making Links ----------"
echo "--------------------------------------"
