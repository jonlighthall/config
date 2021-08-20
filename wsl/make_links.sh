#!/bin/sh
SRCDIR=$HOME/config/wsl
TGTDIR=$HOME

## Link from config repo

for my_link in .bash_profile #.emacs.d .gitconfig .rootrc
do

if [ -L $TGTDIR/${my_link} ] ; then
    echo "is link"
    if [ -e $TGTDIR/${my_link} ] ; then
	echo "Good link"
    else
	echo "Broken link"
    fi
elif [ -e $TGTDIR/${my_link} ] ; then
    echo "Not a link"
else
    echo "Missing"
    fi

if [ -f $TGTDIR/${my_link} ]; then
    echo "$TGTDIR/${my_link} is a regular file"
else
    echo "$TGTDIR/${my_link} is not a regular file"
fi


if [ -e $TGTDIR/${my_link} ]; then
    echo "$TGTDIR/${my_link} exists"
else
    echo "$TGTDIR/${my_link} does not exist"
fi

if [ -d $TGTDIR/${my_link} ]; then
    echo "$TGTDIR/${my_link} is a directory"
else
    echo "$TGTDIR/${my_link} is not a directory"
fi

# first, backup existing copy
if [ -f $TGTDIR/${my_link} ] || [ -L $TGTDIR/${my_link} ] || [ -d $TGTDIR/${my_link} ]; then
    echo Backing up ${my_link}...
    mv -v $TGTDIR/${my_link} $TGTDIR/${my_link}_$(date +'%Y-%m-%d-t%H%M')
fi
# then link
ln -vs ${SRCDIR}/${my_link} $TGTDIR/${my_link}
    
done

return

my_link=~/.bash_profile


if [ -d ~/.emacs.d ]; then
    echo Backing up .emacs.d
    mv -v ~/.emacs.d/ ~/.emacs.d_old/
fi
ln -vs ${SRCDIR}/.emacs.d/ ~/.emacs.d

if [ -f ~/.gitconfig ]; then
    echo Backing up .gitconfig
    mv -v ~/.gitconfig ~/.gitconfig_old
fi
ln -vs ${SRCDIR}/.gitconfig ~/.gitconfig

if [ -f ~/.rootrc ]; then
    echo Backing up .rootrc
    mv -v ~/.rootrc ~/.rootrc_old
fi
ln -vs ${SRCDIR}/.rootrc ~/.rootrc
