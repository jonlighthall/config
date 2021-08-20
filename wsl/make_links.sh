#!/bin/sh
SRCDIR=~/config/wsl

## Link from config repo
my_link=~/.bash_profile

if [ -L ${my_link} ] ; then
    echo "is link"
    if [ -e ${my_link} ] ; then
	echo "Good link"
    else
	echo "Broken link"
    fi
elif [ -e ${my_link} ] ; then
    echo "Not a link"
else
    echo "Missing"
    fi

if [ -f ${my_link} ]; then
    echo ${my_link} found
else
    echo ${my_link} not found
fi


if [ -e ${my_link} ]; then
    echo ${my_link} exists
else
    echo ${my_link} does not exist
fi

if [ -f ${my_link} ]; then
    echo Backing up ${my_link}...
    mv -v ${my_link} ${my_link}_$(date +'%Y-%m-%d-t%H%M')
fi

if [ -f ${my_link} ]; then
    echo Backing up .bash_profile...
    mv -v ${my_link} ${my_link}_old 
fi
ln -vs ${SRCDIR}/.bash_profile ~/.bash_profile

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
