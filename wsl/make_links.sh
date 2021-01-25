#!/bin/sh
SRCDIR=~/config/wsl

## Link from config repo
if [ -f ~/.bash_profile ]; then
    echo Backing up .bash_profile
    mv ~/.bash_profile ~/.bash_profile_old 
fi
ln -s ${SRCDIR}/.bash_profile ~/.bash_profile

if [ -d ~/.emacs.d ]; then
    echo Backing up .emacs.d
    mv ~/.emacs.d/ ~/.emacs.d_old/
fi
ln -s ${SRCDIR}/.emacs.d/ ~/.emacs.d

if [ -f ~/.gitconfig ]; then
    echo Backing up .gitconfig
    mv ~/.gitconfig ~/.gitconfig_old
fi
ln -s ${SRCDIR}/.gitconfig ~/.gitconfig

if [ -f ~/.rootrc ]; then
    echo Backing up .rootrc
    mv ~/.rootrc ~/.rootrc_old
fi
ln -s ${SRCDIR}/.rootrc ~/.rootrc
