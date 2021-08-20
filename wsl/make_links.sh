#!/bin/sh
SRCDIR=$HOME/config/wsl
TGTDIR=$HOME

## Link from config repo

for my_link in .bash_profile .gitconfig .rootrc #.emacs.d 
do
    echo
    if [ -L $TGTDIR/${my_link} ] ; then
	echo "$TGTDIR/${my_link} is already a link"
	echo -n " The link is... "
	if [ -e $TGTDIR/${my_link} ] ; then
	    echo "valid"
	else
	    echo "broken"
	fi
    elif [ -e $TGTDIR/${my_link} ] ; then
	echo "$TGTDIR/${my_link} exists"
	    echo -n " It is... "
	if [ -f $TGTDIR/${my_link} ]; then
	    echo "a regular file"
	else
	    echo -n "not a regular file, but... "
	    if [ -d $TGTDIR/${my_link} ]; then
		echo "a directory"
	    else
		echo "not a directory"
	    fi
	fi	
    else
	echo "$TGTDIR/${my_link} does not exist"
    fi

    # first, backup existing copy
    if [ -f $TGTDIR/${my_link} ] || [ -L $TGTDIR/${my_link} ] || [ -d $TGTDIR/${my_link} ]; then
	echo " Backing up ${my_link}..."
	mv -v $TGTDIR/${my_link} $TGTDIR/${my_link}_$(date +'%Y-%m-%d-t%H%M')
    fi

    # then link
    echo "Making ${my_link} link..."
    ln -vs ${SRCDIR}/${my_link} $TGTDIR/${my_link}
done

return


if [ -d ~/.emacs.d ]; then
    echo Backing up .emacs.d
    mv -v ~/.emacs.d/ ~/.emacs.d_old/
fi
ln -vs ${SRCDIR}/.emacs.d/ ~/.emacs.d

