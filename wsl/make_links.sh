#!/bin/sh
SRCDIR=$HOME/config/wsl
TGTDIR=$HOME

echo "--------------------------------------"
echo "--------- Start Making Links ---------"
echo "--------------------------------------"

## Links from config repo
for my_link in .bash_profile .emacs.d .gitconfig .rootrc
do
    echo
    echo -n "$TGTDIR/${my_link} "
    if [ -L $TGTDIR/${my_link} ] ; then
	echo "is already a link"
	echo -n " The link is... "
	if [ -e $TGTDIR/${my_link} ] ; then
	    echo "valid"
	else
	    echo "broken"
	fi
    elif [ -e $TGTDIR/${my_link} ] ; then
	echo "exists"
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
	echo "does not exist"
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
echo "--------------------------------------"
echo "--------- Done Making Links ----------"
echo "--------------------------------------"
