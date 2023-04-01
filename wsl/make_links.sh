#!/bin/bash
TAB="   "

# set source and target directories
SRCDIR=$HOME/config/wsl
TGTDIR=$HOME

echo "--------------------------------------"
echo "------ Start Linking Repo Files-------"
echo "--------------------------------------"

# list of files to be linked
for my_link in .bash_profile .emacs.d .gitconfig .rootrc .inputrc .bash_logout
do
    echo -n "source file $SRCDIR/${my_link}... "
    if [ -e $SRCDIR/${my_link} ]; then
	echo "exists "
	echo -n "${TAB}link $TGTDIR/${my_link}... "
	if [ -e $TGTDIR/${my_link} ] ; then
	    echo -n "exists and "
	    if [[ $SRCDIR/${my_link} -ef $TGTDIR/${my_link} ]]; then
		echo "already points to ${my_link}"
		echo "${TAB}skipping..."
		continue
	    else
		echo -n "will be backed up..."
		mv -v $TGTDIR/${my_link} $TGTDIR/${my_link}_$(date +'%Y-%m-%d-t%H%M')
	    fi
	else
	    echo "does not exist"
	fi
	echo -n "${TAB}making link... "
	ln -sv $SRCDIR/${my_link} $TGTDIR/$my_link
    else
	echo "does not exist"
    fi
done
echo "--------------------------------------"
echo "--------- Done Making Links ----------"
echo "--------------------------------------"
