#!/bin/bash

# Pagers
# -------
# add syntax highlighting to less, etc
# git clone https://github.com/Lindydancer/e2ansi.git
# git clone https://github.com/Lindydancer/face-explorer.git

start_dir=$PWD
echo "starting directory = ${start_dir}"

# find downloads directory
dir_download=${HOME}/downloads
echo -n "install directory ${dir_download}..."
if [ -d ${dir_download} ]; then
    echo "found"
else
    echo "not found"
    dir_Down=${HOME}/Downloads
    echo -n "directory ${dir_Down}..."
    if [ -d ${dir_Down} ]; then
        echo "found"
        echo "linking..."
        ln -s ${dir_Down} ${dir_download}
    else
        echo "not found"
        echo "making directory..."
        mkdir -pv ${dir_download}
    fi
fi

# find installation directory
dir_install=${dir_download}/git
echo -n "install directory ${dir_install}..."
if [ -d ${dir_install} ]; then
    echo "found"
else
    echo "not found"
    echo "making directory..."
    mkdir -pv ${dir_install}
fi

# clone repositories
cd ${dir_install}
for repo in e2ansi face-explorer; do

    if [ -d $repo ]; then
        echo "$repo already exists"
    else
        echo "cloning $repo..."
        git clone https://github.com/Lindydancer/${repo}.git
        echo "done"
    fi
done

echo "done updating repositories"
echo "returning to starting directory..."
cd ${starting_dir}
