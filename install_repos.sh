# Pagers
# -------
# add syntax highlighting to less, etc
# git clone https://github.com/Lindydancer/e2ansi.git
# git clone https://github.com/Lindydancer/face-explorer.git

dir_install=${HOME}/downloads

echo -n "install directory ${dir_install}..."
if [ -d ${dir_install} ]; then
    echo "found"
else
    echo "not found"
fi
