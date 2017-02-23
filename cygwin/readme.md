#dotfiles for Cygwin on Windows
All of the files in this directory are copied to the home directory in Cygwin.

##Bash

##Git
```bash
git config --global core.editor '/cygdrive/c/Program\ Files\ \(x86\)/emacs-25.1-i686-w64-mingw32/bin/emacs.exe `cygpath --windows ${1}` && set'
```

##Emacs
Make a directory junction from the parent directory `C:\Users\jonli\AppData\Roaming\` using the following command
`mklink /J .emacs.d C:\Users\jonli\OneDrive\Documents\.config\cygwin\.emacs.d`
```bash
mklink /J .emacs.d C:\Users\jonli\OneDrive\Documents\.config\cygwin\.emacs.d
```