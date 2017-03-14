# dotfiles for Cygwin on Windows
All of the files in this directory are copied to the home directory in Cygwin.

## Bash
Use the following commands to create symbolic links from the home directory to the config directory
```bash
ln -s ~/config/cygwin/.bashrc .bashrc
ln -s ~/config/cygwin/.bash_profile .bash_profile
```

## Git
Use the following command to set properly set Emacs as the Git editor.
```bash
git config --global core.editor '/cygdrive/c/Program\ Files\ \(x86\)/emacs-25.1-i686-w64-mingw32/bin/emacs.exe `cygpath --windows ${1}` && set'
```

## Emacs
Make a directory junction from the parent directory `C:\Users\jonli\AppData\Roaming\` using the following command

```bash
mklink /J .emacs.d C:\Users\jonli\OneDrive\Documents\.config\cygwin\.emacs.d
```
