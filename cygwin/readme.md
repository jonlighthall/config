# dotfiles for Cygwin on Windows
One way to utilize the files in this directory is to copy them  the Cygwin home directory. However, changes in the files will not be tracked by Git. Linux-like symbolic links can be made using the `ln -s` command. This approach will allow the files to be read by Cygwin, but they cannot be opened in programs like emacs. As of this writing, links created from within OneDrive using the `mklink` command will cause OneDrive to synchronize indefinitely.

## Bash
Use the following commands to create a symbolic link from the home directory to the `config\cygwin` directory
```bash
ln -s ~/config/cygwin/.bash_profile .bash_profile
```

## Emacs
Make a directory junction from the parent directory `C:\Users\jonli\AppData\Roaming\` using the following command. Open a command prompt as Administrator. Since the location of the link is outisde of OneDrive, this will not cause a OneDrive synchronization problem.

```bash
rm -r .emacs.d
mklink /J .emacs.d C:\Users\jonli\OneDrive\Documents\.cygwin_home\config\cygwin\.emacs.d
```
Symbolic directory junctions cannot be made using the `ln` command. Creating a link using the `mklink` command will cause OneDrive errors. In the meantime, use `rsync` from the Cygwin home `C:\Users\jonli\OneDrive\Documents\.cygwin_home\` using the following command. 

```bash
rm -r .emacs.d
rsync -vr config/cygwin/.emacs.d/ ./.emacs.d/	
```
## Git
Use the following command to create a symbolic link from the home directory to the `config\cygwin` directory.
```bash
ln -s ~/config/cygwin/.gitconfig .gitconfig
```
Use the following command to properly set Emacs as the Git editor under Cygwin. Without this setting, Emacs cannot be used as the default Git editor: the conflict between the DOS file path and the cygpath will prevent commit messages to be saved.
```bash
git config --global core.editor '/cygdrive/c/Program\ Files\ \(x86\)/emacs-25.1-i686-w64-mingw32/bin/emacs.exe `cygpath --windows ${1}` && set'
```

## ROOT
Use the following command to create symlink from the user directory to the location of the file. This command should be used from both
* the Windows home directory (`C:\Users\jonli`) and 
* the Cygwin home dicectory (`C:\Users\jonli\OneDrive\Documents\.cygwin_home`).

Open a command prompt as Administrator.
```bash
rm .rootrc
mklink .rootrc C:\Users\jonli\OneDrive\Documents\.cygwin_home\config\cygwin\.rootrc
```
