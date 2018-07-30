# dotfiles for Windows Subsystem for Linux
One way to utilize the files in this directory is to copy them to the WSL home directory. However, changes in the files will not be tracked by Git. Linux-like symbolic links can be made using the `ln -s` command.

## Bash
Use the following commands to create a symbolic link from the home directory to the `config\cygwin` directory
```bash
ln -s ~/config/cygwin/.bash_profile ~/.bash_profile
```

## Emacs

```bash
rm -r .emacs.d
rsync -vr config/cygwin/.emacs.d/ ./.emacs.d/	
```
## Git
Use the following command to create a symbolic link from the home directory to the `config\cygwin` directory.
```bash
ln -s ~/config/cygwin/.gitconfig .gitconfig
```

## ROOT
Use the following command to create symlink from the user directory to the location of the file. This command should be used from 

* the Cygwin home dicectory (`C:\Users\jonli\OneDrive\Documents\.cygwin_home`).

Open a command prompt as Administrator.
```bash
rm .rootrc
mklink .rootrc C:\Users\jonli\OneDrive\Documents\.cygwin_home\config\cygwin\.rootrc
```

```bash
rm .rootrc
rsync -v config/cygwin/.rootrc ./
```
