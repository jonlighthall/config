# dotfiles for Windows Subsystem for Linux
One way to utilize the files in this directory is to copy them to the WSL home directory.
However, changes in the files will not be tracked by Git.
Linux-like symbolic links can be made using the `ln -s` command.

## Contents
[`install_packages`](install_packages.sh)
[`make_links`](make_links.sh)
[`make_links_etc`](make_links_etc.sh)
[`make_links_external`](make_links_external.sh)
[`configure`](configure.sh)

## Installation
````
cd config/wsl
./install_packages.sh
./make_links.sh
````

or run the script [`configure`] (configure.sh).

## Scripts
The following commands are executed by the scripts.

### Bash

Use the following command to create a symbolic link from the home directory to the `config\wsl`
directory.

```bash
ln -s ${HOME}/config/wsl/.bash_profile ${HOME}/.bash_profile
```

### Emacs

```bash
rm -r .emacs.d
ln -s ${HOME}/config/wsl/.emacs.d/ ${HOME}/.emacs.d

```
### Git
On a fresh install of WSL, Git will first need to be installed.
```bash
sudo apt -y install git
```

If the preceeding command fails, the following should allow the command to run
```bash
sudo apt update && sudo apt -y upgrade
```

Github will require SSH keys.
The SSH keys are stored in the following private repository.
The repository password will be needed.
```bash
git clone https://jonlighthall@bitbucket.org/jonlighthall/.ssh.git ${HOME}/.ssh
chmod 600 ${HOME}/.ssh/id_rsa
git clone git@github.com:jonlighthall/config.git
```

Use the following command to create a symbolic link from the home directory to the
`config\cygwin` directory.

```bash
ln -s ${HOME}/config/wsl/.gitconfig ${HOME}/.gitconfig
ln -s /mnt/c/Users/jonli/OneDrive/Documents/.cygwin_home/.git-credentials ${HOME}/.git-credentials
cp /mnt/c/Users/jonli/OneDrive/Documents/.cygwin_home/.git-credentials ${HOME}/
```

### SSH

WSL does not allow chmod changes to Windows files. SSH files require certain permissions. A copy
of the ssh files is required.

```bash
rsync -vr /mnt/c/Users/jonli/OneDrive/Documents/.cygwin_home/.ssh ${HOME}/.ssh
chmod 600 ${HOME}/.ssh/config 
chmod 600 ${HOME}/.ssh/id_rsa
```

### ROOT

Use the following command to create symlink from the user directory to the location of the
file. This command should be used from

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

### X11
Prerequisites 
An X windows server is needed use the following link to download Xming.

https://sourceforge.net/projects/xming/files/latest/download
