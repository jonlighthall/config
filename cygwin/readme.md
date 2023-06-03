# dotfiles for Cygwin on Windows
One way to utilize the files in this directory is to copy them  the Cygwin home directory. However, changes in the files will not be tracked by Git.
Linux-like symbolic links can be made using the `ln -s` command.
This approach will allow the files to be read by Cygwin, but they cannot be opened in programs like emacs.
As of this writing, links created from within OneDrive using the `mklink` command will cause OneDrive to synchronize indefinitely.

## Bash
Use the following commands to create a symbolic link from the home directory to the `config\cygwin` directory
```bash
ln -s ~/config/linux/.bashrc .bashrc
ln -s ~/config/cygwin/.bash_profile ~/.bash_profile
ln -s ~/config/linux/.bash_aliases_<local> .bash_aliases
```
The file `.bashrc` is loaded directly by `.bash_profile` but may still need to be linked to the home directory for system use. 
The `.bash_aliases` files are used for site-specific settings.

## Emacs
Make a directory junction from the parent directory `C:\Users\jonli\AppData\Roaming\` using the following command. 
Open a command prompt as Administrator. Since the location of the link is outside of OneDrive, this will not cause a OneDrive synchronization problem.

```bash
rmdir /s .emacs.d
mklink /J .emacs.d C:\Users\jonli\OneDrive\Documents\.cygwin_home\config\cygwin\.emacs.d
```
Symbolic directory junctions cannot be made using the `ln` command.
Creating a link using the `mklink` command will cause OneDrive errors.
In the meantime, use `rsync` from the Cygwin home `C:\Users\jonli\OneDrive\Documents\.cygwin_home\` using the following command. 

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
Use the following command to create symlink from the user directory to the location of the file.
This command should be used from both
* the Windows home directory (`C:\Users\jonli`) and 
* the Cygwin home directory (`C:\Users\jonli\OneDrive\Documents\.cygwin_home`).

Open a command prompt as Administrator.
```bash
rm .rootrc
mklink .rootrc C:\Users\jonli\OneDrive\Documents\.cygwin_home\config\cygwin\.rootrc
```

```bash
rm .rootrc
rsync -v config/cygwin/.rootrc ./
```

The following is translated from
https://github.com/jonlighthall/programs_and_options.git

Cygwin
======

Installation
------------

Cygwin is available in 32-bit and 64-bit architectures. One of the major
utilities of installing Cygwin is the added functionality it provides
for Emacs. However, since Emacs is only officially supported in 32-bit
format, it is necessary to install the 32-bit version of Cygwin take
take advantage of these functions.

Setup instruction for Cygwin can be found here

<http://x.cygwin.com/docs/ug/setup-cygwin-x-installing.html>

To setup and run Cygwin

1.  Go to <http://www.cygwin.com/setup.exe> to download the latest
    `setup.exe` file. The file should begin downloading automatically.

2.  Run this file and click on the popup window

3.  Select [Install from Internet]{.sans-serif} and click

4.  Install for [All Users]{.sans-serif} in `C:\cygwin`

5.  The local package directory can be the standard download directory:
    [C:/Users/Jon
    Lighthall/Downloads](run:C:/Users/Jon Lighthall/Downloads)

6.  Select [Direct Connection]{.sans-serif}

7.  Select a mirror site such as ~~`ftp://mirror.mcs.anl.gov`~~
    `http://mirror.weathercity.com/` (Vancouver)

This will then load a menu of available packages.

Packages
--------

The packages necessary for a base installation are selected by default.
Some useful additions are:

   **Category**  **Program**               **Suggested Package(s)**
  -------------- ------------------------- -----------------------------
                 fortran                   `gcc-fortran`
                 C++                       `gcc-g++`
                 SVN                       `subversion`
                 Xmgrace                   `grace`
                 SSH                       `openssh`
                 Cygwin Bash Prompt Here   `chere`
                 GNU Apell                 `aspell`
                 Dictionary                `aspell-en`
                 Cygwin/X                  `xinit, X-start-menu-icons`

After selecting these packages, the Cygwin installer will automatically
select addition packages to resolve any dependencies.

The packages given above can be installed by executing the following
command as a batch file.

>      setup-x86.exe ^
>     --packages ^
>     subversion,^
>     grace,^
>     openssh,^
>     chere,^
>     aspell,^
>     aspell-en,^
>     xinit,^
>     gcc-g++,^
>     gcc-fortran,^

Home Directory
--------------

Cygwin (and programs started from within Cygwin) start in the
["home"](run:C:/cygwin/home/) directory. To change the name of the home
directory from the inherited Windows user name, the following changes
should be made.

### Versions prior to 1.7.34

In Cygwin versions before 1.7.34, the home directory was set in
`/etc/passwd`. The command `mkpasswd > /etc/passwd` can be used to
generate the text file if missing. Then search for the user name and
change the location of the home directory specified in the sixth, or
second-to-last, argument of the entry

### Version 1.7.34 and beyond

The `/etc/passwd` file is no longer generated by default. Instead, the
home directory can be specified in `/etc/nsswitch.conf`. Add the line

>      db_home:  /home/lighthall

the default value is `/home/%U` which uses the Windows username.
However, this line will only change the user directory and not the user
name. Additionally, add the line

> `set HOME=C:\cygwin\home\lighthall`

to the `C:/cygwin/Cygwin.bat` file (optional).

To move the home directory out of the Cygwin directory (e.g., to move
the home directory into the My Documents directory) a Cygwin link must
be made. First, make the destination directory and copy the contents of
original directory into the new directory. Then delete the original
directory and make a link.

>     ln -s /cygdrive/c/Users/Jon\ Lighthall/Documents/.cygwin_home /home/lighthall

Username
--------

In order to change the user name, a `/etc/passwd` file must be manually
generated using the following command.

>     mkpasswd -l > /etc/passwd

Modify the `/etc/passwd` file as follows. For example, in the following
line the first instance of `Jon Lighthall` is the user name and the last
instance is the home directory.

>     Jon Lighthall:unused:1000:513:U-JCL-N5010-Win7\Jon Lighthall,S-1-5-21-238998706-
>     2098893708-732278632-1000:/home/Jon Lighthall:/bin/bash

Launching
---------

### mintty

For some reason, ROOT won't launch from a `mintty` window (from within
Cygwin), so it is advantageous to start Cygwin from `cmd` with the
following batch file.

Right-click on the Cygwin shortcut (under the Start menu) and add
`-p 0,0` to the target and (optionally) set the shortcut key to
`Ctrl + Alt + t` to match a Linux (Ubuntu) environment.

### Cygwin.bat

>     @echo off
>
>     C:
>     chdir C:\cygwin\bin
>
>     bash --login -i

The target of the shortcut can be entered as follows to include the
Cygwin icon.

> `C:\cygwin\Cygwin.bat -i C:\cygwin\Cygwin-Terminal.ico`

### bash

Similarly, Cygwin can be invoked from the command prompt using the
command `bash -li` create a shortcut for bash and add the cygwin icon
and change the terminal colors as needed.

Windows integration
-------------------

Add cygwin to path to use Linux commands from the Windows command
prompt. Under System Properties, Advanced, Environment Variables...,
select Path in the System variables window and click Edit.... Click new
and enter `C:\cygwin\bin`.

File system navigation from within Cygwin ~~cannot go above the
`cygwin/` directory without the use of links (see
§ [\[root\]](#root){reference-type="ref" reference="root"})~~ is
accomplished with the command `cd /cygdrive/`. To create mintty shell
integrations for open here, enter the following command after installing
chere.

>     chere -i -t mintty

creates [Bash Prompt Here]{.sans-serif} entry in context menu and an
entry in the installed programs list. Be sure that the `.bashrc` file
does not have a `cd ~` entry. Then you may need to add
`C:\Users\Jon\OneDrive\Documents\.cygwin_home` to the start in box under
the program short cut on the menu
