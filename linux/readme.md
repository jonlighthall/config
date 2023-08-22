copy these files to the appropriate folders

## Bash
Use the following commands to create symbolic links from the `/home/lighthall` directory to the `/home/lighthall/config` directory
```bash
ln -s ${HOME}/config/linux/.bashrc ${HOME}/.bashrc
ln -s ${HOME}/config/linux/.bash_profile ${HOME}/.bash_profile
ln -s ${HOME}/config/linux/.bash_aliases_<local> ${HOME}/.bash_aliases
```
The file `.bashrc` is loaded directly by `.bash_profile` but may still need to be linked to the home directory for system use. 
The `.bash_aliases` files are used for site-specific settings.

Use the following commands to create symbolic links in the `/home/lighthall/bin` directory
```bash
ln -s /home/lighthall/Downloads/emacs-24.5/src/emacs /home/lighthall/bin/emacs
ln -s /home/Downloads/git-2.8.4/bin/git /home/lighthall/bin/git
```

## Emacs
.emacs is usually located in the home directory `${HOME}/`
```bash
ln -s ${HOME}/config/linux/.emacs ${HOME}/.emacs
```

`vc-git.el` is located in `${HOME}/Downloads/emacs-<verson>/lisp/vc/`

## ROOT
Use the following command to create symlink from the home directory to the location of the file. 
```bash
rm .rootrc
ln -s ${HOME}/config/linux/.rootrc .rootrc
```
