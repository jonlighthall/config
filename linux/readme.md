copy these files to the appropriate folders

##Bash
Use the following commands to create symbolic links from the home directory to the config directory
```bash
ln -s ~/config/linux/.bashrc .bashrc
ln -s ~/config/linux/.bash_profile .bash_profile
ln -s ~/config/linux/.bash_aliases_<local> .bash_aliases
```

Use the following commands to create symbolic links in the `/home/lighthall/bin` directory
```bash
ln -s /home/lighthall/Downloads/emacs-24.5/src/emacs /home/lighthall/bin/emacs
ln -s /home/Downloads/git-2.8.4/bin/git /home/lighthall/bin/git
```

##Emacs

.emacs is usually located in the home directory `~/`

`vc-git.el` is located in `~/Downloads/emacs-<verson>/lisp/vc/`
