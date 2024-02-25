# Configuration files

User configuration files for [Windows](windows), [macOS](apple), and [Linux](linux).
Settings specific to Windows Subsystem for Linux are found in [wsl](wsl).
Settings specific to Cygwin, MinGW, and Git Bash are found in [cygwin](cygwin).

## Cloning *this* repository

Clone the reposity with one of the following commands.

`git clone https://github.com/jonlighthall/config` (Git HTTPS)

`git clone git@github.com:jonlighthall/config.git` (Git SSH)

## Cloning other repositories

To clone the default list of repsoitories, execute the following command.

```git
cd config
./get_repos.sh
```

## Linking

Alternatively, to apply the settings of a given user configureation after this repsoitory cloning, execute the
following command. The `make_links.sh` script will be run in the specified directory.

```git
./get_repos.sh [OS]
```

The following OS options have associated `make_links.sh` scripts: \
[`cygwin`](cygwin) \
[`git_bash`](git_bash) \
[`linux`](linux) \
[`windows`](windows) \
[`wsl`](wsl)

## Sensitive data
[`private`](private) - see <https://github.com/jonlighthall/config_private> \