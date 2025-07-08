# Configuration files

User configuration files for [Windows](windows), [macOS](apple), and [Linux](linux).
Settings specific to Windows Subsystem for Linux are found in [wsl](wsl).
Settings specific to Cygwin, MinGW, and Git Bash are found in [cygwin](cygwin).

## Prerequisites

### Connectivity (WSL)

Install wsl-vpnkit as necessary: https://github.com/sakai135/wsl-vpnkit

### Git

Install git with one of the following commands.
> Debian (Ubuntu):

        sudo apt install git -y


> Red Hat (Oracle, Rocky Linux):
```
    sudo dnf install git -y
```

## Cloning *this* repository

Clone the repository with one of the following commands.

> HTTPS:
```
    git clone https://github.com/jonlighthall/config
```

> SSH:
```
    git clone git@github.com:jonlighthall/config.git
```

### SSH keys

A password-protected SSH key is required when cloning via SSH.

The contents of `.ssh` from a working server will first need to be copied to the new server with the command
`scp -rp ~/.ssh user@server:~/`


## Cloning other repositories

To clone the default list of repositories, execute the following command.

```bash
cd config
./get_repos.sh
```

## Linking

Alternatively, to apply the settings of a given user configuration after this repository cloning, execute the
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
