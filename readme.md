# Configuration files
User configuration files for [Windows](windows), [macOS](apple), and [Linux](linux). Settings specific to Windows Subsystem for Linux are found in [wsl](wsl). Settings specific to Cygwin, MinGW, and Git Bash are found in [cygwin](cygwin). 

## Installation
Clone the reposity with one of the following commands.

`git clone https://github.com/jonlighthall/config` (Git HTTPS)

`git clone git@github.com:jonlighthall/config.git` (Git SSH)

## Configuration
Author names can be rewritten with the folloing code

from <https://help.github.com/articles/changing-author-info/>

````bash
./change.sh
git push --force --tags origin 'refs/heads/*'
````
