for installing Git Bash

    the default home directory for git bash is the same as the windows user home directory
    to create another directory, set the window user environmental variable HOME to the desired directory
    
first copy ssh credentials from
    https://jonlighthall@bitbucket.org/jonlighthall/.ssh.git or 
    https://github.com/jonlighthall/config_private.git
then
    git clone git@github.com:jonlighthall/config.git
    ./get_repos.sh
