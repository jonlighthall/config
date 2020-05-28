# ~/config/cygwin/.bashrc
# Interactive shell settings for Cygwin

# If not running interactively, don't do anything
[[ "$-" != *i* ]] && return

# Source remote aliases
if [ -f ~/.bash_local ]; then
    . ~/.bash_local
fi

if [ -f ~/config/.bash_remotes ]; then
    . ~/config/.bash_remotes 
fi

# User specific aliases and functions

# Settings
# Commands to save
export HISTFILESIZE=50000
export HISTSIZE=40000
# Don't put duplicate lines in the history
export HISTCONTROL=ignoredups
export HISTIGNORE=$'bg:exit:ls:pwd:history:snuffy'
# Realtime history
#shopt -s histappend
PROMPT_COMMAND="history -a;$PROMPT_COMMAND"
# Timestamp history
HISTTIMEFORMAT="%F %T "
# Editors
export EDITOR=emacs
export SVN_EDITOR=emacs
export GIT_EDITOR=emacs
# Prompt
export PS1='[\u@\h \W]\[\e[1;32m\]\$\[\e[0m\] ' #[user@host dir]
export PS1='\[\e[1;32m\][\u@\h \[\e[34m\]\W\[\e[32m\]]$\[\e[0m\] ' #[user@host dir] in color
export PS1='PGI \[\e[1;32m\][\u@\h \[\e[34m\]\W\[\e[32m\]]$\[\e[0m\] ' #text [user@host dir] in color
export PS1='\[\e[1;37;42m\]PGI\[\e[0m\]\[\e[1;32m\] [\u@\h \[\e[34m\]\W\[\e[32m\]]$\[\e[0m\] ' #color text [user@host dir] in color
if [ ! -z "$(command -v __git_ps1)" ]; then
    #echo "creating Git prompt..." 
    export PS1='\[\033]0;$TITLEPREFIX:$PWD\007\]\n\[\033[32m\]\u@\h \[\033[35m\]$MSYSTEM \[\033[33m\]\w\[\033[36m\]`__git_ps1`\[\033[0m\]\n$'
    export PS1='\[\033]0;$TITLEPREFIX:$PWD\007\]\[\033[32m\]\e[0;37m\A\[\e[0;32m\] \u@\h \[\033[35m\]$MSYSTEM \[\033[33m\]\w\[\033[36m\]`__git_ps1`\[\033[0m\]\n$ ' #remove new line, add time, add space
    export PS1='\[\033]0;$TITLEPREFIX:$PWD\007\]\[\033[32m\]\e[0;37m\A\[\e[0;32m\] \u@\[\e[1;34m\]\h \[\033[0;35m\]$MSYSTEM \[\033[33m\]\w\[\033[36m\]`__git_ps1`\[\033[0m\]\n$ ' #remove new line, add time, add space
else
    git_branch() {
	git branch 2>/dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/'
	# 2>/dev/null redirects stderr to null
	# sed -e '/^[^*]/d' deletes any lines not containing *
	# sed -e 's/* \(.*\)/ (\1)/' replaces the first instance of '* '
    }
    if [ -z "$MSYSTEM" ]; then
	#echo "creating Cygwin prompt..." 
	export PS1='\[\e[1;32m\][\u@\h \[\e[34m\]\W\[\e[32m\]\e[35m$(git_branch)\e[32m]$\[\e[0m\] ' #[user@host dir (git)]
	TEXT='PGI'
	export PS1='\[\e[1;37;42m\]$TEXT\[\e[0m\]\[\e[1;32m\] [\u@\h \[\e[34m\]\W\[\e[32m\]\e[35m$(git_branch)\e[32m]$\[\e[0m\] ' # text [user@host dir (git)]
	export PS1='\[\e[1;37;42m\]$TEXT\[\e[0m\]\[\e[1;32m\] [\u@\h \[\e[34m\]\w\[\e[32m\]\e[35m$(git_branch)\e[32m]\n$\[\e[0m\] ' # text [user@host dir (git)]
	export PS1='\[\e[1;37;42m\]$TEXT\[\e[0m\]\[\e[1;32m\] [\u@\h \[\e[34m\]\w\[\e[32m\]\e[35m$(git_branch)\e[32m]\n\e[0;37m\A\e[1;32m $\[\e[0m\] ' # text, post time
	export PS1='\[\e[1;37;42m\]$TEXT\[\e[0m\] \e[0;37m\A\[\e[1;32m\] \u@\h \[\e[34m\]\w\[\e[32m\]\e[35m$(git_branch)\e[32m\n$\[\e[0m\] ' # text, pre time, new line, no brackets
	export PS1='\[\e[1;37;42m\]$TEXT\[\e[0m\] \e[0;37m\A\[\e[1;32m\] \u@\[\e[1;35m\]\h\[\e[1;34m\] \w\[\e[32m\]\e[35m$(git_branch)\e[32m\n$\[\e[0m\] ' # text, pre time, new line, no brackets, highlight host
	export PS1='\e[0;37m\A\[\e[1;32m\] \u@\[\e[1;35m\]\h\[\e[1;34m\] \w\[\e[32m\]\e[35m$(git_branch)\n\[\e[1;37;42m\]$TEXT\[\e[0m\] \e[1;32m$\[\e[0m\] ' # pre time, new line with text, no brackets, highlight host
	export PS1='\e[0;37m\A\[\e[0;32m\] \u@\[\e[1;35m\]\h\[\e[1;34m\] \w\[\e[0;32m\]\e[36m$(git_branch)\n\e[1;32m$\[\e[0m\] ' # pre time, new line, no brackets, highlight host
    else
	#echo "creating MSYS prompt..."
	#echo "\"$MSYSTEM\""
	export PS1='\e[0;37m\A\[\e[1;32m\] \u@\[\e[1;35m\]\h\[\e[1;34m\] \w\[\e[32m\]\e[35m`git_branch`\n\e[1;32m$\[\e[0m\] '
	export PS1='\e[0;37m\A\[\e[1;32m\] \u@\[\e[1;35m\]\h\[\e[1;34m\] $MSYSTEM \w\[\e[32m\]\e[35m`git_branch`\n\e[1;32m$\[\e[0m\] '
	export PS1='\e[0;37m\A\[\e[0;32m\] \u@\[\e[1;35m\]\h\[\e[0;34m\] \e[0;33m\]$MSYSTEM\[\e[1;34m\] \w\[\e[32m\]\e[35m`git_branch`\n\e[1;32m$\[\e[0m\] '
	export PS1='\e[0;37m\A\[\e[0;32m\] \u@\[\e[1;34m\]\h\[\e[0;34m\] \e[0;35m\]$MSYSTEM\[\e[0;33m\] \w\[\e[0;32m\]\e[36m`git_branch`\n\e[1;32m$\[\e[0m\] '
    fi
fi

# Macros
alias ls='ls --color'
alias la='ls -la'
alias lt='ls -ltr'
alias lS='ls -ltS'
alias pwd='pwd -L;pwd -P'
alias ping='ping -c 5'
function duf {	       
    du -k "$@" | sort -n |
    while read size fname; do
     	for unit in k M G T P E Z Y;
	do
	    if [ $size -lt 1024 ]; then
		echo -e "${size}${unit}B${fname}";
		break;
	    fi;
	    size=$((size/1024));
	done;
    done
}

alias du1='duf --max-depth=1'
alias du2='duf --max-depth=2'
alias du0='duf --max-depth=0'

# X Window
export DISPLAY=localhost:0
#alias xwin='startxwin; echo -e "\033c"'
alias xwin='/c/Program\ Files\ \(x86\)/Xming/Xming.exe -multiwindow -silent-dup-error -multiplemonitors &'
alias close='kill -9 `ps -a |grep Xming`; kill -9 `ps -a |grep ssh`;exit'
#kill -9 `ps -a |grep defunct`
#alias ssh='xwin ssh'
#xwin #start xwin on startup

# Cygwin
#set CYGWIN=nodosfilewarning
CYGWIN="${CYGWIN} nodosfilewarning"; export CYGWIN
set HOME=/cygdrive/c/cygwin/home/lighthall/

# Shortcuts
alias vc='/cygdrive/c/Program\ Files\ \(x86\)/Microsoft\ Visual\ Studio/2017/Community/VC/Auxiliary/Build/vcvarsall.bat x86'
FILE='/cygdrive/c/Program\ Files\ \(x86\)/emacs-23.4/bin/runemacs.exe'
FILE='/cygdrive/c/Program\ Files/emacs-24.1/bin/runemacs.exe'
FILE='/c/Users/jlighthall/Downloads/emacs-26.1-x86_64/bin/runemacs.exe'
if [ -f $FILE ]; then
    alias emacs=$FILE    
fi
#alias gsview='/cygdrive/c/Program\ Files/Ghostgum/gsview/gsview64.exe'

# Path additions
#export PATH=$PATH:/cygdrive/c/Program\ Files\ \(x86\)/emacs-25.1-bin-i686-mingw32/bin/
#export PATH=$PATH:/cygdrive/c/Program\ Files/Hugin/bin
#export PATH=$PATH:/cygdrive/c/root/bin
#export ROOTSYS='c:/root'   # must be in DOS format (change path!)
#export PATH='cygpath -u $ROOTSYS'/bin:$PATH
#export PATH=$PATH:/c/WINNT/system32:/c/WINNT:/c/Program\ Files/Microsoft\ Visual\ Studio/Common/Tools/WinNT:/c/Program\ Files/Microsoft\ Visual\ Studio/Common/MSDev98/Bin:/c/Program\ Files/Microsoft\ Visual\ Studio/Common/Tools:/c/Program\ Files/Microsoft\ Visual\ Studio/VC98/bin:/c/Program\ Files/DevStudio/DF/bin:/c/Program\ Files/DevStudio/SharedIDE/bin

# Set PATH so it includes user's private bin if it exists
if [[ ":$PATH" != *":${HOME}/bin"*  ]]; then
    if [ -d "${HOME}/bin" ] ; then
	export PATH=$PATH:${HOME}/bin
    fi
fi
