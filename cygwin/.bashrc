# ~/config/cygwin/.bashrc
# Interactive shell settings for Cygwin

# If not running interactively, don't do anything
[[ "$-" != *i* ]] && return

if [ -z $VB ]; then
    export VB=false
else
    if $VB; then
	# set tab
	fTAB="   "
	TAB+=$fTAB
	# print source name at start
	echo -n "${TAB}running $BASH_SOURCE"
	src_name=$(readlink -f $BASH_SOURCE)
	if [ ! "$BASH_SOURCE" = "$src_name" ]; then
	    echo " -> $src_name"
	fi
	echo "..."
	# source formatting
	fpretty=${HOME}/utils/bash/.bashrc_pretty
	if [ -e $fpretty ]; then
	    source $fpretty
	fi
    fi
fi

# required list
LIST="$HOME/config/.bashrc_common $HOME/config/cygwin/.bashrc_X11"

# optional list
LIST_OPT="$HOME/.bash_local"
for FILE in $LIST_OPT
do
    if [ -f $FILE ]; then
	LIST+=" $FILE"
    else
	if $VB; then
	    echo -e "${TAB}$FILE ${UL}not found${NORMAL}"
	fi
    fi
done

for FILE in $LIST
do
    if $VB; then
	echo "${TAB}loading $FILE..."
    fi
    if [ -f $FILE ]; then
	source $FILE
	if [ $? -eq 0 ]; then
	    if $VB; then
		echo -e "${TAB}$FILE ${GOOD}OK${NORMAL}"
	    fi
	else
	    echo -e "${TAB}$FILE ${BAD}FAIL${NORMAL}"
	fi
    else
	echo -e "${TAB}$FILE ${UL}not found${NORMAL}"
    fi
done

# System-specific aliases and functions

# Prompt
export PS1='[\u@\h \W]\[\e[0;32m\]\$\[\e[0m\] ' #[user@host dir]
export PS1='\[\e[1;32m\][\u@\h \[\e[34m\]\W\[\e[32m\]]$\[\e[0m\] ' #[user@host dir] in color
export PS1='PGI \[\e[1;32m\][\u@\h \[\e[34m\]\W\[\e[32m\]]$\[\e[0m\] ' #text [user@host dir] in color
export PS1='\[\e[1;37;42m\]PGI\[\e[0m\]\[\e[1;32m\] [\u@\h \[\e[34m\]\W\[\e[32m\]]$\[\e[0m\] ' #color text [user@host dir] in color
if [ ! -z "$(command -v __git_ps1)" ]; then
    if $VB; then
	echo "${TAB}creating Git prompt..."
    fi
    export PS1='\[\033]0;$TITLEPREFIX:$PWD\007\]\n\[\033[32m\]\u@\h \[\033[35m\]$MSYSTEM \[\033[33m\]\w\[\033[36m\]`__git_ps1`\[\033[0m\]\n$'
    export PS1='\[\033]0;$TITLEPREFIX:$PWD\007\]\[\033[32m\]\e[0;37m\A\[\e[0;32m\] \u@\h \[\033[35m\]$MSYSTEM \[\033[33m\]\w\[\033[36m\]`__git_ps1`\[\033[0m\]\n$ ' #remove new line, add time, add space
    export PS1='\[\033]0;$TITLEPREFIX:$PWD\007\]\[\033[32m\]\e[0;37m\A\[\e[0;32m\] \u@\[\e[1;34m\]\h \[\033[0;35m\]$MSYSTEM \[\033[33m\]\w\[\033[36m\]`__git_ps1`\[\033[0m\]\n$ ' #remove new line, add time, add space
    export PS1='\[\033]0;$TITLEPREFIX:$PWD\007\]\[\033[32m\]\e[0;37m\A\[\e[0;32m\] \u@\[\e[1;34m\]\h \[\033[0;35m\]$MSYSTEM \[\033[33m\]\w\[\033[36m\]`__git_ps1`\n\e[0;32m$\[\e[0m\] ' #remove new line, add time, add space
else
    git_branch() {
	git branch 2>/dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/'
	# 2>/dev/null redirects stderr to null
	# sed -e '/^[^*]/d' deletes any lines not containing *
	# sed -e 's/* \(.*\)/ (\1)/' replaces the first instance of '* '
    }
    if [ -z "$MSYSTEM" ]; then
	if [ -z "$PGI" ]; then
	    if $VB; then
		echo "${TAB}creating Cygwin prompt..."
	    fi
	    export PS1='\[\e[1;32m\][\u@\h \[\e[34m\]\W\[\e[32m\]\e[35m$(git_branch)\e[32m]$\[\e[0m\] ' #[user@host dir (git)]
	    export PS1='\e[0;37m\A\[\e[0;32m\] \u@\[\e[1;35m\]\h\[\e[1;34m\] \w\[\e[0;32m\]\e[36m$(git_branch)\n\e[0;32m$\[\e[0m\] ' # pre time, new line, no brackets, highlight host
	    export PS1='\e[0;37m\A\[\e[0;32m\] \u@\[\e[1;34m\]\h\[\e[0;33m\] \w\[\e[0;32m\]\e[36m$(git_branch)\n\e[0;32m$\[\e[0m\] ' # pre time, new line, no brackets, highlight host, git bash color
	else
	    TEXT='PGI'
	    if $VB; then
		echo "${TAB}creating $TEXT prompt..."
	    fi
	    export PS1='\[\e[1;37;42m\]$TEXT\[\e[0m\]\[\e[1;32m\] [\u@\h \[\e[34m\]\W\[\e[32m\]\e[35m$(git_branch)\e[32m]$\[\e[0m\] ' # text [user@host dir (git)]
	    export PS1='\[\e[1;37;42m\]$TEXT\[\e[0m\]\[\e[1;32m\] [\u@\h \[\e[34m\]\w\[\e[32m\]\e[35m$(git_branch)\e[32m]\n$\[\e[0m\] ' # text [user@host dir (git)]
	    export PS1='\[\e[1;37;42m\]$TEXT\[\e[0m\]\[\e[1;32m\] [\u@\h \[\e[34m\]\w\[\e[32m\]\e[35m$(git_branch)\e[32m]\n\e[0;37m\A\e[0;32m $\[\e[0m\] ' # text, post time
	    export PS1='\[\e[1;37;42m\]$TEXT\[\e[0m\] \e[0;37m\A\[\e[1;32m\] \u@\h \[\e[34m\]\w\[\e[32m\]\e[35m$(git_branch)\e[32m\n$\[\e[0m\] ' # text, pre time, new line, no brackets
	    export PS1='\[\e[1;37;42m\]$TEXT\[\e[0m\] \e[0;37m\A\[\e[1;32m\] \u@\[\e[1;35m\]\h\[\e[1;34m\] \w\[\e[32m\]\e[35m$(git_branch)\e[32m\n$\[\e[0m\] ' # text, pre time, new line, no brackets, highlight host
	    export PS1='\e[0;37m\A\[\e[1;32m\] \u@\[\e[1;35m\]\h\[\e[1;34m\] \w\[\e[32m\]\e[35m$(git_branch)\n\[\e[1;37;42m\]$TEXT\[\e[0m\] \e[0;32m$\[\e[0m\] ' # pre time, new line with text, no brackets, highlight host
	    export PS1='\e[0;37m\A\[\e[0;32m\] \u@\[\e[1;34m\]\h\[\e[0;34m\] \e[0;35m\]$TEXT\[\e[0;33m\] \w\[\e[0;32m\]\e[36m$(git_branch)\n\e[0;32m$\[\e[0m\] ' # pre time, new line with text, no brackets, highlight host, git bash color
	fi
    else
	if $VB; then
	    echo "${TAB}creating MSYS prompt..."
	    echo "${TAB}\"$MSYSTEM\""
	fi
	export PS1='\e[0;37m\A\[\e[1;32m\] \u@\[\e[1;35m\]\h\[\e[1;34m\] \w\[\e[32m\]\e[35m`git_branch`\n\e[0;32m$\[\e[0m\] '
	export PS1='\e[0;37m\A\[\e[1;32m\] \u@\[\e[1;35m\]\h\[\e[1;34m\] $MSYSTEM \w\[\e[32m\]\e[35m`git_branch`\n\e[0;32m$\[\e[0m\] '
	export PS1='\e[0;37m\A\[\e[0;32m\] \u@\[\e[1;35m\]\h\[\e[0;34m\] \e[0;33m\]$MSYSTEM\[\e[1;34m\] \w\[\e[32m\]\e[35m`git_branch`\n\e[0;32m$\[\e[0m\] '
	export PS1='\e[0;37m\A\[\e[0;32m\] \u@\[\e[1;34m\]\h\[\e[0;34m\] \e[0;35m\]$MSYSTEM\[\e[0;33m\] \w\[\e[0;32m\]\e[36m`git_branch`\n\e[0;32m$\[\e[0m\] '
    fi
fi

#kill -9 `ps -a | \grep defunct`

# Cygwin
CYGWIN="${CYGWIN} winsymlinks:nativestrict"
export CYGWIN
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

TAB=${TAB#$fTAB}
