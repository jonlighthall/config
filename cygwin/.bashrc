# To the extent possible under law, the author(s) have dedicated all 
# copyright and related and neighboring rights to this software to the 
# public domain worldwide. This software is distributed without any warranty. 
# You should have received a copy of the CC0 Public Domain Dedication along 
# with this software. 
# If not, see <http://creativecommons.org/publicdomain/zero/1.0/>. 

# base-files version 4.1-1

# ~/.bashrc: executed by bash(1) for interactive shells.

# The latest version as installed by the Cygwin Setup program can
# always be found at /etc/defaults/etc/skel/.bashrc

# Modifying /etc/skel/.bashrc directly will prevent
# setup from updating it.

# The copy in your home directory (~/.bashrc) is yours, please
# feel free to customise it to create a shell
# environment to your liking.  If you feel a change
# would be benifitial to all, please feel free to send
# a patch to the cygwin mailing list.

# User dependent .bashrc file

# If not running interactively, don't do anything
[[ "$-" != *i* ]] && return

# Shell Options
#
# See man bash for more options...
#
# Don't wait for job termination notification
# set -o notify
#
# Don't use ^D to exit
# set -o ignoreeof
#
# Use case-insensitive filename globbing
# shopt -s nocaseglob
#
# Make bash append rather than overwrite the history on disk
# shopt -s histappend
#
# When changing directory small typos can be ignored by bash
# for example, cd /vr/lgo/apaache would find /var/log/apache
# shopt -s cdspell

# Completion options
#
# These completion tuning parameters change the default behavior of bash_completion:
#
# Define to access remotely checked-out files over passwordless ssh for CVS
# COMP_CVS_REMOTE=1
#
# Define to avoid stripping description in --option=description of './configure --help'
# COMP_CONFIGURE_HINTS=1
#
# Define to avoid flattening internal contents of tar files
# COMP_TAR_INTERNAL_PATHS=1
#
# Uncomment to turn on programmable completion enhancements.
# Any completions you add in ~/.bash_completion are sourced last.
# [[ -f /etc/bash_completion ]] && . /etc/bash_completion

# History Options
#
# Don't put duplicate lines in the history.
# export HISTCONTROL=$HISTCONTROL${HISTCONTROL+,}ignoredups
#
# Ignore some controlling instructions
# HISTIGNORE is a colon-delimited list of patterns which should be excluded.
# The '&' is a special pattern which suppresses duplicate entries.
# export HISTIGNORE=$'[ \t]*:&:[fb]g:exit'
# export HISTIGNORE=$'[ \t]*:&:[fb]g:exit:ls' # Ignore the ls command as well
#
# Whenever displaying the prompt, write the previous line to disk
# export PROMPT_COMMAND="history -a"

# Aliases
#
# Some people use a different file for aliases
# if [ -f "${HOME}/.bash_aliases" ]; then
#   source "${HOME}/.bash_aliases"
# fi

# Source remote aliases
if [ -f ~/config/.bash_remotes ]; then
	. ~/config/.bash_remotes 
fi

#
# Some example alias instructions
# If these are enabled they will be used instead of any instructions
# they may mask.  For example, alias rm='rm -i' will mask the rm
# application.  To override the alias instruction use a \ before, ie
# \rm will call the real rm not the alias.
#
# Interactive operation...
# alias rm='rm -i'
# alias cp='cp -i'
# alias mv='mv -i'
#
# Default to human readable figures
# alias df='df -h'
# alias du='du -h'
#
# Misc :)
# alias less='less -r'                          # raw control characters
# alias whence='type -a'                        # where, of a sort
# alias grep='grep --color'                     # show differences in colour
# alias egrep='egrep --color=auto'              # show differences in colour
# alias fgrep='fgrep --color=auto'              # show differences in colour
#
# Some shortcuts for different directory listings
# alias ls='ls -hF --color=tty'                 # classify files in colour
# alias dir='ls --color=auto --format=vertical'
# alias vdir='ls --color=auto --format=long'
# alias ll='ls -l'                              # long list
# alias la='ls -A'                              # all but . and ..
# alias l='ls -CF'                              #

# Umask
#
# /etc/profile sets 022, removing write perms to group + others.
# Set a more restrictive umask: i.e. no exec perms for others:
# umask 027
# Paranoid: neither group nor others have any perms:
# umask 077

# Functions
#
# Some people use a different file for functions
# if [ -f "${HOME}/.bash_functions" ]; then
#   source "${HOME}/.bash_functions"
# fi
#
# Some example functions:
#
# a) function settitle
# settitle () 
# { 
#   echo -ne "\e]2;$@\a\e]1;$@\a"; 
# }
# 
# b) function cd_func
# This function defines a 'cd' replacement function capable of keeping, 
# displaying and accessing history of visited directories, up to 10 entries.
# To use it, uncomment it, source this file and try 'cd --'.
# acd_func 1.0.5, 10-nov-2004
# Petar Marinov, http:/geocities.com/h2428, this is public domain
# cd_func ()
# {
#   local x2 the_new_dir adir index
#   local -i cnt
# 
#   if [[ $1 ==  "--" ]]; then
#     dirs -v
#     return 0
#   fi
# 
#   the_new_dir=$1
#   [[ -z $1 ]] && the_new_dir=$HOME
# 
#   if [[ ${the_new_dir:0:1} == '-' ]]; then
#     #
#     # Extract dir N from dirs
#     index=${the_new_dir:1}
#     [[ -z $index ]] && index=1
#     adir=$(dirs +$index)
#     [[ -z $adir ]] && return 1
#     the_new_dir=$adir
#   fi
# 
#   #
#   # '~' has to be substituted by ${HOME}
#   [[ ${the_new_dir:0:1} == '~' ]] && the_new_dir="${HOME}${the_new_dir:1}"
# 
#   #
#   # Now change to the new dir and add to the top of the stack
#   pushd "${the_new_dir}" > /dev/null
#   [[ $? -ne 0 ]] && return 1
#   the_new_dir=$(pwd)
# 
#   #
#   # Trim down everything beyond 11th entry
#   popd -n +11 2>/dev/null 1>/dev/null
# 
#   #
#   # Remove any other occurence of this dir, skipping the top of the stack
#   for ((cnt=1; cnt <= 10; cnt++)); do
#     x2=$(dirs +${cnt} 2>/dev/null)
#     [[ $? -ne 0 ]] && return 0
#     [[ ${x2:0:1} == '~' ]] && x2="${HOME}${x2:1}"
#     if [[ "${x2}" == "${the_new_dir}" ]]; then
#       popd -n +$cnt 2>/dev/null 1>/dev/null
#       cnt=cnt-1
#     fi
#   done
# 
#   return 0
# }
# 
# alias cd=cd_func

# Settings
# Store 50000 commands in bash history
export HISTFILESIZE=50000
export HISTSIZE=40000
# Don't put duplicate lines in the history
export HISTCONTROL=ignoredups
export HISTIGNORE=$'bg:exit:ls:pwd:history'
# Realtime history
#shopt -s histappend
PROMPT_COMMAND="history -a;$PROMPT_COMMAND"
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

# Macros
alias ls='ls --color=auto'
# alias ls='ls -hF --color=tty'                 # classify files in colour

# X Window
export DISPLAY=localhost:0
#alias xwin='startxwin; echo -e "\033c"'
alias xwin='/c/Program\ Files\ \(x86\)/Xming/Xming.exe -multiwindow -silent-dup-error -multiplemonitors &'
alias close='kill -9 `ps -a |grep Xming`; kill -9 `ps -a |grep ssh`;exit'
#kill -9 `ps -a |grep defunct`
alias ssh='xwin ssh'
#xwin #start xwin on startup

# Cygwin
#set CYGWIN=nodosfilewarning
CYGWIN="${CYGWIN} nodosfilewarning"; export CYGWIN
set HOME=/cygdrive/c/cygwin/home/lighthall/

# Shortcuts
alias vc='/cygdrive/c/Program\ Files\ \(x86\)/Microsoft\ Visual\ Studio/2017/Community/VC/Auxiliary/Build/vcvarsall.bat x86'
#alias emacs='/cygdrive/c/Program\ Files\ \(x86\)/emacs-23.4/bin/runemacs.exe'
#alias emacs32='/cygdrive/c/Program\ Files/emacs-24.1/bin/runemacs.exe'
#alias gsview='/cygdrive/c/Program\ Files/Ghostgum/gsview/gsview64.exe'

# Path replacements
#export PATH=$PATH:/cygdrive/c/Program\ Files\ \(x86\)/emacs-25.1-bin-i686-mingw32/bin/
#export PATH=$PATH:/cygdrive/c/Program\ Files/Hugin/bin
#export PATH=$PATH:/cygdrive/c/root/bin
#export ROOTSYS='c:/root'   # must be in DOS format (change path!)
#export PATH='cygpath -u $ROOTSYS'/bin:$PATH
#export PATH=$PATH:/c/WINNT/system32:/c/WINNT:/c/Program\ Files/Microsoft\ Visual\ Studio/Common/Tools/WinNT:/c/Program\ Files/Microsoft\ Visual\ Studio/Common/MSDev98/Bin:/c/Program\ Files/Microsoft\ Visual\ Studio/Common/Tools:/c/Program\ Files/Microsoft\ Visual\ Studio/VC98/bin:/c/Program\ Files/DevStudio/DF/bin:/c/Program\ Files/DevStudio/SharedIDE/bin
