# ~/config/.bash_aliases
#
# Aliases and fucnction for interactive UNIX-like shells. Intended to be universally compatible
# with WSL, Debian derivatives (Ubuntu), Red Hat-based distros (Centos, Rock Linux), MinGW (MSYS,
# GitBash), Cygwin, PGI Bash, etc.

# Aliases
alias close='source ${HOME}/.bash_logout;killall -9 -v -u $USER$USERNAME; exit'
alias hello='echo "hello, world"'
alias ping='ping -c 4 -W 1'
alias pkill='pkill -9 -u ${USER}'
alias pwd='pwd -L;pwd -P'
alias up='bash update_repos' # should be sourced?

# diff
alias d='diffy -s'
alias diffy='diff --color=auto --suppress-common-lines -yiEZbwB'

# find
alias fb='ff | perl -lne "print if not -T"' # find binary
alias fd='find -L ./ -not -path "*/.git/*" -type d' # find directory
alias ff='find -L ./ -not -path "*/.git*/*" -type f' # find file
alias ffi='ff -iname'
alias fl='find -L ./ \( -type l -o -xtype l \) -exec ls --color -l {} \;' # find link

# git
alias gitb='git branch -vva  --color=always'
alias gitcp='git cherry-pick'
alias gitd='git diff'
alias gitdn='git diff --name-only'
alias gitr='git remote -v'
alias gits='git status'
alias gitsi='git status --ignored'

# grep
alias g='gr'
alias gr='grep -iIrRn -D skip --exclude-dir=".git"'
alias grep='grep --color=auto'

# history
alias hg='history | grep'
alias hl="\cat ~/.bash_history | sed '/^#[0-9]\{10\}/d' | awk '{print length, \$0}' | sort -n | uniq -c | sort -n -r | sed 's/^\([ \t]*[0-9]*\) [0-9]* \(.*$\)/\1 \2/' | head -n 20"

# list
alias lS='ls -ltS'
alias la='ls -la'
alias lh='ls -ld .?*' # list hidden only
alias ls='ls -l --color'
alias lt='ls -ltr' # sort by time

# Fucntions
alias du0='duf --max-depth=0'
alias du1='duf --max-depth=1'
alias du2='duf --max-depth=2'
function duf {
    du -k "$@" | sort -n |
	while read size fname; do
     	    for unit in k M G T P E Z Y
	    do
		if [ $size -lt 1024 ]; then
		    echo -e "${size}${unit}B${fname}";
		    break;
		fi;
		size=$((size/1024));
	    done
	done
}

function gitl {
    # alias gitl='git log --follow'
    if [ $# -ne 1 ]; then
	git log
    else
	git log --follow $@
    fi
}

function nf {
    if [ $# -eq 0 ]; then
	dir_list=$PWD
    else
	dir_list="$@"
    fi
    for dir in $dir_list
    do
	dir=${dir%/}
	n1=$(find "${dir}" -maxdepth 1 -type f | wc -l)

	# calculate length of longest number
	n2=$(find "${dir}" -type f | wc -l)
	nn=$(echo "(l($n2)/l(10))+1" | bc -l | sed 's/\..*$//')
	# account for commas
	nc=$((($nn-1)/3))
	np=$(($nn + $nc))

	# print results
	printf "%'${np}d files found in ${dir}\n" $n1
	if [ $n1 -ne $n2 ]; then
	    printf "%'${np}d files found in ${dir}/*\n" $n2
	fi
    done
}

# conditional echo
vecho() {
    if [ ! -z ${VB:+dummy} ] && ${VB}; then
	# if VB is (unset or null) or true
	echo "$@"
    fi
}
