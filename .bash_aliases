# Aliases
alias close='source ~/.bash_logout;killall -9 -v -u $USER$USERNAME; exit'
alias d='diffy -s'
alias diffy='diff --color=auto --suppress-common-lines -yiEZbwB'
alias du0='duf --max-depth=0'
alias du1='duf --max-depth=1'
alias du2='duf --max-depth=2'
alias e='emacs'
alias en='emacs -nw'
alias fb='ff | perl -lne "print if not -T"' # find binary
alias fd='find -L ./ -not -path "*/.git/*" -type d' # find directory
alias ff='find -L ./ -not -path "*/.git*/*" -type f' # find file
alias ffi='ff -iname'
alias fl='find -L ./ -maxdepth 1 -type l -exec ls --color -l {} \;' # find link
alias g='gr'
alias gitb='git branch -va'
alias gitcp='git cherry-pick'
alias gitd='git diff'
alias gitdn='git diff --name-only'
alias gitr='git remote -v'
alias gitl='git log'
alias gits='git status'
alias gr='grep -iIrR --exclude-dir=".git"'
alias grep='grep --color=auto'
alias hg='history | grep'
alias lS='ls -ltS'
alias la='ls -la'
alias lh='ls -ld .?*' # list hidden only
alias ls='ls --color'
alias lt='ls -ltr' # sort by time
alias ping='ping -c 5'
alias pkill='pkill -9 -u ${USER}'
alias pwd='pwd -L;pwd -P'
alias up='update_repos'

# Fucntions
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

function nf {
    if [ $# -eq 0 ]; then
	dir_list=$PWD
    else
	dir_list="$@"
    fi
    for dir in $dir_list
    do
	dir=${dir%/}
	n1=$(find ${dir} -maxdepth 1 -type f | wc -l)

	# calculate length of longest number
	n2=$(find ${dir} -type f | wc -l)
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
    if [ ! -z ${VB:+dummy} ] || [ "${VB}"=true ] ; then
	# if VB is (unset or null) or true
	echo "$@"
    fi
}
