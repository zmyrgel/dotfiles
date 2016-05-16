# KSH startup file -*- mode: sh; coding: utf-8-unix; -*-

# skip remaining setup if shell non-interactive
[[ $- != *i* ]] && exit 1

# Read system-wide configuration
if [ -f /etc/ksh.kshrc -a -r /etc/ksh.kshrc ]; then
	. /etc/ksh.kshrc
fi

# Run logout on exit
trap '. $HOME/.ksh_logout; exit' 0

set -o emacs		# Use emacs-style command-line editing.
set +o markdirs		# Add / to names generated from wildcard expansion.
set -o noclobber	# Don't allow > redirection to existing files.
set -o trackall		# Use full pathnames for commands in alias expansions.

alias df='df -h'
alias du='du -h'
alias man='LC_ALL=C man'
alias ls='/bin/ls -F'
alias mkdir='mkdir -p'
alias tailf='tail -f'
alias top='top -1C'
alias ll='ls -l'
alias la='ls -a'
alias md='mkdir -p'
alias rd='rmdir'
alias ff='find . -name $*'
alias msgs='tail -f /var/log/messages'
alias topcpu='ps aux | sort -n +2 | tail -10'
alias topmem='ps aux | sort -n +3 | tail -10'
alias c='tput clear'
alias em="emacsclient -t"
alias mg='mg -n'
alias playiso='mplayer dvd://1 -dvd-device '
alias playdvd='mplayer dvdnav:// /dev/sr0'
alias sizeof='du -sh'
alias vncup='x11vnc -nopw -ncache 10 -display :0 -localhost'
alias xp='xprop | grep "WM_WINDOW_ROLE\|WM_CLASS"'
alias xcap="ffmpeg -f x11grab -s wxga -r 25 -i :0.0 -sameq /tmp/out.mpg"
alias flav='make show=FLAVORS'
alias show_beacons='doas tcpdump -n -i iwn0 -s 1500 -vvv -y IEEE802_11_RADIO subtype beacon'

function s {
	if [ -x /usr/bin/doas ]
	then
		/usr/bin/doas $*
	elif [ -x /usr/bin/sudo ]
	then
		/usr/bin/sudo $*
	fi
}

function base64 {
	perl -MMIME::Base64 -e 'print encode_base64(<>)'
}

function rndpw {
    random="$(dd if=/dev/random bs=10 count=1)" 2>/dev/null
    echo $random | base64
}

function clamscan {
	mkdir -p /tmp/virus
	nice -n 19 clamscan --verbose --max-recursion=20 -m -i --detect-pua --move=/tmp/virus -r
	cd /tmp/virus
	echo -n "Viruses:"
	ls
}

function spell {
	echo "$@" | aspell -a
}

function calc {
	echo "$*" | bc;
}

# Add Latest JDK dir to PATH
add_path /usr/local/jdk-1.8.0/bin
add_path /usr/local/jdk-1.7.0/bin
add_path /usr/local/jdk-1.6.0/bin

if [ -d /usr/local/share/doc/posix/man ]
then
	alias pman='man -M /usr/local/share/doc/posix/man'
	alias papropos='apropos -M /usr/local/share/doc/posix/man'
	alias pwhatis='whatis -M /usr/local/share/doc/posix/man'
fi

# Locale stuff
LC_CTYPE=en_us.UTF-8
export LC_CTYPE

# Variables for programming languages
if [ -d /usr/local/go ]
then
	GOROOT=/usr/local/go
	export GOROOT
	add_path $GOROOT/bin
	add_path $GOROOT/pkg/tool/openbsd_amd64
fi
GOPATH=$HOME/workspace
export GOPATH
add_path $GOPATH/bin

add_path $HOME/.rvm/bin
add_path $HOME/.cabal/bin
add_path $HOME/.perl6/2015.12/bin

if [ -x $HOME/.rakudobrew/bin/rakudobrew ]
then
    eval "$($HOME/.rakudobrew/bin/rakudobrew init -)"
fi

if [ -d $HOME/perl5/lib/perl5 ]
then
    eval "$(perl -I$HOME/perl5/lib/perl5 -Mlocal::lib)"
fi

CVSROOT="anoncvs@anoncvs.eu.openbsd.org:/cvs"
export CVSROOT

PS1='${USER}@${HOST%%.*} ${PWD##*/} $ '
export PS1
