# sh/ksh initialization

PATH=$HOME/bin:/bin:/sbin:/usr/bin:/usr/sbin:/usr/X11R6/bin:/usr/local/bin:/usr/local/sbin:/usr/games:.
export PATH HOME TERM

export ENV=$HOME/.kshrc

if [ -x $(which mg) ]
then
	ALTERNATE_EDITOR="mg"
	EDITOR="mg"
	VISUAL="mg"
	FCEDIT="mg"
	export ALTERNATE_EDITOR EDITOR VISUAL FCEDIT
fi

HISTSIZE=5000
HISTFILE=$HOME/.sh_history
PAGER=less
LESS="-i -M -q -S -Sm -F -g --no-init"

export HISTSIZE HISTFILE PAGER LESS
