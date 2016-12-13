# KSH startup file -*- mode: sh; coding: utf-8-unix; -*-

# skip remaining setup if shell non-interactive
[[ $- != *i* ]] && exit 1

# Read system-wide configuration
if [ -f /etc/ksh.kshrc -a -r /etc/ksh.kshrc ]
then
	. /etc/ksh.kshrc
fi

# Run logout on exit
trap '. $HOME/.ksh_logout; exit' 0

set -o emacs		# Use emacs-style command-line editing.
set +o markdirs		# Add / to names generated from wildcard expansion.
set -o noclobber	# Don't allow > redirection to existing files.
set -o trackall		# Use full pathnames for commands in alias expansions.

PS1='${USER}@${HOST%%.*} ${PWD##*/} $ '
#PS1="[\u@\h \W]$ "
export PS1
