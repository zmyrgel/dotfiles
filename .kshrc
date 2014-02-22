#!/usr/bin/env ksh
# KSH startup file

# skip remaining setup if shell is not interactive
[ $- == *i* ] || exit 0

# Base Korn Shell environment
[ -r /etc/ksh.kshrc ] && . /etc/ksh.kshrc

# Disable flow control on terminal
stty -ixon

# Fix backspace
stty erase ^h
#stty erase 

set -o emacs		# Use emacs-style command-line editing.
set +o markdirs		# Add / to all directory names generated from wildcard expansion.
set -o noclobber	# Don't allow > redirection to existing files.
set -o trackall		# Substitute full pathnames for commands in alias expansions.
