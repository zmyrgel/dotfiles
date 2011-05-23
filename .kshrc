#!/usr/bin/env ksh
# KSH startup file

# skip remaining setup if shell non-interactive
[[ -o interactive ]] || return 0

# Base Korn Shell environment
[[ -r /etc/ksh.kshrc ]] && . /etc/ksh.kshrc

# Disable flow control on terminal
stty -ixon

# Fix backspace
stty erase ^h
#stty erase 

set -o emacs		# Use emacs-style command-line editing.
set +o markdirs		# Add / to all directory names generated from wildcard expansion.
set -o noclobber	# Don't allow > redirection to existing files.
set -o trackall		# Substitute full pathnames for commands in alias expansions.

export TTY=$(tty|cut -f3-4 -d/)
export HISTFILE=$HOME/.sh_hist$(echo ${TTY} | tr -d '/')
export HOLD="$(hostname |cut -f1 -d.)"
export PS1=$USER@$HOLD:'$PWD-> '


## Completion stuff
#bind "^I=complete"      # Complete to next unmatch with tab key
#bind "^I=complete-list" # List completions in the dir by tapping on the tab

## Arrow key history hack
#alias __A="^P" # arrow key for the previous command
#alias __B="^N" # arrow key for the next command
#alias __C="^F" # arrow key for one character forwards
#alias __D="^B" # arrow key for one character backwards



