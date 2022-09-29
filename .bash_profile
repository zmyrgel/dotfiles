# .bash_profile

# Get the aliases and functions
[ -f $HOME/.bashrc ] && . $HOME/.bashrc

[ -r $HOME/.shell/functions ] && . $HOME/.shell/functions
[ -r $HOME/.shell/variables ] && . $HOME/.shell/variables
[ -r $HOME/.shell/aliases ]   && . $HOME/.shell/aliases
[ -r $HOME/.shell/$(uname) ]  && . $HOME/.shell/$(uname)
[ -r $HOME/.shell/work ]      && . $HOME/.shell/work

export HOME TERM

HISTFILE=$HOME/.bash_history
