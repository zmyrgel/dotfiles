# sh/ksh initialization

# Import any extra shell configs
export ENV=$HOME/.kshrc

[ -r $HOME/.shell/functions ] && . $HOME/.shell/functions
[ -r $HOME/.shell/variables ] && . $HOME/.shell/variables
[ -r $HOME/.shell/aliases ]   && . $HOME/.shell/aliases
[ -r $HOME/.shell/$(uname) ]  && . $HOME/.shell/$(uname)

export HOME TERM
