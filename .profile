# sh/ksh initialization

export HOME TERM

# Import any extra shell configs
export ENV=$HOME/.kshrc

[ -r $HOME/.shell/functions ] && . $HOME/.shell/functions
[ -r $HOME/.shell/variables ] && . $HOME/.shell/variables
[ -r $HOME/.shell/aliases ] && . $HOME/.shell/aliases
[ -r $HOME/.shell/$(uname) ] && . $HOME/.shell/$(uname)
[ -r $HOME/.shell/work ] && . $HOME/.shell/work

# NPM settings
NPM_CONFIG_PREFIX=$HOME/.npm-global
PATH=$PATH:$HOME/.npm-global/bin

export NPM_CONFIG_PREFIX PATH

# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
export PATH="$PATH:$HOME/.rvm/bin"
