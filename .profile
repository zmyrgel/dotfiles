# sh/bash/ksh initialization

[ -r $HOME/.shell/variables ] && . $HOME/.shell/variables
[ -r $HOME/.shell/aliases ]   && . $HOME/.shell/aliases
[ -r $HOME/.shell/functions ] && . $HOME/.shell/functions

case $(uname) in
    OpenBSD) [ -r $HOME/.shell/openbsd ] && . $HOME/.shell/openbsd ;;
    NetBSD)  [ -r $HOME/.shell/netbsd ]  && . $HOME/.shell/netbsd  ;;
    Linux)   [ -r $HOME/.shell/linux ]   && . $HOME/.shell/linux   ;;
    SunOS)   [ -r $HOME/.shell/sunos ]   && . $HOME/.shell/sunos   ;;
esac

# Init KSH
if [ -r $HOME/.kshrc ]; then
    ENV=$HOME/.kshrc
    export ENV
fi
