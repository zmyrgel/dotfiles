# Setup fonts
#xset +fp /home/zmyrgel/.fonts/Dina-PCF/
#xset +fp /home/zmyrgel/.fonts/Envy Code R PR7/
#xset +fp /home/zmyrgel/.fonts/tamsyn-font-1.6/
#xset fp rehash

# Visual settings
xset -b
xsetroot -solid rgb:11/11/11
xsetroot -cursor_name left_ptr -fg black -bg white
#nitrogen --restore &
wmname LG3D

# Keyboard settings
setxkbmap -option ctrl:nocaps
xmodmap ~/.xmodmap

# Disable laptop screen on dock
#dmesg | grep 'ACPI Dock' >/dev/null && `xrandr --output LVDS1 --off`

# Environment variables for X
export OOO_FORCE_DESKTOP=gnome
export GDK_USE_XFT=1
export QT_XFT=true
cd $HOME

# Start WM
#exec /usr/bin/ssh-agent dwm
#exec /usr/bin/ssh-agent startfluxbox
#exec /usr/bin/ssh-agent $HOME/bin/stumpwm
#exec /usr/bin/ssh-agent scrotwm
exec /usr/bin/ssh-agent wmii

