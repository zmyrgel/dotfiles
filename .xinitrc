# source some X settings
[ -r ~/.Xresources ] && xrdb ~/.Xresources

# Visual settings
xset -b
#xsetroot -solid rgb:11/11/11
#xsetroot -cursor_name left_ptr -fg black -bg white

# Disable laptop screen on dock
#dmesg | grep 'ACPI Dock' >/dev/null && `xrandr --output LVDS1 --off`

if [ -x $(which wmname) ]
then
    wmname LG3D
fi

# Keyboard settings
setxkbmap -layout us -variant altgr-intl -option ctrl:nocaps

# Environment variables for X
export OOO_FORCE_DESKTOP=gnome
export GDK_USE_XFT=1
export QT_XFT=true
cd $HOME

# Start extra applications
if [ -x $(which nm-applet) ]
then
    nm-applet &
fi

if [ -x $(which xscreensaver) ]
then
    xscreensaver &
fi

if [ -x $(which dunst) ]
then
    dunst &
fi

# Set Trackpoint sensitivity
tpset() {
	xinput set-prop "TPPS/2 IBM TrackPoint" "$@";
}
#tpset "Device Accel Constant Deceleration" 0.4

# Restore wallpaper
if [ -x $(which feh) -a -x ~/.fehbg ]
then
    ~/.fehbg &
fi

# Start WM
exec /usr/bin/ssh-agent /usr/bin/i3 2>~/.xerrors.log
#exec /usr/bin/ssh-agent /usr/bin/pekwm 2>~/.xerrors.log
