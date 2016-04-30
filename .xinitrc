### Visual settings ###
xset -b
xsetroot -solid rgb:11/11/11
xsetroot -cursor_name Adwaita

[ -r ~/.Xresources ] && xrdb -merge ~/.Xresources

case $(uname) in
    OpenBSD)
	FONTDIR=/usr/local/share/fonts
	;;
    *)
	FONTDIR=/usr/lib/fonts
	;;
esac
for FONT in $FONTDIR
do
	xset +fp /usr/local/share/fonts/${FONT}
done
xset fp rehash

### Input settings ###
setxkbmap -layout us -variant altgr-intl -option ctrl:nocaps

# Set Trackpoint sensitivity if needed
tpset() {
	xinput set-prop "TPPS/2 IBM TrackPoint" "$@";
}
if [ $(uname) = Linux ]
   tpset "Device Accel Constant Deceleration" 0.4
fi

# Workaround for Firefox Image scaling
MOZ_DISABLE_IMAGE_OPTIMIZE=1
export MOZ_DISABLE_IMAGE_OPTIMIZE

# Environment variables for X
OOO_FORCE_DESKTOP=gnome
GDK_USE_XFT=1
QT_XFT=true
export OOO_FORCE_DESKTOP GDK_USE_XFT QT_XFT

# Set my working dir to $HOME
cd $HOME

### Start extra applications
if [ -x $(which xidle) -a -x $(which xlock) ]
then
	xidle -delay 5 -program "xlock" -timeout 300 &
fi
[ -x $(which nm-applet) ] && nm-applet &
[ -x $(which dunst) ] && dunst &
[ -x $(which wmname) ] && wmname LG3D

# Launch DBUS session
if [ -x $(which dbus-launch) -a -z "${DBUS_SESSION_BUS_ADDRESS}" ]
then
	eval `dbus-launch --sh-syntax --exit-with-session`
fi

# Increase memory limits to 6GB, I got plenty to spare
ulimit -d 6144000

# Restore previous background image, if any
[ -x ~/.fehbg ] && ~/.fehbg &

if [ ! -z "$SSH_AUTH_SOCK" ];
then
	KEYS=$(ls $HOME/.ssh/* | grep -Ev '(authorized_keys|config|known_hosts|pub)$')
	ssh-add $KEYS
fi

# Start WM
exec /usr/bin/ssh-agent i3 2> ~/.xerrors
