### Visual settings ###
xset -b

[ -r ~/.Xresources ] && xrdb ~/.Xresources

case $(uname) in
    OpenBSD)
	FONTDIR=/usr/local/share/fonts
	;;
    *)
	FONTDIR=/usr/share/fonts
	;;
esac
for FONT in $(ls $FONTDIR)
do
	xset +fp ${FONTDIR}/${FONT}
done
xset fp rehash

### Input settings ###
setxkbmap -layout us -variant altgr-intl -option ctrl:nocaps

# Set Trackpoint sensitivity if needed
if [ $(uname) = Linux -a $(dmesg | grep -q 'ThinkPad T460s') ]
then
    xinput set-prop "TPPS/2 IBM TrackPoint" "Device Accel Constant Deceleration" 0.5
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
xlock_bin=$(which xlock)
if [ -x $(which xidle) -a ! -z "$xlock_bin" ]
then
	xidle -delay 3 -se -program "$xlock_bin" -timeout 600 &
fi
[ -x $(which nm-applet) ] && nm-applet &
[ -x $(which dunst) ] && LC_ALL=C dunst &
[ -x $(which wmname) ] && wmname LG3D

# Fix fonts in Java applications
_JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=on -Dswing.aatext=true -Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel'
export _JAVA_OPTIONS

# Launch DBUS session
if [ -x $(which dbus-launch) -a -z "${DBUS_SESSION_BUS_ADDRESS}" ]
then
	eval `dbus-launch --sh-syntax --exit-with-session`
fi

# Increase memory limits to 6GB, I got plenty to spare
ulimit -d 6144000

# Restore previous background image, if any
[ -x ~/.fehbg ] && ~/.fehbg &

# ensure ssh-agent is running
# TODO: ensure $(ssh-agent -s -k) is run on exit
if [ -z "$SSH_AGENT_PID" ]
then
	if [ -x $(which ssh-agent) ]
	then
		eval $(ssh-agent -s)
	fi
fi

if [ ! -z "$SSH_AUTH_SOCK" ]
then
	KEYS=$(ls $HOME/.ssh/* | grep -Ev '(authorized_keys|config|known_hosts|pub)$')
	ssh-add $KEYS
fi

# Displays, T430s LVDS1, T460s
# If we're docked, poweroff the internal display
if xrandr --query | grep -q 'DP2-2 connected'
then
        xrandr --output eDP1 --off
fi

# Start WM
exec /usr/bin/ssh-agent pekwm 2> ~/.xerrors
