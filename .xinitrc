### Visual settings ###
xset -b

[ -r ~/.Xresources ] && xrdb ~/.Xresources

# Setup fonts
case $(uname) in
	OpenBSD)
		FONTDIR=/usr/local/share/fonts
		;;
	*)
		FONTDIR=/usr/share/fonts
		;;
esac
for FONT in $(/bin/ls $FONTDIR)
do
	xset +fp ${FONTDIR}/${FONT}
done
[ -x $HOME/.fonts ] && xset +fp $HOME/.fonts
xset fp rehash

### Input settings ###
if dmesg | grep -qi atreus
then
	setxkbmap -layout us -variant altgr-intl -option ctrl:nocaps
else
	setxkbmap -layout us -variant dvorak -option ctrl:nocaps
fi
[ -x ~/bin/fix-keys.sh ] && ~/bin/fix-keys.sh

# Environment variables for X
GDK_USE_XFT=1
QT_XFT=true
export GDK_USE_XFT QT_XFT

# Fix mouse scrolling in GTK3
GDK_CORE_DEVICE_EVENTS=1
export GDK_CORE_DEVICE_EVENTS

# Enable acceleration in firefox
MOZ_ACCELERATED=1
export MOZ_ACCELERATED

# Enable rust renderer in firefox
#MOZ_WEBRENDER=1
#export MOZ_WEBRENDER

# Set my working dir to $HOME
cd $HOME

### Start extra applications
xlock_bin=$(which xlock)
if [ -x $(which xidle) -a ! -z "$xlock_bin" ]
then
	xidle -delay 3 -se -program "$xlock_bin" -timeout 600 &
elif [ -x $(which xscreensaver) ]
then
	xscreensaver -no-splash &
fi
[ -x $(which nm-applet) ] && nm-applet &
[ -x $(which dunst) ] && LC_ALL=C dunst &
[ -x $(which wmname) ] && wmname LG3D

# Fix fonts in Java applications
export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=lcd_hrgb'

# Launch DBUS session
if [ -x $(which dbus-launch) -a -z "${DBUS_SESSION_BUS_ADDRESS}" ]
then
	eval $(dbus-launch --sh-syntax --exit-with-session)
fi

# Increase default memory limit on OpenBSD
if test $(uname) = "OpenBSD"
then
	ulimit -d 6144000
fi

# Restore previous background image, if any
#[ -x ~/.fehbg ] && ~/.fehbg &

if [ -x $(which xwallpaper) ]
then
	xwallpaper --maximize ~/wallpapers/pluto_blueskies.png
fi

# ensure ssh-agent is running
# TODO: ensure $(ssh-agent -s -k) is run on exit
export SSH_AUTH_SOCK=~/.ssh/ssh-agent.$HOSTNAME.sock
if [ -z "$SSH_AGENT_PID" ]
then
	if [ -x $(which ssh-agent) ]
	then
		eval $(ssh-agent -s -a "$SSH_AUTH_SOCK")
	fi
fi

# Setup monitors
if xrandr --query | grep -q 'DP2-2 connected'
then
	xrandr --output eDP1 --off --output DP2-2 --auto
fi

# Start WM
if test $(hostname) = "asteroid.bittivirhe.fi"
then
	exec ssh-agent stumpwm 2> ~/.xerrors
else
	exec ssh-agent icewm-session 2> ~/.xerrors
fi
