### Visual settings ###
xset b off
#xset r off

[ -r ~/.Xresources ] && xrdb -merge ~/.Xresources

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
if dmesg | grep -qi atreus
then
	setxkbmap -layout us -variant altgr-intl -option ctrl:nocaps
else
	setxkbmap -layout us -variant dvorak -option ctrl:nocaps
fi
#[ -r ~/.xmodmap ] && xmodmap ~/.xmodmap
[ -x ~/bin/fix-keys.sh ] && ~/bin/fix-keys.sh

# Set Trackpoint sensitivity if needed
if [ $(uname) = Linux ]
then
	xinput set-prop "TPPS/2 IBM TrackPoint" "Device Accel Constant Deceleration" 0.4
fi

# Enable Firefox acceleration
#export MOZ_ACCELERATED=1
#export MOZ_WEBRENDER=1

# Environment variables for X
OOO_FORCE_DESKTOP=gnome
GDK_USE_XFT=1
QT_XFT=true
export OOO_FORCE_DESKTOP GDK_USE_XFT QT_XFT

LC_CTYPE=en_US.UTF-8
export LC_CTYPE

# Set my working dir to $HOME
cd $HOME

### Start extra applications
xlock_bin=$(which xlock)
if [ -x $(which xidle) -a ! -z "$xlock_bin" ]
then
	xidle -delay 3 -se -program "$xlock_bin" -timeout 300 &
fi
[ -x $(which nm-applet) ] && nm-applet &
#[ -x $(which dunst) ] && LC_ALL=C dunst &
[ -x $(which wmname) ] && wmname LG3D
[ -x $(which redshift) ] && redshift &
#[ -x $(which tint2) ] && tint2 -c ~/.config/tint2/Numix_tint2/dark_taskbar/tint2rc &

# Launch DBUS session
if [ -x $(which dbus-launch) -a -z "${DBUS_SESSION_BUS_ADDRESS}" ]
then
	eval `dbus-launch --sh-syntax --exit-with-session`
fi

# Increase memory limits to 6GB, I got plenty to spare
ulimit -d 6144000

# increase open files


# Restore previous background image, if any
if [ -x $(which xwallpaper) ]
then
	xwallpaper --zoom  wallpapers/image_3323_1e-Plutos-Blue-Skies.jpg
fi

# ensure ssh-agent is running
# TODO: ensure $(ssh-agent -s -k) is run on exit
if [ -z "$SSH_AGENT_PID" ]
then
	if which ssh-agent >/dev/null 2>&1
	then
		eval $(ssh-agent -s)
	fi
fi

#if [ ! -z "$SSH_AUTH_SOCK" ]
#then
#	KEYS=$(ls $HOME/.ssh/* | grep -Ev '(authorized_keys|config|known_hosts|pub)$')
#	ssh-add $KEYS
#fi

# Display T430s LVDS1, T460s eDP-1
if xrandr --query | grep "DP1 connected" >/dev/null
then
	xrandr --output 'eDP-1' --off
fi

#~/bin/lemonbar.sh | lemonbar -g 1920x15 -b -n "lemonbar" -f '-xos4-terminus-*-*-*-*-14-*-*-*-*-*-iso10646-1' &
#~/bin/lemonbar.sh | lemonbar -g 1920x15 -b -n "lemonbar" -f 'tamsyn-16' &

# Start WM
exec /usr/bin/ssh-agent /usr/local/bin/icewm-session 2> ~/.xerrors
