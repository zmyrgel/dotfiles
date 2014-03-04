#!/bin/sh
#
# Add this to your menu, if you have pekwm's dynamic menu support:
#
# SubMenu = "Backgrounds" {
#   Entry = "" { Actions = "Dynamic /path/to/this/file /path/to/wallpapers" }
# }
#
# If you add (without the #):
#    qiv -x -e -n ~/.pekwm/current
# to ~/.pekwm/start file, the last background should be saved
# across different sessions too!
#
# This script take one arg; the directory to parse and automatically
# get images.

function wallpaper_feh
{
    feh --bg-scale "$1"
}

function wallpaper_qiv
{
    qiv -z "$1"
}

function wallpaper_imagemagick
{
    import -window root "$1"
}

function is_in_path
{
    which $1 >/dev/null 2>&1
    return $?
}

function detect_command
{
    is_in_path "feh"
    if test $? -eq 0; then
        command="feh"
        return
    fi

    is_in_path "qiv"
    if test $? -eq 0; then
        command="qiv"
        return
    fi

    is_in_path "display"
    if test $? -eq 0; then
        command="display"
        return
    fi
}

function usage
{
    echo "usage: ${0} WALLPAPER_DIR WALLPAPER"
    echo ""
    exit 0
}

function usage_command
{
    echo "Unable to find any supported commands for setting wallpaper"
    echo ""
    echo "Supported commands are:"
    echo ""
    echo "  * feh, http://feh.finalrewind.org/"
    echo "  * qiv, http://spiegl.de/qiv/"
    echo "  * imagemagick, http://www.imagemagick.org/"
    echo ""
    exit 1
}

# Check usage
if test -z "${1}"; then
    usage
    exit 1
fi

command=""

detect_command
if test -z "${command}"; then
    usage_command
fi

if test -z "${2}"; then

    wp_dir="${1}"

    echo "Dynamic {"

    # Check that wallpaper directory exists, if it does not exist create a
    # dummy entry that says the dir does not exist.
    if test -d "${wp_dir}"; then
        ( cd ${wp_dir};
            for wallpaper in *; do
                # Wallpapers must readable files with image extension.
                if test -r "${wallpaper}" && echo "${wallpaper#*.}" | egrep -q 'png|jpeg|gif|jpg'; then
                    echo "Entry = \"${wallpaper}\" { Actions = \"Exec ${0} ${1} ${wallpaper}\" }"
                fi
            done )
    else
        echo "Entry = \"No such directory ${wp_dir}\" { Actions = \"None\" }"
    fi

    echo "}"
else
    # Check for configuration file, if the environment is not set the
    # script is not being run from pekwm, then exit with failure.
    if test -f "${PEKWM_CONFIG_FILE}"; then
        wallpaper="${1}/${2}"
        # Set wallpaper
        case "${command}" in
            feh)
                wallpaper_feh "${wallpaper}"
                ;;
            qiv)
                wallpaper_qiv "${wallpaper}"
                ;;
            magick)
                wallpaper_magick "${wallpaper}"
                ;;
            *)
                usage
                ;;
        esac

        ln -fs "${wallpaper}" "${HOME}/.pekwm/current"

    else
        exit 1
    fi

fi

exit 0
