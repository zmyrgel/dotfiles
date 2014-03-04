#!/bin/sh
#
# Add this to your menu, if you have pekwm's dynamic menu support:
#
# SubMenu = "Backgrounds" {
#   Entry = "" { Actions = "Dynamic /path/to/this/file /path/to/gtk system themes (theme)" }
#   Entry = "" { Actions = "Dynamic /path/to/this/file /path/to/gtk user themes (theme)" }
# }
#
# This script take one arg; the directory to parse and automatically
# get images.

# Check usage
if test -z "${1}"; then
    echo "usage: $0 /path/to/theme (theme)";
    exit 1
fi

function write_gtkrc
{
    theme="${1}"
    echo "# -- THEME AUTO-WRITTEN DO NOT EDIT" > ~/.gtkrc-2.0
    echo "include \"${theme}/gtk-2.0/gtkrc\"" >> ~/.gtkrc-2.0
    echo "include \"~/.gtkrc.mine\"" >> ~/.gtkrc-2.0
    echo "# -- THEME AUTO-WRITTEN DO NOT EDIT" >> ~/.gtkrc-2.0
}

if test -z "${2}"; then
    theme_dir="${1}"

    echo "Dynamic {"

    # Check that theme directory exists, if it does not exist create a
    # dummy entry that says the dir does not exist.
    if test -d "${theme_dir}"; then
        ( cd ${theme_dir};
            for theme in *; do
                # Themes must directories with gtk-2.0/gtkrc file
                theme_path="${theme_dir}/${theme}"
                if test -d "${theme}" -a -r "${theme_path}/gtk-2.0/gtkrc"; then
                    echo "Entry = \"${theme}\" { Actions = \"Exec ${0} ${1} ${theme}\" }"
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
        theme="${1}/${2}"
        write_gtkrc "${theme}"
    else
        exit 1
    fi

fi

exit 0
