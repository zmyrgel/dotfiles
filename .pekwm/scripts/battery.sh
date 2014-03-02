#!/bin/sh
# little battery script for pekwm by thuban <http://forum.ubuntu-fr.org/profile.php?id=60044>
# usage
echo "Dynamic {"
echo "Submenu = \"power\" {"
echo "  Entry = \" $(acpi -b | awk '{print $4}' | sed 's/,//g')\" { Actions = \"Exec source /dev/null & \" }"
echo "  Entry = \" $(acpi -b | awk '{print $5}' | sed 's/,//g') restant\" { Actions = \"Exec source /dev/null & \" }"
echo "}"
exit;
