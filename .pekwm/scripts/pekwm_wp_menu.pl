#!/usr/bin/env perl
#
# Idea based on genfluximages Copyright © 2003 by Steven Grafton
# Script Based on the pekwm_themeset.pl script Copyright © 2003 by the pekwm development team
# Small bits added/changed to show wallpapers by Rds
# Recursive and link bit added by Enrique < http://welcome.to/Webbench > with the help of linux_weenie
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
# This script take one arg; the directory to parse and automatically get images from (and sub directories)

use warnings;
use strict;
use 5.010;

use File::Basename;

my @exts = qw(.png .jpeg .jpg .gif);

my $bgset_cmd = "feh --bg-scale ";

if (@ARGV == 1) {
    print "Dynamic {\n";
    listpics($ARGV[0]);
    print "}\n";
}

sub listpics {
    my $dir = shift;
    opendir my $dh, $dir or die "Can't opendir $dir: $!";

    foreach my $file (readdir $dh) {
        if (-d "$dir/$file") {
            next if $file eq "." or $file eq "..";
            print "Sub menu = \"$file\" {\n";
            listpics("$dir/$file");
            print "}\n";
        } elsif (-f "$dir/$file") {
            my ($name, $path, $suffix) = fileparse("$dir/$file", qr/\.[^.]*/);
            next unless grep {$_ eq $suffix} @exts;
            print "Entry = \"$file\" { Actions = \"Exec $bgset_cmd $dir/$file;Exec ln -fs $dir/$file $ENV{HOME}/.pekwm/current\" }\n";
        }
    }

    closedir $dh;
}
