#!/usr/bin/env perl
################## System PekWM Pipe Menu by Bladdo #######################
#Bunch of system information
#Comment out what you want if you dont want it there
####How to install ##########
#1.Chmod this file +x
#
#2.Copy this file to your scripts directory ex: /usr/share/pekwm/scripts
#
#3.Finally add the following lines to your pekwm menu config
#
#SubMenu = "System" {
#	Entry = { Actions = "Dynamic /usr/share/pekwm/scripts/system.pl" }
#}
##############################################################################
#
#Made by Bladdo - Bladdo.net - Bladdo@bladdo.net
#Feel Free to edit in any way you wish                         
########################################################################################
# changelog : mod by arpinux 2009
# - remove ram section
# - add htop command to cpu menu
##starts menu#######
print "Dynamic {\n";

###processes section ---> shows processes by order of cpu usage###
#($pa,$pb) = split(/COMMAND\n/,`ps -e -o pcpu,cpu,nice,state,cputime,args --sort pcpu | sed '/^ 0.0 /d'`);
#($pa,$pb) = split(/COMMAND\n/,`ps -e -o pcpu,cpu,nice,state,cputime,args -r | sed '/^ 0.0 /d'`);
(@array) = split(/\n/,$pb);

print "Submenu = \"CPU\" {\n";

foreach $temp (@array) {
        $temp =~ s/^ //;
        ($percent,$process) = split(/ .{19}/,$temp);
	if ($process =~ /\//) {
        	($pops,$use) = split(/\/(?!.*\/)/,$process);
	} else {
		$use = $process;
	}	
	print "Entry = \"$use:$percent" . "%\" { Actions = \"Exec \$TERM -e top &\" }\n";

}

print "}\n";	
 
###end processes section############
	

##end dynamic menu##
print "}\n";


