#!/usr/bin/env perl

use strict;
use warnings;

my %times;
my %remains;
my $tp;
my $t;
my $td;
foreach my $line (<STDIN>) {
   my ($time,$msg) = $line =~ m{^(\S+)\s(.*)$};
   next if ( ! defined $time);
   my ($h,$m,$s,$ms) = $time =~ m{(\d\d):(\d\d):(\d\d).(\d\d\d)};
   if (defined $h) {
      $t = ((($h*60)+$m)*60)+$s+($ms/1000.0);
      $td = 0.0; if (defined $tp) { $td = $t - $tp; }
      $tp = $t;
#      printf "%8.3f %6.1fs\t%s\n",$t,$td,$msg;
   }
   
   my ($tag,$r) = $msg =~ m{^(\S+\s+\S+)\s+(.*)$};
   if (defined $tag) {
      $tag =~ s/\d+//g;
      $times{$tag} += $td;
      $remains{$tag} = $r;
   }

}

print "\n------\n";

my $tot = 0.0;
if (%times) {
   for (sort keys %times) {
      $tot += $times{$_};
      printf "%-25s %8.3f (%s)\n", $_,$times{$_},$remains{$_};
   }
}
print  "------------------------- --------\n";
printf "%-25s %8.3f (%.3f min)\n", "total",$tot,$tot/60.0;
