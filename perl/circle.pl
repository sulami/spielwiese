#!/usr/bin/env perl

use strict;
use warnings;

print("Enter circle radius: ");

my $radius = <STDIN>;

my $c = $radius * 3.1415 * 2;
my $a = $radius * $radius * 3.1415;

print("Circle radius is: $c\n");
print("Circle area is: $a\n");
