#! /usr/local/bin/perl -w

use lib qw(./t);
use strict;
use Test_Framework;
use Class::Generate qw(&class &subclass);

# Test the different constructor styles (key/value, positional, mix).

use constant SPEC => { type => "\$", readonly => 1, required => 1 };

sub members_valid($$) {
    my ($obj, $max) = @_;
    for ( my $i = 1; $i <=  $max; $i++ ) {
	return 0 if ! eval "\$obj->m$i == $i";
    }
    return 1;
}

Test {
    class KV_Class => {
	map(($_ => SPEC), qw(m1 m2 m3))
    };
    class Pos_Class => {
	map(($_ => SPEC), qw(m1 m2 m3)),
	new => { style => 'positional m1 m2 m3' }
    };
    class Mix_Class => {
	map(($_ => SPEC), qw(m1 m2 m3 m4)),
	new => { style => 'mix m1 m2' }
    };
    1
};

Test { members_valid KV_Class->new(m1 => 1, m2 => 2, m3 => 3), 3 };
Test { members_valid KV_Class->new(m2 => 2, m1 => 1, m3 => 3), 3 };
Test { members_valid Pos_Class->new(1, 2, 3), 3 };
Test { members_valid Mix_Class->new(1, 2, m3 => 3, m4 => 4), 4 };
Test { members_valid Mix_Class->new(1, 2, m4 => 4, m3 => 3), 4 };

Test {
    subclass KV_Subclass => {
	map(($_ => SPEC), qw(m4))
    }, -parent => 'KV_Class';
    subclass Pos_Subclass => {
	map(($_ => SPEC), qw(m4)),
	new => { style => 'positional m4' }
    }, -parent => 'Pos_Class';
    subclass Mix_Subclass => {
	map(($_ => SPEC), qw(m5 m6)),
	new => { style => 'mix m5' }
    }, -parent => 'Mix_Class';
    1
};

Test { members_valid KV_Subclass->new(m1 => 1, m2 => 2, m3 => 3, m4 => 4), 4 };
Test { members_valid Pos_Subclass->new(4, 1, 2, 3), 4 };
Test { members_valid Mix_Subclass->new(5, m6 => 6, 1, 2, m3 => 3, m4 => 4), 6 };

Report_Results;
