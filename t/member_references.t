#! /usr/local/bin/perl -w

use lib qw(./t);
use strict;
use Test_Framework;

# Test all basic member types:
#   --	Create object instance.
#   --	Access existing values using all relevant accessors
#   --	Modify existing values using all relevant accessors
use A_Class;
A_Class::init();

use Class::Generate qw(&class);

Test {
    class
	All_Member_Types => {
	    Scalar => "\$",
	    Array  => '@',
	    Hash   => '%',
	    Scalar_Class_1 => 'A_Class',
	    Scalar_Class_2 => '$A_Class',
	    Array_Class    => '@A_Class',
	    Hash_Class     => '%A_Class'
	};
};

use vars qw($v);

Test { $v = new All_Member_Types };

Test { $v->Scalar(1); $v->Scalar == 1 };
Test { $v->undef_Scalar; ! defined $v->Scalar };

Test {
    $v->Array([1, 2, 3, 4]);
    for ( 1..4 ) { die unless $v->Array($_-1) == $_ }
    1
};
Test {
    $v->add_Array(5);
    $v->add_Array(6, 7, 8);
    $v->add_Array;
    $v->Array(8, 9);
    $v->Array(0, 88);
    ($v->Array_size == 8 &&
     join(',', $v->Array) eq '88,2,3,4,5,6,7,8,9' &&
     $v->last_Array == 9)
};
Test { $v->undef_Array; $v->Array_size == -1 && ! defined $v->last_Array };

Test {
    $v->Hash({ e1 => 1, e2 => 2 });
    (join(',', sort { $a cmp $b } $v->Hash_keys) eq 'e1,e2' &&
     join(',', sort { $a <=> $b } $v->Hash_values) eq '1,2' &&
     $v->Hash('e1') == 1 && $v->Hash('e2') == 2 &&
     join(',', sort { $a cmp $b } $v->Hash) eq join(',', sort { $a cmp $b } ('e1', 'e2', 1, 2)))
};

Test { $v->undef_Hash; ! defined $v->Hash };

Test { $v->Scalar_Class_1(new A_Class); $v->Scalar_Class_1->value == 1 };
Test { $v->Scalar_Class_2(new A_Class); $v->Scalar_Class_2->value == 2 };

Test {
    $v->Array_Class([new A_Class, new A_Class]);
    join(',', map $_->value, $v->Array_Class) eq '3,4'
};

Test {
    $v->Array_Class(2, new A_Class);
    join(',', map $_->value, $v->Array_Class) eq '3,4,5';
};
Test {
    $v->Hash_Class({ e1 =>  new A_Class, e2 => new A_Class });
    my @keys = sort { $a cmp $b } $v->Hash_Class_keys;
    join(',', map $v->Hash_Class($_)->value, @keys) eq '6,7'
};
Test {
    $v->Hash_Class('e3', new A_Class);
    my @keys = sort { $a cmp $b } $v->Hash_Class_keys;
    join(',', map $v->Hash_Class($_)->value, @keys) eq '6,7,8'
};

Report_Results;
