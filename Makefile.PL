use 5.010;
use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    NAME	 => 'Class::Generate',
    VERSION => '1.11',
    dist => {
	COMPRESS => 'gzip',
	SUFFIX   => 'gz'
    },
);
