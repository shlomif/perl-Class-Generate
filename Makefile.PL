use strict;
use warnings;

use 5.010;
use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    NAME => 'Class::Generate',
    VERSION_FROM => 'lib/Class/Generate.pm',
    dist => {
        COMPRESS => 'gzip',
        SUFFIX   => 'gz'
    },
    (($ExtUtils::MakeMaker::VERSION >= 6.46)
        ? (
            META_MERGE => {
                "meta-spec" => { version => 2 },
                resources => {
                    repository => {
                        type => 'git',
                        url => 'https://github.com/shlomif/perl-Class-Generate.git',
                        web => 'https://github.com/shlomif/perl-Class-Generate',
                    },
                },
            },
        )
        :
        ()
    ),
);
