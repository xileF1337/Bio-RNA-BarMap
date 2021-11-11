#!perl -T
use 5.012;
use strict;
use warnings;

use Test::More;
use File::Spec::Functions;

my $test_count = 6;
plan tests => $test_count;

use Bio::RNA::BarMap;

my $barfile = catfile qw(t data N1M7_barmap_1.out);

open my $barfh, '<', $barfile
    or BAIL_OUT "failed to open test data file '$barfile'";

my $bar_mapping = Bio::RNA::BarMap::Mapping->new($barfh);

my $expected_barriers_file_count = 32;

SKIP:
{
    # Check we found all bar files in the mapping file, and their names are
    # non-zero.
    can_ok $bar_mapping, 'mapped_files';
    my @barriers_files      = $bar_mapping->mapped_files;
    my $barriers_file_count = @barriers_files;
    cmp_ok $barriers_file_count,
           '==',
           $expected_barriers_file_count,
           "Barriers file count",
           ;

    cmp_ok scalar( grep {length $_ > 0} @barriers_files ),
           '==',
           $expected_barriers_file_count,
           "Barriers file names are defined and non-empty",
           ;

    # Check map_file
    can_ok $bar_mapping, 'map_file';

    is $bar_mapping->map_file('10.bar'),
       '11.bar',
       'map_file(): file 10.bar maps to 11.bar'
       ;

    isnt defined $bar_mapping->map_file('39.bar'),
       'map_file(): file 39.bar has no successor'
       ;
}

