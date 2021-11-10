#!perl -T
use 5.012;
use strict;
use warnings;

use Test::More;
use File::Spec::Functions;

use Bio::RNA::BarMap;

my $test_count = 2;
plan tests => $test_count;

my $barfile = catfile qw(t data N1M7_barmap_1.out);
open my $barfh, '<', $barfile
    or BAIL_OUT "failed to open test data file '$barfile'";


SKIP:
{
    # Construct mapping object.
    my $bar_mapping = Bio::RNA::BarMap::Mapping->new($barfh);

    # Map minima to the next file.
    subtest 'Map minima (single step)' => sub {
        plan tests => 5;

        my $from_file = '30.bar';
        my %from_to_min = (     # true mapping from file
            2  => 2,
            20 => 6,
            37 => 39,
            91 => 3,
            41 => 42,
        );

        # Verify each entry.
        while (my ($from_min, $true_to_min) = each %from_to_min) {
            my $to_min = $bar_mapping->map_min_step($from_file, $from_min);
            cmp_ok $to_min, '==', $true_to_min,
                   "min $from_min from $from_file maps to $true_to_min";

            fail 'omg add verification of mapping type'
        }
    };

    # Map to an arbitrary file (in forward direction!)
    subtest 'Map minima (multiple step)' => sub {
        plan tests => 5;
        my $from_file = '30.bar';
        my $to_file   = '33.bar';
        my %from_to_min = (     # true mapping from file
            2  => 4,
            20 => 3,
            37 => 59,
            91 => 5,
            41 => 63,
        );

        # Verify each entry.
        while (my ($from_min, $true_to_min) = each %from_to_min) {
            my $to_min = $bar_mapping->map_min($from_file, $from_min, $to_file);
            cmp_ok $to_min, '==', $true_to_min,
                   "min $from_min ($from_file) maps to $true_to_min ($to_file)";
        }
    };

    #  # Verify mapping type
    #  my ($mapping_type, $to_min) = $mapping->map_min_step($from_file, $from_min);
    #  print "Min $from_min from file '$from_file' is mapped",
    #          Bio::RNA::BarMap::Mapping::Type->is_exact( $mapping_type) ? 'exactly'
    #        : Bio::RNA::BarMap::Mapping::Type->is_approx($mapping_type) ? 'approximately'
    #        : 'with unknown mapping type',
    #        "\n"
    #        ;
    #  print "Mapping arrow: ",
    #        Bio::RNA::BarMap::Mapping::Type->type_to_arrow($mapping_type), "\n";


    # # Check we found all bar files in the mapping file, and their names are
    # # non-zero.
    # can_ok $bar_mapping, 'mapped_files';
    # my @barriers_files      = @{ $bar_mapping->mapped_files };
    # my $barriers_file_count = @barriers_files;
    # cmp_ok $barriers_file_count,
    #        '==',
    #        $expected_barriers_file_count,
    #        "Barriers file count",
    #        ;

    # cmp_ok scalar( grep {length $_ > 0} @barriers_files ),
    #        '==',
    #        $expected_barriers_file_count,
    #        "Barriers file names are defined and non-empty",
    #        ;

    # # Check map_file
    # can_ok $bar_mapping, 'map_file';

    # is $bar_mapping->map_file('10.bar'),
    #    '11.bar',
    #    'map_file(): file 10.bar maps to 11.bar'
    #    ;

    # isnt defined $bar_mapping->map_file('39.bar'),
    #    'map_file(): file 39.bar has no successor'
    #    ;
}

