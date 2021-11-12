#!perl -T
use 5.012;
use strict;
use warnings;

use Test::More;
use Test::Exception;
use File::Spec::Functions;

use Bio::RNA::BarMap;

my $test_count = 5;
plan tests => $test_count;

my $barfile = catfile qw(t data N1M7_barmap_1.out);
open my $barfh, '<', $barfile
    or BAIL_OUT "failed to open test data file '$barfile'";


SKIP:
{
    # Construct mapping object.
    my $bar_mapping = Bio::RNA::BarMap::Mapping->new($barfh);

    subtest 'Mapping provides mapping methods' => sub {
        plan tests => 2;

        can_ok $bar_mapping, 'map_min_step';
        can_ok $bar_mapping, 'map_min';
    };

    # Map minima to the next file.
    subtest 'Map minima, single step' => sub {
        my $from_file   = '30.bar';
        my %from_to_min = (             # true mapping from file
            2  => 2,
            20 => 6,
            37 => 39,
            91 => 3,
            41 => 42,
        );
        plan tests => 1 * %from_to_min;

        # Verify each entry.
        while (my ($from_min, $true_to_min) = each %from_to_min) {
            my $to_min = $bar_mapping->map_min_step($from_file, $from_min);
            cmp_ok $to_min, '==', $true_to_min,
                   "min $from_min from $from_file maps to $true_to_min";
        }
    };

    # Try to map non-existent minima and files (single-step)
    subtest 'Map minima, single step: exceptions' => sub {
        plan tests => 2;

        # Non-mapped Barriers file.
        throws_ok {$bar_mapping->map_min_step('foo.bar', 2)}
                  qr{File '.*' not found in mapping},
                  'Non-existent source bar file throws';
        throws_ok {$bar_mapping->map_min_step('9.bar', 123456)}
                  qr{Minimum .* not found in file '.*'},
                  'Non-existent minimum throws';
    };

    # Map to an arbitrary file (in forward direction!)
    subtest 'Map minima, multi-step' => sub {
        my $from_file = '30.bar';
        my $to_file   = '33.bar';
        my %from_to_min = (     # true mapping from file
            2  => 4,
            20 => 3,
            37 => 59,
            91 => 5,
            41 => 63,
        );
        plan tests => 1 * %from_to_min;

        # Verify each entry.
        while (my ($from_min, $true_to_min) = each %from_to_min) {
            my $to_min = $bar_mapping->map_min($from_file, $from_min, $to_file);
            cmp_ok $to_min, '==', $true_to_min,
                   "min $from_min from $from_file maps to $true_to_min in $to_file";
        }
    };

    # Try to map non-existent minima and files (multi-step)
    subtest 'Map minima, multi-step: exceptions' => sub {
        plan tests => 3;

        throws_ok {$bar_mapping->map_min('foo.bar', 1, '10.bar')}
                  qr{Cannot map file '.*': not contained in BarMap file},
                  'Non-existent source bar file throws';

        throws_ok {$bar_mapping->map_min('9.bar', 1, 'foo.bar')}
                  qr{File '.*' is not mapped to file '.*'},
                  'Non-existent target bar file throws';

        throws_ok {$bar_mapping->map_min('9.bar', 1234567, '12.bar')}
                  qr{Minimum .* not found in file '.*'},
                  'Non-existent minimum throws';
    };
}

# End of t/20-map_states.t
