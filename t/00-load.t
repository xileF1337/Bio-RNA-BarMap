#!perl -T
use 5.012;
use strict;
use warnings;
use Test::More;

plan tests => 1;

BEGIN {
    my $module = 'Bio::RNA::BarMap';
    use_ok($module) || BAIL_OUT "Module $module could not be loaded.";
    # use_ok( 'Bio::RNA::BarMap' ) || print "Bail out!\n";
}

diag( "Testing Bio::RNA::BarMap $Bio::RNA::BarMap::VERSION, Perl $], $^X" );

