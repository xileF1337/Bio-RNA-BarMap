package Bio::RNA::BarMap;
our $VERSION = '0.01';

use 5.012;
use warnings;

use Bio::RNA::BarMap::Mapping;
use Bio::RNA::BarMap::Mapping::Set;
use Bio::RNA::BarMap::Mapping::MinMappingEntry;
use Bio::RNA::BarMap::Mapping::Type;
use Bio::RNA::BarMap::Mapping::FileMappingEntry;

1;

__END__

=pod

=encoding UTF-8

=head1 NAME

Bio::RNA::BarMap - Parse and query I<BarMap> mappings.

=head1 SYNOPSIS

    use v5.12;              # for 'say()' and '//' a.k.a. logical defined-or
    use Bio::RNA::BarMap;

    # Parse a BarMap output file.
    my $barmap_file = 'N1M7_barmap_1.out';     # e.g. the test data in t/data/
    my $mapping = Bio::RNA::BarMap::Mapping->new($barmap_file);

    # Print mapped Barriers files.
    say join q{ }, $mapping->mapped_files;

    # Map a file.
    my $from_file = '8.bar';            # one of the mapped .bar files
    my $to_file = $mapping->map_file($from_file);   # last file maps to undef
    say "$from_file is mapped to ", $to_file // "nothing";

    # Map minimum 2 from file 8.bar to the next file.
    my $from_min = 2;
    my $to_min = $mapping->map_min_step($from_file, $from_min);

    # Map to an arbitrary file (only forward direction!)
    $to_file = '15.bar';
    $to_min = $mapping->map_min($from_file, $from_min, $to_file);

    # Verify mapping type: is the mapping exact (->) or approximate (~>)?
    my ($type, $to_min2) = $mapping->map_min_step($from_file, $from_min);
    say "Min $from_min from file '$from_file' is mapped ",
         $type->is_exact  ? 'exactly' : 'approximately', " to min $to_min2";
    say "Mapping arrow: ", $type->arrow;

=head1 DESCRIPTION

This module provides auxiliary classes to parse, query and print the mapping
file generated by the RNA kinetics simulation tool I<BarMap>, developed
at the Institute of Theoretical Biochemistry (TBI) in Vienna.

Note that this module is B<not> developed and maintained by the authors of
I<BarMap>.

=head1 CLASSES & METHODS

Bio::RNA::BarMap provides several classes for working with I<BarMap>'s output.
The most relevant is L<Bio::RNA::BarMap::Mapping>, which is used to parse the
mapping file generated by I<BarMap>. The constructor C<new()> takes a path or
handle to a I<BarMap> file and creates a new mapping object, which is used to
perform all kinds of queries on the mapping file.

The mapping of a single state to the next I<Barriers> file can be either exact
or approximate, as is indicated by a straight (C<-E<gt>>) or curved
(C<~E<gt>>) arrow in the mapping file. To provide this information, the
C<map_min()> and C<map_min_step()> methods of the mapping object -- when
called in a list context -- return not only the target minimum of the mapping,
but also an object of type L<Bio::RNA::BarMap::Mapping::Type>. The type object
can be queried using its methods C<is_exact()> and C<is_approx()>, and also
converted back to its arrow representation using C<arrow()>.

For more information, please refer to the documentation of the individual
classes.


=head1 AUTHOR

Felix Kuehnl, C<< <felix at bioinf.uni-leipzig.de> >>


=head1 BUGS

Please report any bugs or feature requests to C<bug-bio-rna-barmap at
rt.cpan.org>, or through the web interface at
L<https://rt.cpan.org/NoAuth/ReportBug.html?Queue=Bio-RNA-BarMap>.  I will be
notified, and then you'll automatically be notified of progress on your bug as
I make changes.


=head1 SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc Bio::RNA::BarMap


You can also look for information at the official BarMap website:

L<https://www.tbi.univie.ac.at/RNA/bar_map/>


=over 4

=item * RT: CPAN's request tracker (report bugs here)

L<https://rt.cpan.org/NoAuth/Bugs.html?Dist=Bio-RNA-BarMap>

=item * AnnoCPAN: Annotated CPAN documentation

L<http://annocpan.org/dist/Bio-RNA-BarMap>

=item * CPAN Ratings

L<https://cpanratings.perl.org/d/Bio-RNA-BarMap>

=item * Search CPAN

L<https://metacpan.org/release/Bio-RNA-BarMap>

=back


=head1 LICENSE AND COPYRIGHT

Copyright 2019 Felix Kuehnl.

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see L<http://www.gnu.org/licenses/>.


=cut

# End of Bio/RNA/BarMap.pm

