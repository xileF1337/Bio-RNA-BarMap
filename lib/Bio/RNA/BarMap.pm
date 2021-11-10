package Bio::RNA::BarMap;
our $VERSION = '0.01';

use 5.012;
use warnings;

use Bio::RNA::BarMap::Mapping;

# A simple set class implemented using a hash. Supports storing references.
# Faster then Set::Scalar for this specific use case. It significantly reduces
# the runtime.
package Bio::RNA::BarMap::Mapping::Set {
    use warnings;
    use v5.12;
    use autodie qw(:all);

    use Moose;
    use MooseX::StrictConstructor;
    use namespace::autoclean;
    use List::Util qw(pairmap);

    # Elements are stored in a hash ref. For simple values, key is the element
    # and value is undef. For references, the key gets stringified and the
    # value stores the actual reference.
    has _elems => (is => "ro", init_arg => undef, default => sub { {} });

    # Return all elements. If defined, use the value, else the key.
    sub elements { pairmap {$b // $a} %{ $_[0]->_elems } }

    # Insert elements into the set. Returns itself.
    sub insert {
        my $self = shift;
        # Don't store simple values twice, but preserve references.
        $self->_elems->{$_} = ref $_ ? $_ : undef foreach @_;
        $self;
    }

    __PACKAGE__->meta->make_immutable;
};  # Bio::RNA::BarMap::Mapping::Set


# Min mapping hash entry. "to" and "from" are hash references
package Bio::RNA::BarMap::Mapping::MinMappingEntry {
    use Moose;
    use namespace::autoclean;

    use Scalar::Util qw( weaken );
    use Set::Scalar;

    has 'index' => (is => 'ro', required => 1);

    has 'to_type' => (
        is => 'rw',
        isa => Moose::Meta::TypeConstraint::Enum->new(
                    name   => 'MappingType',
                    values => [
                                Bio::RNA::BarMap::Mapping::Type->exact,
                                Bio::RNA::BarMap::Mapping::Type->approx,
                              ],
               ),
        # Maybe use enum 'EnumName', qw(val1 val2); from
        # Moose::Util::TypeConstraints
    );

    # Ensure object is cleaned after use => use weak refs
    # Init 'from' with empty array ref to immedtialey push to it.
    # has '_from' => (is => 'ro', default => sub { Set::Scalar->new() });
    has '_from' => (is => 'ro', default => sub { Bio::RNA::BarMap::Mapping::Set->new });
    has 'to'    => (is => 'rw', weak_ref => 1, predicate => 'has_to');

    # Always use this method to add 'from' minima. This ensures the refs
    # are weakened and no memory leaks arise.
    sub add_from {
        my ($self, @from) = @_;
        weaken $_ foreach @from;        # turn into weak references
        $self->_from->insert(@from);
    }

    sub get_from {
        my ($self) = @_;
        my @from = $self->_from->elements;
        return @from;
    }

    __PACKAGE__->meta->make_immutable;
}   # Bio::RNA::BarMap::Mapping::MinMappingEntry


# All methods are class methods, it's not meant to be instantiated. It just
# manages the type name string and converts it to arrows, etc.
package Bio::RNA::BarMap::Mapping::Type {
    use Moose;
    use namespace::autoclean;

    # Returns the string used to characterize mappings as exact.
    sub exact  { return __PACKAGE__ . '::EXACT';  }

    # Returns the string used to characterize mappings as approximate.
    sub approx { return __PACKAGE__ . '::APPROX'; }

    # Prevent instantiation.
    sub BUILD  { confess 'Not meant to be instantiated, or is it...?!'; }

    # Returns true iff the passed string matches the EXACT type.
    sub is_exact {
        my ($class, $other) = @_;
        my $is_exact = $other eq __PACKAGE__->exact;
        return $is_exact;
    }

    # Returns true iff the passed string matches the APPROX type.
    sub is_approx {
        my ($class, $other) = @_;
        my $is_approx = $other eq __PACKAGE__->approx;
        return $is_approx;
    }

    # Convert arrows -> and ~> to the appropriate type (exact, approx, resp.).
    # Arguments:
    #   arrow: Arrow to convert.
    # Returns the type name represented by the given arrow.
    sub arrow_to_type {
        my ($class, $arrow) = @_;
        if ($arrow eq '->') {
            return __PACKAGE__->exact;
        }
        elsif ($arrow eq '~>') {
            return __PACKAGE__->approx;
        }
        else {
            confess "Invalid arrow '$arrow'";
        }
    }

    # Convert a given type name to the appropriate arrows -> and ~> (for the
    # exact and approximate type, resp.).
    # Arguments:
    #   type: Name of the type to convert.
    # Returns the arrow representing given type name.
    sub type_to_arrow {
        my ($class, $type) = @_;
        if ($type eq __PACKAGE__->exact) {
            return '->';
        }
        elsif ($type eq __PACKAGE__->approx) {
            return '~>';
        }
        else {
            confess "Invalid type '$type'";
        }
    }
    __PACKAGE__->meta->make_immutable;
}   # Bio::RNA::BarMap::Mapping::Type


# Mini class for entries of the file mapping hash
package Bio::RNA::BarMap::Mapping::FileMappingEntry {
    use Moose;
    use namespace::autoclean;

    has 'name' => (is => 'ro', required => 1);
    # Ensure object is cleaned after use => use weak refs
    has [qw(from to)] => (is => 'rw', weak_ref => 1);

    __PACKAGE__->meta->make_immutable;
}   # Bio::RNA::BarMap::Mapping::FileMappingEntry


__END__

=head1 NAME

Bio::RNA::BarMap - A class for parsing and working with BarMap output.

=head1 VERSION

Version 0.01

=cut

=head1 SYNOPSIS

This module provides auxiliary classes to parse, query and print the results
file generated by the RNA kinetics simulation tool BarMap.

    use Bio::RNA::BarMap;

    # Read in a BarMap output file.
    my $barmap_file = 'barmap.out';
    my $mapping = Bio::RNA::BarMap::Mapping->new($barmap_file);

    # Print mapped Barriers files.
    print join(q{ }, @{ $mapping->mapped_files }), "\n";

    # Map a file.
    my $from_file = 'some.bar';
    my $to_file = $mapping->map_file($from_file);
    if (defined $to_file) {
        print "$from_file is mapped to $to_file\n";
    }

    # Map a minimum to the next file
    my $from_min = 3;             # minimum No. 3
    my $to_min = $mapping->map_min_step($from_file, $from_min);

    # Map to an arbitrary file (in forward direction!)
    my $to_min = $mapping->map_min($from_file, $from_min, $to_file);

    # Verify mapping type
    my ($mapping_type, $to_min) = $mapping->map_min_step($from_file, $from_min);
    print "Min $from_min from file '$from_file' is mapped",
            Bio::RNA::BarMap::Mapping::Type->is_exact( $mapping_type) ? 'exactly'
          : Bio::RNA::BarMap::Mapping::Type->is_approx($mapping_type) ? 'approximately'
          : 'with unknown mapping type',
          "\n"
          ;
    print "Mapping arrow: ",
          Bio::RNA::BarMap::Mapping::Type->type_to_arrow($mapping_type), "\n";



=head1 Methods of C<Bio::RNA::BarMap::Mapping>

Stores the actual mapping file created by BarMap. Can map minima from one
Barriers file to another.

=head2 C<first_mapped_file()>

Returns the name of the first mapped Barriers file of this BarMap
mapping.

=head2 C<path_of_file()>

Given a Barriers file, return its full path. The file is assumed to
reside in the same directory as the BarMap mapping file.

Arguments:

=over

=item barriers_file

The Barriers file for which the path is to be computed.

=back

Returns the path to the given barriers file.

=head2 C<first_mapped_path()>

Returns the path to the first mapped Barriers file of this BarMap
mapping.

=head2 C<map_file()>

Map a Barriers file to its immediate successor file.

Arguments:

=over

=item from_file

Barriers file to be mapped.

=back

Returns the immediate successor Barriers file of C<from_file>, or
undef if C<from_file> points to the last Barriers file.

=head2 C<map_path()>

Given a path to Barriers file, map the Barriers file to its immediate
successor file and return its path.

Arguments:

=over

=item from_path

Path of Barriers file to be mapped.

=back

Returns the path of the Barriers file that C<from_path> was mapped to, or
undef if C<from_path> points to the last Barriers file.

=head2 C<map_file_inv()>

(Inversely) map a Barriers file to its immediate predecessor file.

Arguments:

=over

=item to_file

Barriers file to be mapped to its predecessor.

=back

Returns the immediate predecessor Barriers file of C<to_file>, or undef if
C<to_file> points to the first Barriers file.

=head2 C<map_path_inv()>

Given a path to Barriers file, (inversely) map the Barriers file to its
immediate predecessor file and return its path.

Arguments:

=over

=item to_path

Path of Barriers file to be inversely mapped.

=back

Returns the path of the predecessor Barriers file of C<to_path>, or undef
if C<to_path> points to the first Barriers file.

=head2 C<map_min_step()>

Map a given minimum from a given Barriers file to its corresponding
minimum in the next Barriers file. In list context, the mapping type is
returned, too. The mapping type tells whether a minimum was mapped
exactly (->) or approximately (~>), and is an object of class
C<Bio::RNA::BarMap::Mapping::Type>.

Arguments:

=over

=item from_file

The Barriers file of C<from_min>.

=item from_min

The minimum to be mapped.

=back

In scalar context, returns the corresponding minimum of C<from_min> in the
next Barriers file, or undef if C<from_file> is the last Barriers file of
the mapping. In list context, the mapping type as well as the
corresponding minimum is returned.

=head2 C<map_min()>

Map given minimum from a given Barriers file to its corresponding
minimum in another (successor) Barriers file. In list context, the
mapping type is returned, too. The mapping type tells whether a minimum
was mapped exactly (->) or approximately (~>), and is an object of class
Bio::RNA::BarMap::Mapping::Type. The mapping is considered exact iff all
mapping steps from the initial to the target Barriers file are exact;
otherwise, the mapping is approximate. If the given successor Barriers
file is not actually a successor of the initial Barriers file, an
exception is raised.

Arguments:

=over

=item from_file

The Barriers file of C<from_min>.

=item from_min

The minimum to be mapped.

=item to_file

Target file for which the corresponding minimum is to be determined.

=back

In scalar context, returns the corresponding minimum of from_min in the
target Barriers file. In list context, the mapping type as well as the
corresponding minimum is returned.

=head2 C<get_mapping_type()>

Convenience wrapper for C<map_min()> which only returns the mapping type,
but not the target minimum.

=head2 C<get_mapping_arrow()>

Convenience wrapper for C<map_min()> which only returns the mapping type,
converted to its corresponding arrow string. An exact mapping is
converted to arrow '->', an approximate mapping to arrow '~>'.

=head2 C<map_min_inv_step()>

Get all minima in the preceding Barriers file that map to a given
minimum from a given Barriers file.

Arguments:

=over

=item to_file

The Barriers file of the target minimum.

=item to_min

The target minimum to which the returned minima are mapped.

=back

Returns a list of all minima that are mapped to the target minimum.

=head2 C<map_min_inv()>

Get all minima in a given Barriers file that map to a given
minimum from another Barriers file.

Arguments:

=over

=item to_file

The Barriers file of the target minimum.

=item to_min

The target minimum to which the returned minima are mapped.

=item from_file

The source Barriers file from which the returned minima are.

=back

Returns a list of all minima from the source Barriers file that are
mapped to the target minimum in the target Barriers file.

=head1 Methods of C<Bio::RNA::BarMap::Mapping::Type>

Static utility class to help characterize the mapping of a specific
minimum as exact or approximate. The mapping type itself is given as a string
(not as an object of this class).

=head2 C<exact()>

Returns the string used to characterize mappings as exact.

=head2 C<approx()>

Returns the string used to characterize mappings as approximate.

=head2 C<is_exact()>

Returns true iff the passed string matches the EXACT type.

=head2 C<is_approx()>

Returns true iff the passed string matches the APPROX type.

=head2 C<arrow_to_type()>

Convert arrows -> and ~> to the appropriate type (exact, approx, resp.).

Arguments:

=over

=item arrow

Arrow to convert.

=back

Returns the type name represented by the given arrow.

=head2 C<type_to_arrow()>

Convert a given type name to the appropriate arrows -> and ~> (for the
exact and approximate type, resp.).

Arguments:

=over

=item type

Name of the type to convert.

=back

Returns the arrow representing given type name.

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


=head1 ACKNOWLEDGEMENTS


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

1; # End of Bio::RNA::BarMap

