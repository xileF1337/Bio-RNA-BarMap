# Min mapping hash entry. "to" and "from" are hash references.
package Bio::RNA::BarMap::Mapping::MinMappingEntry;
our $VERSION = '0.01';

use 5.012;
use warnings;

use Moose;
use namespace::autoclean;

use Scalar::Util qw( weaken );
use Set::Scalar;

use Bio::RNA::BarMap::Mapping::Type;

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

1;

# End of Bio::RNA::BarMap::Mapping::MinMappingEntry
