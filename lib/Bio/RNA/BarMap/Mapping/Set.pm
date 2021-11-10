# A simple set class implemented using a hash. Supports storing references.
# Faster then Set::Scalar for this specific use case. It significantly reduces
# the runtime.
package Bio::RNA::BarMap::Mapping::Set;
our $VERSION = '0.01';

use v5.12;
use warnings;

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

1;
# End of Bio::RNA::BarMap::Mapping::Set / Bio/RNA/BarMap/Mapping/Set.pm
