# All methods are class methods, it's not meant to be instantiated. It just
# manages the type name string and converts it to arrows, etc.
package Bio::RNA::BarMap::Mapping::Type;
our $VERSION = '0.01';

use 5.012;
use warnings;

use Moose;
use Moose::Util::TypeConstraints;           # for enum()
use namespace::autoclean;


has '_type' => (
    is       => 'ro',
    isa      => enum([qw(EXACT APPROX)]),
    required => 1,
);

# Only allow construction from a single arrow string.
around BUILDARGS => sub {
    my $orig  = shift;
    my $class = shift;

    confess 'Pass a single arrow string to construct a Mapping::Type object'
        if @_ != 1 or ref $_[0];

    my $arrow = shift;                              # arrow string passed
    if ($arrow eq '->') {
        return $class->$orig(_type => 'EXACT')
    }
    elsif ($arrow eq '~>') {
        return $class->$orig(_type => 'APPROX')
    }
    else {
        confess 'Unknown arrow string in constructor';
    }
};

# Returns a new mapping type object of type 'exact'.
sub exact  { return $_[0]->new('->');  }

# Returns a new mapping type object of type 'approx'.
sub approx { return $_[0]->new('~>'); }

# Returns true iff the object is of EXACT type.
sub is_exact {
    my ($self) = @_;
    return $self->_type() eq 'EXACT';
}

# Returns true iff the object is of APPROX type.
sub is_approx {
    my ($self) = @_;
    return $self->_type() eq 'APPROX';
}

# Return the arrow representation of this object, i.e. '->' for exact and '~>'
# for approx mapping type objects.
sub arrow {
    my ($self) = @_;
    return $self->is_exact ? '->' : '~>';
}


__PACKAGE__->meta->make_immutable;

1;

# End of Bio::RNA::BarMap::Mapping::Type / lib/Bio/RNA/BarMap/Mapping/Type.pm
