# All methods are class methods, it's not meant to be instantiated. It just
# manages the type name string and converts it to arrows, etc.
package Bio::RNA::BarMap::Mapping::Type;
our $VERSION = '0.01';

use 5.012;
use warnings;

use Moose;
#use Moose::Util::TypeConstraints;           # for enum()
use namespace::autoclean;


# has '_type' => (
#     is  => 'ro',
#     isa => enum [qw(EXACT APPROX)],
# );
#
# # Returns a new mapping type object of type 'exact'.
# sub exact  { return __PACKAGE__->new('->');  }
#
# # Returns the string used to characterize mappings as approximate.
#     # Returns a new mapping type object of type 'approx'.
# sub approx { return __PACKAGE__ . '::APPROX'; }
#
# # Prevent instantiation.
# sub BUILD  { confess 'Not meant to be instantiated, or is it...?!'; }
#
# # Returns true iff the passed string matches the EXACT type.
# sub is_exact {
#     my ($class, $other) = @_;
#     my $is_exact = $other eq __PACKAGE__->exact;
#     return $is_exact;
# }
#
# # Returns true iff the passed string matches the APPROX type.
# sub is_approx {
#     my ($class, $other) = @_;
#     my $is_approx = $other eq __PACKAGE__->approx;
#     return $is_approx;
# }
#
# # Convert arrows -> and ~> to the appropriate type (exact, approx, resp.).
# # Arguments:
# #   arrow: Arrow to convert.
# # Returns the type name represented by the given arrow.
# sub arrow_to_type {
#     my ($class, $arrow) = @_;
#     if ($arrow eq '->') {
#         return __PACKAGE__->exact;
#     }
#     elsif ($arrow eq '~>') {
#         return __PACKAGE__->approx;
#     }
#     else {
#         confess "Invalid arrow '$arrow'";
#     }
# }
#
# # Convert a given type name to the appropriate arrows -> and ~> (for the
# # exact and approximate type, resp.).
# # Arguments:
# #   type: Name of the type to convert.
# # Returns the arrow representing given type name.
# sub type_to_arrow {
#     my ($class, $type) = @_;
#     if ($type eq __PACKAGE__->exact) {
#         return '->';
#     }
#     elsif ($type eq __PACKAGE__->approx) {
#         return '~>';
#     }
#     else {
#         confess "Invalid type '$type'";
#     }
# }
# __PACKAGE__->meta->make_immutable;


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

1;

# End of Bio::RNA::BarMap::Mapping::Type / lib/Bio/RNA/BarMap/Mapping/Type.pm
