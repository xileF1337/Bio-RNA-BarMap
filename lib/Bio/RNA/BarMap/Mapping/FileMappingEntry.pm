# Mini class for entries of the file mapping hash.
package Bio::RNA::BarMap::Mapping::FileMappingEntry;
our $VERSION = '0.01';

use 5.012;
use warnings;

use Moose;
use namespace::autoclean;

has 'name' => (is => 'ro', required => 1);
# Ensure object is cleaned after use => use weak refs
has [qw(from to)] => (is => 'rw', weak_ref => 1);

__PACKAGE__->meta->make_immutable;

1;

# End of Bio::RNA::BarMap::Mapping::FileMappingEntry
