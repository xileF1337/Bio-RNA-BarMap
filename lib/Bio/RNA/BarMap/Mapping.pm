package Bio::RNA::BarMap::Mapping;
our $VERSION = '0.01';

use 5.012;
use warnings;

use Moose;
use MooseX::StrictConstructor;
use namespace::autoclean;

use autodie qw(:all);
use Scalar::Util qw(reftype);
use List::Util qw(any);
use File::Spec;


# Hash mapping each minimum to the one it is mapped to in the next landscape.
has '_min_mapping' => (
    is       => 'ro',
    required => 1,
);

# Hash mapping barrier files to each other, i.e. bar1 -> bar2 iff BarMap
# mapped the minima from bar1 to the minima of bar2.
has '_file_mapping' => (
    is          => 'ro',
    init_arg    => undef,       # generated from list of mapped files
    lazy        => 1,
    builder     => '_build_file_mapping',
);

# Array for keeping the order.
has 'mapped_files' => (
    is       => 'ro',
    required => 1,
);

has 'file_name' => (
    is        => 'ro',
    predicate => 'has_file_name',
);

# The path builder is initialized with the path to this mapping's barmap
# file. Given another file name relative to the barmap, it constructs a
# path to this file from the current working directory. This is necessary
# because usually the bar files mentioned in the barmap file reside in the
# same directory and need to be prefixed with the same path.
has '_path_builder' => (
    is       => 'ro',
    init_arg => undef,
    lazy     => 1,
    builder  => '_build_path_builder',
);

# Returns a sub that builds file paths from given file names such that the
# files are in the same dir as the target file/dir given during
# construction.
# Args:
#   target_file: from this file or dir, the target dir will be inferred
# Returns: sub that builds a path from the given file name.
sub _build_path_builder {
    my $self = shift;
    confess 'File name attribute needs to be set to build any paths'
        unless $self->has_file_name;

    my ($target_file_path) = $self->file_name;

    # Volume is for compatibility, e.g. Windows
    my ($file_volume, $file_dir, $target_file_name)
        = File::Spec->splitpath($target_file_path);

    return sub {
        my ($file_name) = @_;

        return File::Spec->catpath($file_volume, $file_dir, $file_name);
    }
}

# Returns the name of the first mapped Barriers file of this BarMap
# mapping.
sub first_mapped_file {
    my $self = shift;
    my $first_file = $self->mapped_files->[0];
    return $first_file;
}

# Given a Barriers file, return its full path. The file is assumed to
# reside in the same directory as the BarMap mapping file.
# Arguments:
#   barriers_file: The Barriers file for which the path is to be computed.
# Returns the path to the given barriers file.
sub path_of_file {
    my ($self, $barriers_file) = @_;
    my $barriers_file_path = $self->_path_builder->($barriers_file);
    return $barriers_file_path;
}

# Returns the path to the first mapped Barriers file of this BarMap
# mapping.
sub first_mapped_path {
    my $self = shift;
    my $first_path = $self->path_of_file($self->first_mapped_file);
    return $first_path;
}

around BUILDARGS => sub {
    my ($orig, $class) = splice @_, 0, 2;
    my @args;
    if ( @_ == 1
         and (not reftype $_[0]
              or reftype $_[0] eq reftype \*STDIN
             )
       ) {
        my $data_handle;
        if (not reftype $_[0]) {                    # file name given
            my $barmap_file_name = shift;
            open $data_handle, '<', $barmap_file_name;
            push @args, (file_name => $barmap_file_name);
        }
        elsif (reftype $_[0] eq reftype \*STDIN) {  # file handle given
            $data_handle = shift;
        }
        else {
            confess 'Invalid constructor call';
        }
        my ($min_mapping, $mapped_files)
            = $class->_process_barmap_file($data_handle);
        push @args, (
             _min_mapping => $min_mapping,
             mapped_files => $mapped_files,
        );
    }
    else {
        @args = @_;
    }

    return $class->$orig(@args);
};

sub BUILD {
    my $self = shift;
    $self->_min_mapping;        # force construction of lazy attribute
}

# Set up the file mapping objects as a "hashed double-linked list"
sub _build_file_mapping {
    my ($self) = @_;
    # my ($to_file, $from_file, );
    my @mapped_files = @{ $self->mapped_files };

    # Construct mapping entry objects
    my %mapping
        = map { $_ => Bio::RNA::BarMap::Mapping::FileMappingEntry->new(
                        name => $_,
                      )
              }
              @mapped_files;

    # Set links to previous and following entries
    $mapping{$mapped_files[$_]}->from($mapping{$mapped_files[$_-1]})
        foreach 1..$#mapped_files;
    $mapping{$mapped_files[$_]}->to($mapping{$mapped_files[$_+1]})
        foreach 0..($#mapped_files-1);

    return \%mapping;
}

sub _process_min_mapping_header {
    my $class = shift;
    my $data_handle = shift;

    # Read header, remove leading '#'
    my $header_line = <$data_handle>;
    confess 'Barfile empty or header does not start with "#"'
        unless defined $header_line and $header_line =~ s/^#//;
    chomp $header_line;
    # Header contains names of mapped files as column names
    my @mapped_files = split /\s+/, $header_line;
    confess 'Found only one mapped file' unless @mapped_files > 1;

    return \@mapped_files;
}

# Split minima separated by ~> or -> and space.
sub _get_min_entry_mins_and_types {
    my $class = shift;
    my ($line) = @_;

    chomp $line;
    $line =~ s/^\s+//;              # trim leading space
    my @mapping_types = do {        # store type of mapping (exact?)
        my @arrows = $line =~ /[-~]>/g;
        # Convert arrows to strings. Append undef for last minimum
        # which is not mapped to anything (aligns indices or arrays).
        ((map {Bio::RNA::BarMap::Mapping::Type->arrow_to_type($_)} @arrows),
         undef
        );
    };
    my @mins = split /\s*[-~]>\s*/, $line;
    if ($mins[0] eq q{}) {              # drop empty first field
        shift @mins;
        shift @mapping_types;           # line started with an arrow
    }
    confess "Invalid minimum, needs to be a whole number in line\n$line"
        if any {not /^\d+$/} @mins;

    return (\@mins, \@mapping_types);
}

sub _process_min_mapping_entries {
    my $class = shift;
    my ($data_handle, $mapped_files_ref) = @_;

    my %min_mapping;
    my @rev_mapped_files = reverse @$mapped_files_ref;
    while ((my $line = <$data_handle>)) {
        my ($mins_ref, $mapping_types_ref)
            = $class->_get_min_entry_mins_and_types($line);

        # Consider reversed arrays to easily align their ends
        my @rev_mins          = reverse @$mins_ref;
        my @rev_mapping_types = reverse @$mapping_types_ref;
        confess "Found too many minima in the following line:\n$line"
            if @rev_mins > @rev_mapped_files;
        confess 'Found wrong number of mapping arrows in the following ',
              "line:\n$line"
            if @rev_mins != @rev_mapping_types;

        # Create mapping entry for last file if not existent yet
        my $to_file = $rev_mapped_files[0];
        my $to_entry
              = $min_mapping{$to_file}{$rev_mins[0]}
            ||= Bio::RNA::BarMap::Mapping::MinMappingEntry->new(
                    index => $rev_mins[0],
                );

        # Create mapping entries for all previous files if not existent yet
        for my $i (1..$#rev_mins) {
            # Create from-entry if non-existent
            my $from_file = $rev_mapped_files[$i];
            my $from_entry
                  = $min_mapping{$from_file}{$rev_mins[$i]}
                //= Bio::RNA::BarMap::Mapping::MinMappingEntry->new(
                        index => $rev_mins[$i],
                    );

            # Check for mappings degenerated by corasify_bmap.pl, i.e. one
            # minimum mapped to several other minima.
            confess join q{ }, 'State', $from_entry->index, 'from file',
                    "'$from_file' was already mapped to state",
                    $from_entry->to->index, "in file '$to_file', but is ",
                    'now remapped to state', $to_entry->index,
                    '(are you using a coarse-grained BarMap file?)'
                if $from_entry->has_to
                   and $from_entry->to->index != $to_entry->index;

           # Set forward and backward links
            $from_entry->to($to_entry);
            $from_entry->to_type($rev_mapping_types[$i]);
            $to_entry->add_from($from_entry);       # 'from' is a list

            $to_file  = $from_file;
            $to_entry = $from_entry;  # next entry in line maps to current
        }
    }

    return \%min_mapping;
}

# Construct the mininum mapping and the Barriers file mapping hashes by
# reading the BarMap file from the provided _data_handle.
sub _process_barmap_file {
    my ($class, $barmap_handle) = @_;

    my $mapped_files_ref
        = $class->_process_min_mapping_header($barmap_handle);

    my $min_mapping_ref
        = $class->_process_min_mapping_entries(
            $barmap_handle,
            $mapped_files_ref
          );

    return ($min_mapping_ref, $mapped_files_ref);
}

# Map a Barriers file to its immediate successor file.
# Arguments:
#   from_file: Barriers file to be mapped.
# Returns the immediate successor Barriers file of from_file, or undef if
# from_file points to the last Barriers file.
sub map_file {
    my ($self, $from_file) = @_;

    my $file_map = $self->_file_mapping->{$from_file};
    confess "Cannot map file '$from_file': not contained in BarMap file"
        unless defined $file_map;

    return defined $file_map->to ? $file_map->to->name
                                 : undef
                                 ;
}

# Given a path to Barriers file, map the Barriers file to its immediate
# successor file and return its path.
# Arguments:
#   from_path: Path of Barriers file to be mapped.
# Returns the path of the Barriers file that from_path was mapped to, or
# undef if from_path points to the last Barriers file.
sub map_path {
    my ($self, $from_path) = @_;
    my $from_file = (File::Spec->splitpath($from_path))[2];
    my $to_file   = $self->map_file($from_file);
    my $to_path   = $self->path_from_file($to_file);
    return $to_path;
}

# (Inversely) map a Barriers file to its immediate predecessor file.
# Arguments:
#   to_file: Barriers file to be mapped to its predecessor.
# Returns the immediate predecessor Barriers file of to_file, or undef if
# to_file points to the first Barriers file.
sub map_file_inv {
    my ($self, $to_file) = @_;

    my $file_map = $self->_file_mapping->{$to_file};
    return defined $file_map ? $file_map->from->name
                             : undef;

}

# Given a path to Barriers file, (inversely) map the Barriers file to its
# immediate predecessor file and return its path.
# Arguments:
#   to_path: Path of Barriers file to be inversely mapped.
# Returns the path of the predecessor Barriers file of to_path, or undef
# if to_path points to the first Barriers file.
sub map_path_inv {
    my ($self, $to_path) = @_;
    my $to_file   = (File::Spec->splitpath($to_path))[2];
    my $from_file = $self->map_file_inv($to_file);
    my $from_path = $self->path_from_file($from_file);
    return $from_path;
}

# Map a given minimum from a given Barriers file to its corresponding
# minimum in the next Barriers file. In list context, the mapping type is
# returned, too. The mapping type tells whether a minimum was mapped
# exactly (->) or approximately (~>), and is an object of class
# Bio::RNA::BarMap::Mapping::Type.
# Arguments:
#   from_file: The Barriers file of from_min.
#   from_min:  The minimum to be mapped.
# In scalar context, returns the corresponding minimum of from_min in the
# next Barriers file, or undef if from_file is the last Barriers file of
# the mapping. In list context, the mapping type as well as the
# corresponding minimum is returned.
sub map_min_step {
    my ($self, $from_file, $from_min) = @_;

    # If from_file is the final barriers file, the state won't map.
    # TODO better check whether from_file maps to a valid file? But this
    # is doubled work when called from map_min...
    my $to_map = $self->_min_mapping->{$from_file}{$from_min};
    return if not defined $to_map or not defined $to_map->to;

    my $to_index        = $to_map->to->index;
    my $to_mapping_type = $to_map->to_type;

    return wantarray ? ($to_mapping_type, $to_index) : $to_index;
}

# Map given minimum from a given Barriers file to its corresponding
# minimum in another (successor) Barriers file. In list context, the
# mapping type is returned, too. The mapping type tells whether a minimum
# was mapped exactly (->) or approximately (~>), and is an object of class
# Bio::RNA::BarMap::Mapping::Type. The mapping is considered exact iff all
# mapping steps from the initial to the target Barriers file are exact;
# otherwise, the mapping is approximate. If the given successor Barriers
# file is not actually a successor of the initial Barriers file, an
# exception is raised.
# Arguments:
#   from_file: The Barriers file of from_min.
#   from_min:  The minimum to be mapped.
#   to_file:   Target file for which the corresponding minimum is to be
#              determined.
# In scalar context, returns the corresponding minimum of from_min in the
# target Barriers file. In list context, the mapping type as well as the
# corresponding minimum is returned.
sub map_min {
    my ($self, $from_file, $from_min, $to_file) = @_;

    my ($next_file, $next_min, $current_mapping_type);
    my $mapping_type = Bio::RNA::BarMap::Mapping::Type->exact;
    while (defined ($next_file = $self->map_file($from_file))) {
        ($current_mapping_type, $next_min)
            = $self->map_min_step($from_file, $from_min);
        # One approx mapping step makes entire mapping chain approx.
        $mapping_type = Bio::RNA::BarMap::Mapping::Type->approx
        if Bio::RNA::BarMap::Mapping::Type->is_approx($current_mapping_type);
            # if $current_mapping_type eq Bio::RNA::BarMap::Mapping::Type->approx;
        return wantarray ? ($mapping_type, $next_min) : $next_min
            if $next_file eq $to_file;

        ($from_file, $from_min) = ($next_file, $next_min);
        # destination file not reached yet, continue
    }
    confess "map_min: '$_[1]' is not mapped to '$to_file'";
}

# Convenience wrapper for map_min() which only returns the mapping type,
# but not the target minimum.
sub get_mapping_type {
    my ($self, $from_file, $from_min, $to_file) = @_;
    my ($mapping_type) = $self->map_min($from_file, $from_min, $to_file);
    return $mapping_type;
}

# Convenience wrapper for map_min() which only returns the mapping type,
# converted to its corresponding arrow string. An exact mapping is
# converted to arrow '->', an approximate mapping to arrow '~>'.
sub get_mapping_arrow {
    my ($self, $from_file, $from_min, $to_file) = @_;
    my $mapping_type
        = $self->get_mapping_type($from_file, $from_min, $to_file);
    my $mapping_arrow
        = Bio::RNA::BarMap::Mapping::Type->type_to_arrow($mapping_type);
    return $mapping_arrow;
}

# Get all minima in the preceding Barriers file that map to a given
# minimum from a given Barriers file.
# Arguments:
#   to_file: The Barriers file of the target minimum.
#   to_min:  The target minimum to which the returned minima are mapped.
# Returns a list of all minima that are mapped to the target minimum.
sub map_min_inv_step {
    my ($self, $to_file, $to_min) = @_;

    # Minima are only part of the mapping if they map to something or
    # something maps to them, so the mins of the last Barriers file may
    # not be contained.
    my $to_min_entry = $self->_min_mapping->{$to_file}{$to_min};
    return unless defined $to_min_entry;

    my @from_mins    = $to_min_entry->get_from;
    my @from_indices = map {$_->index} @from_mins;
    return @from_indices;
}

# Get all minima in a given Barriers file that map to a given
# minimum from another Barriers file.
# Arguments:
#   to_file:   The Barriers file of the target minimum.
#   to_min:    The target minimum to which the returned minima are mapped.
#   from_file: The source Barriers file from which the returned minima
#              are.
# Returns a list of all minima from the source Barriers file that are
# mapped to the target minimum in the target Barriers file.
sub map_min_inv {
    my ($self, $to_file, $to_min, $from_file) = @_;

    my ($prev_file, @prev_mins);
    my @to_mins = ($to_min);
    while (defined ($prev_file = $self->map_file_inv($to_file))) {
        @prev_mins = ();        # clear list
        push @prev_mins, $self->map_min_inv_step($to_file, $_)
            foreach @to_mins;
        # Do not return () before ensuring prev_file eq from_file, because
        # otherwise we would not return an error if from_file is never
        # reached.
        #return () unless @to_mins;              # no mins map, stop here
        return @prev_mins if $prev_file eq $from_file;

        ($to_file, @to_mins) = ($prev_file, @prev_mins);
        # destination file not reached yet, continue
    }
    confess "map_min_inv: '$from_file' is not mapped to '$_[1]'";
}

__PACKAGE__->meta->make_immutable;

# End of Bio::RNA::BarMap::Mapping / Bio/RNA/BarMap/Mapping.pm
