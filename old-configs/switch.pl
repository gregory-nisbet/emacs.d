#!/usr/bin/env perl
use strict;
use warnings FATAL => 'all';
use Digest::file qw[digest_file_hex];
use File::Find;
use feature qw[say];
use Data::Dumper;
use autodie;

# find possible config files.
my @paths;
sub collect_paths {
    # return if we're in the configs directory.
    $_ eq '.' and return;
    push @paths, $File::Find::name;
}

sub inode {
    my ($path) = @_;
    my @stats = stat $path;
    return $stats[1];
}

sub sha_256 {
    my ($path) = @_;
    return digest_file_hex($path, "SHA-256");
}

# is the init file unique by the given criterion? e.g. inode, hash of file contents
# uniqueness criterion must be a string
sub backed_up {
    @_ == 2 or die;
    my ($fun_r, $paths_r) = @_;
    my @paths = @$paths_r;
    # if the file doesn't exist, it is backed up
    return 1 unless (-e "init.el");
    my $init = $fun_r->("init.el");
    # keep track of config values
    my %config = map { (scalar $fun_r->($_)), 1 } @paths;
    return exists $config{$init};
}

# only takes path to config
@ARGV == 1 or die "USAGE PATTERN: 'switch.pl name_of_config.el'";
my $config = shift @ARGV;
# the name of the folder "configs" is hard-coded in.
# remove leading directory if it exists
if ($config =~ /^configs\/(.*)$/) {
    $config = $1;
}
-e "configs/$config" or die 'file does not exist';

# search directory for config files.
find(\&collect_paths, 'configs');

my $backed_up = backed_up(\&inode, \@paths) or backed_up(\&sha_256, \@paths);

die "file not backed up, cowardly refusing to proceed" unless $backed_up;

if (-e "init.el") {
    unlink "init.el" or die "failed to unlink file";
}
link "configs/$config", "init.el" or die "failed to link file";
