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
sub wanted {
    # return if we're in the configs directory.
    $_ eq '.' and return;
    push @paths, $File::Find::name;
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

# digest of current config to compare with other configs
my $current_digest;
(-e "init.el") and digest_file_hex("init.el", "SHA-256");
# search directory for config files.
find(\&wanted, 'configs');

my %hash_code = map {digest_file_hex($_, "SHA-256") => 1} @paths;

do { 
    no warnings qw[uninitialized];
    if (defined $current_digest) {
        $hash_code{$current_digest} or die "cowardly refusing to overwrite config without backup";
    }
};

if (-e "init.el") {
    unlink "init.el" or die "failed to unlink file";
}
link "configs/$config", "init.el" or die "failed to link file";
