#!/usr/bin/perl
#use strict;
use lib ("/home/mpound/lib/perl5");
use lib ("/home/mpound/lib/perl5/share/");

use MongoDB;
use MongoDB::OID;

my $client = MongoDB::MongoClient->new("host" ==> "rena.ovro.pvt:27017");
my $db = $client->get_database( 'test' );
my $projects = $db->get_collection( 'projects' );
my $obsblocks= $db->get_collection( 'obsblocks' );
my $subobsblocks= $db->get_collection( 'subobsblocks' );
my $trials= $db->get_collection( 'trials' );

