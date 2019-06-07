#!/usr/bin/perl 
#$Id: uploaderScript.cgi,v 1.8 2014/03/10 16:33:42 iws Exp $
# -------------------------------------------------------------------
#
# This is the uploader script used by
#
# http://cedarflat.mmarray.org/observing/checkin/
#
# It is a canned script Tony Beasley or Paul Daniel got off the web, but has
# been modified for use with CARMA.
# 
use CGI qw(:all);
use CGI::Carp qw(fatalsToBrowser);
use File::Type qw(:all)
use File::Basename qw(:all)
use Archive::Tar qw(:all)
# sadly, these are not present by default.
#use Archive::Zip qw(:all)
#use Archive::Extract qw(:all)
use IO::Zlib qw(:all)


# -------------------------------------------------------------------
# ORIGINAL DOCUMENTATION:
# 
##################################################################
# UNDER ALL CIRCUMSTANCES THIS HEADER MUST NOT BE REMOVED
##################################################################
# "uploader.cgi"  -  Scripted by CNC Technology.
# "uploader.cgi"  -  changed by Paul
# Web Site: http://www.cnctek.com/bizdb-html/
# E-mail: cnctek@cnctek.com
# 
#
# ///// IMPORTANT NOTE: /////
# This cgi program requires "cgi-lib2.pl" installed on the same
# cgi directory as your "uploader.cgi" program.
#
#
# COPYRIGHT NOTICE                                                           
# This Perl CGI Script is a product of CNC Technology. All rights reserved.  
# Only the user with our permission can use and modify this script.         
# Also permission is granted to execute this script on one domain name only.      
# Without CNC Technology's permission, no part of this script can be           
# reproduced ( other than the backup copy ), resold or redistributed to
# any other indivituals, companies and organizations.                                  
# In all cases this copyright notice must not be removed.                   
# DISCLAIMER:
#
# These Scripts are provided on a "As Is" basis. CNC Technology
# will not be liable ( nor responsible ) whatsoever for any direct
# or indirect damages caused by the use of this Software
# ( or Scripts ).
#
##################################################################

require '/array/utilities/apache2.0.54/cgi-bin/cgi-lib2.pl';

#
##### Start of field definitions - System Config Parameters          ######
##### You will need to check and change these fields                 ######
##### Do not change codes outside this section unless asked          ######
##### All directory pathnames must be relative to the ROOT directory ######
##### Please update with a text editor only                          ######

# Indicate if your server O/S is Unix/Linux or Windows NT
# Set to "unix" if Unix or Linux; set to "nt" if Windows NT
$serverOS = "unix";

# This parameter defines what character set you want the Web
# browser to be set to when viewing your Html pages.
# Default is "". Chinese GB = "gb2312". Chinese Big5 = "Big5".
$charset = "";

# Supervisor Password.
$superpwd = "jflg2m";

# Full pathname of directory (parent) which is 1 level higher than
# the "file upload" directory (directory storing the uploaded files).
# This directory must be resided in a Html directory
# Create this directory manually if it is not already existed.
# Use chmod command to set this directory to writable, i.e. 0777.
# The last "/" character is significant.
$parent_dir = "/array/rt/scripts/checkin/";

# Maximum file size ( in bytes ) allowed to be uploaded to the server
# Note: This value will be superceded by the value defined in the (optional)
# form statement <input type=hidden name="maxfilesize" value="someValue">
# if it is present in your "uploader" html form.
$maxfilesize = "50480000"; 

# Script checking directory
$filedirname = "./";

# Return URL link used by OK and error messages
$return_url = "http://cedarflat.mmarray.org/observing/checkin/index1.html";

# These are the urls that are allowed to execute this program
# eg. @valid = ('abc.com','def.com','xyz.net');
#@valid = ('192.100.16.70');
@valid = ('');

####### End of field definitions ####

############# Must not change the codes after this line ###########
#############    if you don't know what you are doing   ###########

# Parse Form Contents
&ReadParse;

if ($ENV{'REQUEST_METHOD'} ne 'POST') {
   &error_not_a_command;
}

$| = 1;

# Validate & execute command according to Action Type
unless (
   ($in{'action'} eq "uploadfile") ||
   ($in{'action'} eq "createsubdir")) { 
   &error_not_a_command;
}

if ($in{'action'} eq "uploadfile")   {&uploadfile}
if ($in{'action'} eq "createsubdir") {&createsubdir}

exit;

#################################################################

sub sendEmail {
   my $fn = shift;
   $to='obs@mmarray.org, Nikolaus Volgenau <volgenau@ovro.caltech.edu>';
   $from= 'Nikolaus Volgenau <volgenau@ovro.caltech.edu>';
   $subject='New file in checkin directory';

   # Start mail
   open(MAIL, "|/usr/sbin/sendmail -t");

   # Mail Header
   print MAIL "To: $to\n";
   print MAIL "From: $from\n";
   print MAIL "Subject: $subject\n\n";

   # Mail Body
   print MAIL "NOTICE: The file '$fn' has been uploaded into the scripts directory!\n";
   print MAIL "\n";
   print MAIL "The scripts directory is at $parent_dir .\n";
   print MAIL "The observers should process the file ASAP since it\n";
   print MAIL "may contain new observing scripts and/or source catalog.\n";

   # Send it
   close(MAIL);
}

sub uploadfile {
   &check_url_referer;

#   if ($in{'pwd'} ne $superpwd) {
#      &error_password;
#   }

   if (!$in{'sourcefile'}) {
      &error_uploadfile;
   }

   #if (!$in{'filedirname'}) {
   #   &error_no_upload_directory;
   #}

#   if ($in{'filedirname'} =~ /[^a-z0-9A-Z]+/) {
#      &error_invalid_directory_name;
#   }

   if ($in{'maxfilesize'}) {
      $maxfilesize = $in{'maxfilesize'};
   }

   if ($ENV{'CONTENT_LENGTH'} > $maxfilesize) {
      &error_file_too_large;
   }

   $upload_dir = "$parent_dir$in{'filedirname'}";
   $upload_dir = "$upload_dir/$in{'optdir'}";

   if (opendir(DIR,"$upload_dir") != 1) {
      if (mkdir($upload_dir,0777) == 0) {
         `mkdir -p $upload_dir`; 
          if (opendir(DIR,"$upload_dir") != 1) {
          &cannot_create_directory;
          }
      } 
      if ($serverOS =~ /^unix$/i) {
         `chmod 777 $upload_dir`;
      }
   }

   # added MWP 5/23/2009
   # don't allow overwrite of subdirectories, including symlinks
   my $outFile = $upload_dir . $in{'destn_filename'};
   if ( -d $outFile || -l $outFile ) {
      &overwriteDirNotPermitted;
   }

   $upload_dir = "$upload_dir/";
   if ( -e "$upload_dir$in{'destn_filename'}"){
      if ($in{'overwrite'} ne "on") {
         &file_exists_no_checkbox;
      }
   } 

   open(REAL,">$upload_dir$in{'destn_filename'}") || &error_open_file;
   if ($serverOS =~ /^nt$/i) {   
      binmode(REAL);
   }
   print REAL $in{'sourcefile'};
   close(REAL);

   # JEEZUS THEY USE BACKTICKS--SECURITY FLAW!
   if ($serverOS eq "unix") {
      `chmod 0777 $upload_dir`;
      `chmod 0777 $upload_dir$in{'destn_filename'}`;
   }

   # Determine if this is a tar file and/or gzipped,
   # since some people upload a tar(.gz) containing both
   # observing script and source catalog

   #Separate directory, file name, and extension
   #@fileparts = fileparse($upload_dir$in{'destn_filename'},qr/\.[^.]*/);

   # Uncompress/unarchive the file if it was compressed/archived.
   # File list will not have upload_dir pathname prepended.
   @fileList = unpackFile( $upload_dir, $in{'destn_filename'} );
   my $wasArchive = 0;

   if ( scalar(fileList) > 1 ) { $wasArchive = 1; } 

   # check each file for bad chars.
   foreach my $inFile ( @filelist ) {
       my $error = &checkFile( $upload_dir$inFile );
       if ( $error neq "" ) {
           &uploadFailedCheck( $upload_dir$inFile, $in{'destn_filename'}, $error, $wasArchive );
       }
       my $error = checkCatalog( $upload_dir$inFile );
       if ( $error neq "" ) {
           &catalogFailedCheck( $upload_dir$inFile, $in{'destn_filename'}, $error, $wasArchive );
       }
   }

   &upload_ok($in{'destn_filename'});
}

###
# Unpack the archive file and return a list of its contents in an array.
# If the file was not an archive, just return the file name.
# parameters 
#    $inputdir -  input directory (path to archive file)
#    $filename -  input archive file
###
sub unpackFile {
        my ($inputdir, $filename) = @_;
        my $tar;
        my @filelist;
# decide file type based on extension.  yes this is not
# always correct, but there is no good perl magic file implemntation
        chdir $inputdir or die "Could not change directory to $inputdir: $!\n"
        if ( $filename =~ /\.(gz|tgz|taz)$/)
            $tar = Archive::Tar->new( $filename );
#            $type="GZIP";
        } elsif ( $filename =~ /\.Z$/)
            open F, "/usr/bin/uncompress -c $filename|";
            $tar = Archive::Tar->new( *F );
            die $tar->error unless @filelist = $tar->list_files();
            die $tar->error unless $tar->extract();
                
#            $type="COMPRESSED";
        } elsif ( $filename =~ /\.bz2$/)
            open F, "/usr/bin/bunzip2 -c $filename|";
            $tar = Archive::Tar->new( *F );
            die $tar->error unless @filelist = $tar->list_files();
            die $tar->error unless $tar->extract();
#            $type="BZIP";
        } elsif ( $filename =~ /\.zip$/)
            $unzip = "/usr/bin/unzip -l";
            system( "/usr/bin/unzip", "q", $filename) == 0 or die "unzip of input file $file failed: $!\n"
            @filelist = qx{$unzip $filename};
#            $type="ZIP";
        } else {
           $filelist[0] = $filename;
        }
        return @filelist; 
}
         

sub unCompress {
        my ($filename) = @_;
        my $opts="q";
        my $gunzip;
        my $type;
# decide file type based on extension.  yes this is not
# always correct, but there is no good perl magic file implemntation
        if ( $filename =~ /\.(gz|Z|tgz|taz)$/)
            $gunzip = "/bin/gunzip";
            $type="GZIP";
        } elsif ( $filename =~ /\.bz2$/)
            $gunzip = "/usr/bin/bunzip2";
            $type="BZIP";
        } else {
           # assume it is not zipped
           return $filename;
        }

        my $status = system( $gunzip, $opts, $filename);
        if ( $status != 0 ) { &uncompressFailed( $filename, $type );
        return 1;
}

sub unArchive {
        my ($filename) = @_;
        my $type;
        my $opts;
        if( $filename =~ /\.tar$/)
            $untar  = "/bin/tar";
            $opts = "cf";
            $type="TAR";
        } elsif ( $filename =~ /\.zip$/)
            $untar= "/usr/bin/unzip";
            $opts = "q";
            $type="ZIP";
        } else {
          return -1;
        }
        my $status = system( $untar, $opts, $filename);
        if ( $status != 0 ) { &untarFailed( $filename, $type );

        return 1;
}

sub uncompressFailed {
   my ($file, $type) = @_;
   &set_content_type;
   print "<p><blockquote><font size=+1 color=\"FF0000\"><b>You uploaded what appeared to be a compressed file, $file, which I tried to uncompress and failed.</b> The extension on this file led me to believe it was a $type file.  If that is incorrect, try renaming the file to have extension .gz,.tgz,.taz (gzip), .bz2 (bzip), or .Z (compressed) as appropriate.  If it was not a compressed file, be sure the file name does not end in one of these extensions.</font></blockquote></p>";
   print "<p><center><b><a href='http://cedarflat.mmarray.org/observing/checkin/index1.html'> Return to Carma Script Upload </a></b></center></p></body></html>\n";
   exit;
}

sub untarFailed{
   my ($file, $type) = @_;
   &set_content_type;
   print "<p><blockquote><font size=+1 color=\"FF0000\"><b>You uploaded what appeared to be a $type file, $file, which I tried to unarchive and failed.</b> The extension on this file led me to believe it was a $type file.  If that is incorrect, try renaming the file to have extension .tar (TAR), or .zip (ZIP) as appropriate.  If it was not an archive file, be sure the file name does not end in one of these extensions.</font></blockquote></p>";
   print "<p><center><b><a href='http://cedarflat.mmarray.org/observing/checkin/index1.html'> Return to Carma Script Upload </a></b></center></p></body></html>\n";
   exit;
}

#################################################################

sub createsubdir {

   &check_url_referer;

   if (!$in{'parentdirname'} || $in{'parentdirname'} eq "NULL") {
      &error_no_parent_directory;
   }

   $p_dir = "$parent_dir$in{'parentdirname'}";
   $subdir = "$p_dir/$in{'subdirname'}";

   if (-d $subdir) { &subdir_exists; } 

   if  (mkdir($subdir,0777) == 0) {
         &cannot_create_subdirectory;
   } 

   if ($serverOS eq "unix") {
      `chmod 777 $subdir`;
   }

   &createsubdir_ok;
}


##################################################################


sub return {
   print "Location: $ENV{'DOCUMENT_URI'}\n\n";
}

sub check_url_referer {
   $referral_cnt = @valid;
   if ($referral_cnt > 0) {
      foreach $referer (@valid) {
	   if ($ENV{'REMOTE_ADDR'} =~ /$referer/i) {
	      $good_ref = "yes";
            last;
	   }
      }
      if ($good_ref ne "yes") {
         &go_away; 
      }
   }
}

sub subdir_exists {
   &set_content_type; 
   print "<html><body><center><font size=+1 color=\"FF0000\"><b>ERROR: Subdirectory exists</b></font></center>";
   print "<p>Please check and enter again - hit \"BACK\" on your browser.</p></body></html>\n";
   exit;
}

sub file_exists_no_checkbox {
       &set_content_type; 
       print "<html><body><h2><center><font size=+2 color=\"FF0000\"><b>Warning: Desired remote file already exists </b></font></center>";
       print "<p><center>$in{'overwrite'}<br>\n";
       print "<p><center>You are trying to overwrite a file without <br>\n";
       print "setting the overwrite option on the upload page.<br></p>\n";
       print "<p>Hit BACK on your browser, check the overwrite option and try again. </p>\n";
       print "</h2></center></body></html>\n";
       exit;
}

sub error_password {
   &set_content_type; 
   print "<html><body><center><font size=+1 color=\"FF0000\"><b>ERROR: Invalid password</b></font></center>";
   print "<p>You didn't supply a valid password. Please check and enter again.</p></body></html>\n";
   exit;
}

sub error_not_a_command {
   &set_content_type; 
   print "<html><body><center><font size=+1 color=\"FF0000\"><b>ERROR: Not a valid command</b></font></center>";
   print "<p>You did not select a valid command. Please check and try again.</p></body></html>\n";
   exit;
}

sub go_away {
   &set_content_type; 
   print "<html><body><center><font size=+1 color=\"FF0000\"><b>ERROR: Unauthorised Access</b></font></center>";
   print "<p>Request denied. You are attempting to access our server using an unauthorized form.</p></body></html>\n";
   exit;
}

sub cannot_create_subdirectory {
   &set_content_type; 
   print "<html><body><center><font size=+1 color=\"FF0000\"><b>ERROR: Cannot create the subdirectory</b></font></center>";
   print "<p>Please check your input and try again. If the problem repeats, please contact your Webmaster.</p>\n";
   print "<p><center><b><a href=\"$return_url\">Back Home</a></b></center></p></body></html>\n";
   exit;
}

sub cannot_create_directory {
   &set_content_type; 
   print "<html><body><center><font size=+1 color=\"FF0000\"><b>ERROR: Cannot create the upload directory</b></font></center>";
   print "<p>Please check your input and try again. If the problem repeats, please contact your Webmaster.</p>\n";
   print "<p><center><b><a href=\"$return_url\">Back Home</a></b></center></p></body></html>\n";
   exit;
}

sub overwriteDirNotPermitted {
       &set_content_type; 
       print "<html><body><h2><center><font size=+2 color=\"FF0000\"><b>Warning: Desired remote is a directory</b></font></center>";
       print "<p><blockquote>Your file name is identical to an existing directory and overwrite is not permitted.  ";
       print "Change your file name and try uploading again.";
       print "</h2></blockquote>";
   print "<p><center><b><a href=\"$return_url\">Back to upload</a></b></center></p></body></html>\n";
       exit;

}

sub error_invalid_directory_name {
   &set_content_type; 
   print "<html><body><center><font size=+1 color=\"FF0000\"><b>ERROR: Invalid upload directory name</b></font></center>";
   print "<p>Please check your input and try again. Directory name must contain alphanumeric characters only.</p>\n";
   print "<p><center><b><a href=\"$return_url\">Back Home</a></b></center></p></body></html>\n";
   exit;
}

sub set_content_type {
   if ($charset eq "") {
      print "content-type: text/html\n\n";
   } else {
      print "content-type: text/html\; charset=$charset\n\n";
   }
}

##
# check a file for tabs or non-printable chars
# parameters - the file for input
##
sub checkFile {
    my ($infile ) = @_;

    my $python       = "/opt/carmaTools/bin/python";

    #my $rootdir = "/opt/rt/scripts/python";
    my $rootdir = "/home/mpound/src/carma/scripts/python";
    my $checkfortabs = $rootdir . "/checkForTabs.py"

    # run checkForTabs.py on the input 
    # and grab the output
    my @foo = qx{$python $checkfortabs $infile};
    if ( scalar(@foo) == 0 ) {
        return "";
    }
    my $errorString = join('',@foo);
    return $errorString;
}

##
# Verify a catalog file using the checkcat program.
# 
# parameters - the fully-qualified file for input
# If the filename does not end in ".cat" assume it is not a catalog
# file and just return "".
# @return error string if catalog failed check or "" if ok.
##
sub checkCatalog {
    my ($infile ) = @_;

    # not a catalog.
    if ( ! ( $infile =~ /\.cat$/) ) { return ""; }

    # run checkcat on the input and grab the output
    my $checkcat= "/opt/rt/bin/checkcat catalog=$infile";
    my @foo = qx{$checkcat}
    if ( $foo[0] == "OK" ) {
        return "";
    }
    my $errorString = join('',@foo);
    return $errorString;
}


sub uploadFailedCheck {
   my ($fileToRemove, $shortName, $errorString, $wasArchive) = @_;
   unlink $fileToRemove;
   &set_content_type;
   print "<p><blockquote><font size=+1 color=\"FF0000\"><b>The input file $shortName";
   if ( $wasArchive == 1 ) {
      print ", which was unpacked from your uploaded archive file, ";
   }
   print " has tabs or non-printable characters in it.  "
   print "Only CR, LF, and ASCII characters with codes between 31 and 127"
   print " are allowed. Please fix this and upload your file again.</b>"
   print "</font></blockquote></p>";
   print "<p>The errors were: <pre>";
   print $errorString . "</pre><br><hr>";
   print "<p><center><b><a href='http://cedarflat.mmarray.org/observing/checkin/index1.html'> Return to Carma Script Upload </a></b></center></p></body></html>\n";
   exit;
}

sub upload_ok {
   my $fn = shift;
#   &sendEmail($fn);
   &set_content_type;
   print "<p><center><font size=+1 color=\"FF0000\"><b>File Upload Completed</b></font></center>";
   print "<p><center><b><a href='http://cedarflat.mmarray.org/observing/checkin/index1.html'> Return to Carma Script Upload </a></b></center></p></body></html>\n";
   exit;
}

sub createsubdir_ok {
   &set_content_type;
   print "<p><center><b>Subdirectory has been created successfully</b></center></p>\n";
   print "<p><center><b><a href=\"$return_url\">Back Home</a></b></center></p></body></html>\n";
   exit;
}

sub error_no_source_file {
   &set_content_type;
   print "<html><body><center><font size=+1 color=\"FF0000\"><b>ERROR: Source File Is Empty</b></font></center>";
   print "<p>You must select a source file to be uploaded. Please try again.</p>\n";
   print "<p><center><b><a href=\"$return_url\">Back Home</a></b></center></p></body></html>\n";
   exit;
}

sub error_no_parent_directory {
   &set_content_type;
   print "<html><body><center><font size=+1 color=\"FF0000\"><b>ERROR: Upload directory name absent</b></font></center>";
   print "<p>You did not enter an valid parent directory name. Please try again - hit \"BACK\" on your browser. </p>\n";
   print "<p><center><b><a href=\"$return_url\">Back Home</a></b></center></p></body></html>\n";
   exit;
}

sub error_no_upload_directory {
   &set_content_type;
   print "<html><body><center><font size=+1 color=\"FF0000\"><b>ERROR: Upload directory name absent</b></font></center>";
   print "<p>You did not enter an upload directory name. Please try again. Directory name must contain alphanumeric characters only.</p>\n";
   print "<p><center><b><a href=\"$return_url\">Back Home</a></b></center></p></body></html>\n";
   exit;
}

sub error_cannot_open_dir {
   &set_content_type;
   print "<html><body><center><font size=+1 color=\"FF0000\"><b>ERROR: Cannot open the directory</b></font></center>";
   print "<p>Please supply a valid directory name and try again.</p>\n";
   print "<p><center><b><a href=\"$return_url\">Back Home</a></b></center></p></body></html>\n";
   exit;
}

sub error_file_too_large {
   &set_content_type; 
   print "<html><body><center><font size=+1 color=\"FF0000\">ERROR: Upload file too large.</font></center>";
   print "<p>Size of your upload file exceeds $maxfilesize bytes. Please try again.</p>\n";
   print "<p><center><b><a href=\"$return_url\">Back Home</a></b></center></p></body></html>\n";
   exit;
}

sub error_uploadfile {
   &set_content_type; 
   print "<html><body><center><font size=+1 color=\"FF0000\">ERROR: Upload file not specified or empty.</font></center>";
   print "<p>You did not provide a file to be uploaded or it is empty.  Please try again.</p>\n";
   print "<p><center><b><a href=\"$return_url\">Back Home</a></b></center></p></body></html>\n";
   exit;
}

sub error_open_file {
   &set_content_type; 
   print "<html><body><center><font size=+1 color=\"FF0000\">ERROR: Destination upload file cannot be opened.</font></center>";
   print "<p>Please contact Webmaster.</p></body></html>\n";
   exit;
}

