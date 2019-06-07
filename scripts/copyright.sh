#!/bin/bash -x
############################################################################
#
# CARMA Script to insert copyright notice from a file into another file
# at a point containing $Copyright$ on a line by itself. If no such pattern
# is found, no changes are made. The changed file is copied over into
# a destination directory. File containing copyright notice is assumed
# to be parameter 1, file to be modified is parameter2, destination directory
# is parameter3, defaults to /tmp.
#
# @author N. S. Amarnath
# Date:   May 17, 2003
# $Id: copyright.sh,v 1.1 2003/06/27 15:49:43 amar Exp $
#
#############################################################################

# set up status variables
error=1
success=0

if [[ $# -lt 2 ]]; then
    echo "Usage: "$0" <Copyright notice filename> <file to be modified> [<destination directory> = /tmp]"
    exit -1
fi

function Cleanup
{
    if [ -f Copyright.temp.txt ]; then
	\rm -f Copyright.temp.txt
    fi
    if [ -f /tmp/difftext ]; then
	\rm -f /tmp/difftext
    fi
    if [ -f temp.changed  ]; then
	\rm -f temp.changed
    fi
}

copyrightFile=$1
fileToModify=$2
destDirectory="/tmp"
if [ $# -eq 3  ]; then
    if [ -d $3 ]; then
	destDirectory=$3
    fi
fi

# temporarily create local copy of copyright file; sed doesnt expand shell 
# variables (??)
cat $copyrightFile > Copyright.temp.txt

sed -e '/\$Copyright\$/r Copyright.temp.txt' $fileToModify | sed -e '/\$Copyright\$/d;w temp.changed' 
if [ ! -f temp.changed ]; then
    echo "Did not find pattern \$Copyright\$ in the file; no changes made"
    Cleanup
    exit -1
fi

diff temp.changed $fileToModify | sed -e '/^[0-9]/d;s/^< //;/^---/d;/^>/d' > /tmp/difftext

# The copyright notice should be the only change
check=`diff /tmp/difftext Copyright.temp.txt`
echo $check
if [[ check -eq "" ]]; then
    mv temp.changed $destDirectory/$fileToModify
    returnValue=$success
else
    returnValue=$error
fi

Cleanup
exit $returnValue
