#!/bin/bash

CVSROOT=:ext:build@cvs.mmarray.org:/sw/cvscarma
export CVSROOT
CVS_RSH=ssh
export CVS_RSH
STATCVS_DIR=/home/build/statcvs

cd $STATCVS_DIR

# Update CVS tree
if [ -d carma ]; then
/usr/bin/cvs upd -PAd carma > /dev/null
else 
/usr/bin/cvs co carma > /dev/null
fi

# Generate cvs log file
cd carma; /usr/bin/cvs log > carmacvs.log; cd ..;

# Generate cvs statistics
/usr/bin/java -jar statcvs-0.2.2/statcvs.jar -output-dir \
/usr/local/www-docs/mmarray/project/system/statcvs -exclude \
 doc/**:sza/**:conf/**:carma/antenna/bima/hatcreek/**:**/*.jar \
 -title "CARMA" -viewcvs \
 'http://cvs.mmarray.org/viewcvs/viewcvs.cgi/carma/' \
 ./carma/carmacvs.log ./carma/
