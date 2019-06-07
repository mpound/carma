#!/bin/csh -fx
# creates a calibration plots

setenv PM /home/obs/phasemon
setenv WD /array/miriad/cvs/bin/linux
setenv DATA /home/obs/phasemon/data

if ( -e /array/miriad/cvs/MIRRC.linux == 1) then
  source /array/miriad/cvs/MIRRC.linux
else
  echo "Unable to initialize MIRIAD environment!"
  exit 0
endif

# TEMPORARY
setenv LD_LIBRARY_PATH /home/colby/CARMA/build/lib:$LD_LIBRARY_PATH
setenv PATH /home/colby/CARMA/build/bin:$PATH
# TEMPORARY

# make current month directory if it doesn't exist yet,
# otherwise tail current month from the sample.dat file
# ------------------------------------------------------
cd $PM/calibration
setenv MONSTRING `date +"%b%Y"`
setenv DAYSTRING `date +"%d"`
setenv PRETTY `date +"%d %b %Y"`

# Raw samples have no time stamps, but are created every 1
# second. Lets plot what's been put into the raw file for the last
# 24 hours
setenv DAYDIR archive/$MONSTRING/$DAYSTRING
mkdir -p $DAYDIR
cd $DAYDIR
mkdir -p uncorrected
mkdir -p corrected
tail -86400 $PM/data/raw.dat > raw.dat

echo "$PRETTY (UNCORRECTED)" > uncorrected/currentday
echo "$PRETTY (CORRECTED)" > corrected/currentday


# Now produce uncorrected data
carmaPhaseMonitor emulate=true parameters=/opt/rt/conf/phasemonitor/Calibration.tab output=uncorrected replay=raw.dat imr=crap

# Now produce corrected data with current parameters
carmaPhaseMonitor emulate=true parameters=/opt/rt/conf/phasemonitor/CedarFlat.tab output=corrected replay=raw.dat imr=crap

cd uncorrected
$WD/wip -qb -d ../uncorrected.gif/gif $PM/wip/calibration.wip
cd ../corrected
$WD/wip -qb -d ../corrected.gif/gif $PM/wip/calibration.wip

# Now wipe our ass
cd ..
\rm -rf uncorrected
\rm -rf corrected
bzip2 raw.dat

# and copy for current display
cp *.gif ../../..

exit 0
