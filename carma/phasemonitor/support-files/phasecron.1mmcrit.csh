#!/bin/tcsh

setenv MIR /array/miriad/cvs
if ( -e /array/miriad/cvs/MIRRC.linux == 1) then
  source /array/miriad/cvs/MIRRC.linux
else
  echo "Unable to initialize MIRIAD environment!"
  exit 0
endif

setenv PHASEHOME ~obs/phasemon

# 144 entries = 24 hours
@ start = `wc $PHASEHOME/data/sample.dat | awk '{print $1}'` - 144
awk "NR>=$start{print}" $PHASEHOME/data/sample.dat > $PHASEHOME/data/sample.1mmcrit.dat

cd $PHASEHOME/data
wip -qb -d ~obs/web_pages/phasemon/pmn.1mmcrit.gif/gif $PHASEHOME/wip/pmn.carma.1mmcrit.wip


