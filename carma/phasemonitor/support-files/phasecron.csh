#!/bin/tcsh

setenv MIR /array/miriad/cvs
if ( -e /array/miriad/cvs/MIRRC.linux == 1) then
  source /array/miriad/cvs/MIRRC.linux
else
  echo "Unable to initialize MIRIAD environment!"
  exit 0
endif

setenv PHASEHOME ~obs/phasemon

@ start = `wc $PHASEHOME/data/sample.dat | awk '{print $1}'` - 1300
awk "NR>=$start{print}" $PHASEHOME/data/sample.dat > $PHASEHOME/data/sample.dat.temp
#awk '{printf "%5.2f %d %d %d %d %g %g %d %.9g %.9g\n", ($1-int($1))+(int($1)-53736), $2, $3, $4, $5, $6, $7, $8, $9, $10}' $PHASEHOME/data/sample.dat.temp1 > $PHASEHOME/data/sample.dat.temp

cd $PHASEHOME/data
wip -qb -d ~obs/web_pages/phasemon/pmn_status.gif/gif $PHASEHOME/wip/status.hcro.wip
wip -qb -d ~obs/web_pages/phasemon/pmn.gif/gif $PHASEHOME/wip/pmn.carma.wip
#wip -qb -d ~obs/web_pages/phasemon/1mm.gif/gif $PHASEHOME/wip/1mm.wip


