#!/bin/csh -f
# creates a cumulative distribution of delays.  jrf - 04oct03
# automated for carma phasemonitor monthly data - jrf - 11mar06
# made monthly archive info go to archive/$mondir - colby - 13mar06

setenv PM /home/obs/phasemon
setenv WD /array/miriad/cvs/bin/linux
setenv DATA /home/obs/phasemon/data

if ( -e /array/miriad/cvs/MIRRC.linux == 1) then
  source /array/miriad/cvs/MIRRC.linux
else
  echo "Unable to initialize MIRIAD environment!"
  exit 0
endif


# make current month directory if it doesn't exist yet,
# otherwise tail current month from the sample.dat file
# ------------------------------------------------------
cd $PM/monthly
if ( $#argv == 0 ) then
  setenv DATESTRING `date +"%b%Y"`
  set yearmon=`date +"%Y %b"`
  set retro=0
else
  if ( -e $argv[1] ) then
    set smjd=`head -1 $argv[1] | awk '{print $1}'`
    setenv DATESTRING `/opt/rt/bin/timeConvert frommjd=$smjd | awk '{print $2$1}'`
    set yearmon=`/opt/rt/bin/timeConvert frommjd=$smjd | awk '{print $1" " $2}'`
    set retro=1
  else
    echo "Cannot find file $argv[1]"
    exit 1
  endif
endif

# Processed samples are generated every 10 minutes
# this leaves a maximum of 4464 samples generated each month
# (6*24*31)
# tail the last 4464 lines and look for whatever we're after...
setenv MONDIR archive/$DATESTRING
setenv MONSAMPS $MONDIR/sample

mkdir -p archive/$DATESTRING

if ( -e $MONSAMPS.dat ) then
  # samples have already been copied, use the last sample as a timestamp
  set mjd=`tail -1 $MONSAMPS.dat | awk '{print $1}'`

else
  # samples have not already been copied, default to 1st of current month
  set mjd=`/opt/rt/bin/timeConvert tomjd="$yearmon 01 00:00:00"`
endif

# now copy the samples
setenv SMJD $mjd
if ( $retro == 0 ) then
 set tailfile=../sample.dat
else
 set tailfile=$argv[1]
endif

tail -4464 $tailfile | awk 'BEGIN{smjd=ENVIRON["SMJD"];}{ if($1>smjd) print $0;}' >> $MONSAMPS.dat

# Now update files for wip to include info into plot
set lastmjd=`tail -1 $MONSAMPS.dat | awk '{print $1}'`
/opt/rt/bin/timeConvert frommjd=$lastmjd | awk '{print $1" "$2" "$3" "$5}' > $MONDIR/last_sample_time
/opt/rt/bin/timeConvert frommjd=$lastmjd | awk '{print $2" "$1}' > $MONDIR/Month

# make cumulative distribution file in current month directory
# ------------------------------------------------------
rm -f $MONSAMPS.cum
awk '{print $4}' $MONSAMPS.dat | sort -n > ! junk
set nct = `wc junk | awk '{print $1}'` 
echo $nct > $MONDIR/N
awk '{print $1, NR*(100/nct)}' nct=$nct junk >! $MONSAMPS.cum
rm junk

# plot histogram and cumulative distribution in one plot
# from the current month directory
# ------------------------------------------------------
cd $MONDIR
$WD/wip -qb -d cumulative.gif/gif $PM/wip/chisto.wip

# only copy plot into place if we're not doing a retro active data build
if ( $retro == 0 ) then
  cp -f cumulative.gif ../..
endif

exit 0
