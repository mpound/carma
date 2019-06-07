#!/bin/csh -f

# The data reduction steps/code taken heavily from the quality script 

# Initialize parameters.
#  vis is required, and should be of the form /home/obs/data/<miriad_file_name>
#  sources, gainscals, passcals, and fluxcal are optional parameters
set vis      = ''
set sources  = ''
set gaincals = ''
set passcals = ''
set fluxcal  = ''

# Default parameters (user can override on command line)
set ampgainsolint=5      # Selfcal interval (mins) to estimate antenna gains
                         # (0 to calculate based on gaincal fluxes)
set bpsolint=1           # Selfcal interval (mins) for bandpass solution
set dolinecal=0          # Apply the linelength phases to data (1=true)
set edgeflag=2           # Number of edge channels to flag when imaging
set refant=
set c15refant=9          # Default CARMA-15 ref antenna
set c23refant=9          # Default CARMA-23 ref antenna
set szarefant=21         # Default SZA ref antenna
set ref3mm=112           # Fiducial frequency for 3mm grading. 0 => use LO1
set ref1mm=230           # Fiducial frequency for 1mm grading. 0 => use LO1
set smavarplt=1          # Use SMA version of varplt for tsys.gif (1=true)
set update=1             # Update history? (0->No  1-> Yes)
set bad_refants  = ()    # List of bad reference antennas; e.g. (1 2 3)


# Pacs
set pacs   = ""  # Pacs obsblock name
set config = "A"  # CARMA array configuration
set script_dir = "/array/obs/bin"  # Script directory for pacs script


# Override above parameters if given on the command line
foreach a ( $* )
   set check=`echo $a | awk -F= '{print NF}'`
   if ( "$check" == 2 ) set $a
end

# Generate filenames and directory structure
if ($vis == '') then
   if ($#argv == 1) then
       set vis=$1
   else
       echo "*** FATAL: vis= keyword must be specified."
       exit 0
   endif
else if (!(-e $vis)) then
   echo "$vis does not exist!"
   exit 0
endif

# Set output directories.
# DIR_GAINS      is the linux path to the directory
# URL_GAINS      is the URL address
# SUBDIR_RESULTS is the subdirectory underneath DIR_GAINS which contains plots
setenv DIR_GAINS      "/array/utilities/apache2.0.54/htdocs/gains"
setenv URL_GAINS      "/gains"
setenv SUBDIR_RESULTS "results"
cd $DIR_GAINS

# Set output directory name for gif files
# ROOT     is the name of the miriad file (without the ".mir")
# OUTDIR   is the subdirectory in the results area that will contain the output
# SCRATCH  is a scratch directory to perform the data analysis.
set OBSBLOCK = `echo $vis | awk 'split($1,a,"/") {print a[5]}'`
set ROOT     = $OBSBLOCK:r
set pthtmp = `echo $ROOT | sed 's/\./\|/'`
set prjtag = `echo $pthtmp | awk '{split($1,a,"|"); print a[1]}'`
set obstag = `echo $pthtmp | awk '{split($1,a,"|"); print a[2]}'`

set OUTDIR_ROOT  = "$SUBDIR_RESULTS/$prjtag"
set OUTDIR       = "$OUTDIR_ROOT/$obstag"
set OUTDIR_FULL  = "$DIR_GAINS/$OUTDIR"
set SCRATCH_ROOT = /tmp/scratch/
set SCRATCH      = "$SCRATCH_ROOT/$ROOT"

# Remove old files
find $SCRATCH_ROOT/* -maxdepth 0 -mmin +30 -exec rm -rf '{}' ';'

# Make the directories
if (!(-e $OUTDIR_ROOT))  mkdir $OUTDIR_ROOT
if (!(-e $OUTDIR))       mkdir $OUTDIR
if (!(-e $SCRATCH_ROOT)) mkdir $SCRATCH_ROOT
if (!(-e $SCRATCH))      mkdir $SCRATCH

# Set history file
setenv HISTORY history.html

# Set script log file. 
setenv SCRIPTLOG "http://cedarflat.mmarray.org/quality/$prjtag/$obstag/scriptlog.txt"

# Set font directory. This is needed since user "control" does not
# have this set
setenv PGPLOT_FONT "/array/miriad/cvs/lib/linux/grfont.dat"

# Create web page
echo "" 
echo "*** Creating update web page"
set logfile = $SCRATCH/update.html
rm -rf $logfile update.html
setenv DATE `date`
echo "<html>" > $logfile
echo "<head>" | tee -a $logfile
echo "<title>CARMA Gain plots</title>" | tee -a $logfile
echo '<meta http-equiv="refresh" content="60">' | tee -a $logfile
echo "</head>" | tee -a $logfile
echo "<body>" | tee -a $logfile
echo "<center>" | tee -a $logfile
echo "<H2>Web page created $DATE</H2>" | tee -a $logfile
echo "(automatic updates every 1 minute)<BR>" | tee -a $logfile
echo "</center>" | tee -a $logfile
echo "<BR><h3><font color=blue>Computing gains for $vis ...</font></h3><BR>" | tee -a $logfile
echo "<pre>" | tee -a $logfile

# Allow substitute routines
if ($?meanmed) then
    alias meanmed $meanmed
endif
if ($smavarplt == 1) then
    alias svarplt 'smavarplt dotsize=3'
else
    alias svarplt varplt
endif

# Move to working directory
cd $SCRATCH
echo "Working in directory $SCRATCH" | tee -a $logfile
echo "" | tee -a $logfile


#------------------------ SUBARRAY BLOCK ----------------------------------
# Makes various choices based on the current subarray.

echo "" | tee -a $logfile
echo "*** Copying and listing the data" | tee -a $logfile

# Copy the cross-correlation data
set mirfile_root = `echo $vis | awk 'split($1,a,"/") {print a[5]}'`
rsync -r $vis .

# Run listobs on original miriad file
set listobs = listobs_orig
listobs vis=$mirfile_root log=$listobs

# Run listobs to identify the observing frequency and antennas
set lofreq=`awk '/First LO/ {print $10}' $listobs`
set nlofreq = `echo "$lofreq" | awk '{printf("%d",$1)}'`
set intfreq=`calc -i "$lofreq"`
set antslin=`awk '/Sys Temps/ {print NR+1}' $listobs`
set ants=`sed -n "${antslin}p" $listobs | sed 's/\(.*[deg,mode]\)//'`
set nants = $#ants
set chanperband=`uvlist vis=$mirfile_root options=var,full | grep nschan | head -1 | awk '{print $NF}'`
set modedesc=`uvio vis=$mirfile_root | grep modedesc | tail -1 | awk '{print $4}' | grep -c b`

# identify which correlator
if ($chanperband == 15 && $modedesc == 0) then
    set corr = 'WB'
else
    set corr = 'SL'
endif
echo "" | tee -a $logfile
echo "Correlator type: $corr" | tee -a $logfile

# Identify the (sub)array
if ($corr == 'WB') then
    set subarray = 2
else if ($corr == 'SL') then
    set subarray = 1
endif
echo "" | tee -a $logfile
echo "Subarray number: $subarray" | tee -a $logfile

# Identify the number of ants
#if ($ants[1] > 15) then
if ($#ants < 9) then
    set array = 'CARMA-3.5m'
else if ($#ants > 15) then
    set array = 'CARMA23'
else
    set array = 'CARMA15'
endif
echo "" | tee -a $logfile
echo "Configuration type: $array" | tee -a $logfile


# Set the data type
if ($array == 'CARMA15') then
    set datatype = 3
    echo "OVRO+BIMA mode"
    set nxy='3,5'         # all antennas
    set nxygp='5,3'       # found antennas, gains(time)
    set nxybp='3,5'       # found antennas, bandpass
    set nxytsys='3,5'     # found antennas, systemp
else if ($array == 'CARMA23') then
    if ($nlofreq > 60) then
        set datatype = 8
        echo "CARMA23 3mm USB Mode"
    else
        set datatype = 6
        echo "CARMA23 1cm LSB Mode"
    endif
    echo "Ultimate mode: OVRO+BIMA+SZA"
    set nxy='4,6'
    set nxygp='6,4'
    set nxybp='4,6'
    set nxytsys='6,4'
else if ($array == 'CARMA-3.5m') then
    if ($nlofreq > 60) then
        set datatype = 4
        echo "SZA 3mm USB Mode"
    else
        set datatype = 2
        echo "SZA 1cm LSB Mode"
        set tmax=150
    endif
    set nxy='4,6'
    set nxygp='4,2'
    set nxybp='3,3'
    set nxytsys='5,3'
endif
echo "Data type code: $datatype"

# Make a copy of the file that can be manipulated.
# Remove any spectral windows with zero channels.
set mirfile = $mirfile_root.copy
rm -rf $mirfile
prthd in=$mirfile_root > tmp.prthd
set li=`awk '/Spectrum/ {print NR+1}' tmp.prthd`
set lf=`awk '/channels/ {print NR-1}' tmp.prthd`
set drop=`sed -n "$li,${lf}p" tmp.prthd | awk '{if ($2==0) print $1}'`
# For arrays incl. 3.5m dishes we also need to toss out one sideband
# We drop the USB for 1cm data and the LSB for 3mm data
if ($array == 'CARMA-3.5m' || $array == 'CARMA23') then
    set lsb=`sed -n "$li,${lf}p" tmp.prthd | awk -v lo=$lofreq '{if ($3<lo) print $1}'`
    set usb=`sed -n "$li,${lf}p" tmp.prthd | awk -v lo=$lofreq '{if ($3>lo) print $1}'`
    if ($datatype == 2 || $datatype == 6) set drop=($drop $usb)
    if ($datatype == 4 || $datatype == 8) set drop=($drop $lsb)
endif
set drop2=`echo $drop | sed 's/ /\n/g' | sort -n | uniq`
set flag=`echo $drop2 | sed 's/ /,/g'`
set select="-source(noise)"
if ($flag != "") then
    echo "Discarding windows $flag"
    set select="$select,-win($flag)"
endif
uvcat vis=$mirfile_root select="$select,-auto" out=$mirfile options=nowide

# Flag bad windows
if ($array == "CARMA-3.5m") then
   uvflag vis=$mirfile select="ant(17)(18),win(2)" flagval=flag
endif

# Choose the reference antenna
if ($refant == '') then
    if ($array == 'CARMA-3.5m') set refant=$szarefant
    if ($array == 'CARMA15') set refant=$c15refant
    if ($array == 'CARMA23') set refant=$c23refant
endif

# Make a list of good reference antennas
set goodants = ()
foreach ant ($ants) 
   set bad = 0
   foreach bant ($bad_refants)
       if ($ant == $bant) set bad = 1
   end
   if ($bad == 0) set goodants = ($goodants $ant)
end

# Choose a new reference antenna if default is unavailable
set reffound = 0
foreach ant ($goodants)
    if ($refant == $ant) then
        set reffound = 1
    endif
end
if ($reffound == 0) then
    echo "*** WARNING: Requested reference antenna $refant not found" | tee -a $logfile
    set refant = $goodants[1]
endif
echo "Using $refant as the reference antenna" | tee -a $logfile


#------------------------ LISTOBS BLOCK ----------------------------------
# Provides an observations summary and classifies the observed targets.

# Re-generate the wideband channels if necessary
uvlist vis=$mirfile options=var,full log=tmp.uvlist
set nspect=`grep nspect tmp.uvlist | sed 's/.*nspect[ ]*://' | awk '{print $1}'`
set npol=`grep npol tmp.uvlist | sed 's/.*npol[ ]*://' | awk '{print $1}'`
set pol1=`grep Polar tmp.prthd | sed 's/.*://' | sed 's/,/ /' | awk '{print $1}'`   
if (`itemize in=$mirfile/vartable | grep nwide` == '') then
    echo "Re-creating $nspect wideband windows"
    rm -rf uvwide.mir
    uvwide vis=$mirfile out=uvwide.mir nwide=$nspect
    set mirfile=uvwide.mir
endif

# Get the date and frequency
set CalDate=`awk '/Chronology/ {print $5}' $listobs`
set lofreq=`awk '/First LO/ {print $10}' $listobs`

# Get the maximum and median baseline length
set lambda=`calc -f f8.6 "0.3/$lofreq"` # meters
grep Bsln $listobs | sed 's/\(.*:\)//' | awk '{print sqrt($1^2 + $2^2 + $3^2)}' \
    >! bsln.out
set maxbsln=`sort -n bsln.out | tail -1`
set medbsln=`meanmed infile=bsln.out | tail -1 | awk '{print $2}'`
set baselen=`calc -f f8.2 "$lambda*$maxbsln"`

# Get the source list and their intents
set beglin2=`awk '/Observed Sources/ {print NR+2}' $listobs`
set endlin2=`awk '/Frequency Set-up/ {print NR-2}' $listobs`
set allobs=`sed -n "$beglin2,${endlin2}p" $listobs | awk '{print $1}'`
set purpose=`sed -n "$beglin2,${endlin2}p" $listobs | awk '{print $2}'`
set i=1
set defsources=
set defgaincals=
set defpasscals=
set deffluxcal=
echo "" | tee -a $logfile
while ($i <= $#allobs)
    if ($allobs[$i] != "NOISE") then
       if ($purpose[$i] =~ *B*) set defpasscals = ($defpasscals $allobs[$i])
       if ($purpose[$i] =~ *G*) set defgaincals = ($defgaincals $allobs[$i])
       if ($purpose[$i] =~ *S*) set defsources = ($defsources $allobs[$i])
       if ($purpose[$i] =~ *F*) set deffluxcal = ($deffluxcal $allobs[$i])
       echo "Source $allobs[$i] has purpose $purpose[$i]" | tee -a $logfile 
    endif
    set i = `expr $i + 1`
end

# If there are 2 or more passcals, remove planets:
if ($#defpasscals > 0) then
    foreach passcal ($defpasscals)
        if ($passcal =~ {MERCURY,VENUS,MARS,JUPITER,SATURN,URANUS,NEPTUNE,MWC349}) then
            if ($#defpasscals > 0) then
                set defpasscals=`echo $defpasscals | sed "s/$passcal//"`
            endif
        endif
    end
endif

# If there are 2 or more gaincals, remove planets:
if ($#defgaincals > 1) then
    foreach gaincal ($defgaincals)
        if ($gaincal =~ {MERCURY,VENUS,MARS,JUPITER,SATURN,URANUS,NEPTUNE,MWC349}) then
            if ($#defgaincals > 1) then
                set defgaincals=`echo $defgaincals | sed "s/$gaincal//"`
            endif
        endif
    end
endif

# If there are 2 or more fluxcals, choose a planet:
if ($#deffluxcal > 1) then
    foreach obj ($deffluxcal)
        if ($obj !~ {MERCURY,VENUS,MARS,JUPITER,SATURN,URANUS,NEPTUNE,MWC349}) then
            if ($#deffluxcal > 1) then
                set deffluxcal=`echo $deffluxcal | sed "s/$obj//"`
            endif
        endif
    end
    if ($#deffluxcal > 1) set deffluxcal=$deffluxcal[1]
endif

# State the default classifications
echo "" | tee -a $logfile
echo "Default assignments, based on source intent:" | tee -a $logfile
echo "sources : $defsources" | tee -a $logfile
echo "gaincals: $defgaincals" | tee -a $logfile
echo "passcals: $defpasscals" | tee -a $logfile
echo "fluxcal: $deffluxcal" | tee -a $logfile

# Reset if some calibrator lists are blank
if ("$defsources"  == "" && $#defgaincals > 0) set defsources  = ($defgaincals)
if ("$defpasscals" == "" && $#defgaincals > 0) set defpasscals = ($defgaincals)
if ("$defgaincals" == "" && $#defpasscals > 0) set defgaincals = ($defpasscals)
if ($#defpasscals == 0 && $#defgaincals == 0 && $#deffluxcal > 0) then
   set defgaincals = ($deffluxcal)
   set defpasscals = ($deffluxcal)
endif

# Use the command-line parameters, with intent as fallback
if ($sources == '')  set sources=`echo $defsources | sed 's/ /,/g'`
if ($gaincals == '') set gaincals=`echo $defgaincals | sed 's/ /,/g'`
if ($passcals == '') set passcals=`echo $defpasscals | sed 's/ /,/g'`
if ($fluxcal == '')  set fluxcal=$deffluxcal

# Compile lists of sources
set sourcelis=`echo "$sources" | sed 's/,/ /g'`
set gaincalis=`echo "$gaincals" | sed 's/,/ /g'`
set passcalis=`echo "$passcals" | sed 's/,/ /g'`
if ("$gaincalis" != "") then
   if ("$sourcelis" == "") set sourcelis = $gaincalis
   if ("$passcalis" == "") set passcalis = $gaincalis
endif
if ("$passcalis" != "") then
   if ("$sourcelis" == "") set sourcelis = $passcalis
   if ("$gaincalis" == "") set gaincalis = $passcalis
endif
set allcals = ()
set allobjs = ()
foreach obj ($gaincalis $passcalis $fluxcal)
    set val=`echo "$allcals" | grep "$obj"`
    if ("$val" == "") set allcals = ($obj $allcals)
end
foreach obj ($sourcelis $gaincalis $passcalis $fluxcal)
    set found = 0
    foreach a ($allobjs)
      if ($obj == $a) set found = 1
    end
    if ($found == 0) set allobjs = ($obj $allobjs)
end

# Print object classification
echo "" | tee -a $logfile
echo "Actual assignments:" | tee -a $logfile
echo "sources : $sources" | tee -a $logfile
echo "gaincals: $gaincals" | tee -a $logfile
echo "passcals: $passcals" | tee -a $logfile
echo "fluxcal : $fluxcal" | tee -a $logfile

# Get the elapsed project time, note this includes NOISE integrations
set beglin=`awk '/Sys Temps/ {print NR}' $listobs`
@ beglin ++
@ beglin ++
set endlin=`wc $listobs | awk '{print $1-1}'`
sed -n "$beglin,${endlin}p" $listobs > listobs.tab
head -1 listobs.tab > log1
tail -1 listobs.tab > log2
set starttime=`awk '{print $2}' log1`
set stoptime=`awk '{print $2}' log2`
set starthours=`echo $starttime | awk '{split ($1,h,""); print h[1] h[2]}'`
set startminutes=`echo $starttime | awk '{split ($1,h,""); print h[3] h[4]}'`
set stophours=`echo $stoptime | awk '{split ($1,h,""); print h[1] h[2]}'`
set stopminutes=`echo $stoptime | awk '{split ($1,h,""); print h[3] h[4]}'`
set addminutes=`awk '{print $4}' log2`
set starttime = `calc -f f6.3 "$starthours+$startminutes/60.0"`
set stoptime  = `calc -f f6.3 "$stophours+($stopminutes+$addminutes)/60.0"`
set stoptime  = `echo $starttime $stoptime | awk '{if ($1 > $2) print ($2 + 24); else print $2}'`
set totaltime = `calc -f f6.1 "$stoptime-$starttime"`

# Determine the correlator setup
prthd in=$mirfile options=full log=tmp.prthd 
set beglin=`awk '/Channels/ {print NR}' tmp.prthd`
@ beglin ++
set endlin=`awk '/Total number of channels/ {print NR}' tmp.prthd`
set endlin=`expr $endlin - 1`
sed -n "$beglin,${endlin}p" tmp.prthd > tmp.corr1
set corrno   = `cat tmp.corr1 | awk '{print $1}'`
set channels = `cat tmp.corr1 | awk '{print $2}'`
set corfreq  = `cat tmp.corr1 | awk '{print $3}'`
set bandw    = `cat tmp.corr1 | awk '{printf "%5d", 1000*$2*sqrt($4*$4)}'`

# Output information about the correlator setup
echo "" | tee -a $logfile
echo "Correlator setup:" | tee -a $logfile
cat tmp.corr1 | tee -a $logfile

# Get the 500 MHz window on phasecal, otherwise just use largest bw
set buse=
set maxbw = 0
set win_flag = ""
set nchan_wideband
echo "" |& tee -a $logfile
echo "*** Finding continuum bands" |& tee -a $logfile
foreach i ($corrno)
    echo "    Band $i : bandwidth = $bandw[$i]" |& tee -a $logfile
    set isWide = `echo $bandw[$i] 468.0 | awk '{print ($1>=$2)}'`
    set isWideMaxbw = `echo $maxbw 468.0 | awk '{print ($1>=$2)}'`
    if (`echo $channels[$i] 0 | awk '{print ($1>$2)}'` == 1 && \
             (`echo $bandw[$i] $maxbw | awk '{print ($1>=$2)}'` == 1 || \
             $isWide == 1)) then
        if ($bandw[$i] == $maxbw || ($isWide == 1 && $isWideMaxbw == 1)) then
           if ($buse != "") set buse = "$buse,"
           set buse = "$buse$i"
           @ nchan_wideband += $channels[$i]
        else
           set buse = "$i"
           set nchan_wideband = $channels[$i]
        endif
        set maxbw = $bandw[$i]
    endif
end
echo "Using windows $buse, BW $maxbw MHz for gain calibration" | tee -a $logfile
if ($maxbw == 0) then
   echo "*** Bug in run_gainCheck.csh"
   echo "tmp.corr1 :"
   cat tmp.corr1
endif

# Set number of wide-band channels
set wideband=`echo $buse | sed 's/,/ /g'`
set nwideband = $#wideband

# Output some useful diagnostics, including UT, LST, and weather data
foreach var (rmspath tau230) 
   setenv OUT $var.gif
   rm -rf $OUT
   varplt vis=$mirfile yaxis=$var log=$var.log device=$OUT/gif
end

# Clean up
rm -f $listobs log[1-2]


#------------------------ SYSTEMP BLOCK ----------------------------------

echo "" | tee -a $logfile
echo "*** Plotting system temperatures" | tee -a $logfile

# Plot system temperatures
setenv PLOT_TSYS plot_tsys.gif
rm -rf $PLOT_TSYS 
set mirtsys = $mirfile
if ($npol >= 2) alias svarplt varplt  # smavarplt does not work with fullstokes
svarplt vis=$mirtsys yaxis=systemp nxy=$nxytsys device=$PLOT_TSYS/gif
if ($array == "CARMA-3.5m") mv ${PLOT_TSYS}_2 $PLOT_TSYS

#--------------------------- CSFLAG BLOCK -------------------------------
echo "" | tee -a $logfile
echo "*** Flagging shadowed data" | tee -a $logfile

csflag vis=$mirfile > tmp.csflag
echo -n "csflag:" | tee -a $logfile
tail -1 tmp.csflag | tee -a $logfile

#--------------------------- LINECAL BLOCK -------------------------------
echo "" | tee -a $logfile
echo "*** Plotting the linelength phases" | tee -a $logfile
setenv PLOT_LINECAL plot_linecal.gif
rm -rf $PLOT_LINECAL
linecal vis=$mirfile 
smagpplt vis=$mirfile yaxis=phase yrange=-200,200 options=wrap,dots,nofit \
    device=$PLOT_LINECAL/gif nxy=$nxygp dotsize=5
rm -rf ${PLOT_LINECAL}_2
if ( $dolinecal == 0 ) rm $mirfile/gains

#--------------------------- OBSTIME BLOCK--------------------------------
echo "" | tee -a $logfile
echo "*** Evaluating observing time statistics" | tee -a $logfile

echo "" | tee -a $logfile
echo "Total project time $totaltime hrs from start to finish" | tee -a $logfile

# Split the data in order to get time on each source
set timesum=0
foreach obj ($allobjs)
    rm -rf $obj
    uvcat vis=$mirfile out=$obj select="source($obj)" options=unflagged
    uvindex vis=$obj log=tmp.uvindex
    set term=`grep "Total observing time" tmp.uvindex | awk '{print $5}'`
    echo "Total observe time $term hrs for $obj" | tee -a $logfile
    set timesum=`echo "$timesum+$term" | bc -l`
end
echo "Total observe time $timesum hrs for entire track" | tee -a $logfile


#--------------------------- UVDISTANCE BLOCK------------------------------
# Plot calibrated phases vs. uv-distance.
# Calibrates the gaincal data and estimates decorrelation over 5 mins.

echo "" | tee -a $logfile
echo "*** Plotting calibrated phase vs. uvdistance" | tee -a $logfile

# Merge the gain calibrators into a single dataset
rm -rf gaincals.mir
uvcat vis=$mirfile select="source($gaincals)" options=nopol,nopass out=gaincals.mir

#------------------------------ PASSBAND BLOCK ------------------------------
# Calibrates the bandpass and copies bandpass gains to data.
# A separate passband is done for wide-band windows only since narrow band
# windows can corrup the passband solution.

echo "" | tee -a $logfile
echo "*** Calibrating the bandpass" | tee -a $logfile

# Skip this if no passband calibrators.
set nopasscals = 1
if ($#passcals == 1 && "$passcals" != '') then
   # Select the appropriate passband data.
   set nopasscals = 0
   rm -rf passcals.mir passcals_wide.mir
   echo "uvcat vis=$mirfile select=source($passcals) options=nopol,nopass out=passcals.mir " | tee -a $logfile
   uvcat vis=$mirfile select="source($passcals)" options=nopol,nopass out=passcals.mir
#  uvcat vis=$mirfile select="source($passcals),win($buse)" options=nopol,nopass out=passcals_wide.mir
   if ($npol >= 2) then
      setenv PLOT_PB_AMP_RR   plot_passband_amp.gif
      setenv PLOT_PB_PHASE_RR plot_passband_phase.gif
      setenv PLOT_PB_AMP_TEMP   plot_passband_amp.gif_2
      setenv PLOT_PB_PHASE_TEMP plot_passband_phase.gif_2
      setenv PLOT_PB_AMP_LL   plot_passband_amp_LL.gif
      setenv PLOT_PB_PHASE_LL plot_passband_phase_LL.gif
      rm -rf $PLOT_PB_AMP_RR $PLOT_PB_PHASE_RR $PLOT_PB_AMP_LL $PLOT_PB_PHASE_LL $PLOT_PB_AMP_TEMP $PLOT_PB_PHASE_TEMP
   endif
   setenv PLOT_PB_AMP   plot_passband_amp.gif
   setenv PLOT_PB_PHASE plot_passband_phase.gif
   rm -rf $PLOT_PB_PHASE $PLOT_PB_AMP
   # Compute the passband solutions and plot.
   echo "mfcal vis=passcals.mir interval=$bpsolint refant=$refant" | tee -a $logfile
   mfcal vis=passcals.mir interval=$bpsolint refant=$refant |& tee -a $logfile
#  echo "mfcal vis=passcals_wide.mir interval=$bpsolint refant=$refant" | tee -a $logfile
#  mfcal vis=passcals_wide.mir interval=$bpsolint refant=$refant |& tee -a $logfile
   # Plot passband
   smagpplt vis=passcals.mir options=bandpass,nofit device=$PLOT_PB_AMP/gif \
       xaxis=chan yrange=0,2 nxy=$nxybp select='pol(RR)' filelabel=1
   smagpplt vis=passcals.mir options=bandpass,wrap,nofit device=$PLOT_PB_PHASE/gif \
       xaxis=chan yaxis=phase yrange=-200,200 nxy=$nxybp select='pol(RR)' filelabel=1
   if ($npol >= 2) then
      # Change gif_2 to LL.gif files
      mv $PLOT_PB_AMP_TEMP $PLOT_PB_AMP_LL
      mv $PLOT_PB_PHASE_TEMP $PLOT_PB_PHASE_LL
   endif

endif

#----------------------------- GAINS BLOCK -------------------------------
echo "" | tee -a $logfile
echo "*** Calibrating the gains" | tee -a $logfile

# Apply the bandpass to gain calibrators
if ($nopasscals == 0) then
    foreach cal ($gaincalis)
        gpcopy vis=passcals.mir out=$cal options=nocal
        echo "" | tee -a $logfile
        echo "Passband gains copied to $cal" | tee -a $logfile
    end
endif

# Plot gains(time) using a short solution interval
echo "" | tee -a $logfile
echo "*** Plotting gains" | tee -a $logfile
rm -rf refant$refant.gains
foreach cal ($gaincals)
    echo "" |& tee -a $logfile
    echo "--- Processing $cal" |& tee -a $logfile
    rm -rf gcal.mir
    echo "    Using bands $buse" |& tee -a $logfile
    uvcat vis=$cal select="win($buse)" options=nocal out=gcal.mir |& tee -a $logfile
    mfcal vis=gcal.mir interval=$ampgainsolint refant=$refant options=nopass |& tee -a $logfile
    if ($cal == $gaincals[1]) then
        gpcopy vis=gcal.mir out=refant$refant.gains mode=create options=nopass
    else
        gpcopy vis=gcal.mir out=refant$refant.gains mode=merge options=nopass
    endif
end
setenv PLOT_GAIN_AMP   plot_gains_amp.gif
setenv PLOT_GAIN_PHASE plot_gains_phase.gif
rm -rf $PLOT_GAIN_AMP $PLOT_GAIN_PHASE
gpplt vis=refant$refant.gains nxy=$nxygp yaxis=amp yrange=0,3 \
    device=$PLOT_GAIN_AMP/gif |& tee -a $logfile
gpplt vis=refant$refant.gains nxy=$nxygp yaxis=phase yrange=-200,200 \
    device=$PLOT_GAIN_PHASE/gif options=wrap|& tee -a $logfile
rm -rf refant$refant.avgains
gpcopy vis=refant$refant.gains out=refant$refant.avgains mode=create
puthd in=refant$refant.gains/senmodel value='GSV' type=ascii
puthd in=refant$refant.gains/interval value=0.1 type=real

# Average the amplitude gains and report to logfile
gpaver vis=refant$refant.avgains interval=1000 options=scalar
gpplt vis=refant$refant.avgains yaxis=amp log=ampgains.dat
echo "" | tee -a $logfile
echo -n "Calibrated antenna gains from $gaincals" | tee -a $logfile
echo " (soln interval $ampgainsolint min)" | tee -a $logfile
tail -n +5 ampgains.dat | tee -a $logfile

#----------------------------- IMAGE BLOCK -------------------------------
echo "" | tee -a $logfile
echo "*** Imaging the sources"| tee -a $logfile

# Copy gains to sources and make images of each:
foreach src ($gaincalis $sourcelis)
    # Copy the bandpass gains
    echo " " | tee -a $logfile
    uvlist vis=$src options=spectra log=tmp.uvlist1
    set ch0=`grep 'starting chan' tmp.uvlist1 | sed 's/.*:[ ]*//'`
    set nch=`grep number tmp.uvlist1 | sed 's/.*:[ ]*//'`
    uvlist vis=$src options=var,full log=tmp.uvlist
    if ($nopasscals == 0) then
        gpcopy vis=passcals.mir out=$src options=nocal
        echo "Passband gains copied to $src" | tee -a $logfile
    endif
    # Apply bandpass gains, copy gains
    rm -rf $src.cal
    uvcat vis=$src out=$src.cal options=nocal
    gpcopy vis=refant$refant.gains out=$src.cal options=nopass
    set nspect=$#ch0
    # Loop through the spectral windows, generating maps.
    set i=1
    set im=1
    rm -f $src.olay
    set region = ""
    set yolay = 1.0
    while ($i <= $nspect)
        # Message
        echo "" | tee -a $logfile
        echo "*** Making map of vis=$src.cal, window $i, line=chan,1,$ch0[$i],$nch[$i]" | tee -a $logfile

        # Set imsize and cell size
        set id=`printf "%02d" $i`
        rm -rf $src.$i.map $src.$id.beam
        set pow=`calc -i "1+log(1e4/$baselen/$lofreq)/log(2)"`
        set cell=`echo $pow | awk '{print 2**$1}'`
        if ($i == 1 && `calc -i $cell` <= 1) then
           set region = "region=quarter"
           set yolay  = 0.75
        endif
        if ($nlofreq < 60) then
            set imsize=`echo $pow | awk '{print 2**(10-$1)}'`
        else if ($nlofreq < 200) then
            set imsize=`echo $pow | awk '{print 2**(8-$1)}'`
        else
            set imsize=`echo $pow | awk '{print 2**(7-$1)}'`
        endif
        if ($imsize > 512) set imsize=513

        # Special section for Sci2 since we do not want to image the outriggers
        set selectInvert = "select=-ant(18,19),on(1)"
        if ($array == "CARMA23") set selectInvert = "select=on(1)"

        # Run invert
        rm -rf $src.$id.map
        invert vis=$src.cal map=$src.$id.map imsize=$imsize \
            cell=$cell line=chan,1,$ch0[$i],$nch[$i] $selectInvert \
            options=systemp,mosaic robust=1 | tee tmp.invert
        rm -rf $src.$id.sen $src.$id.gain
        mossen in=$src.$id.map sen=$src.$id.sen gain=$src.$id.gain
        rm -rf $src.$id.snr
        set minval=`histo in=$src.$id.sen | grep Min | awk '{printf "%3.1e",$3}'`
        echo "Normalizing $src.$id.sen by $minval"
        maths exp="<$src.$id.map>/<$src.$id.sen>" out=$src.$id.snr
        if (-e $src.$id.snr) then
            echo "Window $id has theoretical rms $minval Jy at map center" \
                | tee -a $logfile
            set xpos=0
            set xpos=`gethd in=$src.$id.map/naxis1 | awk '{printf "%d",$1*0.50}'`
            set ypos=`gethd in=$src.$id.map/naxis2`
            set ypos=`calc "$yolay*$ypos" | awk '{printf "%d",$1*0.95}'`
#           set ypos=`gethd in=$src.$id.map/naxis2 | awk '{printf "%d",$1*0.95}'`
            echo -n "clear abspix abspix " >> $src.olay
            # echo -n "$corfreq[$i]_GHz/${bandw[$i]}_MHz/\gs=$minval " >> $src.olay
            # echo -n "Win_$i/${bandw[$i]}_MHz/\gs=$minval " >> $src.olay
            echo -n "Win_${i}_\gs=$minval " >> $src.olay
            echo "yes $xpos $ypos $im $im" >> $src.olay
            set im=`expr $im + 1`
        else
            echo "ERROR: Window $id could not be imaged" | tee -a $logfile       
        endif
#       rm -rf $src.$id.map $src.$id.sen
        set i=`expr $i + 1`
    end
    # Merge the maps (after noise normalization) and plot.
    rm -rf $src.snr $src.gain
    imcat in="$src.*.snr" out=$src.snr options=relax
    puthd in=$src.snr/bunit value='SIGMA'
    imcat in="$src.*.gain" out=$src.gain options=relax
    set min=`histo in=$src.snr | grep Min | awk '{printf "%d",$3+0.5}'`
    set max=`histo in=$src.snr | grep Max | awk '{printf "%d",$3+0.5}'`
    rm -rf $src.map.gif
    set nplots = 1
    set nxymaps = "4,4"
    if ($nspect == 8) set nxymaps = 4,2
    if ($nspect == 4) set nxymaps = 2,2
    if ($max > 50) then
        cgdisp in=$src.snr,$src.gain type=p,c range=0,0,lin,1 \
        levs1=0.5 options=full,solneg2,black,wedge,mirr device=$src.map.gif/gif \
        labtyp=arcsec lines=2,2,2 csize=0.6,0,0.7 olay=$src.olay \
        nxy=$nxymaps slev=a,1 cols1=0 $region
    else
        cgdisp in=$src.snr,$src.gain type=p,c range=0,0,lin,1 \
        levs1=4 levs2=0.5 options=full,solneg2,black,wedge,mirr \
        device=$src.map.gif/gif \
        labtyp=arcsec lines=2,1,2,2 csize=0.6,0,0.7 olay=$src.olay \
        nxy=$nxymaps slev=a,1,a,1 cols1=4 cols2=0 $region
    endif
    if (-e $src.map.gif_2) then
       mv $src.map.gif_2 $src.map.2.gif
       set nplots = 2
    endif
    rm -rf $src.*.gain $src.*.snr

    # Generate continuum map.
    # If the number of continuum channels is less than the number of
    # spectral windows, then we have to make a copy of the uv data set
    set cvis = $src.cal
    if ($nwideband < $nspect) then
       echo "Creating continuum visibility file for $src" | tee -a $logfile
       rm -rf $src.cal.cont.mir
       echo "uvcat vis=$src.cal out=$src.cont.mir select=win($buse)" | tee -a $logfile
       uvcat vis=$src.cal out=$src.cal.cont.mir select="win($buse)" | tee -a $logfile
       set cvis = $src.cal.cont.mir
    endif
    echo "Creating continuum image with $nchan_wideband channels" | tee -a $logfile
    rm -rf $src.cmap $src.csen $src.cgain $src.csnr
    invert vis=$cvis map=$src.cmap imsize=$imsize $selectInvert \
           cell=$cell line=chan,1,1,$nchan_wideband options=systemp,mosaic \
           robust=1 | tee tmp.invert
    mossen in=$src.cmap sen=$src.csen gain=$src.cgain
    maths exp="<$src.cmap>/<$src.csen>" out=$src.csnr
    puthd in=$src.csnr/bunit value='SIGMA'
    cgdisp in=$src.csnr,$src.cgain type=p,c range=0,0,lin,1 \
           options=full,solneg1,black,wedge device=$src.cmap.gif/gif \
           labtyp=arcsec lines=2,3 nxy=1,1 slev=a,1 \
           levs1=0.5 cols1=0 $region
end

#----------------------------- GRADE BLOCK -------------------------------
echo "" | tee -a $logfile
echo "*** Grading the project" | tee -a $logfile

echo " " | tee -a $logfile
# Calculate mean and median values
@ N = 1
set value=(0 0 0)
foreach var (rmspath precipmm tau230)
    rm -f meanmed.out
    echo "Track statistics for ${var}:" | tee -a $logfile
    grep -v '#' $var.log | grep -v ' 0.000' | awk '{print $3}' > tmp.varplt
    meanmed infile=tmp.varplt | tee -a meanmed.out $logfile
    set median = `tail -1 meanmed.out | awk '{print $2}'`
    set mean = `tail -1 meanmed.out | awk '{print $3}'`
    set value[$N] = $median
    @ N++
    echo " " | tee -a $logfile
end


echo "The maximum baseline length for this track is $baselen m" | tee -a $logfile

# Rmspath score, referenced to either 112 or 230 GHz
set intfreq=`calc -i "$lofreq"`
if ($intfreq < 150) then
    set reffreq = $ref3mm
else
    set reffreq = $ref1mm
endif
if ($reffreq == '0') then
    set reffreq=$lofreq
    set lambda2=$lambda
else
    set lambda2=`calc -f f8.6 "0.3/$reffreq"`
endif
set rmstau = `calc "$value[1]*2*PI/($lambda2*1e6)"`
set rmstau = `calc -f f6.2 "0.5*($rmstau**2)*($baselen/100)**0.833"`
echo "Effective opacity at $reffreq GHz due to phase noise: $rmstau" \
    | tee -a $logfile
echo "" | tee -a $logfile

# Precipmm score based on tipper data, referenced to either 112 or 230 GHz
echo "Atmospheric opacity based on tau225 from the tipper" | tee -a $logfile
if ($intfreq < 150) then
   # convert to mmh2o for obstau input
   set pwv=`calc -f f6.2 "($value[3]-0.005)/0.06"`
   # calculate tau(freq,zenith) for 3mm band
   set tauz=`obstau altitude=2.2 freq=$reffreq mmh2o=$pwv | tail -1 | awk '{print $8}'`
   # calculate tau(freq,45deg)
   set pwvtau=$tauz # no elev correction
   #set pwvtau=`calc -f f6.2 "$tauz*1.414"`
else
   # calculate tau(freq,45deg)
   set pwvtau=$value[3] # no elev correction
   #set pwvtau=`calc -f f6.2 "$value[3]*1.414"`
endif
echo "Opacity at $reffreq GHz due to atmospheric absorption: $pwvtau" | tee -a $logfile

# Composite score
echo " " | tee -a $logfile
set tottau = `calc -f f6.2 "$rmstau+$pwvtau"`
echo "Total opacity: $tottau" | tee -a $logfile
set finscore = `calc -i "100-19*$tottau"`
echo "COMPOSITE SCORE = 100-(19*tau) = $finscore" | tee -a $logfile

# Calculate the letter grades
set grade = (0)
@ N = 1
foreach score ($finscore)
    if ($score >= 90) then
        set grade[$N]='A'
    else if ($score >= 80) then
        set grade[$N]='B'
    else if ($score >= 70) then
        set grade[$N]='C'
    else if ($score >= 60) then
        set grade[$N]='D'
    else 
        set grade[$N]='F'
    endif
    if ($grade[$N] != 'D' && $grade[$N] != 'F' && $score < 100) then
        @ remainder = $score % 10
        if ($remainder >= 7) then
            set sign='+'
        else if ($remainder < 3) then
            set sign='-'
        else
            set sign=' '
        endif
    else if ($score >= 100) then
        set sign='+'
    else
        set sign=' '
    endif
    set grade[$N]="$grade[$N]$sign"
    @ N++
end

echo "COMBINED GRADE: $grade[1]" | tee -a $logfile
echo " " | tee -a $logfile
echo "This is based on weather conditions only." | tee -a $logfile

rm -f bsln.out meanmed.out


# Generate uvcoverage plot
echo "" | tee -a $logfile
echo "*** Generating uv-coverage plot" | tee -a $logfile
setenv PLOT_UVCOVERAGE plot_uv.gif
rm -rf $PLOT_UVCOVERAGE
smauvplt axis=uc,vc options=nobase,equal vis=$mirfile \
         select=-'source(NOISE)' device=$PLOT_UVCOVERAGE/gif

# Run pacs 
if ($pacs != "" && $pacs != "True" && $pacs != "0" && $pacs != "1") then
   # Run pacs data reduction script
     $script_dir/gainCheckPacs.csh root=$mirfile_root \
         refant_sza=$refant_sza refant_carma=$refant copy=1 \
         config=$config outdir=$OUTDIR_FULL pacs=$pacs |& tee -a $logfile
endif

# Create web page
echo "" | tee -a $logfile
echo "*** Creating plot web page" | tee -a $logfile
mv $logfile $OUTDIR_FULL

# Create plot web page - create new file
setenv LOG_HTML $SCRATCH/index_tmp.html
rm -rf $LOG_HTML

# Start plot web page
echo "<html>" > $LOG_HTML
echo "<head>" >> $LOG_HTML
echo "<title>CARMA gain plots for sci$subarray</title>" >> $LOG_HTML
echo '<meta http-equiv="refresh" content="300">' >> $LOG_HTML
echo "</head>" >> $LOG_HTML
echo "<body>" >> $LOG_HTML
echo "<center>" >> $LOG_HTML
echo "<H2>Gain plots for sci$subarray</H2>" >> $LOG_HTML
echo "Last updated on $DATE<BR>" >> $LOG_HTML
echo "(automatic updates every 5 minutes by <tt>$0</tt>)<BR>" >> $LOG_HTML
echo "</center>" >> $LOG_HTML

# Information
echo "<BR><BR>" >> $LOG_HTML
echo "<table border=0>" >> $LOG_HTML
setenv REDUCTIONLOG "$URL_GAINS/$OUTDIR/update.html"
echo "<TR><TD><b><a href="$SCRIPTLOG">Script log</a></TD></TR>" >> $LOG_HTML
echo "<TR><TD><b><a href="$REDUCTIONLOG">Reduction log</a></TD></TR>" >> $LOG_HTML
echo "<TR><TD><b>Miriad file</b></TD><TD>$vis</TD></TR>" >> $LOG_HTML
echo "<TR><TD><b>Total time</b></TD><TD>$totaltime hours</TD></TR>" >> $LOG_HTML
echo "<TR><TD><b>Track type</b></TD><TD>$array</TD></TR>" >> $LOG_HTML
echo "<TR><TD><b>Reference Ant.</b></TD><TD>$refant</TD></TR>" >> $LOG_HTML
echo "<TR><TD><b>Source(s)</b></TD><TD>$sources</TD></TR>" >> $LOG_HTML
echo "<TR><TD><b>Gain Cal.</b></TD><TD>$gaincals</TD></TR>" >> $LOG_HTML
echo "<TR><TD><b>Passband Cal.</b></TD><TD>$passcals</TD></TR>" >> $LOG_HTML
echo "<TR><TD><b>LO freq</b></TD><TD>$lofreq GHz</TD></TR>" >> $LOG_HTML
echo "<TR><TD><b>Maximum bandwidth</b></TD><TD>$maxbw MHz</TD></TR>" >> $LOG_HTML
echo "<TR><TD><b>Wideband windows</b></TD><TD>$buse</TD></TR>" >> $LOG_HTML
echo "<TR><TD><b>Maximum baseline</b></TD><TD>$baselen m</TD></TR>" >> $LOG_HTML
echo "<TR><TD><b>Phase monitor</b></TD><TD>$value[1] um (median)</TD></TR>" >> $LOG_HTML
set tau230 = $value[3]
echo "<TR><TD><b>Tau230 (tipper)</b></TD><TD>$tau230 (median)</TD></TR>" >> $LOG_HTML
echo "<TR><TD><b>Tau (lofreq)</b></TD><TD>$pwvtau (median @ 45 deg elevation)</TD></TR>" >> $LOG_HTML
echo "<TR><TD><b>Combined grade</b></TD><TD>$grade[1]</TD></TR>" >> $LOG_HTML
# if ($pacs != "") echo "<TR><TD><b>PACS filler info</b></TD><TD>$ut1 $ut2</TD></TR>" >> $LOG_HTML
echo "</table>" >> $LOG_HTML

# Plots
echo "<center>" >> $LOG_HTML 

# Amplitudes vs time
echo "<h3>Amplitudes vs time</H3><br>" >> $LOG_HTML
echo "<img src=$URL_GAINS/$OUTDIR/$PLOT_GAIN_AMP>" >> $LOG_HTML
echo "<BR><BR><BR><BR>" >> $LOG_HTML

# Phase vs time
echo "<h3>Phases vs time</h3><br>" >> $LOG_HTML
echo "<img src=$URL_GAINS/$OUTDIR/$PLOT_GAIN_PHASE>" >> $LOG_HTML
echo "<BR><BR><BR><BR>" >> $LOG_HTML

# tau(230) vs time
echo "<h3>Zenith opacity (230 GHz)  vs time</h3><br>" >> $LOG_HTML
echo "<img src=$URL_GAINS/$OUTDIR/tau230.gif>" >> $LOG_HTML
echo "<BR><BR><BR><BR>" >> $LOG_HTML

# Sky rms vs time
echo "<h3>Sky rms vs time</h3><br>" >> $LOG_HTML
echo "<img src=$URL_GAINS/$OUTDIR/rmspath.gif>" >> $LOG_HTML
echo "<BR><BR><BR><BR>" >> $LOG_HTML

# Sky Tsys vs time
echo "<h3>Tsys (SSB) vs time</h3><br>" >> $LOG_HTML
echo "<img src=$URL_GAINS/$OUTDIR/$PLOT_TSYS>" >> $LOG_HTML
echo "<BR><BR><BR><BR>" >> $LOG_HTML

# Passband
if ($#passcals == 1 && "$passcals" != '') then
   if ($npol >= 2) then
      echo "<h3>RR Passband amplitudes</h3><br>" >> $LOG_HTML
      echo "<img src=$URL_GAINS/$OUTDIR/$PLOT_PB_AMP_RR>" >> $LOG_HTML
      echo "<BR><BR><BR><BR>" >> $LOG_HTML
      echo "<h3>RR Passband Phases</h3><br>" >> $LOG_HTML
      echo "<img src=$URL_GAINS/$OUTDIR/$PLOT_PB_PHASE_RR>" >> $LOG_HTML
      echo "<BR><BR><BR><BR>" >> $LOG_HTML
      echo "<h3>LL Passband amplitudes</h3><br>" >> $LOG_HTML
      echo "<img src=$URL_GAINS/$OUTDIR/$PLOT_PB_AMP_LL>" >> $LOG_HTML
      echo "<BR><BR><BR><BR>" >> $LOG_HTML
      echo "<h3>LL Passband Phases</h3><br>" >> $LOG_HTML
      echo "<img src=$URL_GAINS/$OUTDIR/$PLOT_PB_PHASE_LL>" >> $LOG_HTML
      echo "<BR><BR><BR><BR>" >> $LOG_HTML
   else 
      echo "<h3>Passband amplitudes</h3><br>" >> $LOG_HTML
      echo "<img src=$URL_GAINS/$OUTDIR/$PLOT_PB_AMP>" >> $LOG_HTML
      echo "<BR><BR><BR><BR>" >> $LOG_HTML
      echo "<h3>Passband Phases</h3><br>" >> $LOG_HTML
      echo "<img src=$URL_GAINS/$OUTDIR/$PLOT_PB_PHASE>" >> $LOG_HTML
      echo "<BR><BR><BR><BR>" >> $LOG_HTML
   endif
endif

# Gain calibrators
foreach src ($gaincalis)
   echo "<h3>SNR Image of $src (individual windows)</h3><br>" >> $LOG_HTML
   echo "<img src=$URL_GAINS/$OUTDIR/$src.map.gif>" >> $LOG_HTML
   if ($nplots == 2) echo "<img src=$URL_GAINS/$OUTDIR/$src.map.2.gif>" >> $LOG_HTML
   echo "<BR><BR><BR><BR>" >> $LOG_HTML
   echo "<h3>SNR Image of $src (continuum)</h3><br>" >> $LOG_HTML
   echo "<img src=$URL_GAINS/$OUTDIR/$src.cmap.gif>" >> $LOG_HTML
   echo "<BR><BR><BR><BR>" >> $LOG_HTML
end

# Source 
foreach src ($sourcelis)
   echo "<h3>SNR Image of $src (individual windows)</h3><br>" >> $LOG_HTML
   echo "<img src=$URL_GAINS/$OUTDIR/$src.map.gif>" >> $LOG_HTML
   if ($nplots == 2) echo "<img src=$URL_GAINS/$OUTDIR/$src.map.2.gif>" >> $LOG_HTML
   echo "<BR><BR><BR><BR>" >> $LOG_HTML
   echo "<h3>SNR Image of $src (continuum)</h3><br>" >> $LOG_HTML
   echo "<img src=$URL_GAINS/$OUTDIR/$src.cmap.gif>" >> $LOG_HTML
   echo "<BR><BR><BR><BR>" >> $LOG_HTML
end

# Linecal
echo "<h3>Linecal data</h3><br />" >> $LOG_HTML
echo "<img src=$URL_GAINS/$OUTDIR/${PLOT_LINECAL}><BR>" >> $LOG_HTML
if ($pacs != "") echo "<img src=$URL_GAINS/$OUTDIR/plot_linecal_sza.gif>" >> $LOG_HTML
echo "<br /><br /><br /><br />" >> $LOG_HTML

# UV Coverage
echo "<h3>UV coverage</h3><br />" >> $LOG_HTML
echo "<img src=$URL_GAINS/$OUTDIR/${PLOT_UVCOVERAGE}>" >> $LOG_HTML
echo "<br /><br /><br /><br />" >> $LOG_HTML

# pacs
if ($pacs != "") then
   # SZA amplitudes
     echo "<h3>SZA amplitudes (Jy)</h3><br />" >> $LOG_HTML
     echo "<img src=$URL_GAINS/$OUTDIR/sza_amp_time.gif>" >> $LOG_HTML
     echo "<br><br><br><br>" >> $LOG_HTML

   # SZA gains
     echo "<h3>SZA self-cal gains</h3><br />" >> $LOG_HTML
     echo "<img src=$URL_GAINS/$OUTDIR/sza_amp.gif>" >> $LOG_HTML
     echo "<br><br><br><br>" >> $LOG_HTML

   # SZA phases
     echo "<h3>SZA self-cal phases</h3><br />" >> $LOG_HTML
     echo "<img src=$URL_GAINS/$OUTDIR/sza_phases.gif>" >> $LOG_HTML
     echo "<br><br><br><br>" >> $LOG_HTML

   # phases vs. time (phase calibrator without PACS)
     echo "<h3>Phases vs. time (phase calibrator without PACS)</h3><br />" >> $LOG_HTML
     echo "<img src=$URL_GAINS/$OUTDIR/carma_time_phase_nobuddy.gif>" >> $LOG_HTML
     echo "<br><br><br><br>" >> $LOG_HTML

   # Phases vs. time (phase calibrator with PACS)
     echo "<h3>Phases vs. time (phase calibrator with PACS)</h3><br />" >> $LOG_HTML
     echo "<img src=$URL_GAINS/$OUTDIR/carma_time_phase_buddy.gif>" >> $LOG_HTML
     echo "<br><br><br><br>" >> $LOG_HTML

   # Phases vs. uvdist (without PACS)
     echo "<h3>Phases vs. uvdist (phase calibrator without PACS)</h3><br />" >> $LOG_HTML
     echo "<img src=$URL_GAINS/$OUTDIR/carma_uvdist_phase_nobuddy.gif>" >> $LOG_HTML
     echo "<br><br><br><br>" >> $LOG_HTML

   # Phases vs. uvdist (with PACS)
     echo "<h3>Phases vs. uvdist (phase calibrator with PACS)</h3><br />" >> $LOG_HTML
     echo "<img src=$URL_GAINS/$OUTDIR/carma_uvdist_phase_buddy.gif>" >> $LOG_HTML
     echo "<br><br><br><br>" >> $LOG_HTML

   # Minipacs results
     if (-e $SCRATCH"/phase_sep.gif") then 
        echo "<h3>Mini-pacs results: phase rms vs. separation</h3><br />" >> $LOG_HTML
        echo "<img src=$URL_GAINS/$OUTDIR/phase_sep.gif>" >> $LOG_HTML
        echo "<br><br><br><br>" >> $LOG_HTML
        echo "<h3>Minipacs results: phase rms vs. baseline length</h3><br />" >> $LOG_HTML
        echo "<img src=$URL_GAINS/$OUTDIR/phase_bsl.gif>" >> $LOG_HTML
        echo "<br><br><br><br>" >> $LOG_HTML
     endif

   # Phases vs. time - baseline (without PACS)
#    echo "<h3>Phases vs. uvdist (without PACS)</h3><br />" >> $LOG_HTML
#    echo "<img src=$URL_GAINS/$OUTDIR/carma_time_phase_bsl_nobuddy.gif>" >> $LOG_HTML
#    echo "<br><br><br><br>" >> $LOG_HTML

   # Phases vs. time - baseline (with PACS)
#    echo "<h3>Phases vs. uvdist (with PACS)</h3><br />" >> $LOG_HTML
#    echo "<img src=$URL_GAINS/$OUTDIR/carma_time_phase_bsl_buddy.gif>" >> $LOG_HTML
#    echo "<br><br><br><br>" >> $LOG_HTML
endif

# Move the plots
  rm -rf $OUTDIR_FULL/*.gif
  mv $SCRATCH/*.gif $OUTDIR_FULL

# Move to main gains directory
  cd $DIR_GAINS

# History
if ($update != 0) then
   # Get last N files except current file
     setenv NKEEP 20
     rm -rf tmp.history
     grep -v ">$ROOT<" $HISTORY | head -$NKEEP > tmp.history

   # Make new history file
     rm -rf $HISTORY
     echo "<TR align=center><TD align=left><a href=$URL_GAINS/$OUTDIR/index.html>$ROOT</a></TD><TD>$grade[1]</TD><TD>$CalDate</TD><TD>$totaltime</TD><TD algn=right>$lofreq</TD><TD>$value[1]</TD><TD>$tau230</TD><TD><a href="$SCRIPTLOG">$DATE</a></TD></TR>" > $HISTORY
     cat tmp.history >> $HISTORY
     rm -rf tmp.history

   # Add history to web page
     echo "<BR><BR><BR>" >> $LOG_HTML
     echo "<center>" >> $LOG_HTML
     echo "<table border=2 cellspacing=2 cellpadding=2>" >> $LOG_HTML
     echo "<caption><a href=$URL_GAINS/$SUBDIR_RESULTS>Recent tracks</a></caption>" >> $LOG_HTML
     echo "<TR bgcolor=yellow><TH>Obsblock</TH>" >> $LOG_HTML
     echo "<TH nowrap>Grade</TH>" >> $LOG_HTML
     echo "<TH nowrap>UT Date</TH>" >> $LOG_HTML
     echo "<TH nowrap>Total time<BR>(hours)</TH>" >> $LOG_HTML
     echo "<TH nowrap>LO freq<BR>(GHz)</TH>" >> $LOG_HTML
     echo "<TH nowrap>Phase<BR>(um)</TH>" >> $LOG_HTML
     echo "<TH nowrap>Tau @ 225 GHz</TH>" >> $LOG_HTML
     echo "<TH nowrap>Last reduced<BR>(Click to see script log)</TH></TR>" >> $LOG_HTML
     cat $HISTORY >> $LOG_HTML
     cp $HISTORY $OUTDIR
     echo "</table>" >> $LOG_HTML
     echo "</center>" >> $LOG_HTML
endif

# End web page
echo "</body></html>" >> $LOG_HTML

# Move the links
cp $LOG_HTML $OUTDIR_FULL/index.html
if ($update != 0) then 
   rm -rf index_sci$subarray.html
   ln -s $OUTDIR/index.html index_sci$subarray.html
endif

# Clean up all tmp files
rm -rf tmp*

# Compute fluxes
#cd $SCRATCH
#setenv PYTHONPATH /array/obs/bin
#python -c "import fluxes; fluxes.flux('$mirfile_root')"

# Computing fluxes again, using temporary version of 
# John's fluxcal script, edited by Chat, Amber, Statia
#cd $SCRATCH
#setenv PYTHONPATH /array/obs/bin
#python -c "import fluxes_temp; fluxes_temp.flux('$mirfile_root')"
#if ($pacs != "") then
#  python -c "import fluxes_temp; fluxes_temp.flux('/home/obs/data/$pacs.mir')"
#endif
