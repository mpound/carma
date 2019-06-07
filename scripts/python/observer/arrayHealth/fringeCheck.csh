#!/bin/csh -fe

umask 0002

# Pgplot
setenv PGPLOT_BACKGROUND_GIF black
setenv PGPLOT_FOREGROUND_GIF white
setenv PGPLOT_BACKGROUND_PS  white
setenv PGPLOT_FOREGROUND_PS  black

setenv PGPLOT_BACKGROUND $PGPLOT_BACKGROUND_GIF
setenv PGPLOT_FOREGROUND $PGPLOT_FOREGROUND_GIF


# Set font directory at Cedar Flat, if needed
if ($?PGPLOT_FONT == 0) then
  setenv PGPLOT_FONT "/array/miriad/cvs/lib/linux/grfont.dat"
endif


# Set arguments
set vis    = ""   # Miriad file name
set output = "./" # Output directory
set refant = 7    # Reference antenna
set fsfc   = 0    # full-Stokes fringeCheck if fsfc=1


# Override above parameters if given on the command line
foreach a ( $* )
   set check=`echo $a | awk -F= '{print NF}'`
   if ( "$check" == 2 ) set $a
end
set vis_orig = $vis

# Check arguments
if ($vis == "") then
   echo "Error entering command line arguments"
   echo "   vis=<s>    Miriad file name"
   echo "   output=<s> Output directory"
   echo "   refant=<d> Reference antenna"
   exit(1)
endif
   echo "command line arguments"
   echo "   vis=<s>    Miriad file name"
   echo "   output=<s> Output directory"
   echo "   refant=<d> Reference antenna"


# Messages
echo " *** Running fringe check on miriad file $vis"
echo " *** Running fringe check with output directory $output"
echo " *** Running fringe check with reference antenna $refant"


# Set output directory
if ($output != "." && $output != "./") then 
   rm -rf $output
   if (!(-e $output)) mkdir $output
endif


# Get lo frequency
listobs vis=$vis log=listobs1
# The number of antennas present.
@ nants = `grep hhmmss listobs1 | awk '{print NF-4}'`
echo "**** Found $nants antennas ***"
set beglin=`awk '/Sys Temps/ {print NR}' listobs1`
@ beglin ++
@ beglin ++
set endlin=`wc listobs1 | awk '{print $1-1}'`
set lofreq=`awk '/First LO/ {print $10}' listobs1`
set lofreq=`calc -i "$lofreq"`


# Figure out how many correlator bands are present
prthd in=$vis options=full log=tmp.prthd
set beglin=`awk '/Channels/ {print NR}' tmp.prthd`
@ beglin ++
set endlin=`awk '/Total number of channels/ {print NR}' tmp.prthd`
set endlin=`expr $endlin - 1`
sed -n "$beglin,${endlin}p" tmp.prthd > tmp.corr1
set windows = `cat tmp.corr1 | awk '{print $1}'`
set nwindows = $#windows
echo ""
echo "*** Found $nwindows windows in the miriad file"
echo "*** Using windows $windows"

# default panels is 5 cols, 3 rows
set nxy="5,3"
# default is to use both SBs
set sb_good = "both"
# for 1cm, use only LSB
if ($lofreq < 40) then
   set sb_good = "lower"
else
# for Sci2 3mm, use only USB
   if ( $nwindows == "32") then
      set sb_good = "upper"
   endif
endif
echo "*** Using $sb_good sideband for astronomical source"
echo "*** Using windows $windows for astronomical source"
# For CARMA23: 4 cols, 6 rows
if ( $nants > 15 ) then
   set nxy="4,6"
endif
# For Sci2: 3 cols, 3 rows
if ( $nants < 10 ) then
   set nxy="3,3"
   # Flag data from bad band2 corr, bsln 17-18
   #uvflag vis=$vis flagval=flag select="ant(17)(18),window(2)"
   #uvflag vis=$vis flagval=flag select="ant(17)(18),window(18)"
endif

# Plot amplitudes vs. uv-distance for astronomical source
set source = "-source(noise)"
set ext    = "_source_uvdist"
set vislist = ""
set visremove = ""
echo ""
echo " *** Copying uv data for astronomical source ***" 
echo ""
set badwindows = ""
set win_lsb = ""
set win_usb = ""
foreach WIN ($windows)
   # Which sideband are we in?
     set j = `calc -i "$nwindows/2"`
     set isLSB = 1
     if ($WIN > $j) set isLSB = 0
     if ($isLSB == 1) then
        if ($win_lsb != "") set win_lsb = "$win_lsb,"
        set win_lsb = "$win_lsb$WIN"
     else 
        if ($win_usb != "") set win_usb = "$win_usb,"
        set win_usb = "$win_usb$WIN"
     endif

   # Set output miriad file
     set outputMiriad = "$output/$WIN.m"

   # Cat data
     if (-e $outputMiriad) rm -rf $outputMiriad
     uvcat vis=$vis select="win($WIN),$source,-auto" out=$outputMiriad

   # Determine number of channels
     prthd in=$outputMiriad options=full log=tmp.prthd
     set beglin=`awk '/Channels/ {print NR}' tmp.prthd`
     @ beglin ++
     set endlin=`awk '/Correlations are stored / {print NR}' tmp.prthd`
     set endlin=`expr $endlin - 2`
     sed -n "$beglin,${endlin}p" tmp.prthd > tmp.corr1
     set corrno = `cat tmp.corr1 | awk '{print $1}'`
     set channels = `cat tmp.corr1 | awk '{print $2}'`

   # Append to list if there are more than zero channels
     if ($channels > 0) then
        if ( $sb_good == "both" || \
             ($sb_good == "lower" && $isLSB == 1) || \
             ($sb_good == "upper" && $isLSB == 0)) then
           if ($vislist != "") set vislist = "$vislist,"
           set vislist   = "$vislist$outputMiriad"
        endif
     else 
        if ($badwindows != "") set badwindows = "$badwindows,"
        set badwindows = "$badwindows$WIN"
     endif
     set visremove = "$visremove $outputMiriad"
end
echo ""
echo " *** Bad windows with zero channels : $badwindows"
echo ""
echo ""
echo " *** Running SMAUVPLT on astronomical source *** "
echo ""
echo "LSB = $win_lsb"
echo "USB = $win_usb"
smauvplt options=nocal,nobase vis=$vislist axis=uvd,amp select=$source device=${vis_orig}$ext.gif/gif
setenv PGPLOT_BACKGROUND $PGPLOT_BACKGROUND_PS
setenv PGPLOT_FOREGROUND $PGPLOT_FOREGROUND_PS
smauvplt options=nocal vis=$vislist axis=uvd,amp select=$source device=${vis_orig}${ext}_base.ps/vcps  nxy=2,2
ps2pdf ${vis_orig}${ext}_base.ps
rm -rf $visremove
setenv PGPLOT_BACKGROUND $PGPLOT_BACKGROUND_GIF
setenv PGPLOT_FOREGROUND $PGPLOT_FOREGROUND_GIF


# Copy desired windows. This is necessary since mfcal gets confused 
# if data are missing for an entire window.
#set out = fjunk_copy
#rm -rf $out
#set win = `echo $windows | sed 's/ /,/g'`
#uvcat vis=$vis select="win($win)" options=nocal,nopass out=$out
#set vis = $out


# Make spectral plot of amplitude vs. channel for astronomical source
set source = "-source(noise),-purpose(P)"
set ext    = "_source_gpplt"
#
echo ""
echo " *** Running MFCAL for astronomical source ***" 
echo ""
set win_astro = `echo $windows | sed 's/ /,/g'`
if ($sb_good == "lower") set win_astro = $win_lsb
if ($sb_good == "upper") set win_astro = $win_usb
mfcal vis=$vis select="$source,win($win_astro),-auto" interval=1 refant=$refant
gplist vis=$vis
#
echo ""
echo " *** Plotting MFCAL bandpass solution for astronomical source ***" 
echo ""
smagpplt vis=$vis device=${vis_orig}${ext}_pb_amp.gif/gif nxy=$nxy \
         xaxis=chan yrange=0,2 options=bandpass,nofit,dots 
# Change the names of the second SMAGGPLT output files to LL, for FULLSTOKES and DUALPOL
if( -e ${vis_orig}${ext}_pb_amp.gif_2 ) then
    mv ${vis_orig}${ext}_pb_amp.gif_2 ${vis_orig}${ext}_pb_amp_LL.gif
endif
smagpplt vis=$vis device=${vis_orig}${ext}_pb_pha.gif/gif nxy=$nxy \
         xaxis=chan yrange=-180,180 options=bandpass,nofit,dots,wrap yaxis=phase
if( -e ${vis_orig}${ext}_pb_pha.gif_2 ) then
    mv ${vis_orig}${ext}_pb_pha.gif_2 ${vis_orig}${ext}_pb_pha_LL.gif
endif
echo ""
echo " *** Plotting MFCAL amplitude solution for astronomical source ***" 
echo ""
mfcal vis=$vis select="$source,win($win_astro),-auto" interval=0.1 refant=$refant
smagpplt vis=$vis device=${vis_orig}${ext}_amp.gif/gif nxy=$nxy \
         yrange=0,5 options=dots dotsize=10 
rm -rf $vis/{gains,bandpass}
#
if( $fsfc == 1 ) then
    echo ""
    echo " *** Plotting XYPHASE bandpass solution for 10m telescopes ***" 
    echo ""
    uvspec vis=$vis axis=chan,ph yrange=-180,180 \
	select="auto,-ant(7,8,9,10,11,12,13,14,15),win(1,2,3,4,5,6,7,8),purpose(P)" \
	interval=1000 nxy=3,2 device=${vis_orig}_xyphase.gif/gif
endif

# Run self cal solution on noise source. This has several purposes:
#   (1) Checks for phase closure errors
#   (2) Determines which antennas do not have any data

# First, select appropriate data
echo ""
echo " *** Copying noise source ***" 
echo ""
set output2 = "fjunk"
rm -rf $output2
uvcat vis=$vis select="source(noise),-auto,-purpose(P)" out=$output2


# Plot noise-source amplitude spectra
echo ""
echo " *** Plotting noise source amplitudes ***" 
echo ""
smauvspec vis=$output2 device=${vis_orig}_noise_amp.gif/gif interval=99999 \
          options=nocal,nopass,nopol,nobase nxy=1,1 
setenv PGPLOT_BACKGROUND $PGPLOT_BACKGROUND_PS
setenv PGPLOT_FOREGROUND $PGPLOT_FOREGROUND_PS
smauvspec vis=$output2 device=${vis_orig}_noise_amp_base.ps/vcps interval=99999 \
          options=nocal,nopass,nopol nxy=2,2 
ps2pdf ${vis_orig}_noise_amp_base.ps
setenv PGPLOT_BACKGROUND $PGPLOT_BACKGROUND_GIF
setenv PGPLOT_FOREGROUND $PGPLOT_FOREGROUND_GIF


# Run selfcal solution on noise
echo ""
echo " *** Running MFCAL on noise source ***" 
echo ""
mfcal vis=$output2 interval=99999 refant=$refant select="win($win_lsb)"
# gplist vis=$output2
echo ""
echo " *** Plotting noise source spectra ***" 
echo ""
smauvspec vis=$output2 device=${vis_orig}_noise_pha.gif/gif interval=99999 \
          axis=ch,phase options=nopol,nobase nxy=1,1 select="win($win_lsb)"
setenv PGPLOT_BACKGROUND $PGPLOT_BACKGROUND_PS
setenv PGPLOT_FOREGROUND $PGPLOT_FOREGROUND_PS
smauvspec vis=$output2 device=${vis_orig}_noise_pha_base.ps/vcps interval=99999 \
          axis=ch,phase options=nopol nxy=2,2 select="win($win_lsb)" yrange=-5,5
ps2pdf ${vis_orig}_noise_pha_base.ps
setenv PGPLOT_BACKGROUND $PGPLOT_BACKGROUND_GIF
setenv PGPLOT_FOREGROUND $PGPLOT_FOREGROUND_GIF


# Plot selfcal phase solution on noise
echo ""
echo " *** Plotting noise source bandpass solution ***" 
echo ""
smagpplt vis=$output2 device=${vis_orig}_noise_pha_selfcal.gif/gif \
          xaxis=chan yaxis=phase options=bandpass,nofit,wrap nxy=$nxy yrange=-200,200


# Cleanup
if ($output != "." && $output != "./") then 
   echo ""
   echo " *** Cleaning up files ***" 
   echo ""
   rm -rf $output $output2
endif
