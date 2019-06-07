#! /bin/csh -f
#
# calculates new phasemonitor parameters - jrf 31aug02
#
#  swx  swy nex  ney   swa                     nea                    swm     nem
# 0.00 0.00 0.00 0.00 -1.57 -1.57 -1.57 -1.57 -1.57 -1.57 -1.57 -1.57 1.1 0.9 1.1 0.9     # 31aug02
#
#  swx, swy are the x and y offsets of the voltage ellipse for the sw antenna (plot units)
#  swa is the rotation angle of the major axis clockwise from vertical (radians)
#  swm is the square root of the ellipticity: sqrt(major/minor)
onintr finish

#set parms = 'carma_params'      # current carma parameter file
#set parms = '/falcon/phasemon/src/oldphasemon_params.txt'	# current bima phasemon parameters
#set parms = '/falcon/phasemon/carma_parms'	# current carma phasemon parameters
set parms = /home/obs/phasemon/test_parms

set swx = `tail -1 $parms | awk '{print $1}'`
set swy = `tail -1 $parms | awk '{print $2}'`
set nex = `tail -1 $parms | awk '{print $3}'`
set ney = `tail -1 $parms | awk '{print $4}'`
set swa = `tail -1 $parms | awk '{print $5}'`
set nea = `tail -1 $parms | awk '{print $9}'`
set swm = `tail -1 $parms | awk '{print $13}'`
set nem = `tail -1 $parms | awk '{print $15}'`

echo ' '
tail -1 $parms
echo "---------------------------------------------------------------"
echo "you MUST start with zero offsets for this procedure to work."
echo "the current params should have x,y=0; rot=0; ellip=1:"
echo 'current phasemonitor parmameters are:'
echo  "     X    Y    Rot  Ellip   [volts, radians, & root(maj/min)]"
echo  "SW: $swx $swy $swa $swm "
echo  "NE: $nex $ney $nea $nem "
echo "---------------------------------------------------------------"
echo ' '
echo 'SW dish: enter x,y offsets, rotation angle in DEG cw from vert, and maj/min ratio:'
set n = "$<"; set newsw = (`echo $n`)
echo 'NE dish: enter x,y offsets, rotation angle in DEG cw from vert, and maj/min ratio:'
set n = "$<"; set newne = (`echo $n`)

# old offsets MINUS new offsets (with ellipse rotation & scaling removed)  
set sx = `echo $swx $swm $newsw[1] | awk '{print $1-($3/$2)}'`
set sy = `echo $swy $swm $newsw[2] | awk '{print $1-($3*$2)}'`
set nx = `echo $nex $nem $newne[1] | awk '{print $1-($3/$2)}'`
set ny = `echo $ney $nem $newne[2] | awk '{print $1-($3*$2)}'`

# old rotation angles MINUS new angles
set sa = `echo $swa $newsw[3] | awk '{print $1-($2/57.3)}'`
set na = `echo $nea $newne[3] | awk '{print $1-($2/57.3)}'`

# old major axis scaling MULTIPLIED by new scaling (minor axis scales as 1/major)
set sm = `echo $swm $newsw[4] | awk '{print $1*sqrt($2)}'`
set nm = `echo $nem $newne[4] | awk '{print $1*sqrt($2)}'`

# form new parameter set
echo 'The new phasemonitor parameter set is:'
echo "--------------------------------------------------------------------------------------------"
echo $sx $sy $nx $ny $sa $na $sm $nm | \
  awk '{printf "%5.2f %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f\n", \
  $1,$2,$3,$4,$5,$5,$5,$5,$6,$6,$6,$6,$7,1/$7,$8,1/$8}' \
  | tee new.tmp
echo " "
echo "Replace current parameter file ($parms) with new values? (y)"
if ($< != n) then
  echo '#' >! hash.tmp 
  date >! date.tmp
  set n = `wc $parms | awk '{print $1-1}'`
  head -$n $parms >! oldparms.tmp
  tail -1 $parms >! oldline.tmp
  paste -d\- hash.tmp oldline.tmp >! oldline2.tmp
  paste new.tmp hash.tmp date.tmp >! newline.tmp
  cat oldparms.tmp oldline2.tmp newline.tmp >! newparms.tmp
else
  goto finish
endif

echo " "
cat newparms.tmp
echo " "
echo "Are you sure you want to replace the file? (y)"
if ($< != n) then
  mv newparms.tmp $parms
  echo "File replaced.  Don't forget to stop/restart phasemon"
  echo " "
  cat $parms
endif

finish:
rm *.tmp
exit 0
