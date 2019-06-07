# resolverCheck.py
# 13-dec-2012
# checks match between inductosyn and resolver angles, computes new azoff and eloff
#   values to be written into conf/antenna/bima/drives.tab

# note: there is one bug: suggestion can be off by 1 degree; make sure integer
#   values for twodeg are even!

import subarrayCommands as SAC
import math

#azoff = [46370, 11709, 52153, 42606, 9314, 18673, 13370, 29962, 35170]
# note: these values are overwritten later
azoff = [46370, 11619, 52153, 42606, 9314, 18673, 13370, 29962, 35170]
eloff = [-3039, 4358, 26816, 21798, -4130, 47302, -10780, 26437, -3275]

def oneAntAz( nant, offset ) :
  trueAz = SAC.queryDouble("Bima%d.AntennaCommon.Drive.Track.actualAzimuth" % nant )
  resolver = SAC.queryInt("Bima%d.BimaSpecific.Drive.Track.azResolver" % nant )
  cos = SAC.queryInt("Bima%d.BimaSpecific.Drive.Track.azCosEnc" % nant )
  sin = SAC.queryInt("Bima%d.BimaSpecific.Drive.Track.azSinEnc" % nant )
  coarse = resolver - offset
  if (coarse < 0) :
    coarse = coarse + 65536
  degrees = (coarse * 360.)/65536.
  fine = math.atan2( float(sin), float(cos) ) * 180./math.pi
  if (fine < 0. ) :
    fine = fine + 360.
  fine = fine/180.
  twodeg = int(degrees - fine + 0.5)
  correctAz = twodeg + fine
  deltaOffset = int( 65536.*(degrees - correctAz)/360.)
  #print "bima%d   resolver %5d   cos %6d  sin %6d   offset %6d  coarse %7.3f  fine %.3f  trueAz %7.3f  2deg %7.3f  %3d" \
  #  % (nant, resolver, cos, sin, offset, degrees, fine, trueAz, (degrees-fine), twodeg) 
  print "bima%d   resolver %5d   offset %6d   coarse %7.3f  fine %.3f   2deg %7.3f  %3d   err %3d   new %6d" \
    % (nant, resolver, offset, degrees, fine,  (degrees-fine), twodeg, deltaOffset, offset+deltaOffset) 

def oneAntEl( nant, offset ) :
  trueEl = SAC.queryDouble("Bima%d.AntennaCommon.Drive.Track.actualElevation" % nant )
  resolver = SAC.queryInt("Bima%d.BimaSpecific.Drive.Track.elResolver" % nant )
  cos = SAC.queryInt("Bima%d.BimaSpecific.Drive.Track.elCosEnc" % nant )
  sin = SAC.queryInt("Bima%d.BimaSpecific.Drive.Track.elSinEnc" % nant )
  coarse = resolver - offset
  if (coarse < 0) :
    coarse = coarse + 65536
  degrees = (coarse * 360.)/65536.
  fine = math.atan2( float(sin), float(cos) ) * 180./math.pi
  if (fine < 0. ) :
    fine = fine + 360.
  fine = fine/180.
  twodeg = int(degrees - fine + 0.5)
  correctEl = twodeg + fine
  deltaOffset = int( 65536.*(degrees - correctEl)/360.)
  #print "bima%d   resolver %5d   cos %6d  sin %6d   offset %6d  coarse %7.3f  fine %.3f  trueEl %7.3f  2deg %7.3f" \
  #  % (nant, resolver, cos, sin, offset, degrees, fine, trueEl, (degrees-fine)) 
  print "bima%d   resolver %5d   offset %6d   coarse %7.3f  fine %.3f   2deg %7.3f  %3d   err %3d   new %6d" \
    % (nant, resolver, offset, degrees, fine, (degrees-fine), twodeg, deltaOffset, offset+deltaOffset) 

fin = open( "/opt/rt/conf/antenna/bima/drives.tab", "r" )
for line in fin :
  if not line.startswith("#") :
    a = line.split()
    for nant in range(1,10,1) :
      if a[0] == "bima%d" % nant :
        azoff[nant-1] = int(a[7])
        eloff[nant-1] = int(a[8])
#print "azoff: ", azoff
#print "eloff: ", eloff     
    
for nant in range(1,10,1) :
  oneAntAz( nant, azoff[nant-1] )
print " "
for nant in range(1,10,1) :
  oneAntEl( nant, eloff[nant-1] )
