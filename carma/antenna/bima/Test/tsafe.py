#!/usr/bin/python
curAz = 10
curEl = 25
azLow=305
azHigh=325
elLow=2.5
elHigh=87.5
for curAz in range(-50,410,2):
    if ( curAz >= 360.0 ):
      adjAz = curAz - 360.0
    else:
      adjAz = curAz

    # adjust for safe ranges < 0
    if ( azLow <= 0.0 and (fabs(azLow) + curAz) >= 360.0 ):
      adjAz = curAz - 360.0

    if ( (adjAz > azLow and adjAz < azHigh) and (curEl > elLow and curEl < elHigh) ) :
      collisionmpe = "SAFE"
    #// It is possible that both azLow and adjAz are less than zero 
    #// but that adjAz plus 2PI would put it in the safe azimuth range.
    #// That is, the antenna is physically at a safe azimuth, but on the negative
    #// wrap.  So check for that case here.
    else:
         if (( ((adjAz+360) > azLow) and ((adjAz+360) < azHigh)) and  (curEl > elLow and curEl < elHigh) ) :
            collisionmpe = "SAFE"
         else:
            collisionmpe = "UNSAFE"

    print "AZ=%.2f, ADJAZ=%.2f, STATE=%s" % (curAz,adjAz,collisionmpe)
