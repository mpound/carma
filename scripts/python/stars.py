#
# @author Peter Teuben
#
# $Id: stars.py,v 1.80 2013/11/24 16:22:03 scott Exp $
#
# Utilities for optical pointing
#

# system
import math,os,string,time

from cameraControl import opticalSystem
from device import *
import subarrayControl
import subarrayCommands as SAC
#import short
import carmaHelpers as helpers

#
currentElevLimit = SAC.queryDouble('Control.Subarray%i.elevLimit' % subarrayControl.s.getSubarrayNo(), 24)
SAC.elevlimit(0.0)
ostars_ = []
starsUp = []
starsUp = subarrayControl.s.whazUpOptical().split('\n')
for i in range(1,len(starsUp)-1) :
    starName = starsUp[i].split()
    if len(starName) > 0 and starName[0] <> '#' : ostars_.append(starName[0])
SAC.elevlimit(currentElevLimit)
#
# some dummy initialized value
previous_source_ = 'sun'

# working copy; each list element contains a tuple of (source,az,el,mag)
stars_ = []

# max number of queries
qmax_ = 24

def add(source):
    """Add a source (often a planet) to the optical stars list.
    Use the list() and cut() commands to list and remove stars
    from the list. Note you need to add sources *before* list(),
    you cannot add sources to the list(), only remove.
    """
    global ostars_
    ostars_.append(source)

def show(source):
    """Print short ephemeris info of a source. Will typically show e.g.
       source     mjd      ra    dec    az      el   lst
       jupiter 53540.0836 189.00 -2.40 165.14 49.33 11.95
    """
    print subarrayControl.s.info(source)

def elevation(source):
    """Return current elevation of a source, in degrees"""
    srcAzEl = subarrayControl.s.azel(source, 0.0);
    return srcAzEl[1];

def azimuth(source):
    """Return current azimuth of a source, in degrees from north"""
    srcAzEl = subarrayControl.s.azel(source, 0.0);
    return srcAzEl[0];

def sazelmag(source):
    """Return (Source, Az, El, Mag) type for source at current time
    Helper function for point()
    source must be a star, since the magnitude is printed 
    """
    if 1:
        #  bizarre failures in s.azel() showing up on Mar 11, 2006....
        try:
            Azel = subarrayControl.s.azel(source, 0.0);
        except Exception, e:
            print "Problem with s.azel(%s): %s" %(source,e)
            return (source, 0, -10, 0)
        try:
            mag = subarrayControl.s.queryMag(source)
        except Exception, e:
            print "Problem with queryMag(%s): %s" %(source,e) 
            return (source, 0, -10, 0)
        return ( source, Azel[0], Azel[1], mag)
    else:
        cmd = 'checksource source=%s comments=t sexa=f' % source
        # the 2nd (last) line contains all the info
        r=os.popen(cmd).readlines()[1].strip()
        rs=r.split()
        re=r.split('=')
        if len(re) == 2:
            mag=float(re[1])
        else:
            mag=-9.99
            return ( source, float(rs[3]), float(rs[4]), mag)

def distance(source,ref):
    """Return angular distance between a ref and source"""
    if 1:
        srcAzEl = subarrayControl.s.azel(source,0.0);
        refAzEl = subarrayControl.s.azel(ref,0.0);
        deg2rad = 180.0/math.pi;
        srcAz   = srcAzEl[0]/deg2rad;
        srcEl   = srcAzEl[1]/deg2rad;
        refAz   = refAzEl[0]/deg2rad;
        refEl   = refAzEl[1]/deg2rad;
        cosDist =  math.sin(refEl)*math.sin(srcEl) + math.cos(refEl)*math.cos(srcEl)*math.cos(refAz-srcAz);
        dist    = math.acos(cosDist)*deg2rad;
        return  float(dist);
    else:
        cmd = 'distance source=%s ref=%s' % (source,ref)
        r=os.popen(cmd).readlines()
        dist=r[1].split()[5]
        return float(dist)

def stars(elmin=0,magmax=100,sort='az',northSouth='all',doBackwards=False,binSize=20.0):
    """Select and display the list of stars above selected
    elmin, magmax, and sort (one of 'az', 'za', 'el' or 'mag')
    The selected stars are stored in the global list stars_
    Parameters:
     elmin: minimum elevation in degrees. Defaults to 0, so that stars that
            will rise later in the run will also be selected. Another
            elevation check when the star is about to be oberved should be
            done.
    """
    global stars_
    if northSouth == 'north' : cutAz = 269.0
    elif northSouth == 'south' : cutAz = 0.0
    else : cutAz = 330.0
    localLat = 37.0+16.0/60.0+49.37285/3600.0
    def cmpa(x,y):
        # sorting helper for azimuth (note the breakpoint at cutAz!!!)
        def optaz(a):
            if a<cutAz: return a
            return a-360
        a=optaz(x[1])
        b=optaz(y[1])
        if a<b: return -1
        if a>b: return 1
        return 0
    def cmpz(x,y):
        # sorting helper for reverse azimuth (note the breakpoint at cutAz!!!)
        def optaz(a):
            if a<cutAz: return a
            return a-360
        a=optaz(x[1])
        b=optaz(y[1])
        if a<b: return 1
        if a>b: return -1
        return 0
    def cmpe(x,y):
        # sorting helper for elevation
        if x[2]<y[2]: return -1
        if x[2]>y[2]: return 1
        return 0
    def cmpza(x,y) :
        # sorting helper for zenith angle
        if x[2]<y[2]: return 1
        if x[2]>y[2]: return -1
        return 0
    def cmpm(x,y):
        # sorting helper for optical magnitude
        if x[3]<y[3]: return -1
        if x[3]>y[3]: return 1
        return 0
    # report
    if elmin  < -99 and magmax > 99: print "Warning: Selecting all stars, use elmin= or magmax="
    if elmin  > -99: print "Selecting stars above elevation %g deg" % elmin
    if magmax <  99: print "Selecting stars brighter than %g mag" % magmax
    print "Sorting mode: ",sort
    # sorting mode
    if sort == 'el':
        my_cmp=cmpe
    elif sort == 'mag':
        my_cmp=cmpm
    elif sort == 'za':
        my_cmp=cmpza
    elif sort == 'az':
        my_cmp=cmpa
    elif sort == '-az' :
        my_cmp=cmpz
    else:
        print "Warning: sorting mode %s not supported, using az" % sort
        my_cmp=cmpa
    # empty the list again
    stars_=[]
    # Keep the user happy
    print "Hang on, marching through the ephemeris of %d stars" % len(ostars_)
    for s in ostars_:
        s1=sazelmag(s)
        if s1[2] < elmin or s1[3] > magmax:
            continue
        #if (((short.getDec(s) < localLat) and northSouth == 'north') or (short.getDec(s) > localLat) and (northSouth == 'south')) :
        dec = (SAC.getRaDec(s))[1]
        if (((dec < localLat) and northSouth == 'north') or \
           (dec > localLat) and (northSouth == 'south')) :
            continue
        stars_.append(s1)
    stars_.sort(cmpa)
    bins = []
    breakIndex = [0]
    starsPart = []
    starsTemp = []
    starsAz_ = []
    for i in range(len(stars_)) :
        starsAz_.append(stars_[i][1])
        if starsAz_[i] > cutAz : starsAz_[i]=starsAz_[i]-360.0
    startAz = cutAz-360.0 # degrees
    for i in range(int(360/binSize+1)) : bins.append(int(startAz+binSize*i))
    j=0
    for i in range(len(bins)-1) :
        while((starsAz_[j] < bins[i+1]) and (starsAz_[j] >= bins[i]) and (j < len(stars_)-1)) : j=j+1
        breakIndex=breakIndex+[j]
    breakIndex[len(breakIndex)-1]=breakIndex[len(breakIndex)-1]+1
    for i in range(len(bins)-1) :
        if i%2 : my_cmp = cmpe
        else : my_cmp = cmpza
        starsPart = stars_[breakIndex[i]:breakIndex[i+1]]
# Last bin sort in AZ ONLY!!!  Saves you alot of trouble later!
#        if i==(len(bins)-2) :
#            starsTemp=starsTemp+starsPart
#        else :
        starsPart.sort(my_cmp)
        starsTemp=starsTemp+starsPart
    stars_ = starsTemp
    counter = 0
    while counter < len(stars_)-1 :
        az1 = int(stars_[counter][1]*10.0)
        az2 = int(stars_[counter+1][1]*10.0)
        el1 = int(stars_[counter][2]*10.0)
        el2 = int(stars_[counter+1][2]*10.0)
        if ((az1 == az2) and (el1== el2)) : cut(counter+2)
        else : counter=counter+1
    i=0
    if doBackwards : stars_.reverse()
    print "  i name az(deg) el(deg)   magn"
    print "-------------------------------"
    for s in stars_:
        i=i+1
        if len(s[0]) == 4 :  
            print "%3d %s     %6.1f  %6.1f  %6.2f" % (i,s[0],s[1],s[2],s[3])
        else : print "%3d %s %6.1f  %6.1f  %6.2f" % (i,s[0],s[1],s[2],s[3])
    print "-------------------------------"

def nearest(source):
    """Find the nears object in the stars() list to the named source"""
    def mycmp(a,b):
        return -cmp(a[1],b[1])
    dmin = 999.999
    smin = 'Unknown'
    if len(stars_) == 0:
        print "No stars have been selected, go use 'stars()'"
        return
    sdlist=[]
    for s in stars_:
        d = distance(s[0],source)
        sdlist.append((s[0],d))
        if d < dmin:
            dmin = d
            smin = s[0]
    sdlist.sort(mycmp)
    for sd in sdlist:
        print "%s at %g" % (sd[0],sd[1])
    print "Nearest object from stars() to %s is %s at %g deg" % (source,smin,dmin)

def list(show=0):
    """Print the current selection from stars()"""
    global stars_
    if len(stars_) == 0:
        print "No stars have been selected, go use 'stars()'"
        return
    if show == 0:
        i=0
        for s in stars_:
            i=i+1
            print i,s[0],s[1],s[2],s[3]
    else:
        if show > 0 and show <= len(stars_):
            s = stars_[show-1]
            print show,s[0],s[1],s[2],s[3]
        else:
            print "Bad star index"

def cut(id=0):
    """Remove a star stars() by its order in the list (a 1 based number)"""
    global stars_
    n = len(stars_)
    if n == 0:
        print "No stars have been selected, go use 'stars()'"
        return
    if id <= 0 or id > n:
        print "Illegal id, valid are 1..%d" % n
        return
    if id == 1:
        stars_ = stars_[1:]
    elif id == n:
        stars_ = stars_[:n-1]
    else:
        stars_ = stars_[:id-1] + stars_[id:]
#    list()

def reorder(idx=[]):
    """reorder the list according to the input (1 based) lookup list"""
    global stars_
    n = len(stars_)
    if n == 0:
        print "No stars have been selected, go use 'stars()'"
        return
    if len(idx) == 0:
        print "Need an index list"
        return
    for i in idx:
        if i<1 or i>n:
            print "Illegal index value ",i
            return
    stars__ = stars_
    stars_ = []
    for i in idx:
        stars_.append(stars__[i-1])
    list()

def point(ants, gui=True, zoom=1, mode=1, flap=True, 
                elmin=15, writeFITS=True, brightness=50,isInner=False,
                subtractBackground=False,ncoadd=1,dazbkg=2.0,
                centroidLoopMax=16,minsamples=None):
    """Start a list of optical pointing for all stars selected by stars()
    as shown with list(). Use cut() to delete and add() to add new sources.
    Use point1() to just do one source.
    Parameters:
     ants: a list of antennas, or a single antenna
     gui: show the gui, default=True
     zoom: zoom factor, default=1
     mode: 0 is old style serial pointing
           1 is allow one ant to slew to next source if ant is done centroiding
     flap: Close the flap/turn camera off and remove optical pointing constants
           on completion when True. Leaving the optical pointing constants in
           place will ruin radio data, so use this with care!! defaukt=True
     elmin: minimum elevation in degrees
     writeFITS: Write FITS files with sum of images, default=True
     brightness: Framegrabber brightness control, default=50. 
                 Use 30 in daytime
    """
    global stars_
    if len(stars_) == 0:
        print "No stars have been selected, go use 'stars()'"
        return
    else:
        print "POINT:: using %d stars for ants=%s zoom=%g" \
            %(len(stars_),ants,zoom)
    previous_source_ = stars_[0][0]
    print previous_source_
    scnt = 0
    smax = len(stars_)
    antlist = helpers.makeList(ants)
    # Always turn on cameras (open flaps/caps) for all antenna
    # before looping over all stars
    SAC.camera(SAC.ON, antlist)
    for s in stars_:
        scnt = scnt + 1
        src = s[0]
        print "POINT1(%s) %d/%d at %s" % (src,scnt,smax,time.ctime())
        nextSrc = None
        if scnt < smax: nextSrc = stars_[scnt][0]
        try:
            point1(src, ants, showGUI=gui, zoom=zoom, mode=mode, 
                nextSource=nextSrc, elmin=elmin,
                writeFITS=writeFITS, brightness=brightness,isInner=isInner,
                subtractBackground=subtractBackground,ncoadd=ncoadd,dazbkg=dazbkg,
                centroidLoopMax=centroidLoopMax,minsamples=minsamples)
        except KeyboardInterrupt, e:
            print "Quitting point because of keyboard interrupt"
            break
        except Exception, e:
            print "POINT1 failed with exception: %s; skipping source %s" \
                %(e, s[0])
    if flap:
        # Turn off all cameras, close flaps/caps, go back to radio constants
        subarrayControl.s.camera(carma.antenna.common.OFF, antlist)

def point1(source, ants, elmin=15.0, showGUI=True, zoom=1, mode=0, 
    nextSource=None, writeFITS=True, brightness=50,isInner=False,
    subtractBackground=False, ncoadd=1, dazbkg=2.0, centroidLoopMax=16,
    minsamples=None):
    """Optical pointing on a single source,
    making sure no tracking is done below elmin
    Usually called by point(), but can be used for single
    pointings. Although elmin can be set here, it probably
    should not be fiddled with to waste slewing time.
    ants needs to be a list in the 1..15 range to refer
    to OVRO and BIMA antennae
    This method contains **NO** control of the camera(on/off) or of
    the optical/radio pointing constant selection. If you write code
    that uses point1() you must control this in your code.
    Parameters: see point()
    """
    global previous_source_
    antlist = helpers.makeList(ants)
    el      = elevation(source)
    
    # Check for source too low, and bail out if it is
    if el < elmin:
        print 'Skipping %s because elevation(%.1f) is less than limit(%.1f)' \
            %(source,el,elmin)
        return
        
    # Check for maximum elevation as well
    maxelLimit = 87.0
    if el > maxelLimit:
        print 'Skipping %s because elevation(%.1f) is greater than limit(%.1f)' \
            %(source,el,maxelLimit)
        return

    # Print message
    msg = "Pointing on %s at el=%5.1f  (elmin = %5.1f); ants = %s" % (source, el, elmin, str(ants))
    print msg
    SAC.scriptlog(msg)
        
    d = distance(source,previous_source_)
    print " Distance from %s to %s is %.1f deg" %(previous_source_,source,d)
    if mode > 0:
        if nextSource != None:
            print " Next source will be %s" % nextSource
        else:
            print " Last source."
    # Send all antennas tracking to new source, even though some are
    # already there.
    if isInner :
        SAC.track(source)
    else : SAC.track(source, antlist, phaseCenter=False, waiton=SAC.NONE)
    antsTodo = antlist
    while len(antsTodo) > 0 :
        r = SAC.wait(SAC.TRACK, antsTodo, waiton=SAC.ANY)
        antsReady = r.ready
        antsTodo  = r.notready
        for i in antsReady :
            show(source)
            #print "Loading camera on antenna %d" %i
            opticalSystem(i, auto=True, object=source, repeat=4, zoom=zoom,
                    showGUI=showGUI, dontClose=True, 
                    brightness=brightness, ncoadd=ncoadd, dazbkg=dazbkg,
                    subtractBackground=subtractBackground, 
                    centroidLoopMax=centroidLoopMax, minsamples=minsamples)
            # now that this ant is done, send it to next source
            if mode > 0:
                if nextSource != None: 
                    if isInner : SAC.track(nextSource)
                    else : SAC.track(nextSource, i, waiton=SAC.NONE)
                previous_source_ = source

def m1(el1, daz1, el2, daz2):
    """Get a rough estimate for the m1 coefficient based on two
    measurements at reasonably well separated elevations with their
    respective azimuth offsets. It returns an (intercept,slope)
    pair for the line dAz=a+b*cos(El). The new coefficient
    is then: m_new = m_old - slope"""
    x1=math.cos(el1 * math.pi/180.0)
    x2=math.cos(el2 * math.pi/180.0)
    b = (daz2-daz1)/(x2-x1)
    a = b*x1-daz1
    return (a,b)

def grid(iant,xgrid=[0],ygrid=[0],sleep=4):
    """march over a grid"""
    d=Carma(iant).drive()
    d.setOffset(xgrid[0],ygrid[0])
    time.sleep(sleep)
    time.sleep(sleep)
    for y in ygrid:
        for x in xgrid:
            print x,y
            d.setOffset(x,y)
            time.sleep(sleep)

def showm():
    """show the m1..m2.. constants for the OVRO dishes in a format
    close to the optical.data file for the old POINTS program
    """
    def show1(i):
        coeff=[]
        for m in range(5):
            a=SAC.queryDouble('carma.Ovro%d.Drive.Point.Constants.m%d' % (i+1,m+1)  ,qmax_)
            coeff.append(a)
        for o in range(3):
            a=SAC.queryDouble('carma.Ovro%d.Drive.Point.Constants.o%d' % (i+1,o+1)  ,qmax_)
            coeff.append(a)
        return coeff
    print '          ant       m1      m2      m3      m4      m5      o1      o2      o3'
    for i in range(6):
        m = show1(i)
        print '          00%d    %7.3f %7.3f %7.3f %7.3f %7.3f %7.3f %7.3f %7.3f' % (i+1,m[0],m[1],m[2],m[3],m[4],m[5],m[6],m[7])
                   
                                   
def showp():
    """show the pointing offsets for the OVRO dishes 
    """
    def show1(i):
        a=SAC.queryDouble('carma.Ovro%d.Drive.Point.offsetAz' % (i+1)  ,qmax_)
        e=SAC.queryDouble('carma.Ovro%d.Drive.Point.offsetEl' % (i+1)  ,qmax_)
        return (a,e)
    print '          ant       dAz     dEl'
    for i in range(6):
        (a,e) = show1(i)
        print '          00%d    %7.3f %7.3f' % (i+1,a,e)
                   
                                   
def convertm(pfile,npfile):
    def show1(i):
        coeff=[]
        for m in range(5):
            a=SAC.queryDouble('carma.Ovro%d.Drive.Point.Constants.m%d' % (i+1,m+1)  ,qmax_)
            coeff.append(a)
        for o in range(3):
            a=SAC.queryDouble('carma.Ovro%d.Drive.Point.Constants.o%d' % (i+1,o+1)  ,qmax_)
            coeff.append(a)
        return coeff
    # get coeffs
    ocoeff=[]
    for i in range(6):
        ocoeff.append(show1(i))
    # open file
    fp = open(pfile,'r')
    data = fp.readlines()
    fp.close()
    #
    n=len(data)
    source='ABCD'
    fp = open(npfile,"w")
    n=0
    for i in data:
        n=n+1
        d=i.split()
        ant=int(d[0][4])
        mjd=float(d[3])
        az=float(d[4])
        el=float(d[5])
        dazel=float(d[6])
        delev=float(d[7])
        fp.write("MM%d OPT %-12s %s %11.5f  %6.2f  %6.2f %6.2f 0.00 %6.2f 0.00\n" % (ant,source,d[1],mjd,az,el,dazel,delev))
        m=ocoeff[ant-1]
        fp.write('          00%d  %7.3f %7.3f %7.3f %7.3f %7.3f %7.3f %7.3f %7.3f\n' % (ant,m[0],m[1],m[2],m[3],m[4],m[5],m[6],m[7]))
    fp.close()
    print "Written %d records to %s" % (n,npfile)

# This is a slew simulator and includes the wrap logic
# and drive rates. Direct copy from short.py
def totalSlewTime(starList, ant=1) :
    clockTime = 0.0
    for i in range(len(starList)-1) :
        s1 = starList[i]
        s2 = starList[i+1]
        if list in [type(s1)] : s1 = s1[0]
        if list in [type(s2)] : s2 = s2[0]
        if tuple in [type(s1)] : s1 = s1[0]
        if tuple in [type(s2)] : s2 = s2[0]
        clockTime=clockTime + slewEstimate(s1, s2, ant)
    return clockTime
    
def totalStarTime(starList, telescope) :
    if telescope < 7 :
        addOn=len(starList)*15.0/60.0
    elif telescope < 16 :
        addOn=len(starList)*10.0/60.0
    #return short.totalSlewTime(starList,telescope)+addOn
    return totalSlewTime(starList, telescope) +addOn
