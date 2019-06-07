# @file
#
# Python script to read monitor log file and grab the essentials related to monitor dropouts
# Prints out times and delta times (minutes) of dropouts.
# @author Steve Scott
# $Id: opticalDataAnz.py,v 1.4 2012/01/04 00:11:29 scott Exp $
#
# $CarmaCopyright$


import sys, time, calendar


def getFilename(filestring) :
    "Returns filename."
    fname = "/var/log/carma/monitor.log"
    if filestring != None :
        if isinstance(filestring, int): fname = fname + '.' + str(filestring)
        else: fname = filestring
    print "Filename:", fname
    return fname


def getT0(fname):
    'Return first timestamp in file, as a frame count'
    f = open(fname)
    l = f.readline() 
    f.close()
    print "Reference time (first timestamp in file): " + l[0:15]
    monthday = l[0:6]
    hhmmss = l[7:15]
    t0string = monthday + " " +str(time.gmtime().tm_year) + " " + hhmmss
    #print "*"+t0string+"*" 
    tm = time.strptime(t0string, "%b %d %Y %H:%M:%S") 
    frame0_tm = time.strptime("Jan 01 2000", "%b %d %Y") 
    #print tm
    t0      = calendar.timegm(tm)
    frame0  = calendar.timegm(frame0_tm)
    t0Frame = 2*(t0 - frame0)
    #print t0Frame, t0, t0%86400, (t0%86400)/3600  
    return t0Frame
    
def convertTime(timestring):
    """Convert a 16 char timestring into a Unix time.
    This time string is in the same format as the time stamp in the data"""        
    #monthday   = timestring[0:6]
    #hhmmss     = timestring[7:15]
    #timestring = monthday + " " +str(time.gmtime().tm_year) + " " + hhmmss
    tm = time.strptime(timestring, "%y%b%d:%H:%M:%S") 
    return calendar.timegm(tm)

def unixTimeToString(t) :
     return time.strftime("%d%b%y %H:%M:%S", time.gmtime(t))    
 
def parseit(l):
    items = l.split()
    a = int(items[1])
    t = convertTime(items[0])
    return t, a
    
                                            
def anz(fname = "/home/obs/pointing/subOpticalInfo.txt", suppressGood=False,
        singles=False):
    """Process the optical pointing data file and print out ant numbers
    of missing data.
    Parameters:
     fname: data file name
     suppressGood: don't print when all 15 ants succeeded
     singles: only print single ant failures"""

    if fname == "archive": 
        fname =  "/home/obs/pointing/subOpticalInfo2007-2010.txt"     
    f = open(fname)    
    # Form a list where each element is a line
    lines = f.readlines()
    f.close()
    
    print "Lines:", len(lines)
    totlines = len(lines)
    linenum = 0
    curtime = 0
    ants = range(1,16)
    for l in lines:
        linenum += 1
        #print linenum, ants
        if len(l) > 20:
            t,a = parseit(l)
            timestr = unixTimeToString(t)
            #print "%d/%d" %(linenum,totlines), timestr, t, a 
            if abs(curtime-t) > 1:
                #print linenum, t, a, ants
                if len(ants) < 15:
                    if singles :
                        if len(ants) == 1: print timestr, ants[0]
                    else:
                        if len(ants) > 0: print timestr, ants
                        elif not suppressGood: print timestr
                ants = range(1,16)
                curtime = t
            #else:
            #print ants.count(a), ants, a 
            if ants.count(a) > 0: ants.remove(a)
anz()
#anz(suppressGood=True, singles=True, fname="archive")
