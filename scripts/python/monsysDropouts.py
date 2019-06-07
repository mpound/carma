# @file
#
# Python script to read monitor log file and grab the essentials related to monitor dropouts
# Prints out times and delta times (minutes) of dropouts.
#
# Instructions for use:
#  eng1: import monitorSystemDropouts as md
#  help 
# @author Steve Scott
# $Id: monsysDropouts.py,v 1.22 2014/07/08 21:00:10 scott Exp $
#
# $CarmaCopyright$


import sys, time, calendar


#Dictionary of short aliases for subsystem names
aliases = {
    'Ovro1':'O1',
    'Ovro2':'O2',
    'Ovro3':'O3',
    'Ovro4':'O4',
    'Ovro5':'O5',
    'Ovro6':'O6',
    'Bima1':'B1',
    'Bima2':'B2',
    'Bima3':'B3',
    'Bima4':'B4',
    'Bima5':'B5',
    'Bima6':'B6',
    'Bima7':'B7',
    'Bima8':'B8',
    'Bima9':'B9',
    'Sza1':'Z1',
    'Sza2':'Z2',
    'Sza3':'Z3',
    'Sza4':'Z4',
    'Sza5':'Z5',
    'Sza6':'Z6',
    'Sza7':'Z7',
    'Sza8':'Z8',
    'SlcBand1':'S1',
    'SlcBand2':'S2',
    'SlcBand3':'S3',
    'SlcBand4':'S4',
    'SlcBand5':'S5',
    'SlcBand6':'S6',
    'SlcBand7':'S7',
    'SlcBand8':'S8',
    'WbcBand1':'W1',
    'WbcBand2':'W2',
    'WbcBand3':'W3',
    'WbcBand4':'W4',
    'WbcBand5':'W5',
    'WbcBand6':'W6',
    'WbcBand7':'W7',
    'WbcBand8':'W8',
    'WbcBand9':'W9',
    'WbcBand10':'W10',
    'WbcBand11':'W11',
    'WbcBand12':'W12',
    'WbcBand13':'W13',
    'WbcBand14':'W14',
    'WbcBand15':'W15',
    'WbcBand16':'W16',
    'CarmaSlcBand1':'C1',
    'CarmaSlcBand2':'C2',
    'CarmaSlcBand3':'C3',
    'CarmaSlcBand4':'C4',
    'CarmaSlcBand5':'C5',
    'CarmaSlcBand6':'C6',
    'CarmaSlcBand7':'C7',
    'CarmaSlcBand8':'C8',
    'SlPipeline':'SP',
    'WbPipeline':'WP',
    'Sldc':'SD',
    'Wbdc':'WD',
    'Test':'TS',
    'Loberotator':'LR',
    'MasterClock':'MC',
    'PhaseMonitor':'PM',
    'Weather':'WX',
    'Alarm':'AL',
    'Imr':'IM',
    'LineLength':'LL',
    'LoRef':'LO',
    'Control':'CO',
    'DelayEngine':'DE',
    'ProjectDatabaseManager':'PD',
    'OpacityMonitor': 'OM',
    'SystemStatus': 'SS',
    'SignalPath': 'PP',
    'Astro': 'AS',
    'Dataflow': 'DF'
}

def getFilenames(logNums=range(0,11), logname="monitor",logdir="/var/log/carma/") :
    """Returns list of filenames, time ordered old to new.
    Params:
      logNums: a list of log numbers, e.g. [3,4,5]. An integer will be 
        converted to a single element list."""
    fnameBase = logdir + logname + ".log"
    if isinstance(logNums, int): logNums = [logNums]
    fnames = list()
    logNums.sort(reverse=True)
    for i in logNums:
        if i == 0: f = fnameBase
        else: f = "%s.%d" %(fnameBase, i)
        fnames.append(f)
    return fnames
    
def getRestarts(fname) :
    f = open(fname)    
    # Form a list where each element is a line
    alltext = f.readlines()
    f.close()
    t = [line for line in alltext if line.find('control system') != -1]
    print "Control system restarts"
    for line in t:
        legit = False
        if line.find('start') != -1 and line.find('End') != -1: 
            stext = "START"
            legit = True
        if line.find('stop') != -1 and line.find('Beg') != -1: 
            stext = "STOP "
            legit = True
        if legit: print stext, line[0:15] 

class RestartRange(object):
    def __init__(self):
        self.hasStop   = False
        self.isComplete = False
        self.stopTime  = 0
        self.startTime   = 0
    def processLine(self, line):
        if self.isComplete: 
            print "!!!This restart range is already complete!!!"
            return
        if not self.hasStop:
            if line.find('Beginning') != -1 and line.find('stop') != -1: 
                self.stopTime = convertTime(line[0:15]) - 1
                self.hasStop  = True
            elif line.find('End') != -1 and line.find('stop') != -1:
                # Normally we see Beginning stop, but this is just to cover all 
                self.stopTime = convertTime(line[0:15]) - 20
                self.hasStop  = True
            elif line.find('Beginning') != -1 and line.find('start') != -1: 
                print "Serious problem - start with no initial stop, ignoring!"
                print line
            elif line.find('start') != -1 and line.find('End') != -1: 
                print "Serious problem - start with no initial stop, ignoring!"
                print line
        else :        
            if line.find('Beginning') != -1 and line.find('stop') != -1: 
                print "Beginning stop when already stopped - ignoring"
                print line
            elif line.find('End') != -1 and line.find('stop') != -1: 
                # We expect this...
                pass
            elif line.find('Beginning') != -1 and line.find('start') != -1:
                # Set the time for debugging, but not complete yet 
                self.startTime = convertTime(line[0:15]) + 120 # 2 mins after
            elif line.find('start') != -1 and line.find('End') != -1: 
                self.startTime = convertTime(line[0:15]) + 60 # Minute after
                self.isComplete  = True
    def getRange(self):
        "Returns list containing Unix times of beginning and end of restart"
        if not self.isComplete: return [1e12,0] 
        return [self.stopTime, self.startTime]         
    
def getRestartRanges(fnames) :
    ranges = list()
    currentRange = RestartRange()
    oldline = ""
    for fname in fnames:
        f = open(fname)    
        # Form a list where each element is a line
        alltext = f.readlines()
        f.close()
        t = [line for line in alltext if line.find('control system') != -1]
        for line in t :
            if line[-1] == '\n': line = line[:-1]
            # Skip duplicate lines
            if line == oldline: continue
            oldline = line
            print line
            currentRange.processLine(line)
            if currentRange.isComplete :
                ranges.append(currentRange.getRange())
                currentRange = RestartRange()        
    return ranges 

def ctime2string(ctime, fmt="%d %b %Y %H:%M:%S"):
    GMTTuple = time.gmtime(ctime)
    gmtAsString = time.strftime(fmt, GMTTuple)
    return gmtAsString 
    
def timediff2string(ctime, printDays=True):
    fmt="%H:%M:%S"
    GMTtuple = time.gmtime(ctime)
    st = ""
    days = GMTtuple.tm_yday - 1
    if days > 0: st = "%3dd " %(days)
    else :       st = "     "
    st += time.strftime(fmt, GMTtuple)
    return st 
        
def printRanges(ranges, printDays=True) :
    #print ranges
    for r in ranges:
        begin = ctime2string(r[0])
        end   = ctime2string(r[1])
        diff  = timediff2string(r[1]-r[0], printDays)
        print "%s   %s    %s" %(begin, end, diff)
                   
def getOperationalRanges(fnames, 
        printRestartRanges=False, printOperationRanges=False) :
    f = open(fnames[0])
    line = f.readlines()[0]
    f.close()
    t0 =  convertTime(line[0:15])    
    restartRanges = getRestartRanges(fnames)
    farFuture=2000000000
    if len(restartRanges) == 0: return [(t0, farFuture)]                                
    operationalRanges = list()
    start = t0
    for rr in restartRanges :
        stop = rr[0]
        operationalRanges.append((start,stop))
        start = rr[1]
    f = open(fnames[-1])
    line = f.readlines()[-1]
    f.close()
    tlast =  convertTime(line[0:15])    
    operationalRanges.append((start,tlast))
    if printRestartRanges:
        print "Restart Times"
        printRanges(restartRanges, False)
    if printOperationalRanges:
        print "Operational Time Ranges"
        printRanges(operationalRanges, True)
    return operationalRanges
        
def getLinesWithDiscardedSubsystems(fnames) :
    discardLines = list()
    for fname in fnames:
        f = open(fname)

        # Form a list where each element is a line
        alltext = f.readlines()
        f.close()

        # Form a new list eliminating each line that contains 'character' in it
        # These contain non-printing chars that screw up the terminal if
        # you accidentally try to print them, so get rid of them
        text = [ line for line in alltext if line.find('character') == -1] 
        #print text[0:100]

        t = [ l for l in text if l.find('Discarding') != -1] 
        discardLines + t
        
    return t

def isOperational(t, operationalRanges) :
    """Assumes operational ranges are from oldest to newest"""
    if t < operationalRanges[0][0]:     return True
    if t > operationalRanges[-1][1]:    return True
    for r in operationalRanges:
        if (t >= r[0]) and (t<= r[1]) : return True
    return False
    
def createMonitorSummary(logNums=range(0,11)) :
    outputFname = "monConcat.log"
    out = open(outputFname, 'w')
    fnames = getFilenames(logNums)
    operationalRanges = getOperationalRanges(fnames, True, True)
    for fname in fnames:
        f = open(fname)

        # Form a list where each element is a line
        alltext = f.readlines()
        f.close()

        # Form a new list eliminating each line that contains 'character' in it
        # These contain non-printing chars that screw up the terminal if
        # you accidentally try to print them, so get rid of them
        text = [ line for line in alltext if line.find('character') == -1] 
        #print text[0:100]

        for l in text:
            t = convertTime(l[0:15])
            if (isOperational(t, operationalRanges)) and \
               (l.find("faultSystem") == -1) and \
               (l.find("monitorAverageWriter") == -1) and \
               (l.find("data frame count") == -1) and \
               (l.find("invalid pipeline data") == -1) and \
               (l.find("lt-dumpMonitor") == -1) and \
               True : out.write(l)
        
    out.close()
    
def printRestarts(logNums=range(0,11)) :
    fnames = getFilenames(logNums)
    operationalRanges = getOperationalRanges(fnames, True, True)
    
     
def efind(st, word, start=0, end=sys.maxint) :
    """Finds a word within the string and returns index just past word, 
    or -1 if word not found."""
    i = st.find(word, start, end)
    if i == -1: return -1
    return i + len(word)
    
def allafter(st, word, start=0, end=sys.maxint) :
    "Returns everything following word, with leading whitespace removed"
    i = efind(st, word, start, end) 
    #print "i:", i, "  stlen:", len(st)
    if i == -1: return ""
    return st[i:].lstrip()

def aliasword(s) :
    "Replace string with alias from aliases dict; if no alias return string"
    try :
        return aliases[s]
    except Exception :
        return s

def alias(s) :
    "Handles alias substitution, including those with range specifications"
    i = s.find("-")
    if i == -1:
        return aliasword(s)
    else :
        a = s[:i]
        b = s[i+1:]
        return aliasword(a) + "-" + aliasword(b)

def countSubsystemsWord(s) :
    "Returns number of subsystems in a word"
    i = s.find("-")
    if i == -1:
        return 1
    else :
        beg = int(s[i-1:i])
        l = len(s)
        fin = int(s[l-1:l])
        return fin-beg+1

def countSubsystems(s) :
    "Returns number of subsystems in a list of subsystem words"
    total = 0
    for w in s:
        total += countSubsystemsWord(w)
    return total

def listToCSV(alist) :
    "turn list of strings into a single comma separated value text string"
    st = ""
    firstElement = True
    for s in alist :
        if firstElement :
            firstElement = False
        else :
            st += ","
        st += s
    return st

def dumpAbbreviations() :
    print "Subsystem abbreviations"
    keys = aliases.keys()
    keys.sort()
    for k in keys:
        s = "  %s %s" %(aliases[k], k)
        print s

def countAbbreviations() :
    return len(aliases)

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
    """Convert a 15 char timestring (w/o year) into a Unix time.
    This time string is in the same format as the time stamp in the logs"""        
    monthday   = timestring[0:6]
    hhmmss     = timestring[7:15]
    timestring = monthday + " " +str(time.gmtime().tm_year) + " " + hhmmss
    tm = time.strptime(timestring, "%b %d %Y %H:%M:%S") 
    return calendar.timegm(tm)
    
def getSubsystemList(line) : 
    subsystemLoc = line.find("Subsystem")
    if subsystemLoc == -1: return list()                         
    subsystemsLoc = line.find("Subsystems")
    # make sure that 'Subsystems' is not a subsequent one
    if subsystemsLoc != subsystemLoc: subsystemsLoc = -1
    if subsystemsLoc == -1:
        r = allafter(line, "Subsystem")
        if len(r) == 0: return list()
        #print r
        subsys = [r.split()[0]]
        #print subsys
    else:
        r = allafter(line, "Subsystems")
        if len(r) == 0: return list()
        #print r
        s = r.split()[0] # CSV of subsystem names
        subsys = s.split(',')
    return subsys    

def unixTimeToString(t) :
     return time.strftime("%b %d %H:%M:%S", time.gmtime(t))    
                                            
def monsysDropouts(filestring=None, restartsOnly=False, 
                   suppressRestartEvents=True, raw=False):
    """Process a monitor system log file and print out time and subsystems of 
    monitor system dropouts.  Parameters:
      filestring: if not input, will process the current monitor system file.
       If an integer, will process that monitor file, e.g. monsysDropouts(4) will
       use /var/log/carma/monitor.log.4. If a string, then the string will be used as
       a filename.
      restartsOnly: when True only prints out times of system restarts. 
       Default is False. 
      suppressRestartEvents: Removes dropouts occurring during and
       one minute after restarts. Deault is True"""
      
    firstline = True  
    fname = getFilenames()
    #getRestarts(fname)
    ranges = getRestartRanges(fname)
    #for r in ranges: print "Range:", r[0], r[1]
    for r in ranges: 
        print "Restart range: ", unixTimeToString(r[0]), "-", \
            unixTimeToString(r[1])
    if restartsOnly: return

    print ""
    maxSubsystems = countAbbreviations()
    dumpAbbreviations()
    t0 = getT0(fname)
    t = getLinesWithDiscardedSubsystems(fname)   
    # Print the header lines
    print "DelT0: minutes since reference time"
    print "DelT:  minutes since last drop"
    m  = "A double slash in the subsystem list separates subsystems "
    m += "that are late by 2 frames"
    print m
    print " from subsystems that are late by a single frame."
    print ""
    print "   UT of drop    DelT0   DelT  Subsystems" 

    #Loop over all lines that have discarded subsystems in them                
    for line in t:
        # Get timestamp
        ts = int(allafter(line, "timestamp").split()[0])
        date = line[0:15]
        if firstline :
            firstline = False
            lastts = ts
            #print line
            #print date
        minutes = (ts-t0)/120.0 
        deltaMinutes = (ts-lastts)/120.0
        lastts = ts
        minString  = "%7.1f" %minutes   
        dMinString = "%6.1f" %deltaMinutes   
        # Can have two sets of subsystem names because some are 2 frames late 
        # and others 1
        # Now get the first set of subsystem names
        subsys1 = getSubsystemList(line)
        # Skip over first 'Subsystem' so we can look for a 2nd set
        line2 = allafter(line, "Subsystem")
        # Now get the second set of subsystem names
        subsys2 = getSubsystemList(line2)

        # Substitute in alias names for subsystems
        alias1 = []
        alias2 = []
        for ss in subsys1:
            alias1.append(alias(ss)) 
        for ss in subsys2:
            alias2.append(alias(ss)) 
        totalSubsystemCount = countSubsystems(subsys1) + countSubsystems(subsys2)  
        #print "maxSubsys:", maxSubsystems, "  SubsystemCount:", subsystemCount 
        subsysString = "error"
        if  totalSubsystemCount >= maxSubsystems :
            if countSubsystems(subsys1) == totalSubsystemCount:
                subsysString = '***ALL***'
            else :
                subsysString = '(***ALL***)' + listToCSV(alias1) + '//' + listToCSV(alias2)    
        elif countSubsystems(subsys2) == 0:       
            subsysString = listToCSV(alias1)
        else :    
            subsysString =  listToCSV(alias1) + '//' + listToCSV(alias2)   
             
        withinRestart = False
        if len(ranges) > 0 :
            timestamp = convertTime(date)
            for r in ranges:
                if timestamp > r[0] and timestamp < r[1]: withinRestart = True
                
        if (not suppressRestartEvents) or (not withinRestart) : 
            if raw : print line
            else : print date, minString, dMinString, " "+ subsysString

def testmp(mps=[
             "Sza2.Tracker.mode",
             "Sza5.Tracker.mode",
             "Loberotator.Channel2.delay",
             "Ovro5.AntennaCommon.Drive.Track.actualAzimuth",
             "Bima3.BimaSpecific.Drive.controlSwitch",
             "LoRef.LoRefSynthesizer1.synthFreqCmd",
             "Control.SpectralLineCorrelator.SlcBand6.ControlBandPoints.bandwidth",
             "DelayEngine.DelayData5.totalDelayPol1",
             "SignalPath.IFSwitchyard.Switchyard.switchPosition11",
             "SignalPath.LOSwitchyard.Switchyard.switchPosition13"],
             reportInterval=1000,
             printTimes=False) :
    """Continously check validity of a set of monitor points.
    Parameters:
        mps: a list of monitor point names
        preortInterval: how often we pring out results (in frames)
        printTimes: boolean indicating whether to print out the times of errors 
           when they happen (default=False)"""
    import subarrayCommands as cmd
    
    nMPs = len(mps)
    i = 1
    for m in mps:
        print i, m
        i += 1
    print ""
    m = "%5s  %6s" %("t", "samps")
    for i in range(nMPs): m += " %5d" %(i+1)
    print m
    
    count = 0
    nMPs = len(mps)
    errs = []
    for i in range(nMPs): errs.append(0)
    t0 = time.time()
    while True:
        count += 1
        r = cmd.queryMpValues(mps, nothrow=True)
        t = time.time()
        lt = time.localtime()
        for i in range(nMPs): 
            if r[i] == None: 
                errs[i] += 1
                if printTimes:
                    tenths = int(round(10*(t-int(t))))
                    m = time.strftime("%H:%M:%S", lt) + ".%d" %tenths
                    m += "  " + mps[i]
                    print m
        dt = t - t0
        if count%reportInterval == 0:
            m = "%5.0f: %6d" %(dt, count)
            for i in range(nMPs): m += " %5d" %errs[i]
            print m
        cmd.sleep(0.5)
               
            
        
        
                           
