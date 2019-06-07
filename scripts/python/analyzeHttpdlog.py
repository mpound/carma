# @file
#
# Python script to read http log files and do some simple analysis
# @author Steve Scott
# $Id: analyzeHttpdlog.py,v 1.3 2010/02/18 20:14:07 scott Exp $
#
# $CarmaCopyright$


import sys, time, calendar
import pylab as plt
from matplotlib.dates import date2num
from matplotlib.dates import DayLocator, HourLocator, MinuteLocator
from matplotlib.dates import DateFormatter, MONDAY, DAILY
import datetime
from math import *
import types
import string


def getFilename(filestring) :
    "Returns filename."
    fname = "/var/carma/log/monitor.log"
    if filestring != None :
        if isinstance(filestring, int): fname = fname + '.' + str(filestring)
        else: fname = filestring
    print "Filename:", fname
    return fname

def getQuotedString(input) :
    beg = input.find('"')
    if beg == -1: return input, "StringNotFound", True
    last = input[beg+1:].find('"')
    if last == -1: return input, "NoFinalQuote", True
    ostr = input[beg+1:beg+1+last]
    if len(input) <= beg+last+1: return "", ostr, True
    output = input[beg+last+2:]
    allDone = (output.find('"') == -1)
    return input[beg+last+2:], ostr, allDone
    
def getAllQuotedStrings(input) :
    allDone = False
    rtn = []
    while not allDone :
        initialInput = input
        input, ostr, allDone = getQuotedString(input) 
        #print initialInput, input, ostr, allDone
        rtn.append(ostr)
        if allDone: return rtn 
def removeSeparator(input) :
    """Takes a list of strings and returns a list of strings
    with any '-' string removed."""
    o = []
    for s in input: 
        if s != '-': o.append(s)
    return o            
        
def getLines(fname="/home/scott/access.log", startLine=0, maxLines = -1, 
             maxHosts=10, verbose=False) :
    googleBoxAddress = '192.100.16.141'
    f = open(fname)    
    # Form a list where each element is a line
    allLines = f.readlines()
    f.close()
    totLines = len(allLines)
    hostsDict = {}
    rows = []
    if startLine > 0:
        if startLine > totLines:
            raise Exception, "Requested start line exceeds # lines in file(%d)" %l 
    if maxLines > 0 or startLine > 0:
        if maxLines < 0: allLines = allLines[startLine:]
        else :           allLines = allLines[startLine:startLine+maxLines] 
    gboxTot        = 0
    internalMsgTot = 0          
    for line in allLines :
        try :
            host,rest = line.split('- ', 1)
        except:
            print "****Couldn't find '- ' to split line****"
            print line
            continue    
        host = host[:-1]
        auth,rest = rest.split(' ', 1)
        timestrbeg = rest.find("[")
        timestrend = rest.find("]")
        if timestrbeg == -1 or timestrend == -1:
            print "No timestring found", line
            continue
        timestr = rest[timestrbeg+1: timestrend-6]
        rest = rest[timestrend+1:] 
        quotedStrings = getAllQuotedStrings(rest)
        quotedStrings = removeSeparator(quotedStrings)
        timeSecs = convertTime(timestr)
        isInternalMsg = quotedStrings[-1].find('internal dummy') != -1
        isGoogleBox   = (host == googleBoxAddress)
        if isGoogleBox : gboxTot += 1
        if isInternalMsg: internalMsgTot += 1
        #print timestr, quotedStrings[-1], isInternalMsg
        data = [host, auth, timestr, timeSecs, quotedStrings, isInternalMsg, isGoogleBox]
        rows.append(data) 
        if verbose : print data
        if not isInternalMsg and not isGoogleBox: 
            if hostsDict.has_key(host): hostsDict[host] += 1
            else:                       hostsDict[host] = 1
    # Turn dictionary into a list        
    hosts = list(hostsDict.items()) 
    hosts.sort(lambda i1, i2: cmp(i2[1], i1[1]))   # Sort by number of hits    
    print "Number of lines in file:", totLines  
    print "Number of hosts in file:", len(hosts)  
    if False :
        for i in hosts:  
            print "%16s: %6d" %i 
    # Now cut down the number of hosts
    othersTot = 0
    otherHosts = 0
    if len(hosts) > maxHosts:
        for i in range(maxHosts, len(hosts)): othersTot += hosts[i][1]
        otherHosts = len(hosts)-maxHosts
        hosts[maxHosts:] = [] # Erase entries beyond maxHosts
        if verbose:
            for i in hosts:  
                print "%16s: %6d" %i 
        hosts.append(("%d other hosts" %otherHosts, othersTot))
    if verbose and othersTot > 0: 
        print hosts[maxHosts][0],  "with", hosts[maxHosts][1], "total hits"        
    return [rows, hosts, gboxTot, internalMsgTot, othersTot]   

def calcBin(t, t0, intervalSecs):
    return int((t-t0)/intervalSecs)
        
def processData(interval=5, fname="/home/scott/access.log",startLine=0, maxLines=-1,
        maxHosts=5, verbose=False):
    """Interval is in minutes for binning counts"""
    results = getLines(fname=fname, startLine=startLine, maxLines=maxLines,
                maxHosts=maxHosts, verbose=verbose)
    rows, hosts, gboxTot, internalMsgTot, othersTot = results
    t0 = rows[0][3]
    intervalSecs = interval*60
    t0 = intervalSecs*int(t0/intervalSecs)
    hostsDict = {}
    for i in range(len(hosts)):
        hostsDict[hosts[i][0]] = i 
    #print hostsDict               
    maxBin = calcBin(rows[-1][3], t0, intervalSecs)
    shortHeaders = ["Time", "Tot", "Int", "Gbox"]
    longHeaders  = ["Time", "Total Hits", "Internal dummy cnx", "CARMA GoogleBox"]
    bins = []
    bmaster = [0, 0, 0] # For total, dummy, gbox
    i = 0
    for h in hosts:       
        shortHeaders.append(string.ascii_letters[i])
        i += 1
        longHeaders.append(h[0])
        bmaster.append(0)
    if othersTot != 0: shortHeaders[-1] = "Oth"    
    # Set up all bins with time and 0 in accumulators    
    for b in range(maxBin+1):
        bins.append([t0+b*intervalSecs]+bmaster)
    for r in rows :
        b = calcBin(r[3], t0, intervalSecs)
        if r[5] :  bins[b][2] += 1 # dummy
        else : 
            bins[b][1] += 1 # total
            # Google box
            if r[6] : bins[b][3] += 1
            else : # Some other host
                host = r[0]
                #print host, hostsDict.has_key(host)
                if hostsDict.has_key(host): bins[b][hostsDict[host]+4] += 1
                else :                      bins[b][-1] += 1            
    return [shortHeaders, longHeaders, bins, gboxTot, internalMsgTot, othersTot]

def printData(fname="/home/scott/access.log",interval=5, maxHosts=5,
        startLine=0, maxLines=-1, verbose=False) :
    """Print data from the http log. The data are summed into bins covering
    a specified time interval (few minutes).
    Parameters:
     fname: httpd log file name
     interval: bin size in minutes (default=5)
     maxHosts: number of specific hosts to display (those with most hits),
               default=5
     startLine: line number to begin processing log file (default=0)
     maxLines: number of lines to process (default=-1 = all)
     verbose: print extra debugging info for programmer (default=False)"""
    if type(fname) != types.StringType: 
        raise Exception, "Input filename(fname) must be astring"
    results = processData(fname=fname, 
                      interval=interval,   maxHosts=maxHosts, 
                      startLine=startLine, maxLines=maxLines,
                      verbose=verbose)
    shortHeaders, longHeaders, rows, gboxTot, internalMsgTot, othersTot = results
    print "GboxTot:", gboxTot, "InternalTot", internalMsgTot, "OthersTot:", othersTot
    maxIndex = len(shortHeaders) 
    for i in range(1, maxIndex):
        print "%s: %s" %(shortHeaders[i], longHeaders[i])                 
    w = 11
    out = ""
    for s in shortHeaders:
        out += "%*s " %(w,s) 
        w = 5  
    print out             
    dashes = "----------------"
    out = ""
    w = 11
    for s in shortHeaders:
        out +=  "%s " %dashes[:w] 
        w = 5               
    print out             
    for r in rows:
        s = time.strftime("%d%b:%H:%M", time.localtime(r[0]))
        for i in range(1,len(r)): s += " %5d" %r[i]
        print s
           
def plotData(fname="/home/scott/access.log",interval=5, startLine=0, maxLines=-1,
        maxHosts=5) :
    """Plot data from the http log. The data are summed into bins covering
    a specified time interval (few minutes). The plot is interactive.
    Parameters:
     fname: httpd log file name
     interval: bin size in minutes (default=5)
     startLine: line number to begin processing log file (default=0)
     maxLines: number of lines to process (default=-1 = all)
     maxHosts: number of specific hosts to display (those with most hits),
               default=5"""
    if type(fname) != types.StringType: 
        raise Exception, "Input filename(fname) must be a string"
    results = processData(fname=fname, interval=interval,
                    startLine=startLine, maxLines=maxLines, maxHosts=maxHosts)
    shortHeaders, longHeaders, rows, gboxTot, internalMsgTot, othersTot = results

    t      = list()
    traces = list()
    nHosts = len(rows[0])-1
    for i in range(nHosts): traces.append([])
    timestr = time.strftime("%d%b:%H:%M", time.localtime(rows[0][0]))
    print "Start time of data:", timestr
    for r in rows:
        dt = datetime.datetime.fromtimestamp(r[0])
        t.append(date2num(dt))
        for i in range(nHosts): traces[i].append(r[i+1])
    plt.close()
    tspan = max(t) - min(t) # In days
    if tspan > 3.5:
        tmin = int(min(t)) - 0.1
        tmax = int(max(t)) + 1.0 + 0.1
    else :
        tmin = (int(24*min(t)) - 0.1)/24.0
        tmax = (int(24*max(t)) + 1.0 + 0.1)/24.0
    numdays = tmax-tmin
    #print "Numdays:", numdays, "Numhrs:", numdays*24
    if tspan < 1: style  = ':-'
    else:         style  = '--'
    f = plt.figure(1, [13.0,8.0])
    plotList = []
    colors = 'bgrcmyk'
    for i in range(nHosts):
        marker = style[i%2] + colors[i%7]
        plotList.append(plt.plot_date(t, traces[i], marker, figure=f))
    #hitPlot = plt.plot_date(t,hits, hitMarker, xdate=True, label="Hits", figure=f)
    #dummyPlot = plt.plot_date(t,dummys, '-', xdate=True, label="Internal dummy cnx",figure=f)
    handle = plt.gca()
    handle.set_xlim(tmin, tmax) 
    daily   = DayLocator() # every day
    hourly  = HourLocator()
    hours2  = HourLocator(range(0,24,2))
    hours4  = HourLocator(range(0,24,4))
    hours6  = HourLocator(range(0,24,6))
    hours12 = HourLocator(range(0,24,12))
    mins30  = MinuteLocator(range(0,60,30))
    mins10  = MinuteLocator(range(0,60,10))
    daymonFmt  = DateFormatter("%d%b")
    daymonhrFmt  = DateFormatter("%d%b%H:")
    hrminFmt     = DateFormatter("%H:%M")
    if numdays < 0.3:    
        handle.xaxis.set_major_locator(mins30)
        handle.xaxis.set_minor_locator(mins10)
        handle.xaxis.set_major_formatter(hrminFmt)
    elif numdays < 0.6 :    
        handle.xaxis.set_major_locator(hourly)
        handle.xaxis.set_minor_locator(mins10)
        handle.xaxis.set_major_formatter(hrminFmt)
        handle.xaxis.set_major_formatter(daymonhrFmt)
    elif numdays < 1.2 :    
        handle.xaxis.set_major_locator(hours2)
        handle.xaxis.set_minor_locator(mins30)
        handle.xaxis.set_major_formatter(hrminFmt)
    elif numdays < 2.4 :    
        handle.xaxis.set_major_locator(hours6)
        handle.xaxis.set_minor_locator(hourly)
        handle.xaxis.set_major_formatter(daymonhrFmt)
    elif numdays < 4 :    
        handle.xaxis.set_major_locator(hours12)
        handle.xaxis.set_minor_locator(hours2)
        handle.xaxis.set_major_formatter(daymonhrFmt)
    else : # More than 4 days
        handle.xaxis.set_major_locator(daily)
        handle.xaxis.set_major_formatter(daymonFmt)
        if numdays > 6 :
            handle.xaxis.set_minor_locator(hours12)
        else :    
            handle.xaxis.set_minor_locator(hours6)
    plt.xlabel('Time')         
    plt.ylabel('Hits per %.1f minutes' %interval) 
    plt.legend(plotList, longHeaders[1:])        
    plt.grid(True)
    plt.show() 
    
def convertTime(timestr) :
    tm = time.strptime(timestr, "%d/%b/%Y:%H:%M:%S")
    t = time.mktime(tm) 
    return t
