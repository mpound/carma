# Helper methods for carma base code
#
# @author Steve Scott
# $Id: carmaHelpers.py,v 1.42 2013/04/19 16:13:16 abeard Exp $
#
# $CarmaCopyright$
#

import calendar
import os
import re 
import sys
import string
import time

def makeList(x) :
    """If input is a scalar, puts into single item list; 
       lists pass straight thru"""
    if list == type(x) : return x
    else :               return [x]  

def getCatalogFullPathname(filename) :
    """ Look for a catalog in /array/rt/catalogs, or in full path
        if given, or in current working directory.
    """
    if os.path.isfile( filename ) :
        return filename
    else :
        f = "/array/rt/catalogs/" + filename
        if os.path.isfile( f ) :
            return f

    raise Exception, "Did not find catalog \'" + filename + \
      "\' in path given, current directory or /array/rt/catalogs."


def getScriptFullPathname(filename) :
    """Checks if a file with .py or .obs extension exists within the 
    current python path and returns the fully-qualified pathname. 
    Appends .obs or .py to filename.
    Parameters:
      filename: file name (appended to path)
    Return: Fully-qualified path name of file"""
    exc = None
    extensions = [".obs", ".py"]
    for e in extensions:
        if ( filename[0] == "/" ) :
            f = filename + e
            try :
                g = open(f)
                g.close()
                return f
            except Exception: pass
        else :
            for p in sys.path :

                # if p <> '' add a trailing slash
                # otherwise ensure we have a fully-qualified 
                # pathname for the current working directory.
                if len(p) > 0 : p += '/'
                else : 
                    tp = p
                    p += os.getcwd() + '/'
                if False: print "trying MODIFIED [%s] [%s]\n" % (tp,p)

                f = p + filename + e
                # Remove /misc (sym link)
                if f.find('/misc/array') != -1:  f = f[5:]
                #print f
                try : 
                    g = open(f)
                    g.close()
                    return f
                except Exception, exc: pass 
    raise Exception, "Did not find file \'" + filename + \
                    "\' in path with either .obs or .py extension"         

def getCarmaBuildPath():
    """Finds path to carma build/install from assumption about relative location
    of the lib/python directory in sys.path.
    Returns path to build, terminated with a / ."""
    for p in sys.path:
        b = p.find('/lib/python')
        if p.endswith('/lib/python') and b != -1:
            return p[:b]
    raise Exception, "Did not find a potential build/install path"

def getCarmaBinaryPath():
    """Finds path to carma binaries from assumption about relative location
    of the lib/python directory in sys.path.
    Returns path to binary, terminated with a / ."""
    return getCarmaBuildPath() + "/bin"

def getCarmaConfPath():
    """Finds path to carma conf directory from assumption about relative
    location of the lib/python directory in sys.path.
    Returns path to conf, terminated with a / ."""
    return getCarmaBuildPath() + "/conf/"

def checkNTP(hostname, status=False, verbose=False):
    """Checks the status of ntp on a given host by using the ntpq command
    and returns a list of strings. The string contents depends on the mode 
    in which the program is run. Modes:
    default: returns a zero length string if offset and jitter are within spec;
      returns err message if offset or jitter for preferred clock is large or
      if there is no preferred clock or if there is a communication error.
    status: returns clock name, offset and jitter for the preferred clock
    verbose: returns all ntpq output verbatim"""
    b = hostname.find('.')
    if b == -1 :
        shorthostname = hostname
    else :
        shorthostname = hostname[:b]
    exe = "ssh " + hostname + " /usr/sbin/ntpq -p"
    invalue, fo, fe = os.popen3(exe)
    s = fe.readline()
    if len(s) > 0:
        e = "%11s err: %s" %(shorthostname,s.rstrip('\r\n'))
        #print "err", output
        return [e]
    # Skip over two lines of header
    fo.readline()
    fo.readline()
    ntpoutput = []
    readMore = True
    # Collect all the lines of ntp output
    while readMore :
        s = fo.readline().rstrip('\r\n')
        #print "read:", len(s), s
        if len(s) == 0: break
        ntpoutput.append(s)
        #print ntpoutput
    #print "ntpOutput:", ntpoutput
    # Process the output according to the mode
    if verbose : 
        output = []
        for s in ntpoutput : 
            s = "%11s:%s" %(shorthostname, s) 
            output.append(s)
        return output
    # Find preferred clock; return error msg if none found
    preferredIndex = -1
    index = 0
    for s in ntpoutput :
        if s[0] == '*': preferredIndex = index
        index += 1
    if preferredIndex == -1: 
        return ["%s: Trouble, no preferred clock found!!!" %shorthostname]
    
    # Extract clockname, offset and jitter
    s = ntpoutput[preferredIndex]
    s = s[1:] # Clip off first character
    token = s.split()
    clockname = token[0]
    delay  = float(token[7])      # in millisecs
    offset = float(token[8])*1000 # now in microsecs
    jitter = float(token[9])*1000 # now in microsecs
    if status :
        s = "%15s: clock:%-18s   offset:%6i   jitter:%6i  microseconds" \
                %(shorthostname, clockname, offset, jitter)
        return [s]
    badOffset = abs(offset) > 1000
    badJitter = jitter > 1000
    if badOffset or badJitter :
        s = "%15s: clock=%-18s" %(shorthostname, clockname)
        if badOffset: s += "  excessive offset: %iusec" %offset
        if badJitter: s += "  excessive jitter: %iusec" %jitter
        return [s]
    else :
        return [""]
  
        
def printStringSeq(seqOfStrings) :
    """ Prints each string in a sequence of strings on a separate line,
    skipping any zero length strings"""
    for s in seqOfStrings: 
        if len(s) > 0: print s    
            
     
def containsWhitespace( seq ) :
   """ Return true if input sequence/string contains any white space
   characters, false otherwise."""
   return containsAny( seq , string.whitespace )

def filterNonPrintable(str):
    """Return the input string with non-printable characters removed.
       Printable is defined here as only the printable ASCII characters.
       No unicode.
    """
    return ''.join(c for c in str if ord(c) > 31 or ord(c) == 9 or ord(c) == 10 or ord(c) == 13)

def containsNonPrintable(str):
    """Return False if the the input string contains
       only printable ASCII characters.  Return True if it
       contains unicode or non-printable ASCII
    """
    for c in str :
       if ( ord(c) <= 31 and ord(c) != 9 and ord(c) != 10 and ord(c) != 13 ) : return True
    return False

# sequence-checking methods from Python Cookbook pp. 16-18
def containsAny( seq, aset ) :
    """Check whether input sequence contains ANY items in input set.
    seq  : sequence to be examined
    aset : set of items sequence should be checked for
    Returns true if any item is in sequence, false otherwise.
    """
    for c in seq :
        if c in aset : return True

    return False

def containsOnly( seq, aset ) :
    """Check whether input sequence contains ONLY items in input set.
    seq  : sequence to be examined
    aset : set of items sequence should be checked for
    Returns true if sequence contains ONLY input items, false otherwise.
    """
    for c in seq :
        if c not in aset : return False

    return True

def containsAll( seq, aset ) :
    """Check whether input sequence contains ALL items in input set.
    seq  : sequence to be examined
    aset : set of items sequence should be checked for
    Returns true if ALL items are in sequence, false otherwise.
    """
    return not set( aset ).difference( seq )

def sortByAttr(seq, attr) :
   """Sort a list of objects by an attribute of the objects. 
      Taken from Python Cookbook section 5.3
      seq  : sequence to be sorted
      attr : name of object attribute on which to sort (in quotes) 
      Returns sorted list.
   """
#  Use this when we move to Python 2.4
#   return sorted( seq, key=operator.attrgetter(attr) )
   intermed = [ (getattr(x,attr), i, x) for i , x in enumerate(seq) ]
   intermed.sort()
   return [ x[-1] for x in intermed ]

def sortByAttrInPlace(seq, attr) :
   """In place sort of a list of objects by an attribute of the objects. 
      Taken from Python Cookbook section 5.3
      seq  : sequence to be sorted
      attr : name of object attribute on which to sort (in quotes) 
   """
#  Use this when we move to Python 2.4
#   seq.sort( key=operator.attrgetter(attr) )
   seq[:] = sortByAttr(seq, attr)

def diff(file1, file2, verbose=False) :
    """Difference two files.  
       The difference ignores whitespace, and is done via a system
       call to 'diff -qbtw'.  See diff manpage for more details.

       Return value: True if they differ, False if not.

       Parameters:
          file1 - the first file for differencing
          file2 - the first file for differencing
          verbose - True if you want more reporting, including option of
                    seeing the differences.
    """
    diffStr  = "diff -btw " + file1 + " " + file2
    diffqStr = "diff -qbtw " + file1 + " " + file2 + " >/dev/null"
    diffq   = os.system( diffqStr )
    if ( diffq != 0 ) :
       if ( verbose ) :
            print "#### Warning: Files %s and %s differ ####" % ( file1, file2 )
            ans = raw_input("Do you want to see the differences? [y/n] ")
            a = ans.upper()
       if ( a == "Y" ) :
            diffOut = os.popen( diffStr ).read()
            print diffOut
       return True
    else :
       if ( verbose ) :
           print "Files %s and %s are the same." % ( file1, file2 )

    return False


# - moved these from subarrayCommands to avoid circular dependencies
def convertHms(value):
    """ Converts input format of HH:MM:SS to decimal value

     Input : String value of the format HH:MM:SS
     Output: Decimal equivalent of sgn(HH) * (abs(HH) + MM/60.0 + SS/3600.0)
             where sgn is the sign of HH

     Examples:
         (1) ra  = convertHms('13:34:00')
         (2) ra  = convertHms('13:34')
         (3) dec = convertHms('-11:23:23')

     If input value is an integer or float, a floating point value is returned"""
    if str in [type(value)] :
        tmp = value.split(':')
        if len(tmp) == 3 : val = abs(float(tmp[0]))+float(tmp[1])/60.0+float(tmp[2])/3600.0
        elif len(tmp) == 2 : val = abs(float(tmp[0]))+float(tmp[1])/60.0
        elif len(tmp) == 1 : val = abs(float(tmp[0]))
        else : return 'Improper time format'
        if '-' in value: val *= -1.0
    elif int in [type(value)] or float in [type(value)] :
        val = float(value)
    else :
        raise Exception, 'Improper time format: '+str(value)
    return val


def convertHmsString(value, ndec=0, showSeconds=True):
    """ Converts floating point to HH:MM:[SS.S]

        Inputs: value - Floating point number
                ndec  - number of decimal points to print seconds
                showSeconds - If True, show seconds, otherwise just
                              use HH:MM. Default:True

        Output: String of format hh:mm:[ss.s]
    """

    t = value
    if int in [type(value)] or float in [type(value)]:
        x = abs(value)
        h = int(x)
        m = int(60 * (x - h))
        sec = 3600.0 * (x - h - m/60.0)

        t = str("%.2d" % h) + ':' + str('%.2d' % m) 

        if showSeconds  :
            # Add seconds
            t += ':'
            format = '%0' + str(ndec+3) + '.' + str(ndec) + 'f'
            if ndec <= 0: format = '%.2d'
            t += str(format % sec)

        if value < 0.0: t = '-' + t
    return t

def frame2ctime( frame ):
    """Convert a frame to ctime.  
    Once frame time is converted to ctime you can do pretty much anything you
    want using the python time module (e.g. pretty print, etc).
    Parameters:
     frame: Number of integer half seconds since Jan 1st 2000 00:00:00 UTC
    Returns:
     Floating point ctime."""
    frameEpochTuple = (2000, 1, 1, 0, 0, 0, 0, 0, 0) # Time tuple in LOCAL TIME!
    frameEpochInCtime = time.mktime( frameEpochTuple ) - time.timezone
    framesInSeconds = frame / 2.0
    return frameEpochInCtime + framesInSeconds

def mjd2ctime(mjd):
    """Convert an MJD to ctime.  
    Once frame time is converted to ctime you can do pretty much anything you
    want using the python time module (e.g. pretty print, etc).
    Parameters:
     mjd: Modified Julian Daynumber
    Returns:
     Floating point ctime."""
    return 86400.0*(mjd - 40587)

def string2frame( str ):
    """Parse 'flexible format' time string and return corresponding frame.
    Parameters:
     str: String to parse - see string2ctime for a detailed format description.
    Return:
     frame corresponding to input string."""

    ctime = string2ctime( str )
    
    frameEpochTuple = (2000, 1, 1, 0, 0, 0, 0, 0, 0) # Time tuple in LOCAL TIME!
    frameEpochInCtime = time.mktime( frameEpochTuple ) - time.timezone

    # 0.25 is to avoid rounding errors - fractional part is dropped in cast
    return long( 2.0 * ( ctime - frameEpochInCtime ) + 0.25 )

# This is defined separately so it can be appended to various __doc__ strings
_stringDateFormat = \
"""String dates are represented in UTC using a flexible format specifying 
either an exact date or a time relative to now.  Exact dates are of the
form 'ddMmmyyyy:HH:MM:SS'.  If a portion is omitted, sensible defaults are 
used in its place.  Examples are:
 '24Sep2004:15:30:00' - 3:30 PM September 24th 2004.
 '01Jan2001' - Midnight January 1st 2001.
 '01Jan' - Midnight January 1st of this year.
 '2Dec09:1' - 1AM December 2nd of this year.
Relative dates are specified with a number followed by 'days', 'months' or 
'years' all of which may be abbreviated to d,m or y.  For example:
 '1d 2m' - 2 months and a day ago.
 '1d 2d 3m 1y' - 1 year 3 months and 3 days ago.
 '14m 1y' - 2 years, 2 months ago.
 '1 day 2 months 3 years ago' - Just that."""

def string2ctime( str ):
    """Parse input 'flexible time' string and return corresponding ctime."""    
    months = ['jan', 'feb', 'mar', 'apr', 'may', 'jun', 
              'jul', 'aug', 'sep', 'oct', 'nov', 'dec' ]
    lmonths = ['january', 'february', 'march', 'april', 'may', 'june', 
               'july', 'august', 'september', 'october', 'november', 'december']

    def parseDateTimeMatch( match ):
        """Parse date time match and return a time tuple representing UTC."""
        # Create a time tuple with the parsed values filling in defaults if 
        # not specified in the match.
        groupdict = match.groupdict() 

        now = time.gmtime( ) # Time tuple of 'now' in gmtime

        day = groupdict['day']
        month = groupdict['month']
        year = groupdict['year']
        hour = groupdict['hour']
        minute = groupdict['minute']
        second = groupdict['second']

        if day == None: day = now.tm_mday
        else: day = int( day ) 
        if month == None: month = now.tm_mon
        else: month = months.index( groupdict['month'][:3].lower() ) + 1
        if hour == None or hour == '': hour = 0 
        else: hour = int(hour)
        if minute == None or minute == '': minute = 0 
        else: minute = int(minute)
        if second == None or second == '': second = 0 
        else: second = int(second)

        if year == None or year == '':
            thisYear = now.tm_year
            lastYear = now.tm_year - 1
            if month < now.tm_mon: 
                year = thisYear
            elif month > now.tm_mon: 
                year = lastYear
            else: 
                if day < now.tm_mday:
                    year = thisYear
                elif day > now.tm_mday:
                    year = lastYear
                else:
                    nowSecs = now.tm_hour * 3600 + now.tm_min * 60 + now.tm_sec
                    inSecs = hour * 3600 + minute * 60 + second
                    if inSecs <= nowSecs:
                        year = thisYear
                    else:
                        year = lastYear

        elif len( year ) == 2:
            year = 2000 + int( year ) 
        else:
            year = int( year )
        
        timetuple = (year, month, day, hour, minute, second, 0, 0, 0)
        return calendar.timegm( timetuple ) # Returns ctime


    # Define allowed syntax in terms of regular expressions.  The named groups
    # (?P<id>) are designed to trivialize parsing via the re.groupdict member.
    dayReGroup = "(?P<day>\d{1,2})"
    monthReGroup = "(?P<month>" + '|'.join( months ) + '|'.join( lmonths ) + ")"
    yearReGroup = "(?P<year>(\d{2}){0,2})" # Year optionally 0, 2 or 4 digits.
    dateRegex = "%s%s%s:?"%(dayReGroup,monthReGroup,yearReGroup)
    hourReGroup = "(?P<hour>\d{1,2})"
    minuteReGroup = "(?P<minute>\d{0,2})"
    secondReGroup = "((?P<second>\d{0,2}))"
    timeRegex = "%s(:%s(:%s)?)?$"%( hourReGroup, minuteReGroup, secondReGroup )

    dateAndTimeRegex = "%s($|:%s$)"%(dateRegex, timeRegex)
    dateAndTimeRe = re.compile( dateAndTimeRegex, re.I )
    match = dateAndTimeRe.match( str )
    if match:
        ctime = parseDateTimeMatch( match )
        return ctime
        
    # Note below the (%s){0} or 'match exactly zero times' - this is to pick up 
    # the groups even if they are empty - kludge city
    timeOnlyRegex = "(%s){0}:?%s"%( dateRegex, timeRegex)
    timeOnlyRe = re.compile( timeOnlyRegex, re.I )
    match = timeOnlyRe.match( str )
    if match:
        ctime = parseDateTimeMatch( match )
        return ctime

    # No match with formal date syntax, try the 'special' format (e.g 1d).
    quantityDaysReGroup = "((?P<quantdays>\d+) ?d(ays?)? ?(ago)? ?)"
    quantityMonthsReGroup = "((?P<quantmonths>\d+) ?m(onths?)? ?(ago)? ?)"
    quantityYearsReGroup = "((?P<quantyears>\d+) ?y(ears?)? ?(ago)? ?)"
    
    def parseSpecialTime( str ):
        """Parse 'special' time format and return a corresponding ctime."""
    
        daysRe = re.compile( quantityDaysReGroup, re.I )
        daysAgo = 0
        startPos = 0
        match = daysRe.search( str, startPos )
        while match:
            daysAgo += int( match.groupdict()['quantdays'] )
            startPos = match.end()
            match = daysRe.search( str, startPos )

        monthsRe = re.compile( quantityMonthsReGroup, re.I )
        monthsAgo = 0
        startPos = 0
        match = monthsRe.search( str, startPos )
        while match:
            monthsAgo += int( match.groupdict()['quantmonths'] )
            startPos = match.end()
            match = monthsRe.search( str, startPos )

        yearsRe = re.compile( quantityYearsReGroup, re.I )
        yearsAgo = 0
        startPos = 0
        match = yearsRe.search( str, startPos )
        while match:
            yearsAgo += int( match.groupdict()['quantyears'] )
            startPos = match.end()
            match = yearsRe.search( str, startPos )

        now = time.gmtime( ) # Time tuple of 'now' in gmtime
        
        # Months must be in 1..12 range
        y,m = divmod( monthsAgo, 12 )
        if m >= now.tm_mon:
            year = now.tm_year - yearsAgo - y - 1
            month = ( 12 + now.tm_mon ) - monthsAgo
        else:
            month = now.tm_mon - m
            year = now.tm_year - yearsAgo - y
        
        then = ( year, 
                month,
                now.tm_mday - daysAgo,
                now.tm_hour,
                now.tm_min,
                now.tm_sec, 
                0, 0, 0 )

        return calendar.timegm( then ) # Returns ctime


    specialTimeRegex = "(%s|%s|%s)+$"%( quantityDaysReGroup, 
                                       quantityMonthsReGroup,
                                       quantityYearsReGroup )

    specialTimeRe = re.compile( specialTimeRegex, re.I )
    match = specialTimeRe.match( str )
    if match:
        ctime = parseSpecialTime( str )
        return ctime        
    raise ValueError, "String does not match flexible date string format."


string2ctime.__doc__ += '\n'+_stringDateFormat   

def frame2string( frame, fmt="%d %b %Y %H:%M:%S" ):
    """Output frame as string according to format in UTC/GMT."""
    fctime = frame2ctime( frame )
    frameGMTTuple = time.gmtime( fctime )
    gmtAsString = time.strftime( fmt, frameGMTTuple )
    return gmtAsString 

def currentFrame( ):
    """Return current frame time."""
    frameEpochTuple = (2000, 1, 1, 0, 0, 0, 0, 0, 0) # Time tuple in LOCAL TIME!
    frameEpochInCtime = time.mktime( frameEpochTuple ) - time.timezone
    currentCtime = time.time()
    return long( 2.0 * ( currentCtime - frameEpochInCtime ) ) 

def parseKeywords(invalue) :
    """Takes an input string and parses it into a dictionary of 
    keywords and values, suitable for processing input to the run command.
    The Input string is assumed to be whitespace separated 'keyword=value' 
    pairs. The only exception to this format is that if 'help'
    (case insensitive) appears anywhere in the input string then an
    entry with keyword help and value True will be generated. 
    All the keywords will be converted to lower case. Whitespace is 
    not allowed within keywords."""
    if type(invalue) != str :
        raise Exception("Input must be a string")
    output = dict()
    # See if we have requested help
    foundHelp = False
    lowerInput = invalue.lower()
    locHelp = lowerInput.find("help")
    if locHelp != -1 :
        foundHelp      = True
        output["help"] = True
        # Remove help from the input string
        invalue= invalue.replace(input[locHelp:locHelp+4], "")
    # Parse the line by breaking at the equals signs
    pieces = invalue.split('=')
    # The first piece is special as it is only a key
    keys = [pieces[0].strip().lower()]
    # The last piece is special because it is only a value
    lastPiece = pieces[-1].strip()
    # Remove first and last pieces that we handled specially
    pieces = pieces[1:-1]
    values = []
    # All of these pieces have a value and the next key separated by whitespace
    for p in pieces:
        v, k = p.split(None, 1)
        values.append(v.strip())
        keys.append(k.strip().lower())
    values.append(lastPiece)
    # Put keys and values into the output dictionary
    # If we don't have enough values, put in 'bogus'
    for i in range(len(keys)) :
        v = "bogus"
        if i < len(values) : v = values[i]
        output[keys[i]] = v
    # Check for valid keys (no whitespace) 
    for k in output.keys() :
        if (len(k.split()) > 1) and not foundHelp :
            raise Exception("Keyword '%s' cannot have embedded whitespace" %k)     
    return output 

def printNeighbor( neighborList ) :
    """ debug method to print the members of a list of NearestInfo objects"""
    for n in neighborList  :
        print "%8s %8s %8.3f %8.3f %8.3f %8.2f %s %13.6f" % ( n.name,n.reference,n.distance,n.azimuth,n.elevation,n.brightness,n.isOptical,n.mjd )

def prefixStringList( prefix, stringList ):
    """Recursively iterate through all lists in stringList
    and prefix string contents with input prefix. 
    Parameters:
     prefix String to prefix
     stringList Possibly recursive list of strings.
    """
    output = []
    for i in stringList:
        if list == type( i ): output.append( prefixStringList(prefix, i) )
        else: output.append( prefix + i )
    return output

def getAbsoluteMPName(subarrayController, mp):
    """Lookup an absolute MP name, with perfect capitalization"""
    r = subarrayController.queryMonitorPoint([mp])[0]
    if r.found is not True:
        raise Exception("no such monitor point exists: %s" % mp)

    return r.name

def getAstroBandName(abnum):
    """Return the astroband name for a given number"""
    if abnum < 1 or abnum > 24:
        raise Exception('AstroBand number (%d) must be in range [1-24]' % abnum)

    if abnum <= 8:
        return 'CarmaSlcBand' + str(abnum)
    else:
        return 'WbcBand' + str(abnum - 8)


class NumberRange(object):
    """Helper class used by formatAsRanges()"""
    def __init__(self):
        self.numbers = []

    def addToRange(self, num):
        if len(self.numbers) == 0:
            self.numbers.append(num)
            return

        if num == self.numbers[-1] + 1:
            self.numbers.append(num)
            return

        raise ValueError('number is not in range')

    def isValid(self):
        return len(self.numbers) > 0

    def __str__(self):
        if len(self.numbers) == 0:
            raise ValueError('range is not valid: no numbers')

        if len(self.numbers) == 1:
            return str(self.numbers[0])

        return '%d-%d' % (self.numbers[0], self.numbers[-1])

    def __repr__(self):
        return str(self)

def formatAsRanges(nums):
    """Routine for converting lists of numbers to  a string 
       formatted as sorted ranges.   Adapted from Tom Costa's
       C++ implementation by Ira Snyder
        
       Parameter:  
           nums  - the input list of numbers
    """

    # turn the list into a set to eliminate duplicates, then
    # back into a list so we can sort it
    nums = set(nums)
    nums = list(nums)

    # handle empty or single item lists
    if len(nums) < 2:
        return str(nums)

    # sort the list
    nums.sort()

    ranges = []
    currentRange = NumberRange()
    for i in nums:
        try:
            currentRange.addToRange(i)
        except ValueError:
            ranges.append(currentRange)
            currentRange = NumberRange()
            currentRange.addToRange(i)

    if currentRange.isValid():
        ranges.append(currentRange)

    return str(ranges)
    
