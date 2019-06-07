# $Id: arrayHealthUtils.py,v 1.12 2009/04/07 16:52:00 lamb Exp $
# This file contains functions that may be generally useful for array
# health scripts.
#
# @Author James W. Lamb, Caltech
#
# History
# 07-Apr-2009: JWL      Deprecated command line parsing -- use Steve Scott's
# 22-Oct-2008: JWL      Changed 'sc.comment' to 'sc.rtdComment'
# 20-Oct-2008: JWL      Changed "from * import <xx>" to "import <xx> [as yy]"
# 19-Sep-2008: JWL      Made log directory name available for LogMessage instance
# 04-Aug-2008: JWL      Changed import of time
# 03-Aug-2008: JWL      Fixed error in elapsed time for logging
# 25-Jul-2008: JWL      Added standardized logging class
# 20-Jun-2008: JWL      Fixed bug in string handling in options parser
# 17-Jun-2008: JWL      Improved option parsing
# 13-Jun-2008: Original version
#

# Required imports

import string as string
import os
import printFunctions as pf
import time as t
import subarrayCommands as sc

#------------------------------------------------------------------------------
# Time functions

def dhms(deltaTime) :
    """ Converts a time (difference) to days hh:mm:ss

        Parameters:

            deltaTime
                Time in seconds to convert to a string

        Return:
            If time is < 1 day, returns a time string in "hh:mm:ss: format
            (e.g., "03:27:51").
            Otherwise it includes the day (e.g., "4d 10:15:21")
    """
    seconds = deltaTime % 60
    deltaTime = (deltaTime - seconds) // 60
    minutes = deltaTime % 60
    deltaTime = (deltaTime - minutes) // 60
    hours = deltaTime % 60
    days = (deltaTime - hours) // 24
    if days > 0 :
        timeStr = "%dd %02d:%02d:%02d" % (days, hours, minutes, seconds)
    else :
        timeStr = "%02d:%02d:%02d" % (hours, minutes, seconds)
    return timeStr

#end dhms


#------------------------------------------------------------------------------
# File and directory functions

def getDataPath(pathName, verbose = True) :
    """ Returns a valid directory for writing data

        This function will test to see if the /array/rt/ directory is
        available. If so it will add the user specified path to it,
        and if not it will be appended to /tmp/. Failing that it will
        append to the current directory. The user specified path may
        have an indefinite number of subdirectories.

        Parameters:

            pathName
                The directory under /array/rt to write data to

            verbose = True
                Writes progress messages to the terminal if it needs
                to create new files

        Return:
            A string with the full path name
    """

    # Explicitly disallow absolute path
    pathName = pathName.lstrip('/')

    # Check for existence of predefined start directory
    dataPath = "/array/rt/"
    if not os.path.exists(dataPath) :
        dataPath = "/tmp/"
    if not os.path.exists(dataPath) :
        dataPath = ""

    # Check to see that each level exists, and create if it does not

    subdirs = string.split(pathName, '/')
    for subdir in subdirs :
        if subdir != "" :
            dataPath += subdir + '/'
            if not os.path.exists(dataPath) :
                if verbose :
                    pf.printMessage("Creating directory: " + dataPath)
                os.mkdir(dataPath)
    return dataPath

# end getDataPath


#------------------------------------------------------------------------------
# Option parsing functions

def parseOptionsErrorHandler(fn, msg) :
    """ Error handler for the options parsing functions.

        Prints a message and then throws an exception.

        Parameters:
            fn
                Name of the calling function

            msg
                Message to be printed out with the exception
    """

    errorMessage = fn + ' ** error: ' + msg
    raise Exception, errorMessage

# end parseOptionsErrorHandler


def setKey(keyval, key, val) :
    """ Sets keyword values for the parseOptionList() function

        Parameters:

        keval
            A dictionary of keywords and values

        key
            A key to add

        val
            The corresponding value for the key
    """
    err = False

    vtype = type(keyval[key])
    if vtype == type(None) :
        if val.lower() == 'none':
            keyval[key] = None
        else :
            err = True

    elif vtype == bool :
        if val.lower() == 'true':
            keyval[key] = True
        elif val.lower() == 'false':
            keyval[key] = False
        else :
            err = True

    elif vtype == int :
        try:
            keyval[key] = int(val)
        except ValueError :
            err = True

    elif vtype == float :
        try:
            keyval[key] = float(val)
        except ValueError :
            err = True

    elif (vtype == list) or (vtype == tuple) or (vtype == dict) :
        try :
            keyval[key] = eval(val)
        except SyntaxError :
            err = True

    else :
        keyval[key] = val

    if err :
        try :
            keyval[key] = eval(val)
        except SyntaxError :
            keyval[key] = val
    return
# end setKey


def printValidKeywords(keyval) :
    """ Print a sorted list of keywords in the supplied dictionary
    """
    pf.printError("Allowed keywords:")
    validKeys = keyval.keys()
    validKeys.sort()
    for kw in validKeys :
        pf.printMessage("  " + kw)
    return

#end printValidKeywords


def parseOptionList(options, keyval) :
    """ Parses a list of options (such as those passed by the 'run' command)

        Takes parameter values from the options string and puts them into
        the corresponding dictionary entries in 'keyval'

        Parameters:

            options
                A string of keyword options (see below)

            keyval
                A dictionary of keywords and corresponding values

        The option list contains keywords and their values associated by the
        '=' sign. The sets of keywords may be separated by commas or spaces.
        Keywords may be: int, float, string, boolean, lists, tuples or
        dictionaries. Examples:

            'opt1 = True, opt2=3.5 opt3 = "string 1" opt4 = "string \'#2\'"'
            "opt1 = [1, 2, 3], opt2 = [1,5, 0.6], opt3 = False, kw = 'Hello'"

        Spaces are ignored, except in strings. The function first tries to
        interpret the supplied values as the same type in the 'keyvals'
        dictionary. If this fails it chooses the 'best' type for the value.
        This allows, for example:
            'loFreq = None' and 'loFreq = 95.0'
            'ants = 1' and 'ants = [1, 2, 5]'

        Note: This has a similar function to the readOptions() function in the
        obsdef2.py. It is included here because of problems in importing obsdef2
        in certain circumstances, and to handle list arguments and different
        argument types.
    """
    print 60 * '='
    print "WARNING: 'parseOptionList()' from 'arrayHealthUtils.py'"
    print "is DEPRECATED. Please use 'processInputParameters()'"
    print "from 'runCommand.py'"
    print 60 * '='
    if type(options) != str :
        raise Exception, "'options' must be a string"
    if type(keyval) != dict :
        raise Exception, "'keyval' must be a dictionary"

    GET_KEY_WORD  = 0
    GET_KEY_VALUE = 1

    state = GET_KEY_WORD
    err = False
    i = 0
    key = ''
    parenLevel = 0
    numChars = len(options)
    stringDelim = ""

    # See if we have requested help
    if (options.lower().find('help') > -1) :
        if keyval.has_key('Help') or keyval.has_key('help') or keyval.has_key('HELP') :
            # Caller is expecting a help keyword anyway, just continue and assume it
            # will be handled by the caller
            pass
        else :
            # Not expecting help keyword, so give what help we can and bail out
            printValidKeywords(keyval)
            raise Exception, "Help not handled explicitly by script (see option list above)"

    while (i < numChars) and not err :
        char = options[i]
        if state == GET_KEY_WORD :
            if char == '=' :
                val = ''
                # Make sure keyword exists
                if not keyval.has_key(key):
                    errorMessage = "Invalid keyword: '" + key + "'"
                    printValidKeywords(keyval)
                    err = True
                state = GET_KEY_VALUE
            elif char.isalnum() :
                key += char
            elif char.isspace() :
                pass
            else :
                errorMessage = "Invalid character: '" + char + "'"
                err = True

        elif state == GET_KEY_VALUE :

            # If we are in a string, pass all characters except end of string delimiter
            if stringDelim != "" :
                if char == stringDelim :
                    stringDelim = ""
                    if parenLevel == 0 :
                        setKey(keyval, key, val)
                        key = ""
                        state = GET_KEY_WORD
                    else :
                        val += char
                else :
                    val += char
            # Is this the start of a string? (Invalidate preceding chars not in brackets)
            elif (char == '"') or (char == '"') :
                if parenLevel == 0 :
                    val = ""
                else :
                    val += char
                stringDelim = char
            # Allow alpha numerics, sign, and decimal point
            elif char.isalnum() or (char == '.') or (char == '+') or (char == '-') :
                val += char
            # Flag when we are inside parentheses so commas and spaces and strings are accepted
            elif (char == '(') or (char == '[') or (char == '{') :
                val += char
                parenLevel += 1
            elif (char == ')') or (char == ']') or (char == '}') :
                val += char
                parenLevel -= 1
                if parenLevel < 0 :
                    err = True
            elif char.isspace() :
                if (val != '') :
                    if parenLevel > 0 :
                        pass
                    else :
                        setKey(keyval, key, val)
                        key = ""
                        state = GET_KEY_WORD
            elif (char == ',') :
                if parenLevel > 0 :
                    val += char
                else :
                    setKey(keyval, key, val)
                    key = ""
                    state = GET_KEY_WORD
            else :
                errorMessage = 'Invalid character: ' + char
                err = True

            # Last value does not have explicit delimiter, so make sure it is terminated
            if i == numChars - 1 :
                setKey(keyval, key, val)
        i += 1

    if err: parseOptionsErrorHandler("parseOptionList", errorMessage)

    return

# end parseOptionList

#------------------------------------------------------------------------------
# Logging and messaging functions

class LogMessage :
    def __init__(self, scriptDescr, logPath, logFile, idString) :
        """ Initialize the Array Health logging and messaging functions.
            Opens the specified log file to append to (creates it if it
            does not exist).

            Parameters:

                scriptDescr
                    A short description of the script (possibly just the script name).
                    This is shown in the RTD comment field.

                logPath
                    Path to the log file. This will be put under /array/rt

                logFile
                    Name of the file to log to

                idString
                    Version identification, usually derived from the CVS
                    revision string
        """
        self.logDir = getDataPath(logPath, verbose = True)
        self.logFile = open(self.logDir + logFile, 'a')
        self.scriptDescr = scriptDescr
        self.startTime = t.time()

        # Console initialization message
        pf.printMessage("\n" + 80 * '-')
        pf.printMessage("%s, %s" % (scriptDescr, idString))
        pf.printMessage("\nOpening logfile %s" % self.logDir + logFile)

        # Log file initialization message
        self.logFile.write("\n" + 80 * '-')
        self.logFile.write(t.strftime("\nStart: %Y-%m-%d, %H:%M:%S (LST)", t.gmtime()))
        self.logFile.write("\n%s, %s" % (scriptDescr, idString))

        # RTD comment field message
        sc.rtdComment("Array Health - %s: Starting..." % self.scriptDescr)
        return

    def message(self, message, noLog = False, noConsole = False, noRtd = False) :
        """ Send a message to the console, the RTD comment field, and the log file.

            Parameters:

                message
                    A message string to send

                noLog = False
                    Whether or not the message should go in the log file

                noConsole = False
                    Whether or not the message should go to the SAC where the
                    script is run from

                noRtd = False
                    Whether or not the message should be displayed in the
                    RTD comment field for the subarray.
        """
        timeStr = t.strftime("%H:%M:%S", t.gmtime())
        if not noLog :
            self.logFile.write("\n%s: %s" % (timeStr, message))
        if not noConsole :
            pf.printMessage(message)
        if not noRtd :
            sc.rtdComment("Array Health - %s: %s" % (self.scriptDescr, message))
        return

    def close(self) :
        """ Close the log file.

            Any further messages will cause an exception to be raised.
        """
        pf.printMessage("Closing log file")
        elapsedTime = dhms(int(t.time() - self.startTime))
        timeStr = t.strftime("%H:%M:%S", t.gmtime())
        self.logFile.write("\n%s: Elapsed time = %s" % (timeStr, elapsedTime))
        self.logFile.write(t.strftime("\nStop: %Y-%m-%d, %H:%M:%S (LST)", t.gmtime()))
        self.logFile.close()
        sc.rtdComment("")
        pf.printMessage("Finished. Elapsed time = " + elapsedTime)
        return
