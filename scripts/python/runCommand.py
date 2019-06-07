#@file
# Helper class and methods to manage input parameters for the run command, 
# which is found in subarrayCommands.py
# @author Steve Scott
# $Id: runCommand.py,v 1.15 2010/11/17 22:46:56 scott Exp $
#
# $CarmaCopyright$
#
"""Module: runCommands.py"""

import sys   
import carmaHelpers as helpers
 
class ScriptReturn(Exception) :
    """This exception can be used to return gracefully from a sciprt
    without seting off the alarm. """
  
class ScriptInputError(Exception) :
    """Exception to report a problem with script input. Suppresses
    traceback; just the error message is reported to avoid 
    unnecessary confusing information."""
  
class ScriptError(Exception) :
    """Exception to report a problem within a script. Suppresses
    traceback; just the error message is reported to avoid 
    unnecessary confusing information."""

class antlist(object):
    """Defines a type for a list of antenna numbers. If a single
    integer is input then it will be converted to a list."""
    def __init__(self, ants):
        if type(ants) == list:
            for a in ants:
                if type(a) != int: 
                    m = "all antenna numbers in the list must be integers"
                    raise ScriptInputError, m
            self.ants = ants
        elif type(ants) == int:
            self.ants = [ants]
        else :
            m = "an antlist must be a single integer or list of integers"
            raise ScriptInputError, m
            
                        
class Value(object):
    """A class to define a keyword/value and all its attributes.
    Almost all the attibutes are defined as part of the constructor.
    There are other methods for checking and setting the value,
    getting the value, and displaying the value and its attributes.
    Constructor parameters:
      type: data type (e.g. bool, float, int, antlist). A type of None
            will inhibit any type checking of the input value.
            Default=None.
      required: bool indicating whether an input value is required.
            Default=False.
      default: value to be used if this keyword/value is not input.
            Default value of default=None.
      noneAllowed: allow values equal to None. Default=False.
      description: A short description that is included in the one line
            synopsis of this keyword.   """
    def __init__(self, name,
                  type=None,  required=False,  default=None,
                  noneAllowed=False, description="No description") :
        self.name         = name
        self.noneAllowed  = noneAllowed
        self.type         = type
        self.required     = required
        self.default      = default
        self.descrip      = description
        self.val          = default
        self.wasInput     = False
    def value(self, v):
        """Set the value and check that it is consistent with the attributes.
        returns bool, errorString
        The bool is True if there is an error, in which case info about the
        error is given in the errorString.
        Parameter:
         v: the value for this keyword"""
        goodReturn = False, "Value is OK"
        tv = type(v) 
        if v == None:
            if not self.noneAllowed:       
                m = "Parameter '%s' not allowed to have value None"  %self.name
                return True, m 
            else :
                self.val = v 
        elif self.type == antlist:
            if tv == antlist :
                a = v
            else:
                try :
                    a = antlist(v)
                except Exception, e:
                    m = "Parameter '%s' must be an antlist; %s" \
                            %(self.name, e)
                    return True, m 
            self.val = a.ants
        elif (self.type != None) and (tv != self.type) :
            if self.type == float and tv == int: 
                self.val = float(v)
            elif self.type == bool and tv == int: 
                self.val = bool(v)
            else :    
                m = "Parameter '%s' must be of type '%s', not '%s'" \
                        %(self.name, self.type.__name__, tv.__name__)
                return True, m 
        else :
            self.val = v 
        self.wasInput = True 
        return goodReturn
    def getValue(self) : 
        """Get the value"""
        return self.val  
    def getValueStr(self) :     
        """Get the value as a string"""
        if self.type == str :
           return '"%s"' %self.val
        else :   
            return str(self.val)  
    def header(keywordWidth=12) :
        """Get a header for listing keyword/value attributes"""
        def dash(len, leadingSpace = 0):
            s = ""
            for i in range(leadingSpace): s += " "
            for i in range(len): s += "-"
            s += " "
            return s  
        b = "" # Used for blank space
        w1 = 1 + (keywordWidth - 7)/2
        w2 = keywordWidth - 7 - w1
        s ="%*sKeyword%*s%3sType%2sReq'd%2sDefault Val%3sNone" \
                %(w1,b,w2,b,b,b,b,b) \
                 + "   Short description"
        s += "\n"+dash(keywordWidth-1,1)+dash(8)+dash(3)+dash(14)
        s +=  dash(5,1)+dash(24,1)    
        return s  
    header = staticmethod(header) 
    def list(self, keywordWidth=12) :
        """Get a string for this keyword/value describing its attributes.
        The string does not have an embedded newline."""
        if self.type == None: typestr = "N/A"
        else :                typestr = self.type.__name__
        if self.required :    reqstr  = "Yes"
        else :                reqstr  = "No"
        if self.noneAllowed : nonestr = "OK"
        else :                nonestr = "NotOK"
        m = "%*s %8s %3s %14s  %5s  %s"  %(keywordWidth, self.name, typestr,
               reqstr, self.default, nonestr, self.descrip)
        return m    

def _getSubarrayCommmandsFrame():
    """Loop over all frames finding the one from subarrayCommands.
    When it is in this frame, the doc string will be set to the 
    doc string from the module that it is interpreting.
    This is necessary because sometimes this file is not directly imported
    in the script being executed by the run command. Throws exception
    if it cannot find the frame."""
    emsg = "Cannot find frame for subarrayCommands"
    for i in range(20):
        try : 
            f = sys._getframe(i)
            n = f.f_globals["__name__"]
            if n == "subarrayCommands" : return f
        except :
            raise Exception, emsg
    raise Exception, emsg       
                
class Params(dict):
    """When a script is run it is passed a set of keyword/value pairs
    as function parameters that need to be available to the script.
    This class is a collection of the possible keywords used
    to run a script, and the constraints on the values associated 
    with the keywords. These constraints allow the input values to
    be checked for type, required input and defaults. See the 
    add method for the details of defining keywords and the 
    ingestInputParameters for the processing that is done after all
    the parameters are defined. After the ingestion,  each defined
    keyword will become an attribute of this class instance, with the
    value set to the input value or default as appropriate. Help is
    automatically a keyword that will print out the help when it is set 
    to True and whenever there are input errors. The help includes a list
    of all defined keywords and their attributes and user defined text
    that is input with a method or as the first statement in the script.
    There are also numerous methods to get the input and defined keywords 
    as dictionaries, strings and lists of strings.
    Example usage:
     import runCommand as rc
     p = rc.Params()
     p.add("keyword1", type=float,   default=3.14159, required=False)
     p.add("source",   type=string,  required=True)
     p.add("ants",     type=antlist, default=0)
     p.add("weird",    type=None,    default=None, required=False)
     p.processInputParameters()
     print "keyword1:", p.keyword1  # See how keywords are now attributes!
     print "Input:", p.inputString()
     print "Keyword/values:", p.keywordValueString()""" 
    def __init__(self, **kw): 
        self.__dict__.update(kw)
        self.defaultHelpStr = "No help has been defined for this script"
        f = _getSubarrayCommmandsFrame()
        self.docString = f.f_locals.get('__doc__')
        self.helpStr = None
        self.inhibitExceptions = False
        self.add("help", type=bool, default=False,
                 description="Print out help and exit")
        # Stash away the globals from subarrayCommands; we use some of them 
        self.globes = f.f_globals
        self.keyVals = {} 
              
    def addValue(self, value) :
        """Add a Value instance to this parameter set.
        Parameter:
         value: An instance of a Value """
        self.update({value.name: value})
    def add(self, name, 
                  type=None, required=False,  default=None,
                  noneAllowed=False, description="No short description") :
        """Add another keyword and Value to the parameter set.
        The parameters describe the constraints on the value (see Value).
        Parameters:
          type: data type (e.g. bool, float, int, antlist). A type of None
                will inhibit any type checking of the input value.
                Default=None.
          required: bool indicating whether an input value is required.
                Default=False.
          default: value to be used if this keyword/value is not input.
                Default value of default=None.
          noneAllowed: allow values equal to None. Default=False.
          description: A short description that is included in the one line
                synopsis of this keyword.      """          
        if self.has_key(name) : 
            m = "%s is already entered as a keyword" %name         
            if self.inhibitExceptions :
                print m
            else :    
                raise Exception, m
        self.addValue(Value(name, type=type, required=required, 
                                 default=default, noneAllowed=noneAllowed,
                                 description=description))

    def noExceptions(self) : 
        """Turn off raising exceptions on errors. 
        Probably only useful for testing."""
        inhibitExceptions = True  
 
    def scriptnameString(self, suppressFilename=False):
        """Get a string with the script name and file name
        Parameter:
          suppressFilename: default=False"""
        # Put this in a try block in case the variables are not there
        try :
            f = ""
            if not suppressFilename : 
                f = "\n(file: %s)"  %self.globes["fullScriptName"]  
            return "%s%s" %(self.globes["scriptName"], f) 
        except :
            m  = "Fatal error: could not find scriptName and/or "
            m += "fullSciptName variables"   
            print m
            raise Exception, m  
            
    def help(self, helpstr=None, printScriptHeader=True):
        """If no parameter is given, prints out help.
        Parameter:
         helpstr: defines the help text. If this has never been 
            defined then the help text is gotten from the first
            statement in the script file (this must be before any
            imports). Default=None
         printScriptHeader: bool to control printing of script name
            and script file name. Default=True.   """
        if helpstr == None: 
            print self._line()
            if printScriptHeader :
                print "Help for running script " + self.scriptnameString()
                print self._line("-")
            print self._list()
            print self._line("-")
            if self.helpStr == None:
                doc    = self.docString
                hasdoc = doc != None
                if hasdoc and len(doc) > 0: print doc
                else:                       print self.defaultHelpStr
            else : print self.helpStr 
            print self._line()
            return
        else : self.helpStr = helpstr     
             
    def processInputParameters(self, inputParams = None) :
        """The dictionary containing the input keyword/values is gotten
        by default from the global variable 'scriptKeyVals'.
        The input keywords are checked against those defined in this class,
        and then the values are checked against the defined characteristics.
        The following actions are taken:
         keyword match: input keywords must match a defined Parameter keyword
         required keywords: all required keywords must be input
         None check: a value of None is only permitted if noneAllowed is
           True for the value.
         type check: if the defined type is None then no checking is done, 
           otherwise the type must match. Exceptions are that an int is 
           accepted for a float and an antlist is either an integer or a list
           of integers.
         type promotion: if the value expects a float any integer input is 
           converted to a float to avoid inadvertent truncation is subsequent
           computations. When an antlist is expected an integer is converted
           into a list containing the integer.
         default values: if a keyword/value is not input and is not required
           then the default value will be supplied.
         help: if the keyword help has a value not equal to False then the 
           help will be printed out followed by a clean exit of the script.
        After successful completion of processing of the input, each defined
        keyword will become an attribute of this class instance, with the
        value set to the input value or default as appropriate. 
        Parameter:
          inputParams: a dictionary of keyword/values (default=None). The
            default will use the dictionary in the global 'scriptKeyVals'. 
        """   
        # Add the input dictionary to the current instance                 
        self.keyVals = inputParams
        if self.keyVals == None: 
            self.keyVals = self.globes.get("scriptKeyVals")
            if self.keyVals == None:           
                m  = "Could not get the variable 'scriptKeyVals' "
                m += "which should be a global variable in subarrayCommands.py"
                raise Exception(m)
        # Check for help, case insensitive; don't check value type
        for k in self.keyVals.keys():
            if k.lower() == "help" and self.keyVals.get(k) <> False: 
                print "Help requested with the input parameters"
                self.help()
                raise ScriptReturn, " "  # graceful exit
        # Check for undefined input keywords
        badKeys = []        
        for k in self.keyVals.keys():
            if not self.has_key(k) : badKeys.append(k)
        nBadKeys = len(badKeys)    
        if nBadKeys > 0:    
            m = "Fatal error: the input parameter%s " %("s"[nBadKeys==1:])
            for b in badKeys:
                m += "'%s'%s " %(b, (","[b==badKeys[nBadKeys-1]:]))
            if nBadKeys == 1: m += "is"
            else :            m += "are"    
            m += " not defined for this script"
            self._reportErrors(m)
        # Check each value for validity
        errors = []        
        for k in self.keyVals.keys():
            v = self.keyVals.get(k)
            p = self.get(k)
            bad, msg = p.value(v)
            if bad: errors.append(msg)
        nErrors = len(errors)
        if nErrors > 0:
            plural = "s"[nErrors==1:]
            m = "Fatal error%s on the following parameter value%s:" \
                     %(plural, plural)         
            for e in errors: m += "\n  " + e
            self._reportErrors(m)
        # Check that all required keywords were input
        missingKeywords = []        
        for v in self.values():
            if v.required and not v.wasInput: missingKeywords.append(v.name)
        nMissing = len(missingKeywords)
        if nMissing > 0:
            plural = "s"[nMissing==1:]
            m = "Fatal error: value%s for the keyword%s" %(plural,plural) 
            comma = ""    
            for k in missingKeywords: 
                m += "%s %s" %(comma, k)
                comma = ","
            if nMissing == 1: m += " is"
            else :            m += " are"
            m += " required but "
            if nMissing == 1: m += " was"
            else :            m += " were"
            m += " not supplied"    
            self._reportErrors(m)
        # Add all keywords to the current namespace
        keys = self.keys()
        keys.sort()
        for k in keys:
            if k == "help": continue
            if hasattr(self, k) :
                m  = "Params class contains a variable with same name "
                m += "as keyword '%s'; you must change the keyword name" %k
                self._reportErrors(m) 
            setattr(self, k, self[k].getValue())
            
    def inputString(self, separator=", ") :
        """Get the input keyword/values as a one line string.
        Parameters:
         separator: put between each keyword/value, default=', ' """
        s = ""
        sep = ""
        for k in self.keyVals.keys():
            s +=sep + k + "=" + str(self.keyVals.get(k))
            sep = separator
        return s
    def inputDict(self):
        """Get a dictionary of the input keywords and values."""
        return self.keyVals   
    def keywordValueDict(self, suppressHelp=True):
        """Get a dictionary of the keywords and final values.
        Parameter:
         suppressHelp: remove help keyword/value, default=True"""
        r = {}
        for k in self.keys():
            if suppressHelp and k == 'help': continue
            r.update({k: self.get(k).getValue()})
        return r
    def keywordValueStringPairs(self, alphabetize=True, suppressHelp=True):
        """Get a list of string pairs, one for each keyword and value,
        alphabetized by keyword
        Parameter:
         alphabetize: order by keyword, default=True
         suppressHelp: remove help keyword/value, default=True"""
        r = []
        keys = self.keys()
        keys.sort()        
        for k in keys:
            if suppressHelp and k == 'help': continue
            r.append([k, self.get(k).getValueStr()])
        return r
    def keywordValueStrings(self, separator=": ", alphabetize=True,
                                                  suppressHelp=True):
        """Get a list of strings, one for each keyword/value
        Parameters:
         separator: string placed between keyword and value,
            default=': '. Another good value would be '=' or ' = '.
         alphabetize: order by keyword, default=True
         suppressHelp: remove help keyword/value, default=True."""
        r = []
        pairs = self.keywordValueStringPairs(alphabetize, suppressHelp)
        for p in pairs:
            r.append(p[0] + separator + p[1])
        return r                  
    def keywordValueString(self, join="=", sep=", ", suppressHelp=True):
        """Get a single string containing all keyword/values
        Parameters:
         join: string placed between keyword and value,
           default='='.
         sep: separator after a value, default=', ', but could
           use something with a newline in it '\n'
         suppressHelp: remove help keyword/value, default=True."""
        r = ""
        after = ""
        for k in self.keys():
            if suppressHelp and k == 'help': continue
            r += after + k + join + self.get(k).getValueStr()
            after = sep
        return r    
    def keywordValueList(self, keywordWidth=16, separator=": ",
                         alphabetize=True, suppressHelp=True):
        """Get a single string containing all keyword/values,
        one line for each key/val.
        Parameters:
         keywordWidth: field width allocated for keyword
         separator: string placed between keyword and value, default=': '.
         alphabetize: order by keyword, default=True
         suppressHelp: remove help keyword/value, default=True."""
        r = ""
        after = ""
        pairs = self.keywordValueStringPairs(alphabetize, suppressHelp)
        for p in pairs:
            r += after + "%*s%s %s" %(keywordWidth, p[0],separator,p[1])
            after = "\n"
        return r        
    def parameterString(self, suppressFilename=False, 
                         keywordWidth=16, separator=": ",
                         alphabetize=True, suppressHelp=True):
        """Get a single string with a header line with the scriptname and
        optional filename. followed by  keyword/values, one per line.
        Parameters:
         suppressFilename: default=False
         keywordWidth: field width allocated for keyword
         separator: string placed between keyword and value, default=': '.
         alphabetize: order by keyword, default=True
         suppressHelp: remove help keyword/value, default=True."""
        return "Script " + \
                self.scriptnameString(suppressFilename=suppressFilename) + \
                " parameters:\n" + \
                self.keywordValueList(keywordWidth=keywordWidth, 
                                      alphabetize=alphabetize,
                                      suppressHelp=suppressHelp)
                  
    def _list(self) :
        output = Value.header() + "\n"
        keys = self.keys()
        keys.remove("help")
        keys.sort()
        keys.append("help")
        for k in keys :
            output += self.get(k).list() + "\n"
        return output                 
    def _line(self, c="=", len=72):
        s = ""
        for i in range(len): s += c
        return s
  
    def _reportErrors(self, msg) :
        """Prints out help, error message and conditional throws exception"""
        self.help()
        print msg
        print self._line("-")
        if not self.inhibitExceptions :
            raise ScriptInputError, msg                               
              
