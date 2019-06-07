# wrappers for project database manager functions
# @author Marc Pound
# $Id: pdbWrappers.py,v 1.152 2014/12/04 21:29:59 control Exp $ 

import carma
import copy
import glob
import math
import os
import string
import sys
import time
import types


import printFunctions as pf
import carmaHelpers as ch
import device as dv
import Subarray
import textwrap as tw

HR2RAD = math.pi/12.0
TWOPI  = 2.0*math.pi
DEG2RAD = math.pi/180.0

# shortcuts for project EditStatus values
EDIT      = carma.observertools.ESTATUS_EDIT
ADD       = carma.observertools.ESTATUS_ADD
DELETE    = carma.observertools.ESTATUS_DELETE
COPY      = carma.observertools.ESTATUS_REPLICATE
RENAME    = carma.observertools.ESTATUS_RENAME
APPEND    = carma.observertools.ESTATUS_APPEND
REPLACE   = carma.observertools.ESTATUS_REPLACE

# shortcuts for ProjectStatus
COMPLETE   = carma.observertools.PSTATUS_COMPLETE
INCOMPLETE = carma.observertools.PSTATUS_INCOMPLETE
RUNNING    = carma.observertools.PSTATUS_RUNNING
OTHER      = carma.observertools.PSTATUS_OTHER

# shortcuts for observation type
SINGLEPOL       = carma.observertools.TYPE_SINGLEPOL
CARMA23         = carma.observertools.TYPE_CARMA23
DUALPOL         = carma.observertools.TYPE_DUALPOL
FULLPOL         = carma.observertools.TYPE_FULLPOL
CARMA15         = carma.observertools.TYPE_CARMA15
CARMA8          = carma.observertools.TYPE_CARMA8
PACS            = carma.observertools.TYPE_PACS
MAXSENS_DUALPOL = carma.observertools.TYPE_MAXSENS_DUALPOL
MAXSENS_CARMA23 = carma.observertools.TYPE_MAXSENS_CARMA23
MAXSENS_LL      = carma.observertools.TYPE_MAXSENS_LL
PACS_DUALPOL    = carma.observertools.TYPE_PACS_DUALPOL
PACS_FULLPOL    = carma.observertools.TYPE_PACS_FULLPOL

# shortcuts for likelihood
LIKELIHOOD_A    = carma.observertools.LIKELIHOOD_A
LIKELIHOOD_B    = carma.observertools.LIKELIHOOD_B
LIKELIHOOD_C    = carma.observertools.LIKELIHOOD_C
LIKELIHOOD_NONE = carma.observertools.LIKELIHOOD_NONE

# missing node is really an empty string
NODE_NOT_FOUND = "NODE_NOT_FOUND"

# dictionaries (maps) between strings and enums above

editStatusDict = dict( edit = EDIT , add = ADD, delete = DELETE, \
                   copy = COPY , rename = RENAME, append = APPEND, \
                   replace = REPLACE )

pStatusDict    = dict( complete = COMPLETE, incomplete = INCOMPLETE, \
                   running = RUNNING, other = OTHER )

# for reverse lookup
revpStatusDict = { COMPLETE : "COMPLETE", INCOMPLETE : "INCOMPLETE", \
                   RUNNING : "RUNNING", OTHER : "OTHER" }


#Subarray handle.
subarray = Subarray.getSubarray()

class TrialDesc :
    """ Short description of a Trial. Suitable for printing."""
    def __init__(self) :
        self._grade       = "none"
        self._obsDate     = "none"
        self._obsDateTime = "none"
        self._opacity     = -999
        self._rmsPhase    = -999
        self._length      = -999
        self._lstStart    = -999  # hours
        self._lstEnd      = -999  # hours
        self._trialNo     = -1
        self._status      = "UNKNOWN"
        self._hasScript   = False
        self._hasCatalog  = True
        self._nAnts       = -999  # number of antennas
        self._comments    = ""    # observer comments

class OBDesc :
    """ Short description of a obsblock. Suitable for printing or sorting."""
    def __init__(self) :
        self._pid           = "none"
        self._project       = "none"
        self._obsblockNo    = -999
        self._receiverBand  = "none" 
        self._flexChar      = "X"
        self._remainingTime = -999
        self._priority = -999
        self._ra       = -999
        self._dec      = -999
        self._lst1     = -999  # hours
        self._lst2     = -999  # hours
        self._status   = "UNKNOWN"
        self._source   = "none"
        self._selfCal  = False     # Is source self-calibratible?
        self._scChar   = "N"       # char rep of Is source self-calibratible?
        self._freq     = -999
        self._allocation = -999
        self._institution = "none"
        self._PI          = "nobody"
        self._PIemail     = "none"
        self._arrayconfig = "X"
        self._trialList   = []
        self._obsType     = "Single Pol"
        self._isKey       = False
        self._keyChar     = "X"
        self._likelihood  = "C"
        self._nObservedTrials = 0 # number of observed trials,
                                  # which may be less than len(trialList)
                                  # since every obsblock is created with
                                  # one default trial
        self._scriptName = "none" # script name following naming convention:
                                  # proposalId_obsblockNoconfig_freqSourcename.obs
                                  # e.g c0105_5B_220GMAur.obs.  
                                  # All special chars, spaces, etc stripped.
                                  # == projid_obsblock.obs

    def printForStatusTable(self) :
        ra = self._ra / HR2RAD
        rahms = ch.convertHmsString(ra,showSeconds=True)
        dec = self._dec / DEG2RAD 
        dechms = ch.convertHmsString(dec,showSeconds=True)
        print "\nObsblock %s\n   Allocated/Remaining hours: %6.2f/%6.2f" % ( self._pid, self._allocation, self._remainingTime ) 
        print "   Obsblock completion status: %s" % ( self._status )
        print "   Source %s  RA: %s  Dec: %s\n   Array Config: %s" % (self._source, rahms, dechms, self._arrayconfig)
        print "   Observation type: %s" % (self._obsType)
        if ( self._lst1 < 0 )  : self._lst1 += 24.0;
        if ( self._lst2 > 24 ) : self._lst2 -= 24.0;
        lstS1 = ch.convertHmsString(self._lst1,showSeconds=False)
        lstS2 = ch.convertHmsString(self._lst2,showSeconds=False)
        print "   Can be observed over LST Range : ( %s, %s )" % ( lstS1, lstS2 )
        print "   Is flex? ", self._flexChar
         
        if ( len(self._trialList) == 1 and self._trialList[0]._obsDate == "none" ) :
            print "   No observations yet"
        else :
            print "Trial    Date      LST_Observed    #Ants  Length    Grade    Opacity    Rms_Phase  Completion_Status"
            for t in self._trialList  :
                if ( t._obsDate == "none" ) : continue
                while ( t._lstStart < 0 )  :
                    t._lstStart += 24.0
                while ( t._lstEnd > 24 )   :
                    t._lstEnd   -= 24.0
                while ( t._lstEnd < 0 )  :
                    t._lstEnd += 24.0
                while ( t._lstStart > 24 )   :
                    t._lstStart   -= 24.0
                lstS1 = ch.convertHmsString(t._lstStart,showSeconds=False)
                lstS2 = ch.convertHmsString(t._lstEnd,showSeconds=False)
                print "  %2d   %8s  (%s, %s)     %d     %.1f     %3s       %4.2f      %4.0f      %s" %\
                ( t._trialNo, t._obsDate, lstS1, lstS2, t._nAnts, t._length, t._grade, t._opacity, t._rmsPhase, t._status )

#        print "\n"

    def printForLstRangeTable(self, showPriority) :
        ra = self._ra / HR2RAD
        rahms = ch.convertHmsString(ra,showSeconds=False)
        if ( self._lst1 < 0 )  : self._lst1 += 24.0;
        if ( self._lst2 > 24 ) : self._lst2 -= 24.0;
        lstS1 = ch.convertHmsString(self._lst1,showSeconds=False)
        lstS2 = ch.convertHmsString(self._lst2,showSeconds=False)
        if ( showPriority == False ) :
            print "%22s    %4s    %6.2f     %s   ( %s, %s )  %s       %s       %s       %s" %\
            ( self._pid,  self._receiverBand, self._remainingTime, rahms, lstS1, lstS2, self._flexChar, self._scChar, self._keyChar, self._obsType )
        else :
            print "%22s    %4s    %6.2f     %s   ( %s, %s )  %s       %s       %s       %s   %d     %s" %\
            ( self._pid,  self._receiverBand, self._remainingTime, rahms, lstS1, lstS2, self._flexChar, self._scChar, self._keyChar, self._obsType , self._priority, self._likelihood )


    def printForDateTable(self) :
        if ( len(self._trialList) == 1 and self._trialList[0]._obsDate == "none" ) :
            return
        else :
            for t in self._trialList  :
                # if ( t._obsDate == "none" ) : continue
                print " %22s  %s   %8s  %4s" % ( self._pid, t._trialNo, t._obsDate, self._receiverBand )

    def printForRunTable( self ) :
         
        if ( len(self._trialList) == 1 and self._trialList[0]._obsDate == "none" ) :
            return;
        else :
            for t in self._trialList  :
                if ( t._obsDate == "none" ) : continue
                while ( t._lstStart < 0 )  :
                    t._lstStart += 24.0
                while ( t._lstEnd > 24 ) :
                    t._lstEnd   -= 24.0
                while ( t._lstEnd < 0 )  :
                    t._lstEnd += 24.0
                while ( t._lstStart > 24 )   :
                    t._lstStart   -= 24.0
                lstS1 = ch.convertHmsString(t._lstStart,showSeconds=False)
                lstS2 = ch.convertHmsString(t._lstEnd,showSeconds=False)
                print "%s   %2d   %s  %8s  (%s, %s)     %d     %.1f     %3s       %4.2f      %4.0f  %s   %s" %\
                ( self._pid, t._trialNo, self._receiverBand, t._obsDate, lstS1, lstS2, t._nAnts, t._length, t._grade, t._opacity, t._rmsPhase, self._PI, self._institution )


    def printForUptimes(self, fakePriority ) :
        """Returns a string with info for generating uptimes plot. """
        ra = self._ra / HR2RAD
        dec = self._dec / DEG2RAD
        rahms = ch.convertHmsString(ra,showSeconds=True)
        decdms = ch.convertHmsString(dec,showSeconds=True)
        color = 0
        if self._receiverBand == "1MM": color = 100
        if self._receiverBand == "3MM": color = 200
        projectcode = self._pid[0:5]
        sourcename = self._pid[6:]
        retStr = "S C %s  %s    %d   %s   %s    %s  x ;%5.1fhr, flx=%s, pri=%d \n" %\
        ( rahms, decdms, color, projectcode, self._receiverBand, sourcename, self._remainingTime, self._flexChar, fakePriority )
        return retStr

    def schedString(self) :
        ra = self._ra / HR2RAD
        rahms = ch.convertHmsString(ra,showSeconds=True)
        dec = self._dec / DEG2RAD 
        dechms = ch.convertHmsString(dec,showSeconds=True)
        if ( self._lst1 < 0 )  : self._lst1 += 24.0;
        if ( self._lst2 > 24 ) : self._lst2 -= 24.0;
        lstS1 = ch.convertHmsString(self._lst1,showSeconds=False)
        lstS2 = ch.convertHmsString(self._lst2,showSeconds=False)
        instCode = self.convertInstToCode()
        # for output as a CARMA table we need to replace spaces in the source name with "_"
        # so that it is not interpreted as an extra column.
        tableSource = self._source.replace(" ","_")
        if ( self._flexChar == "Y") : 
             isFlex = 1
        else : 
             isFlex = 0
        theString = "%s    %s %2i    %6.3f     %24s   %10s  %10s  %2s  %6.2f  %d  %6.2f %8s " %\
        ( self._project,  self._pid, self._obsblockNo, self._freq, tableSource, rahms, dechms, \
          self._arrayconfig, self._remainingTime, isFlex, self._priority, instCode )
        return theString


    def pSched(self) :
        print self.schedString()


    def convertInstToCode(self) :
        if (self._institution == "none" ) : return self._institution
        if (self._institution == "UMD"  ) : return self._institution
        if (self._institution == "UIUC"  ) : return self._institution
        if (self._institution == "UCB"  ) : return self._institution
        if (self._institution == "CARMA"  ) : return self._institution
        if (self._institution == "U Chicago"  ) : return "CHICAGO"
        if (self._institution == "CalTech"  ) : return "CIT"
        if (self._institution == "UC Berkeley"  ) : return "UCB"
        return "Visitor"

#  ----------------------- END OBDESC CLASS ----------------------------

def mythrow(message) :
    """Exception throwing shortcut."""
    raise Exception, message

def timestamp() :
    return "[ "+time.asctime( time.gmtime() ) + " UT ] "

def filestamp() :
    dateComponents = time.localtime()
    stamp = ".%d-%02d-%02d_%02d%02d" % ( dateComponents[0], dateComponents[1], dateComponents[2], dateComponents[3], dateComponents[4] )
    return stamp

def startDate() :
    dateComponents = time.ctime().split()
    stamp = "%s-%s-%s" % ( dateComponents[4], dateComponents[1], dateComponents[2])
    return stamp


def sortOBDescSequence( obseq, key ) :
  """ In place sort the input OBDesc obsblock description sequence 
      by the given key;
      PRIORITY, LST, TIMELEFT, DATE, NONE
      Returns the sorted list or original if key = "none"
  """
  k = key.upper() 
  if ( k == "LST" ) :
      ch.sortByAttrInPlace( obseq, "_ra" )
      return 
  elif ( k == "PRIORITY" ) :
      ch.sortByAttrInPlace( obseq, "_priority" )
      # reverse the sequence because above sorts by ascending priority,
      # i.e. 0 before 4.
      # NOTE: If we redefine priority, remove this reverse()!!
# MWP 10/20/2007 - Bock unwittingly redefined priority with high # = high priority
# so must put this reverse call back in.
      obseq.reverse()

      return 
  elif ( k == "TIMELEFT" ) :
      ch.sortByAttrInPlace( obseq, "_remainingTime" )
      # we want the list in decreasing time remaining.
      obseq.reverse()
      return 
  elif ( k == "NONE" ) :
      return
  elif (k == "DATE") :
      # first must make flesh out the entries
      tobs = {}
      dates = []
      for ob in obseq :
          for tr in ob._trialList :
              t = copy.deepcopy(ob)
              t._trialList = [tr]
              t._nObservedTrials = 1
              dates.append(tr._obsDateTime)
              tobs[tr._obsDateTime] = t
      dates.sort()
      tseq = []
      for d in dates :
          tseq.append(tobs[d])
      obseq[:] = tseq
      return
  else :
      estr = "Unrecognized sort option: %s " % key 
      estr += ". Valid sort options are LST, PRIORITY, TIMELEFT, NONE."
      mythrow(estr)


def getPdb() :
    """Get a handle to the project database manager.
    Eventually this should use a SubarrayControlImpl method
    so that we can do checking if the PDB DO is up and running or
    not?
    """
    return dv.getProjectDbMgr() 


def splitObsblockId( obsblockID ) :
    """ Split the input obsblockID into tokens based with dot (.) as
        the delimiter.  Valid input will be 'project.obsblock' or
        'project.obsblock.subobslock'.  Other inputs will raise
        exception.  A vector with size 3 is always returned:
        [project, obsblock, subobsblock], with subobsblock being an empty
        string if not present in the input.

    obsblockID:  the fully-qualified obsblock ID 
    (project.obsblock[.subobsblock]), without the trial number.  
    
    allowEmptyObsblock:  In most cases we do not allow the obsblock
      to be an empty string. However, for certain project-level edits
      we only want the project portion.  In this case, valid input
      will be 'project.'
    """

    if ( ch.containsWhitespace( obsblockID ) ) :
        mythrow("Obsblock ID must not contain white space characters.")

    obpieces_ = obsblockID.split('.')
    oblen_ = len( obpieces_ ) 
    if ( oblen_ < 2 ) :
         mythrow("Obsblock ID must have at least project and obsblock.")
    elif ( oblen_ == 2 ) : 
#         pf.printInfo( "Got project = %s "  % obpieces_[0] )
#         pf.printInfo( "Got obsblock = %s " % obpieces_[1] )
#         pf.printInfo( "Using empty subobsblock" )
         project_  = obpieces_[0]
         obsblock_ = obpieces_[1]
         if ( project_ == "" or  obsblock_ == "" ) :
             mythrow("Empty project or obsblock not allowed.")
         subobsblock_ = ""
    elif ( oblen_ == 3 ) : 
#         pf.printInfo( "Got project = %s "     % obpieces_[0] )
#         pf.printInfo( "Got obsblock = %s "    % obpieces_[1] )
#         pf.printInfo( "Got subobsblock = %s " % obpieces_[2] )
         project_     = obpieces_[0]
         obsblock_    = obpieces_[1]
         if ( project_ == "" or  obsblock_ == "" ) :
             mythrow("Empty project or obsblock not allowed.")
         subobsblock_ = obpieces_[2]
         if ( subobsblock_ == "" ) :
             mythrow("Extra dot at end of obsblock.");
    elif ( oblen_ >= 4 ) : 
       # If we are here, there are too many sub-parts to the
       # obsblockId string, so raise an exception.
       # If trial appears to be an int, raise the trial number
       # exception message, otherwise raise a generic one. 
         trial_ = obpieces_[oblen_ - 1]
         if trial_.isdigit() :
             estr  = "Trial number not allowed in input obslock ID. "
             estr += "Use the trialNo parameter if you want a specific "
             estr += "trial number."
         else :
             estr = "Too many dots in obsblock ID?"

         mythrow( estr )
    return [ project_, obsblock_, subobsblock_ ]


def getProjectSequence( project ) :
    """ Return the project sequence for a single project.

    project - The project name, e.g. c0091
    Throws Exception if project not in database
    """
    pdb_  = getPdb()
    if ( pdb_ == 0 ) :
       mythrow("Could not contact project database: check that ProjectDataBaseManagerHost is running")

    piv = carma.observertools.ItemValue( "project", project )
    ivSeq = [ piv ]
    try :
        projectSeq = subarray.queryProject( ivSeq )
    except Exception, ex :
        mythrow( "Project query failed because of an exception:\n" + str(ex) )
        return

    if ( len( projectSeq ) == 0 ) :
       # no match in database.
       mythrow("Project " + project +" is not in the project database.")

    return projectSeq


def addScript(obsblockID, script = "none", catalog = "none" ) :
    """ Insert an observing script to the project database.

    This will only add a script to an INCOMPLETE trial, so if the last
    trial is RUNNING it will create a new trial and mark it INCOMPLETE
    and put the script there.
    
    obsblockID:  The fully-qualified obsblockId, without the trial number.  
     
    script:      The input file containing the script.  If it does not begin
                  with '/', then same directories searched by 
                  subarrayCommmands.run() will be searched.
    catalog:     The input file containing the catalog.  If it does not begin
                  with '/', then same directories searched by 
                  subarrayCommmands.run() will be searched.
    """

    [ project_, obsblock_, subobsblock_ ] = splitObsblockId( obsblockID )
    didCatalog = False
    didScript  = False
    sl = script.lower()
    if ( sl == "none" or sl == "" ):
        scriptFullName = "none"
    else :
        scriptFilepieces = script.split('.')
        scriptFullName = ch.getScriptFullPathname(scriptFilepieces[0])
        didScript = True
    cl = catalog.lower()
    if (cl == "none" or cl == "" ):
        catalogFullName = "none"
    else :
        catalogFullName = ch.getCatalogFullPathname(catalog)
        didCatalog = True

    subarray.projectOscriptAdd(project_, obsblock_, subobsblock_, scriptFullName, catalogFullName )
    # if we got here then it was a success?
    str = "Successfully loaded ";
    if ( didScript ) :
       str += "script %s " % scriptFullName
    if ( didCatalog ) :
       if ( didScript ) : str+="and "
       str += "catalog %s " % catalogFullName
    str +=" into project database for %s." % obsblockID
    print str 


def findScript(obsblockID, script="none", catalog="none", trialNo = -1, verbose=True) :
    """
    Find a script in the project database and write it to a file.
     
    obsblockID:  The fully-qualified obsblockId, without the trial number.  
     
    script:      The output file for the script.  If it does not begin 
                 with '/', then file is put in current working directory.
    catalog:     The output file for the catalog.  If it does not begin 
                 with '/', then file is put in current working directory.
    trialNo:     The trial number for which to get the script.
                 Default is -1 which means return last INCOMPLETE or 
                 RUNNING trial number.

    verbose:     If True, report on locating/saving script.
     
    Raises exception if requested script not found or bad input.
    """
    if ( script == "none" and catalog == "none" ) :
        mesg = "One or both of script and catalog must be specified"
        mythrow(mesg)
    [ project_, obsblock_, subobsblock_ ] = splitObsblockId( obsblockID )

    pdb_  = getPdb()
    if( pdb_ == 0 ) :
       mythrow("Could not contact project database: check that ProjectDataBaseManagerHost is running")

    trialStr = str(trialNo)

    if ( trialNo == -1 ) :
       #astr = "pdb_.projectOscriptFind(%s, %s, %s)" % ( project_, obsblock_, subobsblock_)
       #pf.printInfo(astr)
       scriptSeq_ = pdb_.projectOscriptFind(project_, obsblock_, subobsblock_ )
       script_ = scriptSeq_[0]
       catalog_ = scriptSeq_[1]
    else :
      # we have to do a more complicated query!
       piv = carma.observertools.ItemValue( "project", project_ )
       oiv = carma.observertools.ItemValue( "obsblock", obsblock_ )
       siv = carma.observertools.ItemValue( "subObsblock", subobsblock_ )
       tiv = carma.observertools.ItemValue( "trial", trialStr )
       ivSeq = [ piv, oiv, siv, tiv ]

       try:
            projectSeq = subarray.queryProject( ivSeq )
       except carma.observertools.ProjectDatabaseException, pex :
            print "Unable to find script because of an exception from the Project Database: " + str(pex)
            return False
       except Exception, ex :
            print "Unable to find script because: " + str(ex)
            return False

       if ( len ( projectSeq ) == 0 ) :
          # the specific trial does not exist
          script_ = "none"
       else :
# Doug Friedel's comment:
# NOTE that if there is no script the entry may be NODE_NOT_FOUND or "" 
# I can't remember which it returns off the top of my head.
# This should work since the returned ProjectSequence will contain 1 project
# with 1 obsblock, subObsblock and trial (as no other obsblocks, 
# subObsblocks and trial match the query)
          script_ = projectSeq[0].obsblock[0].subObsblock[0].trial[0].script
          catalog_ = projectSeq[0].obsblock[0].subObsblock[0].trial[0].catalog
    
    if ( script_ == "none" or script_ == "" or script_ == NODE_NOT_FOUND ) :
       print "Could not find script for %s. " % obsblockID
       hasScript = False
       if ( trialNo == -1 ) :
           print "You probably need to add the script with addScript() first."
    else :
        hasScript = True
        if ( script != "none" ) :
           if ( verbose ) :
              print( "Found script, saving to file %s" % script )
           fileObject_ = open(script,'w')
           fileObject_.write( script_ )
           fileObject_.close()
    if ( catalog_ == "none" or catalog_ == "" or catalog_ == NODE_NOT_FOUND ) :
        print "Could not find catalog for %s. " % obsblockID
        hasCatalog = False
        if ( trialNo == -1 ) :
           print "You probably need to add the catalog with addScript() first."
    else :
       hasCatalog = True
       if ( catalog != "none" ) :
           if ( verbose ) :
              print( "Found catalog, saving to file %s" % catalog )
           fileObject_ = open(catalog,'w')
           fileObject_.write( catalog_ )
           fileObject_.close()

    return hasScript


def hasScript( obsblockID, trialNo=-1 ) :
    """ Return True if the obsblockID exists and has a script for 
        the given trial number.  False otherwise, including if an exception.
        was raised.

    obsblockID:  the fully-qualified obsblockId, without the trial number.  
     
    trialNo:     the trial number for which to get the script.
                 Default is -1 which means return last INCOMPLETE 
                 or RUNNING trial number.
    """
    try :
        scriptThere = findScript( obsblockID, script="/dev/null", catalog="dev/null", trialNo=trialNo, verbose=False )
    except Exception :
        return False

    return scriptThere


def isCommissioning( project ) :
    """ Wrapper for SubarrayControl::isCommissioning
    project - The project name, e.g. c0091
    """   
    if ( subarray.isCommissioning( project ) == 1 ) :
        return True
# hack for 1cm observing.
    if ( project == "cx281" ) : 
        return True
    if ( project == "cx309" ) :
        return True

    return False


def isValidObsblock( obsblockID ) :
    """ Validate an obsblockID. Check that exists in the project database
        OR is a commissioning project.
  
    obsblockID:  the fully-qualified obsblockId, without the trial number.  
    """
     
    [ project_, obsblock_, subobsblock_ ] = splitObsblockId( obsblockID )

    pdb_  = getPdb()
    if ( pdb_ == 0 ) :
       estr = "Could not contact Project Database: CONDITIONALLY validating obsblock " + obsblockID
       subarray.comment( estr )
       subarray.log( estr )
       print estr
       return True
    config_ = currentConfig(1) + currentConfig(2)
    piv = carma.observertools.ItemValue( "project", project_ )
    oiv = carma.observertools.ItemValue( "obsblock", obsblock_ )
    siv = carma.observertools.ItemValue( "subObsblock", subobsblock_ )
    ivSeq = [ piv, oiv, siv ]
    try:
        projectSeq = subarray.queryProject( ivSeq )
    except carma.observertools.ProjectDatabaseException, pex :
        print "Could not validate obsblock because of an exception from the Project Database: " + str(pex)
        return False
    except Exception, ex :
        print "Could not validate obsblock because: " + str(ex)
        return False

    # if the query turned up no obsblockID, it may be a commissioning
    # project which is allowed to insert obsblock that does not
    # already exist
    if ( len(projectSeq) == 0 ) :
        return isCommissioning( project_ )
    else :
        if ( not( projectSeq[0].obsblock[0].arrayConfiguration in config_ ) and projectSeq[0].obsblock[0].arrayConfiguration != "X" ) :
            print "This obsblock is not for the current array configuration ('%s')." % config_
            return False
        if ( projectSeq[0].obsblock[0].status == COMPLETE ) :
            print "This project has been marked complete and can not be run"
            return False
    
    return True


def validateObsblock( obsblockID ) :
    """ Validate an obslockID. Throw an exeption if invalid.

    obsblockID:  the fully-qualified obsblockId, without the trial number.  

    """
    #print "TRYING TO VALIDATE " +obsblockID
    if (not isValidObsblock( obsblockID ) ) :
       [ project_, obsblock_, subobsblock_ ] = splitObsblockId( obsblockID )
       eStr = "### ERROR: Obsblock %s not found in project database.\n" % obsblockID
       eStr += "Check the spelling, which is case-sensitive. "
       eStr += "You can also try\n projectStatus(\'"+project_+"\')\n"
       eStr += "to list all obsblocks associated with a project.";
       mythrow( eStr )


def requestNewTrial( project, obsblock, subobsblock="",script="",catalog="",isDualCorr=False ) :
    """Request a new trial in the project database.  There are 3 possible 
    scenarios:

    1. If the last trial is marked PSTATUS_COMPLETE, then no action is
    taken in the database. 

    2. If the last trial is marked PSTATUS_INCOMPLETE, the status is changed to
    PSTATUS_RUNNING.

    3. If the last trial is marked PSTATUS_RUNNING, a new trial is added
    (incrementing the trial number), and marked as PSTATUS_RUNNING.

     
    obsblockID:  the fully-qualified obsblockId, without the trial number.  
    """

    #[ project_, obsblock_, subobsblock_ ] = splitObsblockId( obsblockID )

    # first check if this is a commissioning project, which gets
    # special rules.
    isComm = False
    pdb_  = getPdb()
    if ( pdb_ == 0 ) :
       estr = "Could not contact Project Database: using file method to get trial number."
       subarray.comment( estr )
       subarray.log( estr )
       return myIncrementTrial(project, obsblock, subobsblock)

    if ( isCommissioning( project ) ) :
       project = project.lower()
       isComm = True

    try:
        ac1 = subarray.queryString("Control.Subarray1.configName")
        ac2 = subarray.queryString("Control.Subarray2.configName")
        tid = subarray.projectRun(project,obsblock,subobsblock,isComm,
                    isDualCorr,ac1,ac2,script,catalog)
    except carma.util.UserException, ex:
        print 'ERROR:', ex.errorMsg
        mythrow(ex.errorMsg)
    except carma.observertools.ProjectDatabaseException, pex :
        print "New trial number request failed because:\n" + str(pex) 
        print "Using file-based trial increment instead." 
        subarray.log("New trial number request failed because:\n" + str(pex) )
        subarray.log("Using file-based trial increment instead." )
        return myIncrementTrial(project, obsblock, subobsblock)

    # number of trials in project is one greater than index we want to check

    return tid
     

def gradeProject( obsblockID, trialNo, grade, name, comment = "", length = -1.0 ) :
    """ Add observer grade to a project

    obsblockID:  the fully-qualified obsblockId, without the trial number.  
    
    trialNo:     trial number
    
    grade:       String value indicating the overall grade from the
                 observer. (e.g. 'A','C-', etc.). No default.  This string
                 gets converted to a numeric grade inside of the project
                 database manager.  If you wish to insert a particular
                 numeric grade, then place it in quotes, e.g.  '61.3'.

    name:        the name of the person making this edit. No default.
    
    comment:     any comment from observer about grade. Defaults to empty; 
                 above name and current date will be prepended to the comment.

    length:      the length of time the track took, in hours. Default is 
                -1.0 which means the length calculated by quality should be 
                 used.  This parameter is usually used to pass partial tracks.
    """

    [ project_, obsblock_, subobsblock_ ] = splitObsblockId( obsblockID )
    giv = carma.observertools.ItemValue( "obsGrade", grade )
#    theComment = "(" + timestamp()[2:-2] + ")" + name + " : " + comment
#    civ = carma.observertools.ItemValue( "comments", theComment )
    if ( length < 0.0 and length != -1.0 ):
        mythrow( "The length of the trial must be specified" )
    else :
        try:
            ivSeq = []
            if ( length != -1.0 ) :
                liv = carma.observertools.ItemValue( "trialObservationLength", str(length) )
                ivSeq = [ giv, liv ]
            else :
                ivSeq = [ giv ]
            # Note if we keep civ comment in the item value sequence     
            # then all previous comments get overwritten because 
            # of the EDIT tag.  So grade the project and add the         
            # the comment separately.    
            subarray.projectEdit(project_, obsblock_, subobsblock_, trialNo, \
                             ivSeq, EDIT )
            addComments( obsblockID, trialNo, name, comment)

            # get the actual time the observation started
            piv = carma.observertools.ItemValue( "project", project_ )
            oiv = carma.observertools.ItemValue( "obsblock", obsblock_ )
            siv = carma.observertools.ItemValue( "subObsblock", subobsblock_ )
            ivSeq = [ piv, oiv, siv ]

            try :
                projectSeq = subarray.queryProject( ivSeq )
            except carma.observertools.ProjectDatabaseException, pex :
                mythrow( "Project query failed because of an exception from the Project Database:\n" + str(pex) )
            except Exception, ex :
                mythrow( "Project query failed because:\n" + str(ex) )

# Convert trial observation date which has FITS format date (aka ISO8601)
# yyyy-mmm-ddThh:mm:ss.ss to human readable
            date = projectSeq[0].obsblock[0].subObsblock[0].trial[trialNo-1].trialObservationDateStart.split('T')[0]
            idString = "%s.%d" % ( obsblockID,trialNo )
            allCommentsForTrial = getComments(obsblockID,trialNo, False)
            deleteMe = "Trial %d :" % trialNo
            subsIndent = "                                "
            lb = len(subsIndent)+50
            fileH = open('/misc/array/rt/statistics/summary.txt','a')
# for debugging:
#           fileH = open('/tmp/summary.txt','a')
            commentString = "%s   %30s   %4.1f    %2s" % (date,idString,length,grade)
            fileH.write( commentString + '\n')
            fileH.write(subsIndent+"Comments:\n")        
            for c in allCommentsForTrial :       
                cc = c.split('\n')       
                for ccc in cc :          
                    foobar =  tw.wrap(ccc,lb,expand_tabs=False,replace_whitespace=False,subsequent_indent=subsIndent)    
                    for gg in foobar :   
                        ggg=gg.replace(deleteMe,"")      
                        fileH.write(subsIndent+ggg+'\n')         
         
            fileH.write("\n")    
            print "Updated project database and summary.txt for %s " % idString          
         
        except carma.observertools.ProjectDatabaseException, pex :
            mythrow( "Project edit failed because of an exception from the Project Database:\n" + str(pex) )
        except Exception, ex :
            mythrow( "Project edit failed because:\n" + str(ex) )
        except IOError, ioe :    
            print "Updated project database but could not update summary.txt because:\n" + str(ioe)
        except Exception, ex :
            mythrow( "### ERROR: gradeProject edit failed because:\n" + str(ex) )


def getEditStatusEnumFor( action ) :
    """ Returns the matchined EditStatus enumeration value for the
        input string.   This maps user-friendly string to enumeration
        required by the project database edit function.

    action:      Case-insensitive string indicating edit actions.
                 One of 'edit', 'add', 'delete', 'copy', 'rename', 
                 'append', 'replace'
    """
    strLower = action.lower()
    try :
       return editStatusDict[ strLower ]
    except KeyError :
       estr = "No edit action matching '" + strLower + "'."
       estr += " Valid actions are 'edit', 'add', 'delete', 'copy', 'rename', 'append', 'replace'."
       mythrow( estr )


def getProjectStatusEnumFor( status ) :
    """ Returns the matching ProjectStatus enumeration value for the
        input string.   This maps user-friendly string to enumeration
        returned by the project database query function.

    status:      Case-insensitive string indicating project 
                 status.  One of 'complete', 'running', 'incomplete', 
                 'other'.
    """
    strLower = status.lower()
    try :
       return pStatusDict[ strLower ]
    except KeyError :
       estr = "No project status matching '" + strLower + "'."
       estr += " Valid statuses are  'complete', 'running', 'incomplete', 'other'."
       mythrow( estr )
        

def getProjectStatusStringFor( status ) :
    """ Returns a user-friendly Project Status string for the
        input ProjectStatus enumeration. This is the reverse mapping
        of getProjectStatusEnumFor().
        Returns on of 'complete', 'running', 'incomplete', 'other'.

    status:      ProjectStatus enum value.
    """

    try :
       return revpStatusDict[ status ]
    except KeyError :
       estr = str(status) + " is not a valid ProjectStatus."
       mythrow( estr )
      
def editProject( obsblockID, action, items = [], trialNo = -1 ) :
    """ Edit the project in the project database.
        For deletion of obsblock or subobslock, verification is requested
        before proceeding with the delete.  Note deletion of projects and 
        trials is not permitted.

    obsblockID:  The fully-qualified obsblockId, without the trial number.  
                 Note if you want to delete, rename or copy an obsblock, 
                 use 'none' for the subobslock; e.g. 'c0021.ngc123.none'.

    action:      Case-insensitive string indicating edit action.
                 One of 'edit', 'add',  'delete', 'copy', 'rename', 
                 'append', 'replace'


    items:       sequence of carma.observertool.ItemValue pairs giving
                 values to be acted upon. Default is empty sequence.

    trialNo:     Trial number, default is -1 which means not editting at 
                 the trial level.
    """
    msg = "XXXXX"
    eStatus = getEditStatusEnumFor( action )
    nItems = len( items )
    if ( ( nItems == 0 ) and (eStatus != DELETE ) ):
       # is this correct?
       mythrow("Item value sequence is required for action " + eStatus )

    pdb_  = getPdb()
    if ( pdb_ == 0 ) :
       mythrow("Could not contact project database: check that ProjectDataBaseManagerHost is running")
    [ project_, obsblock_, subobsblock_ ] = splitObsblockId( obsblockID )

    if ( obsblock_ == "" )  :    obsblock_    = "none"
    # special cases are DELETE, RENAME, REPLICATE of an obsblock or subobsblock
    if ( ( eStatus == DELETE ) and (nItems == 0 ) ) : 

        if ( trialNo != -1 ) :
             mythrow("Deletion of trials is not allowed.")

        if ( obsblock_ == "none" ) :
             mythrow("Deletion of projects is not allowed.")

        if ( obsblock_ != "none" and subobsblock_ == "none" ) :
             msg = "Delete the obsblock (" + obsblock_ + ") for " + obsblockID + "? [y/n]"
        elif ( obsblock_ != "none" and subobsblock_ != "" ) :
             msg = "Delete the subobsblock (" + subobsblock_ + ") for " + obsblockID + "? [y/n]"
        elif ( obsblock_ != "none" and subobsblock_ == "" ) :
             msg = "Delete the unnamed subobsblock for " + obsblockID + "? [y/n]"
        elif ( obsblock_ != "none" and subobsblock_ != "none" ) :
             msg = "Delete trial number " + str(trialNo) + " for " + obsblockID + "? [y/n]"

        ans = raw_input( msg )
        if  (ans != "Y") & (ans != "y") :
           print "Ok, quitting"
           return

    try :
        subarray.projectEdit( project_, obsblock_, subobsblock_, trialNo, items, eStatus)
    except carma.observertools.ProjectDatabaseException, pex :
        mythrow( "Project edit failed because of an exception from the Project Database:\n" + str(pex) )
    except Exception, ex :
        mythrow( "Project edit failed because:\n" + str(ex) )


def isUncommentableProject( project ) :          
    """Some project codes are not in the project database can't add comments     
    to them with addComments.  This method will filter those out before          
    we even attempt to contact the project database      
         
    project:  The project code   
    """          
    p = project.upper()          
    if ( p == "NONE" or p == "MAINTENANCE" or p == "WEATHER" ) :         
        return True      
         
    return False

def addComments( obsblockID, trialNo, name, comment ) :
    """ Add comments to the existing comments for the given obsblock and trial.

    obsblockID:  The fully-qualified obsblockId, without the trial number.  

    trialNo:     Trial number. Use -1 to add comments to all trials. No default.

    name:        the name of the person making this edit. No default.

    comment:     The comment to add.  The current date will be prepended to 
                 the comment.
    """
    [ project_, obsblock_, subobsblock_ ] = splitObsblockId( obsblockID )
    if ( isUncommentableProject( project_ ) ) :          
        print "Project code %s doesn't allow comments." % project_       
        return
    #piv = carma.observertools.ItemValue( "project", project_ )
    #oiv = carma.observertools.ItemValue( "obsblock", obsblock_ )
    #siv = carma.observertools.ItemValue( "subObsblock", subobsblock_ )
    #ivSeq = [ piv, oiv, siv, ]
    #try :
    #    projectSeq = subarray.queryProject( ivSeq )
    #except carma.observertools.ProjectDatabaseException, pex :
    #    mythrow( "Project query in addComments failed because of an exception from the Project Database:\n" + str(pex) )
    #except Exception, ex :
    #    mythrow( "Project query in addComments failed because:\n" + str(ex) )

    #if ( len( projectSeq ) == 0 ) :
    #   mythrow("ObsblockID "+obsblockID+" is not in the project database.")

    #numTrials = len( projectSeq[0].obsblock[0].subObsblock[0].trial )
    if ( trialNo != -1 ) :
        #if ( ( trialNo - 1 ) > numTrials ) :
        #   eStr = "### ERROR: There is no trial " +str(trialNo) 
        #   eStr += " for " + obsblockID + " in the project database."
        #   mythrow(eStr)
        #prevComment = projectSeq[0].obsblock[0].subObsblock[0].trial[trialNo-1].obsComments
        #if ( prevComment == NODE_NOT_FOUND ) :
        insertComment = timestamp() + name + " : " + comment
        #else :
        #   insertComment = prevComment+"\n" + timestamp() + name + " : " + comment
        ivSeq = [ carma.observertools.ItemValue( "comments", insertComment ) ]
        try:
            subarray.projectEdit(project_, obsblock_, subobsblock_, trialNo, \
                             ivSeq, APPEND)
        except Exception, ex :
            eStr = "### ERROR: Project edit of "+obsblockID + " trial "
            eStr += str(trialNo) + " failed because:\n" + str(ex) 
            mythrow( eStr )
    else :
        piv = carma.observertools.ItemValue( "project", project_ )
        oiv = carma.observertools.ItemValue( "obsblock", obsblock_ )
        siv = carma.observertools.ItemValue( "subObsblock", subobsblock_ )
        ivSeq = [ piv, oiv, siv, ]
        try :
            projectSeq = subarray.queryProject( ivSeq )
        except carma.observertools.ProjectDatabaseException, pex :
            mythrow( "Project query in addComments failed because of an exception from the Project Database:\n" + str(pex) )
        except Exception, ex :
            mythrow( "Project query in addComments failed because:\n" + str(ex) )

        if ( len( projectSeq ) == 0 ) :
            mythrow("ObsblockID "+obsblockID+" is not in the project database.")

        numTrials = len( projectSeq[0].obsblock[0].subObsblock[0].trial )
        for i in range(0,numTrials) :
            #prevComment=projectSeq[0].obsblock[0].subObsblock[0].trial[i].obsComments 
            tno = projectSeq[0].obsblock[0].subObsblock[0].trial[i].trialID
            #if ( prevComment == NODE_NOT_FOUND ) :
            insertComment = timestamp() + name + " : " + comment
            #else :
            #   insertComment = prevComment+"\n" + timestamp() + name + " : " + comment
            ivSeq = [ carma.observertools.ItemValue( "comments", insertComment ) ]
            try:
                subarray.projectEdit(project_, obsblock_, subobsblock_, \
                                     tno, ivSeq, APPEND)
            except Exception, ex :
                eStr = "### ERROR: Project edit of "+obsblockID + " trial "
                eStr += str(tno) + " failed because:\n" + str(ex) 
                mythrow( eStr )
        return

# for those who forget the "s"
def addComment( obsblockID, trialNo, name, comment ) :
    addComments ( obsblockID, trialNo, name, comment )
    
def getComments( obsblockID, trialNo = -1, doprint=True ) :
    """ Print the comments for the given obsblock and trial.

    obsblockID:  The fully-qualified obsblockId, without the trial number.  

    trialNo:     Trial number, default is -1 which means print comments
                 for all trials
                 
    doprint:     Print the comments to the screen, default: True         
         
    Return a list of comments.
    """
    [ project_, obsblock_, subobsblock_ ] = splitObsblockId( obsblockID )
    piv = carma.observertools.ItemValue( "project", project_ )
    oiv = carma.observertools.ItemValue( "obsblock", obsblock_ )
    siv = carma.observertools.ItemValue( "subObsblock", subobsblock_ )
    ivSeq = [ piv, oiv, siv, ]
    try :
        projectSeq = subarray.queryProject( ivSeq )
    except carma.observertools.ProjectDatabaseException, pex :
        mythrow( "Project query failed because of an exception from the Project Database:\n" + str(pex) )
    except Exception, ex :
        mythrow( "Project query failed because:\n" + str(ex) )


    if ( len( projectSeq ) == 0 ) :
       mythrow("ObsblockID "+obsblockID+" is not in the project database.")

    numTrials = len( projectSeq[0].obsblock[0].subObsblock[0].trial )
    comm = []
    if ( trialNo != -1 ) :
        if ( ( trialNo - 1 ) > numTrials ) :
           eStr = "### ERROR: There is no trial " +str(trialNo) 
           eStr += " for " + obsblockID + " in the project database."
           mythrow(eStr)
        theComment = "Trial %d : %s " % ( trialNo , projectSeq[0].obsblock[0].subObsblock[0].trial[trialNo-1].obsComments )
        comm.append( theComment )
    else :
        for i in range(0,numTrials) :
            t=i+1
            theComment = "Trial %d : %s\n" % ( t, projectSeq[0].obsblock[0].subObsblock[0].trial[i].obsComments )
            comm.append( theComment )

    if doprint:          
        for c in comm: print c   
         
    return comm

def replaceComments(obsblockID, trialNo, name, comment) :
    """ Overwrite existing comments for a trial

    obsblockID:  The fully-qualified obsblockId, without the trial number.  

    trialNo:     Trial number. No default.

    name:        the name of the person making this edit. No default.

    comment:     The comment to insert.  The name and current date will
                 be prepended to the comment.

    Throws ProjectDatabaseException if replacement fails.
    """
    [ project_, obsblock_, subobsblock_ ] = splitObsblockId( obsblockID )
    insertComment = name + " : " + timestamp() + comment
    ivSeq = [ carma.observertools.ItemValue("comments",insertComment) ]
    try :
        subarray.projectEdit(project_, obsblock_, subobsblock_, trialNo, ivSeq, REPLACE)
    except carma.observertools.ProjectDatabaseException, pex :
        mythrow( "Project edit failed because of an exception from the Project Database:\n" + str(pex) )
    except Exception, ex :
        mythrow( "Project edit failed because:\n" + str(ex) )


def deleteComments(obsblockID, trialNo) :
    """ Delete existing comments for a trial by replacing them with ""

    obsblockID:  The fully-qualified obsblockId, without the trial number.  

    trialNo:     Trial number. Use -1 to add comments to all trials. No default.

    Throws ProjectDatabaseException if replacement fails.
    """
    [ project_, obsblock_, subobsblock_ ] = splitObsblockId( obsblockID )
    ivSeq = [ carma.observertools.ItemValue("comments","") ]
    try :
        subarray.projectEdit(project_, obsblock_, subobsblock_, trialNo, ivSeq, REPLACE)
    except carma.observertools.ProjectDatabaseException, pex :
        mythrow( "Project edit failed because of an exception from the Project Database:\n" + str(pex) )
    except Exception, ex :
        mythrow( "Project edit failed because:\n" + str(ex) )

def isEmptySubObsblock( so ) :
  """ Return True if the input subObsblock is named "none" or ""
      False otherwise.
  """
  if ( so.subObsblockID != "none" and so.subObsblockID != "" ) :
      return False
  return True


def OBDescSequenceFromProjectSequence( projectSeq, unique=False ) :
    """Create a sequence of OBDesc objects from the input project sequence.

    projectSeq - the input ProjectSequence (returned from a project query).
    """
    nProj = len( projectSeq )
    obseq = []
    pdb_ = getPdb()
    if ( pdb_ == 0 ) :
       mythrow("Could not contact project database: check that ProjectDataBaseManagerHost is running")
    for i in range(0,nProj) :
       p = projectSeq[i]
       #print p.projectID
       numObsblocks = len( p.obsblock )
       for j in range(0, numObsblocks) :
          o = p.obsblock[j]
          #print o.obsblockID
          initialPid = p.projectID + "." + o.obsblockID
          numSubObsblocks = len( o.subObsblock )
          if( unique ) :
              numSubObsblocks = 1
          isFlex = o.isFlex;
          if ( isFlex == 1 ) :
             flexChar = "Y"
          else :
             flexChar = "N"
          for k in range(0, numSubObsblocks) :
               obd = OBDesc()
               obd._project = p.projectID
               obd._isKey = p.isKeyProject
               if(obd._isKey) :
                   obd._keyChar = "Y"
               else :
                   obd._keyChar = "N"
               obd._scriptName = p.projectID + "_" + o.obsblockID + ".obs"
               obd._institution = p.primaryInvestigator.affiliation
               obd._PI = p.primaryInvestigator.name
               obd._PIemail = p.primaryInvestigator.email
               obd._flexChar = flexChar
               obd._obsblockNo = j+1
               so = o.subObsblock[k]
               #print "SID %s" % (so.subObsblockID)
               #print so.documentName
               if ( not isEmptySubObsblock( so ) ) :
                  obd._pid = initialPid + "." + so.subObsblockID 
                  obd._status = getProjectStatusStringFor( so.status ) 
               else :
                  obd._pid = initialPid
                  obd._status = getProjectStatusStringFor( o.status ) 
               obd._allocation  = o.maxAllocatedTime
               if(o.likelihood == LIKELIHOOD_A) :
                   obd._likelihood = "A"
               elif(o.likelihood == LIKELIHOOD_A) :
                   obd._likelihood = "B"
               elif(o.likelihood == LIKELIHOOD_C) :
                   obd._likelihood = "C"
               else :
                   obd._likelihood = "None"
               obd._arrayconfig = o.arrayConfiguration
               numTrials = len( o.subObsblock[k].trial )
               if ( numTrials == 0 ) :
# this can happen for data taken before the pdb was deployed.
#                   print "** Warning: no trials found for %s " % obd._pid
                   continue
               trial = o.subObsblock[k].trial[0]
               xlen = len(trial.source)
               if ( xlen == 0 ) : 
                   print "*** WARNING: Obsblock %s has trial source length: %d " % ( obd._pid, xlen)
                   continue 
               obd._source = trial.source[0].sourceName
               obd._selfCal = trial.source[0].isSelfcalibratable
               if ( obd._selfCal == True ) :
                   obd._scChar = "Y"
               else :
                   obd._scChar = "N"
               ra = trial.source[0].ra
               obd._ra = ra
               obd._dec = trial.source[0].dec
               # reqLow is always negative and in decimal hours
               # so must convert to radians
               #lowHa = o.reqLowHourAngleCoverage*HR2RAD
               obd._lst1= ( o.lowRa ) / HR2RAD
               #obd._lst1= ( ra+lowHa ) / HR2RAD
               # reqHi is always positive in decimal hours
               # so must convert to radians
               #hiHa = o.reqHiHourAngleCoverage*HR2RAD
#               print "lowHA(hr) = %f hiHa(hr) = %f ra(hr) = %f" % ( lowHa/HR2RAD, hiHa/HR2RAD, ra/HR2RAD )
               obd._lst2=( o.highRa) /HR2RAD 
               obd._receiverBand = o.receiverBand
               obd._remainingTime = o.remainingTime
               obd._freq = o.restFrequency
               if(o.observationType == SINGLEPOL) :
                   obd._obsType = "Single Polarization"
               elif(o.observationType == CARMA23) :
                   obd._obsType = "CARMA 23"
               elif(o.observationType == DUALPOL) :
                   obd._obsType = "Dual Polarization"
               elif(o.observationType == FULLPOL) :
                   obd._obsType = "Full Polarization"
               elif(o.observationType == CARMA15) :
                   obd._obsType = "CARMA 15"
               elif(o.observationType == CARMA8) :
                   obd._obsType = "CARMA 8"
               elif(o.observationType == PACS) :
                   obd._obsType = "PACS"
               elif(o.observationType == MAXSENS_DUALPOL) :
                   obd._obsType = "Maximum Sensitivity Dual Polarization"
               elif(o.observationType == MAXSENS_CARMA23) :
                   obd._obsType = "Maximum Sensitivity CARMA 23"
               elif(o.observationType == MAXSENS_LL) :
                   obd._obsType = "Maximum Sensitivity LL"
               elif(o.observationType == PACS_DUALPOL) :
                   obd._obsType = "PACS Dual Polarization"
               elif(o.observationType == PACS_FULLPOL) :
                   obd._obsType = "PACS Full Polarization"
               else :
                   obs._obsType = "None"
               obd._priority = o.priority
               numTrials = len( o.subObsblock[k].trial )
               for ti in range (0, numTrials ) :
                   t     = o.subObsblock[k].trial[ti]
                   if(t.numberOfAntennas == 0) :
                       continue
                   td    = TrialDesc()
                   td._trialNo = t.trialID
# TODO: make this a non-DO call.
                   td._grade = pdb_.gradeToLetter( t.obsGrade )
                   if ( t.trialObservationDateStart != NODE_NOT_FOUND ) :
                       td._obsDate   = t.trialObservationDateStart.split('T')[0]
                       td._obsDateTime = t.trialObservationDateStart
                       obd._nObservedTrials += 1
                   else :
                       td._obsDate   = "none"
                   td._opacity   = t.averageOpacity
                   td._rmsPhase  = t.averagePhase
                   td._nAnts     = t.numberOfAntennas
                   td._length    = t.trialObservationLength
                   td._lstStart  = t.observedLSTstart / HR2RAD
                   td._lstEnd    = t.observedLSTend / HR2RAD
                   td._status    = getProjectStatusStringFor( t.status ) 
                   td._comments  = t.obsComments
                   if ( t.script == NODE_NOT_FOUND or t.script == "none" \
                        or t.script == "NONE" or t.script == "") :
                        td._hasScript = False
                   else :
                        td._hasScript = True 
                   if ( t.catalog == NODE_NOT_FOUND or t.script == "none" \
                        or t.script == "NONE" or t.script == "") :
                        td._hasCatalog = False
                   else :
                        td._hasCatalog = True 
                   obd._trialList.append(td)

               obseq.append(obd)

    return obseq


def listOBRunInfo ( obSeq ) :
    """ Utility method to information about given trials for every obsblock/trial in a
        given sequence of Obsblock Descriptions.  Very similar to printForStatusTable.

    obSeq - the input OBDesc sequence
    """
    print "Project           Trial  Band   Date   LST_Observed    #Ants  Length    Grade    Opacity    Rms_Phase  PI  Affil"
    for ob in obSeq :
        ob.printForRunTable()

def listOBDates( obSeq ) :
    """ Utility method to list date observed for every obsblock/trial in a
        given sequence of Obsblock Descriptions

    obSeq - the input OBDesc sequence
    """
    print "Obsblock               Trial    Date    Band"
    for ob in obSeq :
        ob.printForDateTable()


def listOBDescInfo( obSeq, showPriority ) :
    """ Utility method to list information about every obsblock in a
        given sequence of Obsblock Descriptions (OBDesc class)

    obSeq - the input OBDesc sequence
    """
    print "         ObsblockID       Band    Hrs_Left    RA        LST_Range    Flex?  Selfcal?  Key Prj?  Observation Type"
    for ob in obSeq :
        ob.printForLstRangeTable( showPriority )


def listOBUptimesInfo ( obSeq ) :
    """ Utility method to generate input file for uptimes plot.
        given sequence of Obsblock Descriptions.  Returns info
        as a (long) string.

    obSeq - the input OBDesc sequence
    """
    retstr = "\n"
    fakePriority = 1
    for ob in obSeq :
        retstr+=ob.printForUptimes(fakePriority)
        fakePriority += 1
    return retstr


def sortProjectInfo( projectSeq, sort="priority" ) :
    """ Utility method to sort information about every obsblock in a
        given ProjectSequence according to sort criterion.

    projectSeq - the input ProjectSequence (returned from a project query).

    sort       - How to sort the output. One of "priority", "lst", "timeleft",
                 "date", "none".  Default is "priority," with highest priority
                 listed first.  Case insensitive.
    """
    nProj = len( projectSeq )
    obseq = OBDescSequenceFromProjectSequence( projectSeq )
    nOb  = len( obseq )
    print "# Found a total of %d projects with %d obsblocks matching your query." % ( nProj, nOb )
    k=sort.upper()
    if ( k  == "NONE" ) :
       print "# Unsorted:" 
    else :
       print "# Sorted by %s:" % k
    sortOBDescSequence( obseq, sort )
    return obseq


def listProjectInfo( projectSeq, sort="priority", showPriority=False, maxProj=0 ) :
    """ Utility method to list information about every obsblock in a
        given ProjectSequence.

     projectSeq - the input ProjectSequence (returned from a project query).

     sort      - How to sort the output. One of "priority", "lst", "timeleft",
                 "none".  Default is "priority," with highest priority listed
                 first.  Case insensitive.
     maxProj   - maximum number of projects to list. Default is zero which means list all projects.
    """
    obseq = sortProjectInfo( projectSeq, sort );

    # if maximum projects/obslocks to list is not defaulted to all, 
    # then truncate the sequence at the requested length
    if ( maxProj < len( obseq ) and maxProj > 0  ) :
        print "###  LIMITING DISPLAY TO %d PROJECTS ###" % maxProj
        listOBDescInfo( obseq[0:maxProj], showPriority )
    else :
        listOBDescInfo( obseq, showPriority )



def needGrades(arrayConfig="current") :
    """Print out which projects need quality and/or observer grades.
    Commissioning projects are excluded from the list.

    arrayConfig - Array Configuration. One of 'A', 'B', 'C', 'D', 'E', 
                  'current', or 'any'.  Case insensitive. Default: current

    """
    aiv = carma.observertools.ItemValue("TOR","")
    biv = carma.observertools.ItemValue("obsGrade","0.0,0.0")
    civ = carma.observertools.ItemValue("dqaOverallGrade","0.0,0.0")
    div = carma.observertools.ItemValue("ENDTOR","")
    eiv = carma.observertools.ItemValue("obsblockStatus","INCOMPLETE")
    fiv = carma.observertools.ItemValue("trialStatus","RUNNING")

    
    ivSeq = [ aiv,biv,civ,div,eiv,fiv ]
    # exclude commissioning projects
    commSeq = [getExclusiveCommSequence()]
    for iv in commSeq :
        ivSeq.append( iv )

    ac = arrayConfig.upper();
    if ( ac == "CURRENT") :
         ac = currentConfig().upper()

    if ( ac != "ANY" ) :
        aciv = carma.observertools.ItemValue( "arrayConfiguration", ac )
        ivSeq.append(aciv)

    try :
        #proj = subarray.queryProject( ivSeq )
        pdb = getPdb()
        proj = pdb.projectQuery( ivSeq )        
    except carma.observertools.ProjectDatabaseException, pex :
        mythrow( "Project query failed because of an exception from the Project Database:\n" + str(pex) )
    except Exception, ex :
        mythrow( "Project query failed because:\n" + str(ex) )

    qpnames = []
    opnames = []

    for p in proj :
        for o in p.obsblock :
            for s in o.subObsblock :
                for t in s.trial :
                    if(t.dqaOverallGrade < 0.01) :
                        qpnames.append(t.documentName)
                    if(t.obsGrade < 0.01) :
                        opnames.append(t.documentName)
                    
    qnum = len( qpnames )

    onum = len( opnames )

    if ( qnum == 0 ) :
        print "No projects require quality grades at this time"
    else :
        print "%d trials need quality grades" % (qnum)
        print " The following trials need quality grades"
        for x in qpnames :
            print "  %s" % (x)


    if ( onum == 0 ) :
        print "No projects require observer grades at this time"
    else :
        print "%d trials need observer grades" % (onum)
        print " The following trials need observer grades"
        for x in opnames :
            print "  %s" % (x)

#def whatsBeenRun() :
#    """ Give info about what's been run so far in the current array"""
#    print "### MWP This is a large query...please be patient."
#    ac = currentConfig().upper()
#    if ( ac == "ANY" ) :
#        print "## ERROR - Could not figure out current array configuration."
#        print "## May have been a temporary frame drop...try again "
#        return;
#
#    projectSeq = projectSequenceInRange( 0 , 23.99, 0, True, "ANY" , ac )
#    obSeq = OBDescSequenceFromProjectSequence( projectSeq )
#    listOBRunInfo( obSeq )
#        

def projectStatus( project, brief=False ) :
    """Print out statistics of a given project.

    project - The project name, e.g. c0091
    brief   - True or False to give brief summary or longer table.
    """
    projectSeq = getProjectSequence( project )
    numObsblocks = len( projectSeq[0].obsblock )
    print "Project %s has %d obsblocks " % ( project, numObsblocks )
    print "  PI: %s  email: %s" % (projectSeq[0].primaryInvestigator.name,projectSeq[0].primaryInvestigator.email)
    print "  Project Completion status: %s" % getProjectStatusStringFor( projectSeq[0].status )
    isKey = "No"
    if(projectSeq[0].isKeyProject) :
        isKey = "Yes"
    print "  Key Project: %s" % (isKey)
    obSeq = OBDescSequenceFromProjectSequence( projectSeq )
    totaltrials = 0
    for ob in obSeq :
        if ( brief == False ) :
            ob.printForStatusTable()
        totaltrials += ob._nObservedTrials
    if ( brief == False ) : print " "
    print "  Total number of observed trials in this project: %d" % totaltrials


def getExclusiveCommSequence() :
    """Return an ItemValue sequence that will exclude all 
       commissioning projects from a query.
    """
    notproj = "notProject"
    return carma.observertools.ItemValue(notproj, "commissioning")
   

def projectsWithinRange( lstStart, lstStop, minHours=1.0, flexHA=True, band="any" , arrayConfig="current", sort="priority", showPriority=False, maxProj=0 ) :
    """Print the list of incomplete projects that can be observed 
       over a given LST range.  
       Commissioning projects opnt, rpnt tilt fringe and test
       are excluded from the list.
     
    lstStart  - Start of desired LST range in decimal hours or sexagesimal
                HH:MM(:SS). Note if you use sexagesimal, you have to
                enclose the arguments in quotes, e.g. '20:32'

    lstStop   - End of desired LST range in decimal hours or sexagesimal
                HH:MM(:SS). Note if you use sexagesimal, you have to
                enclose the arguments in quotes, e.g. '20:32'

    minHours  - Only show projects with at least this many hours left to 
                observe. Default 1.

    flexHA    - True or False indicating whether to include sources 
                with 'flexible hour angle' scheduling or not.
                Default: True

    band      - Only return projects for the given Rx band. One of '3mm', 
               '1mm', '1cm', 'any'. Case-insensitive. Default: any

    arrayConfig - Array Configuration. One of 'A', 'B', 'C', 'D', 'E', 
                  'current', or 'any'.  Case insensitive. Default: current

    sort      - How to sort the output. One of 'priority', 'lst', 'timeleft',
                'none'.  Default is 'priority,' with highest priority listed
                first.  Case insensitive.
    maxProj   - maximum number of projects to list. Default is zero which means
                list all projects.
    """
    l1str = str(lstStart)
    if ( l1str.find(':') != -1 ) :
       lstStart = ch.convertHms(lstStart)
    l2str = str(lstStop)
    if ( l2str.find(':') != -1 ) :
       lstStop = ch.convertHms(lstStop)
    p1 = projectSequenceInRange( lstStart, lstStop, minHours, flexHA, band, arrayConfig )

    nProj = len( p1 )
    if ( nProj == 0 ) :
       mythrow("No projects in the project database matched your inputs.")

    listProjectInfo(p1, sort, showPriority, maxProj )


def projectSequenceInRange( lstStart, lstStop, minHours=1.0, flexHA=True, band="any", arrayConfig="current" ) :

    """Return a sequence of incomplete projects that can be observed 
       over a given LST range. 
       Commissioning projects opnt, rpnt tilt fringe and test
       are excluded from the list.
     
    lstStart  - Start of desired LST range in decimal hours 

    lstStop   - End of desired LST range in decimal hours

    minHours  - Only show projects with at least this many hours left to 
                observe. Default 1.

    flexHA    - True or False indicating whether to include sources 
                with 'flexible hour angle' scheduling or not.
                Default: True

    band      - Only return projects for the given Rx band. One of '3mm', 
               '1mm', '1cm', 'any'. Case-insensitive. Default: any

    arrayConfig - Array Configuration. One of 'A', 'B', 'C', 'D', 'E', 
                  'current', or 'any'.  Case insensitive. Default: current

    """

    if ( minHours < 0 ) :
        mythrow("Minimum hours parameter must positive.")

    pdb_ = getPdb()
    oiv  = carma.observertools.ItemValue( "obsblockStatus", "INCOMPLETE" )
    hriv = carma.observertools.ItemValue( "remainingTime", str(minHours) )
    priv = carma.observertools.ItemValue( "priority", "0.1,100000.0")

    rastart = lstStart*HR2RAD
    raend   = lstStop*HR2RAD
    ##raquery = str(rastart) + "," + str(raend)
    ##raq = str(raquery)
    #print "RA start: %f RA end %f " % ( rastart, raend )
    #print "RA query is [srcRA,%s]" % raquery
    ##raiv = carma.observertools.ItemValue( "requestedRaCoverage", raq )

    b = band.upper() 
    if ( b == "ANY" ) :
        print "# Using any Rx band"
        ##ivSeq = [ oiv, hriv, raiv ]
        ivSeq = [ hriv, oiv ]
    else :
        biv  = carma.observertools.ItemValue( "receiverBand", b )
        print "# Using Rx band %s" % b
        ##ivSeq = [ oiv, biv, hriv, raiv ]
        ivSeq = [ hriv, oiv, biv ]

    ac = arrayConfig.upper();
    if ( ac == "CURRENT") :
         ac = currentConfig().upper()
    
    if ( ac == "ANY" ) :
        print "# Using any array configuration"
    else :
        print "# Using %s array configuration" % ac
        aciv = carma.observertools.ItemValue( "arrayConfiguration", ac )
        ivSeq.append( aciv )
      
    # If flexible HA not desired, we must filter it out.
    # By default it comes through.
    if ( not flexHA ) :
         flexiv= carma.observertools.ItemValue( "isFlex", "false") 
         ivSeq.append( flexiv )

    #ignore certain commissioning projects, see bug 559

    ivSeq.append(priv)
    ivSeq.append(getExclusiveCommSequence())

    try :
        projectSeq = subarray.queryProject( ivSeq )
    except carma.observertools.ProjectDatabaseException, pex :
        mythrow( "Project query failed because of an exception from the Project Database:\n" + str(pex) )
    except Exception, ex :
        mythrow( "Project query failed because:\n" + str(ex) )
    #print len(projectSeq)
    for i in range(len(projectSeq)-1,-1,-1) :
        p = projectSeq[i]
        for j in range(len(p.obsblock)-1,-1,-1) :
            o = p.obsblock[j]
            lowRA = o.subObsblock[0].trial[0].source[0].ra + (o.reqLowHourAngleCoverage*HR2RAD)
            hiRA = o.subObsblock[0].trial[0].source[0].ra + (o.reqHiHourAngleCoverage*HR2RAD)
            if(lowRA < 0.0) :
                lowRA += 2*math.pi
            if(hiRA > 2*math.pi) :
                hiRA -= 2*math.pi
            if(not ((lowRA <= raend and lowRA >= rastart) or (hiRA <= raend and hiRA >= rastart) or (rastart <= hiRA and rastart >= lowRA) or ((hiRA < lowRA) and ((rastart <= hiRA) or (raend <= hiRA) or (rastart >= lowRA) or (raend >= lowRA)))) or projectSeq[i].obsblock[j].status == COMPLETE):
                del projectSeq[i].obsblock[j]
        if(len(projectSeq[i].obsblock) == 0):
            del projectSeq[i]
    return projectSeq


def projectsStartingAt( lstStart, minHours=1.0, haLimit = 3.0, band="any", arrayConfig="current", sort="priority", showPriority=False, maxProj=0 ) :
    """Print the list of incomplete projects that can be observed starting
       at a given LST and that have a given amount of time remaining to
       be observed.
       Commissioning projects opnt, rpnt tilt fringe and test
       are excluded from the list."
 
    lstStart  - Start of desired LST range in decimal hours or sexagesimal
                HH:MM(:SS). Note if you use sexagesimal, you have to
                enclose the arguments in quotes, e.g. '20:32'

    minHours - Only show projects with at least this many hours left to 
               observe. Default 1.

    haLimit  - Only show projects with that will be within hour angle
               +/- haLimit at the LST start time. Default 3.

    band      - Only return projects for the given Rx band. One of "3mm", 
               "1mm", "1cm", "any". Case-insensitive. Default: any

    arrayConfig - Array Configuration. One of "A", "B", "C", "D", "E", 
                  "current", or "any".  Case insensitive. Default: current

    sort      - How to sort the output. One of "priority", "lst", "timeleft",
                "none".  Default is "priority," with highest priority listed
                first. Case insensitive.
    maxProj   - maximum number of projects to list. Default is zero which means                 list all projects.
    """

    if ( minHours < 0 ) :
        mythrow("Minimum hours parameter must positive.")

    pdb_ = getPdb()
    oiv  = carma.observertools.ItemValue( "obsblockStatus", "INCOMPLETE" )
    hriv = carma.observertools.ItemValue( "remainingTime", str(minHours) )
    l1str = str(lstStart)
    if ( l1str.find(':') != -1 ) :
       lstStart = ch.convertHms(lstStart)
    rastart = lstStart - haLimit
    if ( rastart < 0 )   : rastart += 24.0
    raend   = lstStart  + haLimit
    if ( raend >= 24.0  ) : raend -= 24.0
#    print "RA start: %f RA end %f " % ( rastart, raend )
    rastart *= HR2RAD
    raend   *= HR2RAD
    ##raquery = str(rastart) + "," + str(raend)
    ##raq = str(raquery)
#    print "RA query is [srcRA,%s]" % raquery
    ##raiv = carma.observertools.ItemValue("requestedRaCoverage",raq)

    b = band.upper() 
    if ( b == "ANY" ) :
        print "# Using any Rx band"
        #ivSeq = [ oiv, hriv, raiv ]
        ivSeq = [ hriv, oiv ]
    else :
        biv  = carma.observertools.ItemValue( "receiverBand", b )
        print "# Using Rx band %s" % b
        #ivSeq = [ oiv, hriv, raiv, biv ]
        ivSeq = [ hriv, oiv, biv ]

    ac = arrayConfig.upper();
    if ( ac == "CURRENT") :
         ac = currentConfig().upper()
    
    if ( ac == "ANY" ) :
        print "# Using any array configuration"
    else :
        print "# Using %s array configuration" % ac
        aciv = carma.observertools.ItemValue( "arrayConfiguration", ac )
        ivSeq.append( aciv )
    #ignore certain commissioning projects, see bug 559
    ivSeq.append(getExclusiveCommSequence())

    try :
        projectSeq = subarray.queryProject( ivSeq )
    except carma.observertools.ProjectDatabaseException, pex :
        mythrow( "Project query failed because of an exception from the Project Database:\n" + str(pex) )
    except Exception, ex :
        mythrow( "Project query failed because:\n" + str(ex) )
# do the RA search here
    for i in range(len(projectSeq)-1,-1,-1) :
        p = projectSeq[i]
        for j in range(len(p.obsblock)-1,-1,-1) :
            o = p.obsblock[j]
            lowRA = o.subObsblock[0].trial[0].source[0].ra + (o.reqLowHourAngleCoverage*HR2RAD)
            hiRA = o.subObsblock[0].trial[0].source[0].ra + (o.reqHiHourAngleCoverage*HR2RAD)
            if(lowRA < 0.0) :
                lowRA += 2*math.pi
            if(hiRA > 2*math.pi) :
                hiRA -= 2*math.pi
            if(not ((lowRA <= raend and lowRA >= rastart) or (hiRA <= raend and hiRA >= rastart) or (rastart <= hiRA and rastart >= lowRA) or ((hiRA < lowRA) and ((rastart <= hiRA) or (raend <= hiRA) or (rastart >= lowRA) or (raend >= lowRA))))):
                del projectSeq[i].obsblock[j]
        if(len(projectSeq[i].obsblock) == 0):
            del projectSeq[i]
    
    nProj = len( projectSeq )
    if ( nProj == 0 ) :
       mythrow("No projects in the project database matched your inputs.")
    listProjectInfo( projectSeq, sort, showPriority, maxProj )
    

def projectsWithinDate( start, end, band="any", arrayConfig="current", excludeCommissioning=True, verbose=False, sort=True) :
    """
    List all projects within a given date range, possibly limiting by
    band and array configuration.  Commissioning projects are ignored.

    start     - start date -- YYYY-MM-DD, e.g 2007-11-12 means Nov 12, 2007. No default.
    end       - end date -- YYYY-MM-DD, as for start.  No default.
    band      - Only return projects for the given Rx band. One of "3mm", 
               "1mm", "1cm", "any". Case-insensitive. Default: any

    arrayConfig - Array Configuration. One of "A", "B", "C", "D", "E", 
                  "current", or "any".  Case insensitive. Default: current
    verbose   -  True/False ,  print out more or less info. Default: False

    """
    daterange = start +","+end
    div = carma.observertools.ItemValue("trialObservationDate",daterange)
    b = band.upper() 
    if ( b == "ANY" ) :
        print "# Using any Rx band"
        ivSeq = [ div ]
    else :
        biv  = carma.observertools.ItemValue( "receiverBand", b )
        print "# Using Rx band %s" % b
        ivSeq = [ div, biv ]

    ac = arrayConfig.upper();
    if ( ac == "CURRENT") :
         ac = currentConfig().upper()
    
    if ( ac == "ANY" ) :
        print "# Using any array configuration"
    else :
        print "# Using %s array configuration" % ac
        aciv = carma.observertools.ItemValue( "arrayConfiguration", ac )
        ivSeq.append( aciv )

    # exclude commissioning projects if requested
    if ( excludeCommissioning ) :
        print "# Excluding commissioning projects"
        commSeq = [getExclusiveCommSequence()]
        for iv in commSeq :
            ivSeq.append( iv )
    else :
        print "# Including commissioning projects"

    try :
        projectSeq = subarray.queryProject( ivSeq )
    except carma.observertools.ProjectDatabaseException, pex :
        mythrow( "Project query failed because of an exception from the Project Database:\n" + str(pex) )
    except Exception, ex :
        mythrow( "Project query failed because:\n" + str(ex) )

    if ( len(projectSeq) ) == 0 :
       print "No obsblocks found between %s" % daterange
       return
    obseq = None
    if(sort) :
        obseq = sortProjectInfo( projectSeq, "date" )
    else :
        obseq = sortProjectInfo( projectSeq, "none" )
    
    if ( verbose ) :
        listOBRunInfo( obseq )
    else :
        listOBDates( obseq )


# stolen from short.py
def myIncrementTrial(projectName,valObsblock,valSubObsblock='',allSubs=False,dualCorr=False) :
    """Usage:   myIncrementTrial(projectName,obsBlock,subObsblock)
    or:    myIncrementTrial(projectName,obsBlock)
    or:    myiilncrementTrial(projectName,obsBlock,allSubs=True)
    Looks for XML files named projectName.obsBlock.subObsblock.trial.xml* in the
    usual directory: if no such file, returns 1, otherwise returns trial+1 to 
    increment trial number.
    Only used if requestNewTrial() fails.
    Set allSubs=True if you want to increment trial number regardless of subObsblock label.
    """

    try : 
        saNo = subarray.getSubarrayNo()
        if(dualCorr) :
            if(valSubObsblock != '') :
                valSubObsblock += "-SL"
            else :
                valSubObsblock = "SL"
    # handle various subObsblock options via a variable label, index for trial location
        if (allSubs):
           subLabel=''
           argTrial=3
        elif (len(valSubObsblock) == 0):
           subLabel=''
           argTrial=2
        else:
           subLabel=valSubObsblock
           argTrial=3

        
        # first make sure the science data directory exists
	hostName = os.uname()[1]
	if ( hostName == 'labacc.ovro.caltech.edu' ) :
		if ( saNo == 2 ) :
		    scienceDir = '/opt/scratch/sdp/astroheader/WBCorrelIntegrated'
		else :
		    scienceDir = '/opt/scratch/sdp/astroheader/SLCorrelIntegrated'
	else:
		if ( saNo == 2 ) :
		    scienceDir = '/opt/sdp/astroheader/WBCorrelIntegrated'
		else :
		    scienceDir = '/opt/sdp/astroheader/SLCorrelIntegrated'
        if ( not os.path.isdir( scienceDir ) ) :
            mythrow("Could not file science data directory "+scienceDir)

        # check that there is a previous trial: if not, we are done
        if (len(glob.glob(scienceDir + '/astrohdr_'+projectName+'.'+valObsblock+'.'+subLabel+'*.xml*')) == 0 ) :
           trial=1
        else:
           # list the files via Peter's favored popen
           flst=os.popen('ls '+ scienceDir + '/astrohdr_'+projectName+'.'+valObsblock+'.'+subLabel+'*.xml*')
           # convert the object to a list
           alst=flst.readlines()
           # now that we have the list, close the pipe as early as possible
           flst.close()
           # need an array to hold existing trial numbers
           nf=len(alst)
           valTrials=list([0])
           # use the list append method: only append values that convert to integers
           # use try to make sure trial converts to a valid integer
           for files in alst:
              try: int(str(files).strip('\n').strip(scienceDir+'/astrohdr_').split('.')[argTrial])
              except ValueError: valTrials.append(0)
              else: valTrials.append(int(str(files).strip('\n').strip(scienceDir+'/astrohdr_').split('.')[argTrial]))
           trial=max(valTrials)+1
        return trial
    except Exception, ex :
        mythrow( "File-based trial number retrieval failed because:\n" + str(ex) )


def getInvestigatorEmailAddresses( project ) :
    """ Return all investigator email addresses for a given project.
    
    project - The project name, e.g. c0091
    """

    ps = getProjectSequence( project );
    invSeq = [ ps[0].primaryInvestigator, ps[0].coInvestigator ]
    emailSeq = []
    for i in range (0, ps[0].numberOfInvestigators ) :
        emailSeq.append( invSeq[i].email )

    return emailSeq


def schedule(arrayConfig="current", band="3mm", days=7, fixed="none", minHours=1, mode="priority", run=True, pivot=0.0, startLST = 0, outdir="/home/obs/web_pages/schedule", sunLimit=0) :
    """Return a list suitable for input into the scheduling program 
    for all projects for which there is still observing time left. If 
    requested, run the scheduling program to create the actual schedule.
    Commissioning projects opnt, rpnt tilt fringe and test are excluded from 
    the list.

    The parameters marked with a * map directly to inputs to mksched. Type
    'mksched --desc' or 'mksched --key' for more info on that program.

    Parameters:

    arrayConfig * - Array Configuration. One of "A", "B", "C", "D", "E".
                  Note "any" is NOT an option here. Default is current configuration.

    band  *     - Only return projects for the given Rx band. One of "3mm", 
                  "1mm", "1cm". Case-insensitive. Default: 3mm

    days  *     - The maximum number of days to schedule, 0 means no limit. Default: 7

    fixed  *    - File name for fixed blocks from say a previous run.  These will be 
                  added to the schedule BEFORE any new blocks are scheduled. 
                  Default=none which means no fixed blocks.

    minHours    - Only show projects with at least this many hours left to 
                  observe. Default 1.

    mode   *    - Case-insensitive string indicating mode for scheduling 
                  tracks: priority or compact

    run         - Run the mksched program on the output file. Default True

    pivot  *    - Grade below which all projects are scheduled in COMPACT mode.
                  Default 0.
    startLST * -  Local sidereal time at which to start the schedule.  This 
                  is useful if you want to start the schedule at a time you
                  know a previous script is ending.
    outdir     -  output directory. Default is /home/obs/web_pages/schedule.
    sunLimit * - The sun-source angle limit, in degrees. Default: 30 degrees for SL and SH configurations, zero otherwise
    """



    if ( band.upper() == "ANY") :
        mythrow("## ERROR - band must one of 3mm, 1mm or 1cm")

    if ( startLST < 0 or startLST >= 24 ) :
        mythrow( "## ERROR - startLST must be in range 0 to 24 hrs." )

    ac = arrayConfig.upper();
    if ( ac == "CURRENT") :
         ac = currentConfig().upper()
    
    if ( ac == "ANY" ) :
        print "## ERROR - Could not figure out current array configuration."
        print " Try re-running schedule with the arrayConfig=<the letter of the current configuration>."
        return;

    # set sun limit based on array configuration
    if ( ac == "SL" or ac == "SH" ) : 
          slimit = max(sunLimit,30)
    else:
          slimit = sunLimit

    projectSeq = projectSequenceInRange( 0.01, 23.99, minHours, True, band, ac )
    obSeq = OBDescSequenceFromProjectSequence( projectSeq,True )
    fstamp = filestamp()
    tstamp = timestamp()
    # kluge. Assume we don't do ABCDE in Sci2
    if ( ac == "SH" or ac == "SL" ) :
         sci = "Sci2"
    else :
         sci = "Sci1"
    extension = band+"_"+sci+fstamp
    fileName    = outdir+"/mksched.in_"+extension
    fileObject_ = open(fileName,'w')
    if ( run ) : tStr = "True"
    else       : tStr = "False"

    # write the header
    fileObject_.write("# mksched input file generated by project database on %s\n" % tstamp )
    fileObject_.write("# with schedule(%s,%.1f,%s,%d,%s,%s,%s,%.2f,%.2f,%.2f)\n#\n#\n" % (arrayConfig, minHours, band, days, tStr, fixed, mode, pivot,startLST,slimit))
    fileObject_.write("#|  project_name    | obsblockId     |  obsblock       | Freq  | source_name          | RA        | Dec   | configuration | TAC_allocation | Flex_HA | numeric_grade  | Institution \n")
    fileObject_.write("#|     s            |      s         | i               | GHz   | s                    | hms       | dms   | s             | r              | i       | r              | s           \n")
    fileObject_.write("#|                  |                |                 |       |                      |           |       |               |                |         |                |             \n")

    for ob in obSeq :
        schedLine = ob.schedString() + "\n"
        fileObject_.write( schedLine )
    fileObject_.close()
    print "mksched INPUT file written to %s." % fileName
    if ( run ) :
        mout = outdir+ "/mksched.calendar_"+extension
        fout = fileName + ".fixed_blocks"
        print "Running mksched..."
        sdate=startDate()
        queueFile = outdir+"/queue."+sci+".out"
        runStr = "/opt/rt/bin/mksched file=%s band=%s days=%d config=%s start=%s fixed=%s pivot=%f mode=%s startLST=%f verbose=0 subarray=%s queue=%s sunLimit=%f > %s" % ( fileName, band, days, ac , sdate, fixed, pivot, mode, startLST, sci, queueFile, slimit, mout )
        print runStr 
        status = os.system( runStr )
        if ( status != 0 ) :
           errorStr = "Problem executing command: " + runStr
           mythrow( errorStr )
        print "mksched OUTPUT CALENDAR written to %s." % mout
        print "mksched OUTPUT QUEUE written to %s." % queueFile
        print "mksched OUTPUT LIST written to %s." % fout

        print "Moving symbolic links..."
        blower = band.lower()+"_"+sci

        moveStr = "/bin/ln -sf "+mout+" "+outdir+"/calendar."+blower
        status = os.system( moveStr )
        if ( status != 0 ) :
           errorStr = "Problem executing command: " + moveStr 
           mythrow( errorStr )

        moveStr = "/bin/ln -sf "+queueFile+" "+outdir+"/queue."+blower
        status = os.system( moveStr )
        if ( status != 0 ) :
           errorStr = "Problem executing command: " + moveStr 
           mythrow( errorStr )

        moveStr = "/bin/ln -sf "+fout+" "+outdir+"/list."+blower
        status = os.system( moveStr )
        if ( status != 0 ) :
           errorStr = "Problem executing command: " + moveStr 
           mythrow( errorStr )

        moveStr = "/bin/ln -sf "+fileName+" "+outdir+"/input."+blower
        status = os.system( moveStr )
        if ( status != 0 ) :
           errorStr = "Problem executing command: " + moveStr 
           mythrow( errorStr )


def schedule3mm( startLST=0, fixed="/array/rt/scripts/fixedblocks", days=7, sunLimit=0 ) :
    """ Create 5-day schedule for the 3mm projects in both subarrays.
    startLST  -  Local sidereal time at which to start the schedule.  This 
                  is useful if you want to start the schedule at a time you
                  know a previous script is ending.
    fixed      - File name for fixed blocks from say a previous run.  These will be 
                  added to the schedule BEFORE any new blocks are scheduled. 
                  Default=/array/rt/scripts/fixedblocks which normally will include
                  blocks for maintenance, etc.
    days       - The maximum number of days to schedule, 0 means no limit. Default: 7
    sunLimit   - The sun-source angle limit, in degrees. Default: 30 degrees for SL and SH configurations, zero otherwise
    """
    print "Scheduling 3mm projects for Sci1 and Sci2"

    aconfig = currentConfig(1)
    bconfig = currentConfig(2)

    if ( aconfig== "SL" or aconfig == "SH" ) : 
          slimit = max(sunLimit,30)
    else :
          slimit = sunLimit 
    schedule( band="3mm", arrayConfig=aconfig, startLST=startLST, fixed=fixed, days=days, sunLimit=slimit )

    if ( bconfig== "SL" or bconfig == "SH" ) : 
          slimit = max(sunLimit,30)
    schedule( band="3mm", arrayConfig=bconfig, startLST=startLST, fixed=fixed, days=days, sunLimit=slimit )


def schedule1mm( startLST=0 , fixed="/array/rt/scripts/fixedblocks", days=7, sunLimit=0) :
    """ Create 5-day schedule for the 1mm projects
    startLST  -  Local sidereal time at which to start the schedule.  This 
                  is useful if you want to start the schedule at a time you
                  know a previous script is ending.
    fixed      - File name for fixed blocks from say a previous run.  These will be 
                  added to the schedule BEFORE any new blocks are scheduled. 
                  Default=/array/rt/scripts/fixedblocks which normally will include
                  blocks for maintenance, etc.
    days       - The maximum number of days to schedule, 0 means no limit. Default: 7
    sunLimit   - The sun-source angle limit, in degrees.  Default: 0
    """
    print "Scheduling 1mm projects for Sci1"
    schedule( band="1mm", arrayConfig=currentConfig(1), startLST=startLST,  fixed=fixed, days=days, sunLimit=sunLimit )

def schedule1cm( startLST=0 , fixed="/array/rt/scripts/fixedblocks", days=7, sunLimit=25) :
    """ Create 5-day schedule for the 1cm projects in Sci2 only.
    startLST  -  Local sidereal time at which to start the schedule.  This 
                  is useful if you want to start the schedule at a time you
                  know a previous script is ending.
    fixed      - File name for fixed blocks from say a previous run.  These will be 
                  added to the schedule BEFORE any new blocks are scheduled. 
                  Default=/array/rt/scripts/fixedblocks which normally will include
                  blocks for maintenance, etc.
    days       - The maximum number of days to schedule, 0 means no limit. Default: 7
    sunLimit   - The sun-source angle limit, in degrees. Default: 30 degrees for SL and SH configurations, zero otherwise
    """
    print "Scheduling 1cm projects for Sci2"

    aconfig = currentConfig(2)
    if ( aconfig== "SL" or aconfig == "SH" ) : 
          slimit = max(sunLimit,30)
    else :
          slimit = sunLimit 
    schedule( band="1cm", arrayConfig=aconfig, startLST=startLST,  fixed=fixed, days=days, sunLimit=slimit )


def scheduleAll() :
    """ Create 7-day schedule for the 1cm, 1mm, and 3mm projects in both subarrays."""
    schedule3mm()
    schedule1mm()
    schedule1cm()


def currentConfig(subarrayNo = 0) :
    """ Return the current array configuration string. This is
        done by querying configName monitor point and returning the
        first character, since the project database only allows
        A,B,C,D,E,X and configName might be e.g. B07a.  If there
        is any error during the query, the string 'any' will be
        returned. 
        Parameter: subarrayNo - subarray number.  Default: zero which 
        means whatever subarray you have called this command from.
    """
    if( subarrayNo == 0 ) :
            saNo = subarray.getSubarrayNo()
    else :
            saNo = subarrayNo
    try :
        mpName = "Control.Subarray%d.configName" % saNo
        config_ = subarray.queryString( mpName )
        if ( saNo == 1 ) :
            # For subarray1, just return the first letter.
            return config_[0]
        else :
            # For subarray2 the configuration could be "SH" so return
            # entire string.
            return config_
    except Exception:
        return "any"
    

def invokeUptimes() :
    """ Invoke IDL to create uptimes plot from data previously generated 
    """

    idlStr = "/usr/local/bin/idl < /array/rt/uptimes/uptimes_idl >&/dev/null"
    #idlStr = "/usr/local/bin/idl < /array/rt/uptimes/uptimes_idl"
    status = os.system( idlStr )
    if ( status != 0 ) :
      mythrow("Problem running command: "+idlStr)

    # if IDL was successful move the images to the web page
    indir = "/misc/array/rt/uptimes/"
    outdir = "/home/obs/web_pages/schedule/"
    images = [ "uptimes_1mm.png", "uptimes_3mm.png", "uptimes.png" ]
    for img in images :
        moveStr = "mv %s%s %s%s" % ( indir,img,outdir,img )
        status = os.system( moveStr )
        if ( status != 0 ) :
          mythrow("Problem running command: "+moveStr)


def uptimes( band="any" ) :
    """ Generate data for the uptimes plot, using the top 20 projects.  Writes data to 
        /array/rt/uptimes/uptimes.dat and invokes IDL on it.
        Parameters:
          band   - Only return projects for the given Rx band. One of "3mm", 
                  "1mm", "1cm", "any". Case-insensitive. Default: any which means
                   create uptimes for both 3mm and 1mm
    """

    pdb_=getPdb()
    if ( pdb_ == 0 ) :
       mythrow("Could not contact project database: check that ProjectDataBaseManagerHost is running")

    b = band.upper() 
    if ( b == "ANY" ) :
        doAll = True
        mybands = [ "1MM", "3MM" ]
    else :
        doAll = False
        mybands = [ b ]

    ac = currentConfig().upper()
    if ( ac == "ANY" ) :
        print "## ERROR - Could not figure out current array configuration."
        print "## May have been a temporary frame drop...try again."
        return;

    didAllHeader = False
    for theBand in mybands :
        # first copy the header 
        fname = '/array/rt/uptimes/uptimes_'+theBand.lower()+'.dat'
        cpStr =  "cp /opt/rt/conf/data/uptimes_head.dat "+fname
        status = os.system( cpStr )
        if ( status != 0 ) :
            mythrow("Problem copying header file with: "+cpStr )

        if ( doAll and not didAllHeader ) :
            allName = '/array/rt/uptimes/uptimes.dat'
            Allfile = open( allName, 'a')
            cpStr =  "cp /opt/rt/conf/data/uptimes_head.dat "+allName
            status = os.system( cpStr )
            if ( status != 0 ) :
                mythrow("Problem copying header file with: "+cpStr )
            didAllHeader=True


        bandiv  = carma.observertools.ItemValue( "receiverBand", theBand  )
        aiv = carma.observertools.ItemValue("arrayConfiguration", ac )
        biv = carma.observertools.ItemValue("obsblockStatus","INCOMPLETE")

        ivSeq = [ bandiv, aiv,biv,getExclusiveCommSequence() ]
        try :
            projectSeq = subarray.queryProject( ivSeq )
        except carma.observertools.ProjectDatabaseException, pex :
            mythrow( "Project query failed because of an exception from the Project Database:\n" + str(pex) )
        except Exception, ex :
            mythrow( "Project query failed because:\n" + str(ex) )

        obseq = sortProjectInfo( projectSeq,"priority")
        # list only the top 20 projects
        output = listOBUptimesInfo( obseq[0:40] )
        Ufile = open( fname,'a')
        Ufile.write(output)
        Ufile.close();
        if ( doAll ) :
              Allfile = open( allName, 'a' )
              print " appending " +theBand+ " to "+allName
              Allfile.write(output)
              Allfile.close();

        print "File written for band %s to %s." % (theBand , fname)

    print "Invoking IDL..."
    invokeUptimes()
    print "Done."



def gsu() :
   """Run schedule3mm, schedule1mm, and uptimes."""
   schedule3mm()
   schedule1mm()
   uptimes()

def printProjectSequence(pseq):
    numProjects = 0
    numObsblocks = 0
    numSubObsblocks = 0
    numTrials = 0

    for project in pseq:
        numProjects += 1
        print '%s' % project.projectID
        for obsblock in project.obsblock:
            numObsblocks += 1
            print '  %s' % obsblock.documentName
            for subobsblock in obsblock.subObsblock:
                numSubObsblocks += 1
                print '    %s' % subobsblock.documentName
                for trial in subobsblock.trial:
                    numTrials += 1
                    print '      %s' % trial.documentName

    print
    print 'Totals:'
    print '-------'
    print 'projects     - %d' % numProjects
    print 'obsblocks    - %d' % numObsblocks
    print 'subobsblocks - %d' % numSubObsblocks
    print 'trials       - %d' % numTrials

#------------------   TEST MACROS ---------------------------

def testProject( text ) :
    """Test of pdb.

       text - the input
    """
    pdb_  = getPdb()
    output = pdb_.projectTest(text)
    return output


def execdb(com, db):
    """ Execute command in Carpenter's database """
    # Execute command
    os.environ['PGUSER']     = 'obs'
    os.environ['PGPASSWORD'] = 'amrac15'
    os.environ['PGHOST']     = 'cedarflat'
    command = '/array/utilities/jmc/db/pgsql/bin/psql ' + db + ' -t -c "' + com + '"'
    fout = os.popen(command)
    result = fout.read()

    # Split result by end of line character
    rows = result.split("\n")

    # Split lines
    tokens = dict()
    for j in range(len(rows)):
        # Remove blank lines
        r = rows[j]
        if string.strip(r) == "": continue

        # Get columns
        columns = r.split("|")

        # Trim each entry
        for i in range(len(columns)):
            columns[i] = string.strip(columns[i])

        # Save
        tokens[j] = columns[:]
    
    # Done
    return copy.copy(tokens)
