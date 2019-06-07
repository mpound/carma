import sys
import time
import carma
import carmaIni
import carmaHelp
import carmaHelpers as helper
import Subarray
from subarrayCommands import *

# try for up to 3 seconds to query monitor point
RETRY = 6

# sleep for SLEEPTIME seconds between checks
SLEEPTIME=3

# maximum safe azimuth separation in degrees for an antenna 
# which is stopped versus other that are tracking, IFF 
# the stopped antenna is in an unsafe range.
MAX_SAFE_AZ_SEP = 30.0
#
# Elevation in degrees past which antennas are always safe.
SAFE_ELEVATION_E_CONFIG  = 85.0 

#----------------------------------------------------------------------------
#
# @author Marc Pound
# @version $Id: isItSafe.py,v 1.48 2014/09/14 02:23:58 control Exp $
#----------------------------------------------------------------------------

class IsItSafe:

    def __init__( self, ants=[] ) :
    # Class member variable to of which antennas to ignore
        self._ignoreAnts = helper.makeList(ants)
        # for D10b, only C7 and C13 can collide
        #self._allAnts = [7,13]
        # 'E14a' array collision antennas C7,C8,C9,C10,C13,C14,C15
        self._allAnts = [7,8,9,10,13,14,15]
        # bima antenna numbers from input carma antenna numbers
        self._useAnts = [ x-6 for x in self._allAnts if not x in self._ignoreAnts ]
        # These are antennas that are currently safe
        self._safeAnts   = [7,8,9,10,11,12,13,14,15]
        # These are antennas that are currently unsafe
        self._unsafeAnts = []
        self._errorStr   = ""
        x = time.gmtime()
        self._logfile = "/home/obs/checksafe/checksafe%d-%02d-%02d-%02d%02d.log" % (x.tm_year,x.tm_mon,x.tm_mday,x.tm_hour,x.tm_min)

#        self._logfile = "/tmp/checksafe%d-%02d-%02d-%02d%02d.log" % (x.tm_year,x.tm_mon,x.tm_mday,x.tm_hour,x.tm_min)

        # Dictionaries of monitor points to check and their UNSAFE states

        # stop state monitor point
        self._stopStateMPsDict = { \
           "Bima%d.AntennaCommon.Drive.state" : "STOP" , \
        }


        # Safe state monitor point
        self._safeStateMPsDict = { \
           "Bima%d.AntennaCommon.Drive.safeState" : "UNSAFE" , \
        }

        # Collision detection/collided monitor points
        self._collisionMPsDict = { \
          #  "Bima%d.BimaSpecific.StatusBits.collision" :  True , \
           "Bima%d.BimaSpecific.StatusBits.collisionOff" :  True , \
        }

        # Unsafe Drive.state monitor points values.
        # Note use of list as value for multiple unsafe states.
        # This way, we can associate multiple values with each key.
        # So the key "Bima%d.AntennaCommon.Drive.state" maps to
        # all the dis/allowed values of that monitor point.
        # See python cookbook page 173, section 4.15
        self._disallowedDriveStateMPsDict = { }
        self._disallowedDriveStateMPsDict.setdefault("Bima%d.AntennaCommon.Drive.state", set()).add( "ERROR" ) 
        self._disallowedDriveStateMPsDict.setdefault("Bima%d.AntennaCommon.Drive.state", set()).add( "HWLIMIT" ) 
        self._disallowedDriveStateMPsDict.setdefault("Bima%d.AntennaCommon.Drive.state", set()).add( "SWLIMIT" ) 
        self._disallowedDriveStateMPsDict.setdefault("Bima%d.AntennaCommon.Drive.state", set()).add( "DISABLE" ) 
# LOCAL is OK if also SAFE, which is checked first.
#        self._disallowedDriveStateMPsDict.setdefault("Bima%d.AntennaCommon.Drive.state", set()).add( "LOCAL" ) 

        # Allowed Drive.state monitor point values in combination safeState = UNSAFE
        self._allowedDriveStateMPsDict = { }
        self._allowedDriveStateMPsDict.setdefault("Bima%d.AntennaCommon.Drive.state", set()).add( "TRACK" ) 
        self._allowedDriveStateMPsDict.setdefault("Bima%d.AntennaCommon.Drive.state", set()).add( "SLEW" ) 
        self._allowedDriveStateMPsDict.setdefault("Bima%d.AntennaCommon.Drive.state", set()).add( "CLOSE" ) 
        self._allowedDriveStateMPsDict.setdefault("Bima%d.AntennaCommon.Drive.state", set()).add( "STOW" ) 
        

    def invalidMonitorDataStr( self, a ) :
        """ compose error string upon inability to read valid monitor data """
        antName = "Bima%d" % a
        cname   = "C%d" % ( a+6 )
        msg = "Can't get valid monitor data for %s [%3s]" % ( antName, cname )
        return msg

    def readableTimestamp( self ) :
        """ return a simple, human-readable timestamp """
        x = time.gmtime()
        uttime = "%d-%02d-%02d %02d:%02d:%02d   " % (x.tm_year,x.tm_mon,x.tm_mday,x.tm_hour,x.tm_min,x.tm_sec)
        return uttime

    def printAndLog( self, msg ) :
        """Print message to screen and log it with a timestamp to a file"""
        print msg
        # append message with timestamp to current file 
        f = open(self._logfile,'a')
        f.write(self.readableTimestamp()+msg+'\n')
        f.close()


    def checkSubarray( self ):
        """Throw exception if this is not being run from Science 1 subarray"""
        sano = Subarray.getSubarrayNo()
        if ( sano != 1 ) :
           raise Exception, "checksafe() must be run from Sci#1 subarray"


    def checkAllStop( self ) :
        """If all antennas are in STOP Drive.state, then the array is safe."""
        global RETRY
        self.printAndLog("Checking if all antennas are stopped...")
        for a in self._useAnts :
           for k in self._stopStateMPsDict.keys() :
               try: 
                  mp = str(k % a)
                  v = self._stopStateMPsDict[k] 
                  state = queryString(mp, RETRY)
               except Exception, ex :
                  return False
               if ( state <> v ) :
                  return False

        self.printAndLog("All antennas are stopped; array is safe.")
        return True


    def checkCollisionProtection( self ) :
        """Check antenna collision monitor points for completely
           disallowed states (regardless of safeState value).
        """
        global RETRY
        for a in self._useAnts :
           antName = "Bima%d" % a
           cname   = "C%d" % ( a+6 )
           self.printAndLog("Checking collision protection on %s..." % antName)

           for k in self._collisionMPsDict.keys() :
              try: 
                  mp = str(k % a)
                  v = self._collisionMPsDict[k] 
                  state = queryBool(mp, RETRY)
              except Exception, ex :
                  # Raise an exception immediately on the first 
                  # antenna for which we can't get valid monitor data
                  state = "UNKNOWN"
                  self._errorStr += "%10s [%3s] | %40s | %10s\n" % ( antName, cname, mp, state )
                  self._unsafeAnts.append( a )
                  msg = self.invalidMonitorDataStr( a )
                  self.printAndLog(msg)
                  raise Exception, msg
              if ( state == v ) :
                  if ( state == 1 ) : kstate = "True"
                  else              : kstate = "False"
                  self._errorStr += "%10s [%3s] | %40s | %10s\n" % ( antName, cname, mp, kstate )
                  self._unsafeAnts.append( a )
                  s.comment("UNSAFE OR COLLIDED ANTENNA DETECTED: %s [%3s]" % (antName, cname ) )
                  self.printAndLog("UNSAFE OR COLLIDED ANTENNA DETECTED: %s [%3s]" % (antName, cname ) )
                  raise Exception, "UNSAFE OR COLLIDED ANTENNA DETECTED!"


    # As Dick pointed out, this check is unneeded because these
    # cases are covered in the SAFE && ALLOWED check.  That is,
    # the disallowed states are already excluded from the allowed
    # states list and if they are SAFE && DISALLOWED, that's ok because
    # they are in a safe stow position.
    def checkDisallowedDriveStates( self ) :
        """Check antenna drive.state monitor points for completely
           disallowed states (regardless of safeState value).
        """
        global RETRY
           # check for unsafe drive states regardless of safeState value.
        for a in self._useAnts :
           antName = "Bima%d" % a
           cname   = "C%d" % ( a+6 )
           self.printAndLog("Checking for disallowed drive states on %s..." % antName)
           for k in self._disallowedDriveStateMPsDict.keys() :
              # get the set of values associated with the given key.
              # the key is the monitor point name 
              values = self._disallowedDriveStateMPsDict[k]
              try: 
                 # put the antenna number in the "%d" location of
                 # the key string. then query that monitor point.
                  mp = str(k % a)
                  state = queryString(mp, RETRY)
              except Exception, ex :
                  # Raise an exception immediately on the first 
                  # antenna for which we can't get valid monitor data
                  state = "UNKNOWN"
                  self._errorStr += "%10s [%3s] | %40s | %10s\n" % ( antName, cname, mp, state )
                  self._unsafeAnts.append( a )
                  msg = self.invalidMonitorDataStr( a )
                  self.printAndLog(msg)
                  raise Exception, msg
              for v in values :
                 if ( state == v ) :
                      self._errorStr += "%10s [%3s] | %40s | %10s\n" % ( antName, cname, mp, state )
                      self._unsafeAnts.append( a )
                      msg = "UNSAFE DRIVE STATE DETECTED ON %s [%3s]" % (antName, cname)
                      self.printAndLog(msg)
                      s.comment(msg)
                      raise Exception, "UNSAFE DRIVE STATE DETECTED!"


    def checkSafeStateAndAllowedDriveStates( self ) :
        """Check Antenna.safeState == SAFE or 
           ( Antenna.safeState==UNSAFE && Drive.state is allowed )
        """
        # make list of antennas that are not ignored and
        # not known to be safe.
        checkable = [ ant for ant in self._useAnts if not ant in self._safeAnts]

        for a in checkable :
           antName = "Bima%d" % a
           cname   = "C%d" % ( a+6 )
           self.printAndLog("Checking for safe state and allowed drive states on %s..." % antName)
           for k in self._safeStateMPsDict.keys() :
               try: 
                  mp = str(k % a)
                  badState = self._safeStateMPsDict[k] 
                  #print "1 querying %s..." % mp
                  state = queryString(mp, RETRY)
                  #print "1 DONE querying %s...%s" % ( mp, state )
               except Exception, ex :
                  # Raise an exception immediately on the first 
                  # antenna for which we can't get valid monitor data
                  self.printAndLog("Got exception: %s " % str(ex))
                  state = "UNKNOWN"
                  self._errorStr += "%10s [%3s] | %40s | %10s\n" % ( antName, cname, mp, state )
                  self._unsafeAnts.append( a )
                  msg = self.invalidMonitorDataStr( a )
                  self.printAndLog(msg)
                  raise Exception, msg

               # if we have found antenna in its UNSAFE az,el range
               # check that it is tracking, slewing, close, stowed, which are 
               # all allowable.
               if ( state == badState ) :
                   #print "Entering UNSAFE+TRACK check..."
                   for adsKey in self._allowedDriveStateMPsDict.keys() :
                       try: 
                          # get the set of values associated with the given key.
                          # the key is the monitor point name 
                          values = self._allowedDriveStateMPsDict[adsKey]

                          # put the antenna number in the "%d" location of
                          # the key string. then query that monitor point.
                          kmp = str(adsKey % a)
                          #print "trying %s" % kmp
                          kstate = queryString(kmp, RETRY).strip()
                          #print "got state [%s]" % kstate
                       except Exception, ex :
                          self.printAndLog("Got exception: %s " % str(ex))
                          state = "UNSAFE ANTENNA WITH UNKNOWN TRACK STATE"
                          self._errorStr += "%10s [%3s] | %40s | %10s\n" % ( antName, cname, kmp, state )
                          self._unsafeAnts.append( a )
                          comm = state + " %s [%3s]" % ( antName, cname )
                          self.printAndLog(comm)
                          s.comment( comm )
                          msg = self.invalidMonitorDataStr( a )
                          self.printAndLog(msg)
                          raise Exception, msg

                       ok = False

                       # check that antennas are within 30 deg of each
                       # other in azimuth for any drive state except 
                       # STOW.
                       if ( kstate != "STOW" ) :
                           ok = self.azimuthsOk(antName) 
                       else :
                           for goodDriveState in values :
                              if ( kstate == goodDriveState ) :
                                   ok = True

                       if ( ok == False ) :
                           state = "UNSAFE WITH TRACK STATE %s " % kstate
                           self._errorStr += "%s [%3s] | %s | %s\n" % ( antName, cname, kmp, state )
                           self._unsafeAnts.append( a )
                           comm = "UNSAFE ANTENNA DETECTED: %s [%3s] " % ( antName, cname )
                           self.printAndLog( comm )
                           s.comment( comm )
                           raise Exception, "UNSAFE ANTENNA DETECTED!"



    def checkIgnored( self ) :
         """ Check that ignored antennas are in Eng1 or Maint subarrays """
         self.printAndLog("Verifying that ignored antennas really should be ignored...")
         for a in self._ignoreAnts :
             try:
                 mp = "Control.Antenna%d.subarrayNumber" % a
                 sn = queryInt(mp,RETRY)
             except Exception :
                 # if we can't contact it that's ok, it is being ignored
                 self.printAndLog("Couldn't contact ignored antenna C%d - OK" % a)
             if ( sn == 1 or sn == 2 ) :
                 # if we can contact it make sure it is not in sci#1 or sci#2
                 self._unsafeAnts.append(a)
                 self._errorStr = "Antenna C%d is in a Science subarray." % a
                 raise Exception, "Ignored antenna C%d MUST be put in Engineering or Maintenance array" % a
         self.printAndLog("OK, we can ignore antennas %s." % self._ignoreAnts)
         return


    def azimuthsOk( self, antName ) :
        """ Check that all azimuths are within +/-30 degrees of 
            the given antenna.
            antName - name of the reference antenna, e.g. Bima9
        """
#        print "## Entering azimuthsOk##"
        global MAX_SAFE_AZ_SEP
        mpStr = "Bima%d.AntennaCommon.Drive.Track.actualAzimuth"
        elmpStr = "Bima%d.AntennaCommon.Drive.Track.actualElevation"
        refmpStr = str("%s.AntennaCommon.Drive.Track.actualAzimuth" % antName)

        try :
            refAz = queryDouble( refmpStr )
            # make list of antennas that are not ignored and
            # not known to be safe.
            checkable = [ ant for ant in self._useAnts if not ant in self._safeAnts]
            for a in checkable :
                 kmp = str( mpStr % a )
                 az  = queryDouble(kmp)
                 kmp = str( elmpStr % a )
                 el  = queryDouble(kmp) # this second check should be completely redundant, but just in case
                 if ( math.fabs(refAz - az) >= MAX_SAFE_AZ_SEP and el < SAFE_ELEVATION_E_CONFIG-0.5 ) : # give ourselves a tiny bit of leeway
                    self.printAndLog("WARNING: Bima%d and %s are separated by more than %f degrees Azimuth: %f, %f" % ( a, antName, MAX_SAFE_AZ_SEP, refAz, az ) )
                    return False
#                 else :
#                    print "OK: Bima%d and %s are separated by less than %f degrees Azimuth: %f, %f" % ( a, antName, MAX_SAFE_AZ_SEP, refAz, az )
#
            return True
        except Exception, ex :
            self.printAndLog("got exception: %s" % str(ex))
            return False


    def collectSafeAnts( self ) :
        """Create a list of antennas that are either in a SAFE sate
           or at elevation greater than SAFE_ELEVATION_E_CONFIG
        """
        global  SAFE_ELEVATION_E_CONFIG
        self._safeAnts     = []
        for a in self._useAnts :
           antName = "Bima%d" % a
           mpStr = str("%s.AntennaCommon.Drive.Track.actualElevation" % antName)
           kmpStr = str("%s.AntennaCommon.Drive.safeState" % antName )
           cname = "C%d" % ( a+6 )
           try :
               elev  = queryDouble( mpStr )
               if ( elev >= SAFE_ELEVATION_E_CONFIG ) :
                  self._safeAnts.append( a )
                  continue

               state = queryString(mp, RETRY)
               if ( state == "SAFE" ) :
                  self._safeAnts.append( a )

           except Exception :
           # On exception, don't add an antenna to Safe array but
           # don't complain either--that will be done by subsequent 
           # checks.
               pass
        if ( len(self._safeAnts) > 0 ) :
            self.printAndLog("These antennas are in safe positions: %s." % self._safeAnts)
        else :
            self.printAndLog("No antennas are in safe positions.")

        return


    def checkAntennas( self ) :
        """Check antenna monitor points for unsafe values which
           might indicate a collision.
        """
        global RETRY
        if ( len(self._ignoreAnts) != 0 ) :
            self.printAndLog("Ignoring CARMA antennas %s ." % self._ignoreAnts)
        self.printAndLog("Ignoring OVRO antennas.")
        self.printAndLog("Checking BIMA antennas %s ." % self._useAnts)
        self._safeAnts     = []
        self._unsafeAnts   = []
        self._errorStr = "%12s | %40s | %10s\n" % ( "ANTENNA", "MONITOR POINT", "VALUE")

        # Note: Order of checks here matters.

        #self.checkSubarray() 

        if ( self.checkAllStop() == True ) : 
            return

        if ( len(self._ignoreAnts) != 0 ) :
            self.checkIgnored()

        self.checkCollisionProtection()

        # collect the safe antennas so that we don't compare
        # them in the azimuth check.  Note there still a corner
        # case where an antenna could move out of safe range
        # between this call and the next.
        self.collectSafeAnts()

        self.checkSafeStateAndAllowedDriveStates() 

        if len(self._unsafeAnts) <> 0 :
            # this should never occur, as the exception gets raised on  
            # the first unsafe antenna
            raise Exception, "UNSAFE ANTENNA(S) DETECTED!"
        else :
            timestamp = "[ "+time.asctime( time.gmtime() ) + " UT ] "
            self.printAndLog("%s  All antennas appear safe." % timestamp)


    def deadman( self ) :
         """Main loop for checking antenna safety."""
         global SLEEPTIME
         try:
            while ( True ) :
               # do a double-check as additional transient filtering, 
               # which sleep() also helps with.
               try:
                 self.checkAntennas()
                 sleep(SLEEPTIME)
               except Exception:
                 self.printAndLog("**** WARNING: trying 2nd time **** ")
                 sleep(SLEEPTIME)
                 self.checkAntennas()
         except Exception, ex:
            self.printAndLog(" Cancelling integration")
            cancel()
            self.printAndLog(" Stopping all antennas")
            stop(0)
            alarmon()
            self.printAndLog("*****        Exception caught in checksafe:          *****")
            self.printAndLog(str(ex))
            self.printAndLog("\n***** WARNING: ANTENNA(S) POSSIBLY IN UNSAFE POSITION *****")
            self.printAndLog("%s\n" % self._errorStr)
            foo = self._errorStr.split('\n')
            s.comment("checksafe: %s" % foo[1].strip() )



################################ MAIN #################################


def checksafe(ignoreAnts=[]) :
    """Keep Antennas Safe

    ***This script should be run in a Sci1 sac continuously while we are 
    in E array whether there is an observing program going on or not.***

    This script will keep an eye on 6-meter antennas to see if they have the
    potential to collide.  If so, observing will be stopped and the 
    alarm will be turned on.  A message will be printed to indicate
    where the problem may lie.

    Usage:
       from isItSafe import *
       checksafe( ignoreAnts )

    The parameter ignoreAnts are the CARMA antenna numbers of the 
    collidable 6-meter antennas that should be ignored.  E.g.

       checksafe()          # ignore no collidable antennas
       checksafe(7)         # ignore C7
       checksafe([9,13])    # ignore C9, C13
       
    The default is to use all collidable 6-meter antennas 
    (C7,C8,C9,C10,C11,C13,C15), REGARDLESS OF THEIR SUBARRAY MEMBERSHIP.  
    10-meter antennas are not checked.

    There are two reasons why one might ignore an antenna:
     1) It is out-of-commission with power off to its computer and 
        cannot report its status.   Such an antenna should be moved
        to the maintenance subarray with removeAntenna().
     2) It is not being used but has power and can report its status.
        Such an antenna should be moved to Eng1 subarray and given the 
        safe() command.  After that it may be moved to the maintenance 
        subarray.

    The following monitor points are checked for the indicated value
    that will cause preventative action to be taken.

              Monitor Point                        Action Value
         BimaN.BimaSpecific.StatusBits.collision         True 
         BimaN.BimaSpecific.StatusBits.collisionOff      False

         BimaN.AntennaCommon.Drive.safeState             UNSAFE 
                 AND NOT ONE OF
         BimaN.AntennaCommon.Drive.state                 TRACK
         BimaN.AntennaCommon.Drive.state                 SLEW
         BimaN.AntennaCommon.Drive.state                 CLOSE 
         BimaN.AntennaCommon.Drive.state                 STOW

    That is, if safeState is UNSAFE, then Drive.state is checked for
    the allowed values TRACK, SLEW, CLOSE, STOW.  If Drive.state is not
    one of these and safeState is UNSAFE, then action is taken.  Also,
    if all antennas are in STOP state, then the array is considered safe
    and no action is taken.

    Action means: cancel(), stop(), alarmon().

    The check loop is infinite with a 2 second sleep between checks.
    Transient unsafe conditions are filtered by a double-check if an
    unsafe condition is detected and a two-second sleep between checks.
    Two seconds provides a reasonable filter for transient false collisions,
    while still affording safety.

    If any monitor point cannot be read within 3 seconds, action
    is taken.

    On ^C interrupt, this script will stop antennas and set off the alarm.
    """
    s.comment("Running checksafe.")
    isafe = IsItSafe( ignoreAnts )
    isafe.deadman()


# We don't want to do this with run because
# a) it may screw up SCRIPTSTATE variables
# b) options[whatever] gets passed as a string which does 
#    note get translated correctly by helper.makeList.
#    e.g. [7,8]  winds up as [ '[7,8]' ] rather than
#    a list of antenna numbers
#
#
#options = dict()
#options['ignoreAnts'] = None
#
#if ( scriptOptions != '' and scriptOptions <> None ) :
#    od.readOptions(scriptOptions, options)
#
#print "Got ignoreAnts option = :%s:" % options['ignoreAnts']
#print helper.makeList(options['ignoreAnts'])
#isafe = IsItSafe( options['ignoreAnts'] )
#isafe.deadman()
#
