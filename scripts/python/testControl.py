import sys
import carmaIni
import carmaHelp
from subarrayCommands import *
import Subarray
import obsdef3 as od

#----------------------------------------------------------------------------
# This is an integration test suite for the control system.
# It attempts to run various control commands and verify that
# they correctly completed.  It can be run either on the actual
# array or in your sandbox if you have a local CARMA system running.
# If using a local sandbox, you pass simulation=True to fullControlTest().
# This will cause certain tests to avoid situations where the lack
# of hardware would cause the test to fail.  For instance, if simulating,
# qmove()  is called instead of move().
#
# More tests can be added to this test suite by defining the individual
# test in a python method and then calling that test from fullControlTest().
# Be sure to increment the class member variables according to test success
# or failure.
#
# @author Marc Pound
# @version $Revision: 1.23 $
#----------------------------------------------------------------------------

class ControlTester :

    def __init__(self) :
    # Class member variables to give us statistics at the end
    # number of passing/failing tests and names of
    # failing tests
        self._numTestsPassed = 0
        self._numTestsFailed = 0
        self._passingTests = []
        self._failingTests = []
        self._simulation = False

    def reinit(self) :
        import subarrayControl 
        subarrayControl.initialize()

    def myprintf(self,format, *args ) :
        sys.stdout.write(format % args)
        sys.stdout.flush()

    def dot(self) :
        self.myprintf("%s",".")
     
    def passAntennas(self) :
         """Pass antennas back and forth between subarrays"""
         sleeptime = 2.0
         nbounces  = 25
         totaltime = sleeptime * nbounces * 3
         self.myprintf ("Doing %i add/remove antennas on Sci1 and Eng1. This will take about %.0f seconds", nbounces, totaltime )
         try:
            sci1 = Subarray.getSubarrayRef(1)
            eng1 = Subarray.getSubarrayRef(3)

            a = sci1.getAntennaAssignments()
            ants = []
            for i in a:
                num   = i.carmaAntennaNo
                ants.append(num)

            for i in range(25) :
               sci1.removeAntenna([0],True)
               self.dot()
               sleep(sleeptime)
               eng1.addAntenna(ants,False)
               self.dot()
               sleep(sleeptime)
               eng1.removeAntenna([0],True)
               self.dot()
               sleep(sleeptime)
               sci1.addAntenna(ants,False)
               self.dot()

            sleep(sleeptime)
            self.dot()
            a = sci1.getAntennaAssignments()
            ants2 = []
            for i in a:
                num   = i.carmaAntennaNo
                ants2.append(num)
            if ants != ants2  :
                print "Initial and final antennas don't match."
                self._failingTests.append("Matching Antennas")
                self._numTestsFailed+=1
            else :
                self._passingTests.append("Matching Antennas")
                self._numTestsPassed+=1

            print "Add/Remove antennas Test succeeded."
            self._passingTests.append("add/removeAntennas")
            self._numTestsPassed+=1
         except Exception, ex:
            print "Add/Remove antennas Test failed on exception %s", ex
            self._failingTests.append("add/removeAntennas [exception]")
            self._numTestsFailed+=1

    def noiseTest(self) :
        """Turn noise source on/off. Check monitor points"""
        print "Testing noise source commands..."
        try:
             s.noiseSource(True,False)
             sleep(1)
             state = queryBool('sldc.noisesourcecontainer.noisesource.noiseenabled', retries=1)
             if state == False :
                print "Noise source did not turn ON"
                self._failingTests.append("Noise Source ON state")
                self._numTestsFailed+=1
             else :
                print "Noise source turned ON ok"
                self._passingTests.append("Noise Source ON state")
                self._numTestsPassed+=1


             s.noiseSource(False,False)
             sleep(1)
             state = queryBool('sldc.noisesourcecontainer.noisesource.noiseenabled', retries=1)
             if state == True :
                print "Noise source did not turn OFF"
                self._failingTests.append("Noise Source OFF state")
                self._numTestsFailed+=1
             else : 
                print "Noise source turned OFF ok"
                self._passingTests.append("Noise Source OFF state")
                self._numTestsPassed+=1

             print "Noise Source Test Succeeded."
             self._passingTests.append("Noise Source")
             self._numTestsPassed+=1

        except Exception, ex:
          print "Noise Source Test failed on exception %s", ex
          self._failingTests.append("Noise Source [exception]" )
          self._numTestsFailed+=1

    def pswitchTest(self) :
        """Test phase switch state"""
        # turn them all off
        print "Testing phase switch state."
        try :
            s.phaseSwitching(False, 0)
            sleep(1)
            lrbad=False
            for i in range(24) :
               queryVal = "Loberotator.Channel%i.phaseSwitch" % (i+1)
               state = queryInt( queryVal ) 
               if state != 1 :
                  print "Phase switching for channel %i did not turn OFF" % (i+1)
                  lrbad = True
            #turn them all back on
            s.phaseSwitching(True, 0)
            sleep(1)
            for i in range(24) :
               queryVal = "Loberotator.Channel%i.phaseSwitch" % (i+1)
               state = queryInt( queryVal ) 
               if state != 0 :
                  print "Phase switching for channel %i did not turn ON" % (i+1)
                  lrbad = True

            if lrbad :
               self._failingTests.append("phaseSwitch ON|OFF state")
               self._numTestsFailed+=1
            else :
               self._passingTests.append("phaseSwitch ON|OFF state")
               self._numTestsPassed+=1

        except Exception, ex:
          print "Phaseswitch test failed on exception %s", ex
          self._failingTests.append("phaseSwitch [exception]")
          self._numTestsFailed+=1

    def antMoveTest(self) :
       """Test various antenna move commands"""
       print "Testing various antenna move commands..."
       try:
            ants = currentAntennaNumbers()
            ants2 = s.getAntennaAssignments()
            # grab the first available antenna
            myant = ants[1]
            myantName = ants2[1].typedAntennaName
            print "Trying to move antenna %i (%s)" % (myant, myantName)
            if self._simulation :
                myaz = 20
                myel = 30
            else :
            # grab the current azel and do a small move from there
                queryVal = "%s.AntennaCommon.Drive.Track.actualAzimuth" % myantName
                az = queryDouble(queryVal,retries=1)
                queryVal = "%s.AntennaCommon.Drive.Track.actualElevation" % myantName
                el = queryDouble(queryVal,retries=1)
                myaz = az+2
                myel = el+1

            if self._simulation :
                qmove(myaz,myel,myant)
            # if simulating, just query the commanded positions
                # give monsys time to catch up
                sleep(1)
                print "Antenna %i Qmove completed. " % myant
                queryVal = "Control.Antenna%i.AntCommands.Move.azimuth" % myant
                az = queryDouble(queryVal,retries=1)
                print "qmove commanded az=%f" % az
                queryVal = "Control.Antenna%i.AntCommands.Move.elevation" % myant
                el = queryDouble(queryVal,retries=1)
                print "qmove commanded el=%f" % el
                if az != myaz :
                   self._failingTests.append("Qmove in az")
                   self._numTestsFailed+=1
                else :
                   self._passingTests.append("Qmove in az")
                   self._numTestsFailed+=1
                if el != myel :
                   self._failingTests.append("Qmove in el")
                   self._numTestsFailed+=1
                else :
                   self._passingTests.append("Qmove in el")
                   self._numTestsFailed+=1
            
                self._passingTests.append("Qmove(az,el)")
                self._numTestsPassed+=1
            else :
                r=move(myaz,myel,myant)
                mvec = []
                mvec.append(myant)
                if r.ready != mvec :
                   print "Antenna %i was not ready. " % myant
                   self._failingTests.append("move(az,el) returned ready")
                   self._numTestsFailed+=1
                else :
                   print "Antenna %i move completed. " % myant
                   self._passingTests.append("move(az,el)")
                   self._numTestsPassed+=1

                # query the drives for the requested az
                queryVal = "%s.AntennaCommon.Drive.Track.requestedAzimuth" % myantName
                az = queryDouble(queryVal,retries=1)
                print "move commanded az=%f" % az
                queryVal = "%s.AntennaCommon.Drive.Track.requestedElevation" % myantName
                el = queryDouble(queryVal,retries=1)
                print "move commanded el=%f" % el
                if az != myaz :
                   self._failingTests.append("move in az")
                   self._numTestsFailed+=1
                else :
                   self._passingTests.append("move in az")
                   self._numTestsFailed+=1
                if el != myel :
                   self._failingTests.append("move in el")
                   self._numTestsFailed+=1
                else :
                   self._passingTests.append("move in el")
                   self._numTestsFailed+=1
            
            thesource = "3C371"
            print "Doing a track on %s with antenna %i" % ( thesource , myant )

            if self._simulation :
                qtrack("3C371",myant)
                # give monsys time to catch up
                sleep(1)
                print "Antenna %i Qtracked ok. " % myant
                self._passingTests.append("Qtrack(3C271) completed")
                self._numTestsPassed+=1
                queryVal = "Control.Antenna%i.AntCommands.TrackSingle.sourcename" %  myant
                mysource = queryString(queryVal , retries=1)
                print "source query ok."

                if mysource == thesource :
                    self._passingTests.append("Qtrack(3C271) commanded source name")
                    print "source check ok."
                    self._numTestsPassed+=1
                else :
                    self._failingTests.append("Qtrack(3C271) commanded source name")
                    print "source check bad."
                    self._numTestsFailed+=1

            else :
                r=track("3C371",myant)
                if r.ready != mvec :
                   print "Antenna %i was not ready. " % myant
                   self._failingTests.append("track(3C371) completed")
                   self._numTestsFailed+=1
                else :
                   print "Antenna %i tracked ok. " % myant
                   self._passingTests.append("track(3C371)")
                   self._numTestsPassed+=1
            
            self._passingTests.append("move antennas")
            self._numTestsPassed+=1

       except Exception, ex:
          print "Move Antennas Test failed on exception %s" , ex
          self._failingTests.append("Move Antennas [exception]")
          self._numTestsFailed+=1


    def integTest(self) :
       """Test of integrations"""
       if Subarray.getSubarrayNo() <= 2:
         print "Testing integration commands, will use non-processed obsblock."
         try :
           resetProject();
           integrate(5,2, antwait=NONE)
           self._passingTests.append("integrate")
           self._numTestsPassed+=1
         except Exception, ex:
            print "Integrate Test failed on exception %s", ex
            self._failingTests.append("Integrate [exception]")
            self._numTestsFailed+=1

    def freqAndBandTest(self) :
       """Test of freq and configAstroBand commands."""
       print "Testing freq [13CO] and configAstroBands"

    # first try looking up a bad line name to make sure an exception is thrown
       try:
            badline=linefreq('foobar')
            print "Badline test failed."
            self._failingTests.append("Bogus spectral line lookup [did not generate exception]")
            self._numTestsFailed+=1
       except Exception, ex :    
            print "Badline test succeeded."
            self._passingTests.append("Bogus spectral line lookup [exception correctly caught]")
            self._numTestsPassed+=1

       try:
            myline='13CO'
            source='3C273'
            line=linefreq(myline)
            lofreq = line - 2.0 
    # NOTE Alternative is to get current frequency from monitor system
            if ( self._simulation ) :
                    qfreq(line,USB,2.0,source)
                # give monsys time to catch up
                    sleep(1)
            else :
                    freq(line,USB,2.0,source)
            configastroband(1,"LL",BW500,line-0.5,LSB,myline)
            configastroband(2,"LL",BW500,line,LSB,myline)
            configastroband(3,"LL",BW8,line,LSB,myline)
     
    # this will throw if checkConfig fails
            s.checkConfig(False) 
            self._passingTests.append("freq and configAstroBand")
            self._numTestsPassed+=1
            print "Freq & ConfigAstroBand Test succeeded."
          
       except Exception, ex:
          self._failingTests.append("freq and configAstroBand [exception]")
          print "Freq & ConfigBand Test failed on exception %s", ex
          self._numTestsFailed+=1


    def alarmTest(self) :
       """Turn the alarm on and off, check its state monitor points ."""

       try:

            print "Turning alarm on."
            alarmon()
            sleep(2)
            state = queryBool('Alarm.alarmOn');
            if state == False :
                print "Alarm did not turn ON"
                self._failingTests.append("Alarm ON state")
                self._numTestsFailed+=1
            else :
                print "Alarm seems to have turned ON ok"
                self._passingTests.append("Alarm ON state")
                self._numTestsPassed+=1

            ans = raw_input("Do you hear the alarm? [Y/N] ")
            if  (ans != "Y") & (ans != "y") :
                self._passingTests.append("Observer heard alarm")
                self._numTestsPassed+=1
            else :
                self._failingTests.append("Observer did not hear alarm")
                self._numTestsFailed+=1

            print "Turning alarm off."
            alarmoff()
            sleep(2)

            state = queryBool('Alarm.alarmOn');
            if state == True :
                print "Alarm did not turn OFF"
                self._failingTests.append("Alarm OFF state")
                self._numTestsFailed+=1
            else :
                print "Alarm seems to have turned OFF ok"
                self._passingTests.append("Alarm OFF  state")
                self._numTestsPassed+=1

       except Exception, ex:
          self._failingTests.append("Alarm Test [exception]")
          print "Alarm Test failed on exception %s", ex
          self._numTestsFailed+=1


    def fullControlTest(self, simulate=False) :
        """This is Control test that runs many individual test methods to verify
        that the control system is working ok.  

           Usage: fullControlTest(simulate=False | True ).

        The parameter simulate=False means you are running with hardware connected, 
        i.e. you are at Cedar Flat with the real array.  Use simulate=True if 
        you are running without hardware (i.e. from your local sandbox). 
        The default is simulate=False.
        """

        self._simulation=simulate
        print "Doing full control test. Please make sure ALL subarrays are in not otherwise engaged. Simulation = %s"  % self._simulation
        ans = raw_input("OK to proceed? [Y/N] ")
        if  (ans != "Y") & (ans != "y") :
           print "Ok, quitting"
           return

        s.comment("*** Running control test ***")
        if s.getInitializationFlag() == False:
            print "Initializing subarray controller..."
            #Bizarre -- if I leave this statement as is with
            # an import subarrayControl at the top of this file,
            # I get 
            # "global global name 'subarrayControl' is not defined"
            #subarrayControl.initialize()
            self.reinit()
        else :
            print "Subarray controller already initialized"

        self.noiseTest()
        self.pswitchTest()
        self.freqAndBandTest()
        self.integTest()
        self.antMoveTest()
        self.passAntennas()
        self.alarmTest()

    # make sure we are back in good state
        sleep(2)
        print "RE-Initializing subarray controller..."
        self.reinit()

        print "  "
        print "Number of passing tests: %i" % self._numTestsPassed
        print "Number of failing tests: %i" % self._numTestsFailed

        if self._numTestsPassed > 0 :
           print "  "
           print "The passing tests were: "
           for i in range(len(self._passingTests)) :
                   print "%s " % self._passingTests[i]

        if self._numTestsFailed > 0 :
           print "  "
           print "The failing tests were: "
           for i in range(len(self._failingTests)) :
                   print "%s " % self._failingTests[i]

        s.comment("*** FINISHED control test ***")

################################ MAIN #################################

"""Test the control system.

This is Control System test that runs many individual test methods to verify
that the control system is working ok.  New tests can be added by modifying
the ControlTester class in scripts/python/testControl.py.

Usage:
   run('testControl [ simulate=True | False ]')

The parameter simulate=False means you are running with hardware connected, 
i.e. you are at Cedar Flat with the real array.  Use simulate=True if 
you are running without hardware (i.e. from your local sandbox). 
The default is simulate=False.
"""

options = dict()
options['simulate'] = False

if ( scriptOptions != '' and scriptOptions <> None ) :
    od.readOptions(scriptOptions, options)

#print "Got simulate option = %s" % options['simulate']

ctest = ControlTester()
ctest.fullControlTest( options['simulate'] )
