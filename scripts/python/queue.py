import subarrayCommands as commands
import obsdefUtils as utils
import obsdefIndex as odi

def carma23Start(s2, options=dict()):
   # Must be in Sci1
   if commands.subarrayNo != 1:
      raise Exception,'carma23 command in queue file can only be run in sci1'

   # Set flag for sci2 indicating it is not ready for observations
   s2.setScriptBool(odi.INDX_BOOL_SCI2_RESUME_QUEUE, 0)

   # Wait until SCI2 is ready for CARMA23
   # This is determined if 
   #   (1) INDX_BOOL_SCI2_READY_CARMA23 is set to TRUE
   # OR
   #   (2) There are no 3.5m antennas in sci2, but sci1 does contain 3.5m antennas
   # Make sure resume flag is turned off
   readyForCarma23 = (s2.getScriptBool(odi.INDX_BOOL_SCI2_READY_CARMA23) == True)
   useScriptVariables = readyForCarma23
   while not readyForCarma23:
      # Message
      print 'Waiting for sci2 to be ready for CARMA23 observations'

      # Check if we are ready for CARMA23
      if s2.getScriptBool(odi.INDX_BOOL_SCI2_READY_CARMA23) == 1:
         print '  ... CARMA23 flag is set to TRUE in sci2...'
         readyForCarma23 = True
         useScriptVariables = True
      else:
         # Get antennas in sci2
         ant2 = s2.getAntennaAssignments()
         if len(ant2) > 0:
            print '  ... there are still antennas in sci2 ...'
         else:
            # Get antennas in sci1
            ant1 = commands.currentAntennaNumbers()

            # Count number of 3.5m antennas in sci1
            n35 = 0
            for a2 in range(16,24):
               if ant1.count(a2) > 0: n35 += 1

            # Any 3.5m antennas
            if n35 > 0:
               print '  ... there are %d 3.5m antennas in sci1 ...'
               readyForCarma23 = True

      # Start CARMA23?
      if not readyForCarma23: 
         print '  ... sci2 not ready for CARMA23 yet - still waiting ...'
         commands.sleep(10)

   # Print message that we are ready for CARMA23
   if readyForCarma23: 
      print 'Starting CARM23 track...'
   else:
      # In principle, we should never get here...
      raise Exception,'sci2 is not ready for CARMA tracks'

   # Add wideband correlator, if needed
   if options.has_key('wb') and options['wb'] == False:
      # Disable integration alarm for wideband correlator
      commands.alarmIntegdisable(corr=commands.CORR_WIDEBAND, 
                subarray=commands.SCI2)       
   else:
      # Remove correlator from sci2
      if commands.subarrayOwnsCorrelator(2, commands.CORR_WIDEBAND):
         s2.removeCorrelator(commands.CORR_WIDEBAND)
         commands.sleep(5)

      # Add correlator to sci1
      if not commands.subarrayOwnsCorrelator(1, commands.CORR_WIDEBAND):
         commands.addCorrelator(commands.CORR_WIDEBAND)

      
   # Add antennas from sci2, if using script variables for control
   if useScriptVariables:
      # Get list of antennas to add from sci2
      ants = utils.makeListObsdef(s2.getScriptString(odi.INDX_STR_SCI2_ANTENNAS))
      for ia in range(len(ants)): ants[ia] = int(ants[ia])
   else:
      ants = range(16,24)

   # Remove any antennas specified on the options list
   if options.has_key('antskip'):
      for a in options['antskip']:
         if ants.count(a) > 0: ants.remove(a)

   # Make list of antennas which need to be added and are not already in sci1
   antsCurrent = commands.currentAntennaNumbers()
   antsCopy = ants[:]
   for a in antsCurrent:
      if antsCopy.count(a) > 0: 
          antsCopy.remove(a)

   # Add antennas
   if len(antsCopy) > 0:
       s2.removeAntenna(ants, True)
       commands.sleep(5)
       commands.addAntenna(antsCopy, retune=False, tsys=False)

   # Disable coherence alarm
   utils.disableCoherenceAlarm()

def carma23Stop(s2, options=dict()):
   # Must be in sci1
   if commands.subarrayNo != 1:
      raise Exception,'carma23 command in queue file can only be run in sci1'

   # Remove wideband correlator from sci1
   if commands.subarrayOwnsCorrelator(1, commands.CORR_WIDEBAND):
      commands.removeCorrelator(commands.CORR_WIDEBAND)
      commands.sleep(5)

   # Remove 3.5m antennas from sci1
   ants = commands.currentAntennaNumbers()
   for a in range(1,16):
      if ants.count(a) > 0: ants.remove(a);
   commands.removeAntenna(ants)
   commands.sleep(5)

   # Set flag indicating ok to resume sci2 queue
   s2.setScriptBool(odi.INDX_BOOL_SCI2_RESUME_QUEUE, 1)


def queue(filename, **options) :
   # Keep track of scripts that have been run
   completed = []

   # Get subarray
   subarray = commands.subarrayNo

   # Get handle for s2
   s2 = None
   if subarray == 1: s2 = commands.Subarray.getSubarrayRef(2)

   # Loop over file
   moreScripts = True
   while moreScripts:
      # Open file
      file = open(filename, "r")

      # Initialize
      moreScripts = False
      completedPrevious = completed[:]
      completed = []

      # Read through the file and find first script that has not been run
      for lineOrig in file:
         # Remove trailing and ending spaces
         line = lineOrig.lstrip().rstrip()

         # Strip off comment portion of line
         j = line.find('#') 
         if j >= 0: line = line[0:j]
         line = line.lstrip().rstrip()

         # Remove commas
         # line = line.replace(",", " ")

         # Skip if blank line
         if line == "": continue

         # Split line into tokens
         tokens = line.split()

         # See if this script has been run before. If so, skip it.
         found = False
         for c in completedPrevious:
            if c.split() == tokens: 
               if len(completed) == 0:
                  print 'Skipping the following commands since they have been run already:'
               print '    %s' % c
               completed.append(c)
               found = True
               break
         if found: continue
         print 'Queue program is running the following command:'
         print '    %s' % line

         # Is this subarray specific?
         sci = tokens[0].lower()
         iscript = 0 # Indx which contains the script name
         if sci.find("sci") == 0:
            iscript = 1
            if sci != "sci1" and sci != "sci2":
               raise Exception, 'Invalid subarray in queue: '+lineOrig
            if sci == "sci1" and subarray != 1:
               continue
            elif sci == "sci2" and subarray != 2:
               continue

         # First token is the script. 
         # Strip .py or .obs extension and remove quotes
         scriptname = tokens[iscript]
         if scriptname.endswith('.obs'):
            scriptname = scriptname.rstrip('.obs')
         elif scriptname.endswith('.py'):
            scriptname = scriptname.rstrip('.py')
         scriptname = scriptname.replace("'", "")
         scriptname = scriptname.replace('"', "")

         # Now the options
         options = dict()
         for i in range(iscript+1,len(tokens)):
             # Split keyword by equal sign
             t = tokens[i].split("=")
             if len(t) != 2:
                raise Exception,'Error entering option for script %s: option=%s' % (scriptname, tokens[i])

             # Split into keyword and value
             keyword = t[0]
             isString = (t[1].find("'") >= 0) or (t[1].find('"') >= 0)
             isList = (t[1][0] == "[") and (t[1][-1:]=="]")
             value = t[1].replace("'", "").replace('"', '')

             # Reset value
             if isList:
                value = value.replace('[','').replace(']','').replace(',',' ')
                l = utils.makeListObsdef(value)
                for il in range(len(l)):
                    v = l[il]
                    try:
                       i = int(l[il])
                       v = i
                    except:
                       try:
                          x = float(value)
                          v = x
                       except:
                          pass
                    l[il] = v
                value = l[:]
             elif value.lower() == "false":
                value = False
             elif value.lower() == "true":
                value = True
             elif not isString:
                # Check if integer 
                try:
                   i = int(value)
                   value = i
                except:
                   # Check if float
                   try:
                      x = float(value)
                      value = x
                   except:
                      pass

             # Save it
             options[keyword] = value

         # Run the command
         if scriptname.lower() in ['restart', 'restarttrack']:
            result = commands.restartTrack(**options)
         elif scriptname.lower() == "tilt":
            commands.tilt(doplot=False)
            result = True
         elif scriptname.lower() == "carma23start":
            carma23Start(s2, options=options)
            result = True
         elif scriptname.lower() == "carma23stop":
            carma23Stop(s2)
            result = True
         elif scriptname.lower() == "pause":
            # Indicate which antennas are useable
            antsN = commands.currentAntennaNumbers()
            ants = str(antsN).replace('[','').replace(']','').replace(',','')
            commands.s.setScriptString(odi.INDX_STR_SCI2_ANTENNAS, ants)

            # Set flag to indicate sci2 is ready for CARMA23
            commands.s.setScriptBool(odi.INDX_BOOL_SCI2_RESUME_QUEUE, 0)
            commands.s.setScriptBool(odi.INDX_BOOL_SCI2_READY_CARMA23, 1)

            # Now wait until CARMA23 flag is set
            print 'Waiting for CARM23 script to be finished in sci1 before restarting sci2 queue'
            while commands.s.getScriptBool(odi.INDX_BOOL_SCI2_RESUME_QUEUE) == 0:
                commands.sleep(10)

            commands.s.setScriptBool(odi.INDX_BOOL_SCI2_READY_CARMA23, 0)
            if not commands.subarrayOwnsCorrelator(2, commands.CORR_WIDEBAND):
               # The correlator should have been removed by the sci1 queue,
               # but I double check nonetheless
               if commands.subarrayOwnsCorrelator(1, commands.CORR_WIDEBAND):
                   s1 = commands.Subarray.getSubarrayRef(1)
                   s1.removeCorrelator(commands.CORR_WIDEBAND)
                   commands.sleep(5)
               commands.addCorrelator(commands.CORR_WIDEBAND)
            if len(antsN) == 0: antsN = range(16,24)
            commands.addAntenna(antsN, tsys=False, retune=False)

            # Disable coherence alarm
            utils.disableCoherenceAlarm()

            # Command completed successfully
            result = True
         else:
            result = commands.run(scriptname, **options)
            # print 'Running script ',scriptname
            # result = True
            # commands.sleep(30)
         state = commands.s.getScriptState()
         if state == commands.FAILED:
            raise Exception, 'error running script'
         elif result == False:
            raise Exception, 'The script did not end correctly. Stopping the queue program.'
         completed.append(line)
         moreScripts = True
         break

      # Close file
      file.close()
   print 'No more tracks in the queue for sci%d' % subarray
   commands.alarmon()
