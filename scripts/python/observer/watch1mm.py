import time
import os
import subarrayCommands as commands
import carmaHelpers as helpers
import obsdefUtils as utils

def nlimitMax(mylist, maxlimit):
   n = 0
   for x in mylist:
       if x <= maxlimit: n += 1
   return n

def nlimitMin(mylist, minlimit):
   n = 0
   for x in mylist:
       if x >= minlimit: n += 1
   return n

def watch1mm(phase1mm=None, tau1mm=None, auto=True, mm1=False, 
             mm3=False,     nmin=4,      npts=5,    alarm=True,
             subarray=None, lststart=None, lststop=None):
    """ Sounds the alarm the weather station to see if the weather turns suitable 
        for 1mm or drops out of 1mm conditions.

        Inputs:
             alarm     : If True, then sound the alarm when appropriate.
                         Otherwise, only messages are printed
             auto      : If True, dynamically set mm1 or mm3 to True/False based on frequency
             mm1       : If True, sound alarm if conditions are suitable for 1mm weather
             mm3       : If True, sound alarm if conditions drop out of 1mm weather
             nmin      : The minimum number of consecutive npts that need to satisfy
                         the weather conditions.
             npts      : Number of consecutive points to examine
             phase1mm  : maximum acceptable phase for 1mm conditions  (in microns)
             subarray  : Subarray number to watch. Default is 1.
             tau1mm    : maximum acceptable tau   for 1mm conditions
             lststart  : If specied, the weather alarm will not be turned on until after this LST 
                         in hours. This is useful in situations where the observer needs to run 
                         a 3mm track in the near time, and then check on the 1mm weather at a later
                         time.
             lststop   : If specied, the weather alarm will be turned off after this LST 
                         in hours. This is useful in situations where the observer needs to run 
                         a 1mm track in the near time, and then a 3mm weather at a later time.
    """

    # Set subarray number
    if subarray == None: subarray = commands.subarrayNo

    # Get configuration - this must be successful
    config = commands.queryString('Control.Subarray%d.configName' % subarray)[0]
    if config == 'A':
       if phase1mm == None: phase1mm = 150
       if tau1mm   == None: tau1mm   = 0.3
    elif config == 'B':
       if phase1mm == None: phase1mm = 150
       if tau1mm   == None: tau1mm   = 0.3
    elif config == 'C':
       if phase1mm == None: phase1mm = 150
       if tau1mm   == None: tau1mm   = 0.3
    elif config == 'D':
       if phase1mm == None: phase1mm = 200
       if tau1mm   == None: tau1mm   = 0.3
    elif config == 'E':
       if phase1mm == None: phase1mm = 350
       if tau1mm   == None: tau1mm   = 0.3
    else:
       raise Exception,'Could not set phase/tau criteria for configuration '+config

    # Print message
    print time.asctime(),': LST         = ',helpers.convertHmsString(commands.lst())
    print time.asctime(),': Tau   limit = ',tau1mm
    print time.asctime(),': Phase limit = ',phase1mm
    print time.asctime(),': Watching weather in subarray ',subarray
    print ''

    # Monitor point to watch
    mp_phase  = 'PhaseMonitor.skyRMS'
    mp_tau    = 'OpacityMonitor.tau225'
    mp_lofreq = 'Control.Subarray%d.loFreq' % subarray

    # Initialize
    phase_old = 0.0
    tau_old = 0.0
    phase = list()
    tau   = list()

    # Determine how long in seconds before the alarm can sound
    tstart = time.time()
    twaitStart = None
    twaitStop  = None
    if lststart <> None:
       twaitStart = helpers.convertHms(lststart) - commands.lst()   # Hours
       if twaitStart < 0.0: twaitStart += 24.0  # Hours
       twaitStart *= 3600.0                # Seconds
       print time.asctime(),': watch1mm will not sound the 1mm alarm for at least '+utils.dtString(twaitStart/3600.0)
    if lststop <> None:
       twaitStop = helpers.convertHms(lststop) - commands.lst()   # Hours
       if twaitStop < 0.0: twaitStop += 24.0  # Hours
       twaitStop *= 3600.0                # Seconds
       print time.asctime(),': watch1mm will not sound the 1mm alarm after '+utils.dtString(twaitStop/3600.0)

    # Indicate if we are to watch for 1mm weather, 3mm weather, or "auto"
    check1mm = mm1
    check3mm = mm3
    checkauto = auto
    if mm1 == True:
        check3mm  = False
        checkauto = False
    elif mm3 == True:
        check1mm  = False
        checkauto = False
    else:
        checkauto = True

    # nmin cannot be larger than npts.
    if nmin > npts:
        print 'ERROR setting nmin and npts'
        print 'nmin must be <= npts'
        return

    # Start infinite loop
    check1mm_old = None
    check3mm_old = None
    while True:
        # Only check the weather while we are actually integrating.
        # This is to prevent checking weather while retuning (e.g. for
        # radio pointing or a new track). This should not be necessary
        # since I check the obsblock above, but this is a fail-safe
        # in case tuning is set AFTER the rpnt obsblock name is reset.
        # In principle if an observing script was inefficient and had 
        # little integration time, the weather may not be checked at all.
        mp_integrating = 'SlPipeline.IntegratorStageContainer.IntegratorStage.integrating'
        try:
           if commands.s.queryBool(mp_integrating) != 1:
               # print time.asctime(),': Time to check weather, but array is not integrating'
               commands.sleep(10)
               continue
        except KeyboardInterrupt:
           print ''
           print time.ascitime(),': Stopping watch1mm...'
           return
        except:
           continue

        # Are we checking for 3mm or 1mm conditions?
        if checkauto:
            lofreq = 0
            try:
               lofreq = commands.queryDouble('Control.Subarray%d.loFreq' % subarray)
            except:
               continue
            if lofreq < 150.0:
                check3mm = False
                check1mm = True
            else:
                check3mm = True
                check1mm = False
        if check1mm_old != check1mm or check3mm_old != check3mm:
            t = '1mm'
            if check3mm: t = '3mm'
            print time.asctime(),': Watching for weather to change to ',t,' conditions.'
            check1mm_old = check1mm
            check3mm_old = check3mm

        # Read phase monitor
        try:
           phase_new = commands.queryDouble(mp_phase)
           sphase_new = '%8.2f' % phase_new
        except:
           phase_new = None

        # Read opacity from tipper
        try:
           tau_new = commands.queryDouble(mp_tau)
        except:
           tau_new = None

        # If we did not read tau or phase monitor, pause a bit and try again
        if phase_new == None or tau_new == None:
           commands.sleep(30.0)  # Try again shortly
           continue
           
        # Print result
        stau_new = '%5.2f' % tau_new
        print time.asctime(),': tau = ',('%5s' % stau_new),\
                             '    phase = ',('%8s' % sphase_new)

        # See if this is a new phase value. This could fail if the new phase
        # is identical to the old phase
        if phase_new > 0.0 and phase_new != phase_old:
            phase.append(phase_new)
            if len(phase) > npts: phase = phase[1:npts+1]
            phase_old = phase_new

        # Same for tau
        if tau_new > 0 and tau_new != tau_old:
            tau.append(tau_new)
            if len(tau) > npts: tau = tau[1:npts+1]
            tau_old = tau_new
        
        # See if it is 1mm weather now
        if check3mm:
            okTau = (tau1mm == None) or \
                    (len(tau) >= nmin and nlimitMin(tau,tau1mm) >= nmin)
            okPhase = (phase1mm == None) or \
                    (len(phase) >= nmin and nlimitMin(phase,phase1mm) >= nmin)
        else:
            okTau = (tau1mm == None) or \
                    (len(tau) >= nmin and nlimitMax(tau,tau1mm) >= nmin)
            okPhase = (phase1mm == None) or \
                    (len(phase) >= nmin and nlimitMax(phase,phase1mm) >= nmin)

        # If it is 1mm weather or stops being 1mm weather, then sound the alarm.
        # However, if we are pointing, then do not check
        if (check3mm==False and okTau and okPhase) or \
           (check3mm==True and (okTau or okPhase)):
            # Do not sound the alarm while we are pointing. This is 
            # because the frequency may change from the normal science observations.
            # Pointing is established if the obsblock name begins with "rpnt". 
            try:
               obsblock = commands.s.queryString('Control.Subarray%d.obsBlockId' % subarray)
               if obsblock.lower().find('rpnt') == 0 or obsblock.lower().find('none') >= 0:
                   # print time.asctime(),': Not checking weather during radio pointing'
                   commands.sleep(30)
                   continue
            except KeyboardInterrupt:
               print ''
               print time.ascitime(),': Stopping watch1mm...'
               return
            except:
               continue
            band = '1mm'
            if check3mm: band = '3mm'
            print time.asctime(),':      It is ',band,' weather!!!'
            if alarm: 
              # Initialize
              dtSeconds = time.time() - tstart
              waitingForStart = True
              passedStop      = False
              turnOnAlarm     = False
              if twaitStart == None or (twaitStart <> None and dtSeconds > twaitStart):
                 turnOnAlarm = True
                 waitingForStart = False
              if twaitStop <> None and dtSeconds > twaitStop: 
                 turnOnAlarm = False
                 passedStop = True

              # Turn on alarm
              if turnOnAlarm: 
                 commands.alarm1mm( commands.ON )
              elif waitingForStart:
                 print time.asctime(),':      ... but watch1mm alarm is disabled for ' + utils.dtString((twaitStart-dtSeconds)/3600.0)
              else:
                 print time.asctime(),':      ... but watch1mm alarm has passed lst stop time '

        # Sleep for 10 minutes
        commands.sleep(10*60)
