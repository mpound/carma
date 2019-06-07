import numpy as np
from itertools import ifilterfalse
import subprocess
import os
import shlex
import sys

def fitDelays( vis_file, fit_choice, refant=0, path=None, plot=True, local=False, scatter=20, apply_XY_offset=False ):
    """
    fitDelays( vis_file, fit_choice, refant=0, path=None, plot=True, local=False, scatter=20, apply_XY_offset=False )

    Purpose:    
        Corrects telescope delay offsets and receiver delay diffs.  Input files are the output
        from a recent (wideband) fringeCheck.  The number of channels in each band
        MUST BE THE SAME.  fitDelays is robust against dropped correlator bands, and
        will throw an exception if the fringeCheck observing band (1cm, 3mm, 1mm), the array
        (SCI1, SCI2, or CARMA-23), or your fit_choice (OFFSET, RX1MM_POL1, RX1MM_POL2, 
        RX1MM_FULLSTOKES, RX1CM) are incompatible with one another.  
        
        Make sure to correct delay offsets BEFORE correcting receiver delay diffs.  
        
        Delay offsets will need to be fit after each array configuration change.  Diffs should 
        not need to be fit unless receivers have been changed.  In the case of RX1MM_POL2 diffs,
        some change is expected between configurations due to fiber-length differences.

    Instructions:
        To correct delay OFFSETS for SCI1 (c1-c15), use a 3mm fringeCheck from those dishes.
        To correct delay OFFSETS for SCI2 (c16-c23), use a 3mm fringeCheck from those dishes.
        To correct delay OFFSETS in CARMA-23 mode (c1-c23), use data from a 3mm or 1cm fringeCheck23.
        To correct delay DIFFS for SCI1 (c1-c15), use a 1mm fringeCheck from those dishes.
        To correct delay DIFFS for SCI2 (c16-c23), use a 1cm fringeCheck from those dishes.

        If you have changed cables or receivers, make sure first to correct the 
        OFFSETS, and then correct the 1mm/1cm delay DIFFS (a.k.a. rxDelays) by 
        tuning to 1mm/1cm and performing a 1mm/1cm fringeCheck.

        [NOTE REGARDING 1cm OBSERVATIONS: 
        For the 3.5m antennas, it is acceptable to correct the offsets for c16-c23 
        by using a 1cm fringeCheck, assuming the delay diffs are already correct.  
        To correct for the 1cm delay diffs, one would then have to perform a 3mm 
        fringeCheck with c16-c23, and apply corrections to the diffs using the 
        appropriate sign.  This script does not use this method.

        Once 1cm receivers are installed on all 23 antennas, 1cm data will be
        used to set the offsets; until then, 3mm data will be used to set ALL
        offsets, including those on 3.5m antennas.]


    Parameters
    __________
    vis_file : string
            The visibility dataset being used to fit phase slopes.

    fit_choice : string
            Set fit_choice to one of four options:
               1) 'OFFSET' (to correct 3mm delay offsets using either a 
                   SCI1, SCI2, or CARMA-23 fringeCheck)
               2) 'RX1MM_POL1' (to correct delay diffs for SCI1, 1mm LCP)
               3) 'RX1MM_POL2' (to correct delay diffs for SCI1, 1mm RCP)
               4) 'RX1MM_FULLSTOKES' (to correct delay diffs for SCI1, both 1mm RCP 
                   and 1mm LCP; also to examine XYphase for wrapping.)
               5) 'RX1CM' (to correct 1cm delay diffs)

    refant : integer
            The data will be reduced using the specified antenna.  If no antenna
            is specified, the data will be reduced using the first antenna present
            in the array (and in the dataset).

    path : string
            If None, fitDelays looks for the dataset in /opt/sdp/sciencedata.
            Otherwise, fitDelays looks for the dataset in the supplied directory.
            Default: None.

    plot : boolean
            If False, bandpass plots will be suppressed.
            Default: True.

    scatter : int
            The tolerance for scatter in the phase-slope fits.  This prevents fitDelays
            from applying fits to incoherent data.  THINK TWICE before increasing this number,
            as you may end up with terrible delays once the weather improves (or the offending
            antenna starts to behave)!
            Default: 20 deg.

    apply_XY_offset : boolean
            This applies the wholesale RCP vs. LCP delay offset, calculated from the XYphase
            plots during a full-Stokes fringeCheck.  The wholesale offset is fixed by subtracting
            it from the RX1MM_POL1 diffs. *** WARNING: DO NOT APPLY THIS OFFSET unless the 3mm
            offsets, and the 1mm LCP and RCP diffs have already been applied! ***
            Default: False

    local : boolean
            This allows you to run fitDelays on a machine without access to a SAC.  Current
            delays cannot be queried and new delays cannot be applied; however, delay differences
            can be calculated.
            Default: False

            
        EXAMPLE:
        fitDelays('fringe.3c454.2010oct02_1_98.1.mir', 'OFFSET', refant=3, path=None, plot=True )

        Responsible party: Chat Hull (chat@astro.berkeley.edu)
    __________
    """
    if not local :
        import subarrayCommands as SAC

    if not refant :
        raise Exception('Please supply a refant.')

    # Define shortcut for running commands in shell
    def sh(s) :
        return subprocess.Popen(shlex.split(s)).wait()
    def sh_pipe(s) :
        proc = subprocess.Popen(shlex.split(s), stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        stdout,stderr = proc.communicate('stdin')
        if not len(stderr) :
                return stdout.split('\n'),stderr
        else :
                return stdout.split('\n'),stderr.split('\n')

    # Recognize floating point numbers
    def isfloat(x):
        try:
            float(x)
            return True
        except:
            return False

    # Define comment character
    def iscomment(s):
        return s.startswith('#') 

    # Check for valid fit_choice
    if not fit_choice in ['OFFSET','RX1MM_POL1','RX1MM_POL2','RX1MM_FULLSTOKES','RX1CM']:
        raise Exception('Please enter a valid fit_choice.')

    # Copy appropriate visibility data file to /tmp
    print '\nCopying visibility file: %s...\n'%(vis_file)
    if path :
        dir = path
    else :
        dir = '/tmp/'
    vis_file_temp = dir+'vis_file_temp'
    # Delete any previous temporary visibility file
    sh('rm -rf %s'%(vis_file_temp)) 
    # Make new temporary visibility file
    if path :
        if os.path.exists('%s'%vis_file) :
            sh('cp -r %s %s'%(vis_file,vis_file_temp)) 
        else :
            raise Exception('File "%s" not found.  Did you forget to append ".mir"?'%vis_file)
    else :
        if os.path.exists('/opt/sdp/sciencedata/%s'%vis_file) :
            sh('cp -r /opt/sdp/sciencedata/%s %s'%(vis_file,vis_file_temp)) 
        else :
            raise Exception('File "%s" not found.  Did you forget to append ".mir"?'%vis_file)

    # Check that fit_choice matches the polarization of the data in the dataset
    if fit_choice == 'RX1MM_POL1' or fit_choice == 'RX1MM_POL2' or fit_choice == 'RX1MM_FULLSTOKES' :    
        log_file_pol = dir+'polnum.log'   
        # Delete any previous temporary polarization log file
        sh('rm -f %s'%(log_file_pol)) 

        # Get polarization variable from VARPLT
        sh('varplt vis=%s yaxis=pol log=%s'%(vis_file_temp,log_file_pol))

        pol_num_log = open(log_file_pol,'r')
        lines = pol_num_log.readlines()
        pol_num_log.close()

        # Note: RR = -1, LL = -2, RL = -3, LR = -4
        # For RX1MM_FULLSTOKES, I check for different numbers on different lines
        # For RR and LL, I check for the same number on two consecutive lines
        if float( lines[4].split()[2] ) == -2 and float( lines[5].split()[2] ) == -2 and fit_choice != 'RX1MM_POL1' :
            raise Exception('Your choice of %s is inconsistent with the polarization of the data present in %s!'%(fit_choice,vis_file))
        elif float( lines[4].split()[2] ) == -1 and float( lines[5].split()[2] ) == -1 and fit_choice != 'RX1MM_POL2' :
            raise Exception('Your choice of %s is inconsistent with the polarization of the data present in %s!'%(fit_choice,vis_file))
        elif float( lines[4].split()[2] ) != float( lines[5].split()[2] ) and fit_choice != 'RX1MM_FULLSTOKES' :
            raise Exception('Your choice of %s is inconsistent with the polarization of the data present in %s!'%(fit_choice,vis_file))

    # If it's a full-Stokes fringeCheck, UVCAT into RR and LL files, run fringeCheck on each, and exit
    if fit_choice == 'RX1MM_FULLSTOKES' :
        # Make XYphase phase vs. channel log file
        log_file_xyphase = dir+'xyphase.log'
        sh('rm -r %s'%(log_file_xyphase))
        sh('uvspec vis=%s axis=ch,ph yrange=-180,180 select="auto,-ant(7,8,9,10,11,12,13,14,15),purpose(P),win(1,2,3,4,5,6)" \
interval=10000 options=nopass,nocal,nopol log=%s'%(vis_file_temp, log_file_xyphase)) 
        f = open(log_file_xyphase, 'r')
        lines = f.readlines()
        f.close()

        # Assume 47-channel bands
        nchan = 47
        nband = len(lines)/nchan

        # Conversion from channel to frequency
        # This assumes a wide (500 MHz) band
        chan_to_freq = .500/nchan

        # Conversion from slope to delay
        # GHz*ns = 1 turn of phase
        # 360 deg/(GHz*ns) 
        slope_to_delay = 360.

        data = []
        for line in lines :
            data.append(map(float, line.split()))
        phase_data = np.array(data).reshape(nband,nchan,2)

        slopes=[]
        for i in range(nband) :
            channels = phase_data[i][:,0]
            phases = np.unwrap(np.radians(phase_data[i][:,1]))
            fit = np.polyfit(channels, np.degrees(phases), 1, full=True)
            slope = (1/chan_to_freq)*fit[0][0]
            if abs(slope) > 1e-10 :
                slopes.append(slope)

        # Takes median of measured slopes, and converts to a delay
        median_slope = np.median(slopes)
        print '\n\nMedian RCP vs. LCP offset:'
        median_delay = median_slope/slope_to_delay
        print str(round(median_delay, 2))+' ns\n'

        # NOTE: the above is the wholesale RCP vs. LCP offset, which is fixed by subtracting
        # it from the RX1MM_POL1 diffs.

        # Plot XYphase
        if plot:
            out,err = sh_pipe('uvspec vis=%s axis=chan,ph yrange=-180,180 device=/xs select="auto,-ant(7,8,9,10,11,12,13,14,15),purpose(P), \
            win(1,2,3,4,5,6,7,8)" interval=1000 nxy=3,2 device=/xs'%(vis_file_temp))

            if len(err) :
                print "Could not plot XYphase passbands.  Continuing with RR and LL passbands.\n"
            else :
                print "\nHow do the XYphase passbands look?  If the slopes aren't wrapping horrendously, \
                \neverything is probably fine.  Otherwise, a wholesale RCP vs. LCP delay should be applied. \
                \n(Have your local full-Stokes guru deal with this.)\n\nHit enter to continue."
                raw_input()

        if apply_XY_offset :
            select = raw_input('\n*** WARNING! DO NOT APPLY THE RCP vs. LCP OFFSET unless the 3mm delays and the 1mm RCP and LCP diffs have already been applied! ***\n\n\
\nAre you sure you want to apply the wholesale RCP vs. LCP offset?\n\
Enter "yes" if so.\n')
            if select.lower() == 'yes' :
                print '\nRCP vs. LCP offset WILL be applied.\n'
                apply_XY_offset = True
            else :
                apply_XY_offset = False                    
                print '\nRCP vs. LCP offset WILL NOT be applied.\n'
        else :
            print '\nRCP vs. LCP offset WILL NOT be applied.\n'

        if not apply_XY_offset :
            print 'ANTENNA       BEFORE       CHANGE      AFTER              CHOICE'
            print '----------------------------------------------------------------'

        for antenna in range(15):
            # Query delay diffs
            if local :
                diff_current = 999
            else :
                diff_current = SAC.queryDouble('Control.Antenna%d.rxDelay1mmPol1'%(antenna+1), 20)
            # Subtract fitted delays from current delays
            diff_new = diff_current - median_delay
            if apply_XY_offset :
                # Apply wholesale RCP vs. LCP offset to all antennas
                print 'New C%s %s = %.3f ns'%(antenna+1, 'RX1MM_POL1', diff_new)
                SAC.delaydiff(diff_new, antenna+1, SAC.RX1MM, SAC.POL1)
            else :
                print '%2d  %15.3f ns  %8.3f ns  %7.3f ns  %15s'%(antenna+1,diff_current,-median_delay,diff_new,fit_choice)

        # Remove XYphase log
        sh('rm -r %s'%(log_file_xyphase))

        # UVCAT full-Stokes fringeCheck into LL and RR
        print '\n\nSeparating full-Stokes fringeCheck file into LCP and RCP data...\n'
        temp_LL = dir+'temp.LL'
        temp_RR = dir+'temp.RR'
        # Remove previous temporary files
        if os.path.exists('%s'%temp_LL) :
            sh('rm -r %s'%(temp_LL))
        if os.path.exists('%s'%temp_RR) :
            sh('rm -r %s'%(temp_RR))
        # Create new temporary files
        sh('uvcat vis=%s select="pol(LL)" out=%s options=nopass,nocal,nopol'%(vis_file_temp, temp_LL))
        sh('uvcat vis=%s select="pol(RR)" out=%s options=nopass,nocal,nopol'%(vis_file_temp, temp_RR))

        # Run fitDelays on each file
        print '\nRunning fitDelays on LCP (RX1MM_POL1) data...'
        fitDelays( temp_LL, 'RX1MM_POL1', refant, '/tmp/', plot, local )
        print '\nRunning fitDelays on RCP (RX1MM_POL2) data...'
        fitDelays( temp_RR, 'RX1MM_POL2', refant, '/tmp/', plot, local )  
        # Remove temporary files
        for file in [vis_file_temp, log_file_pol, temp_RR, temp_LL] :
            sh('rm -rf %s'%(file))
        return
        
    # Get the active antennas
    log_file_3 = dir+'listobs.log'   
    # Delete any previous temporary listobs log file
    sh('rm -f %s'%(log_file_3)) 

    sh('listobs vis=%s log=%s'%(vis_file_temp,log_file_3)) 

    listobs_log = open(log_file_3,'r')
    lines = listobs_log.readlines()
    listobs_log.close()

    active_scopes=[]
    for line in lines:
        if line.find(':') != -1 and line.find('Antenna') != -1:
            # Find active telescopes
            active_scopes.append(int(line.split(':')[0].split()[1]))
    print '\nThese antennas were used in the observation:'
    print active_scopes
    if not refant in active_scopes:
        raise Exception('C%d was not in the array during this fringeCheck.  Please choose a different refant.'%refant)

    # Get number of channels
    log_file_2 = dir+'uvlist.log'   
    # Delete any previous temporary uvlist log file
    sh('rm -f %s'%(log_file_2)) 

    # Get number of bands and channels-per-band from uvlist output, and determined observing band (1cm, 3mm, 1mm)
    sh('uvlist vis=%s options=spectra log=%s'%(vis_file_temp,log_file_2)) 

    uvlist_log = open(log_file_2,'r')
    lines = uvlist_log.readlines()
    uvlist_log.close()

    nband=0
    absent=0
    chan_nums = np.array([])
    obs_freq = np.array([])
    for line in lines:
        if line.find(':') != -1 and line.find('number of channels') != -1:
            [tag, values] = line.split(':')
            values = np.array(map(int, values.split()))
            chan_nums = np.append(chan_nums,values)
            nband += len(np.nonzero(np.array(values) > 0)[0])
            absent += len(np.nonzero(np.array(values) == 0)[0])
            # Number of channels in each band
            np.append(chan_nums,values)
        elif line.find(':') != -1 and line.find('rest frequency') != -1:
            [tag2, values2] = line.split(':')
            values2 = np.array(map(float, values2.split()))
            obs_freq = np.append(obs_freq,values2)
    nchan = max(chan_nums)
    print '\nNumber of channels per band: %d'%(nchan)
    print 'Total number of present bands: %d'%(nband)
    print 'Total number of absent bands: %d'%(absent)
    if absent > 0 :
        print '\nWARNING: Correlator bands are absent.\n(This is okay in FULLSTOKES or CARMA-23 mode.)'

    # Determining observing band
    if np.max(obs_freq) < 70 :
        band_choice='1cm'
        print '\nThis is a 1cm observation.'
    elif np.max(obs_freq) > 180 :
        band_choice='1mm'
        print '\nThis is a 1mm observation.'
    else :
        band_choice='3mm'
        print '\nThis is a 3mm observation.'

    # Determine observation type; exit if scope_choice, band_choice, and/or fit_choice are incompatible

    # CARMA-23 observations
    if active_scopes[0] < 15 and active_scopes[len(active_scopes)-1] > 15 :
        scope_choice='all'
        print 'This is a CARMA-23 observation.'
        if fit_choice=='OFFSET':
            if band_choice=='3mm':
                print '\nYou are fixing the 3mm CARMA-23 delay OFFSETS.'
            else:
                raise Exception('\nThis fringeCheck was at %s, and is not compatible with your choice of %s.'%(band_choice,fit_choice))
        elif fit_choice=='RX1CM':
            if band_choice=='1cm':
                print '\nYou are fixing the 1cm CARMA-23 delay DIFFS.'
            else:
                raise Exception('\nThis fringeCheck was at %s, and is not compatible with your choice of %s.'%(band_choice,fit_choice))
        elif fit_choice=='RX1MM_POL1':
            raise Exception('\nYou cannot fix CARMA-23 RX1MM_POL1 diffs: the 3.5m antennas do not (yet) have 1mm receivers!')
        elif fit_choice=='RX1MM_POL2':
            raise Exception('\nYou cannot fix CARMA-23 RX1MM_POL2 diffs: the 3.5m antennas do not (yet) have 1mm receivers!')

    # SCI2 observations
    elif active_scopes[0] > 15 :
        scope_choice='3m'
        print 'This is a SCI2 observation.'
        if fit_choice=='OFFSET':
            if band_choice=='3mm':
                print '\nYou are fixing the SCI2 delay OFFSETS.'
            else:
                raise Exception('\nThis fringeCheck was at %s, and is not compatible with your choice of %s.'%(band_choice,fit_choice))
        elif fit_choice=='RX1CM':
            if band_choice=='1cm':
                print '\nYou are fixing the SCI2 RX1CM diffs.'
            else:
                raise Exception('\nThis fringeCheck was at %s, and is not compatible with your choice of %s.'%(band_choice,fit_choice))
        elif fit_choice=='RX1MM_POL1':
            raise Exception('\nYou cannot fix the SCI2 RX1MM_POL1 diffs: the 3.5m antennas do not (yet) have 1mm receivers!')
        elif fit_choice=='RX1MM_POL2':
            raise Exception('\nYou cannot fix the SCI2 RX1MM_POL2 diffs: the 3.5m antennas do not (yet) have 1mm receivers!')

    # SCI1 observations
    else : 
        scope_choice='6m10m'
        print 'This is a SCI1 observation.'
        if fit_choice=='OFFSET':
            if band_choice=='3mm':
                print '\nYou are fixing the SCI1 3mm delay OFFSETS.'
            else:
                raise Exception('\nThis fringeCheck was at %s, and is not compatible with your choice of %s.'%(band_choice,fit_choice))
        elif fit_choice=='RX1CM':
            if band_choice=='1cm':
                print '\nYou are fixing the SCI1 1cm delay DIFFS.'
            else:
                raise Exception('\nThis fringeCheck was at %s, and is not compatible with your choice of %s.'%(band_choice,fit_choice))
        elif fit_choice=='RX1MM_POL1':
            if band_choice=='1mm':
                print '\nYou are fixing the SCI1 RX1MM_POL1 diffs.'
            else:
                raise Exception('\nThis fringeCheck was at %s, and is not compatible with your choice of %s.'%(band_choice,fit_choice))
        elif fit_choice=='RX1MM_POL2':
            if band_choice=='1mm':
                print '\nYou are fixing the SCI1 RX1MM_POL2 diffs.'
            else:
                raise Exception('\nThis fringeCheck was at %s, and is not compatible with your choice of %s.'%(band_choice,fit_choice))

    # Define reference antenna
    if refant == 0:
        refant = active_scopes[0]

    # Number of telescopes (may be larger to make a reasonable matrix shape)
    telescope_num = 24

    # Conversion from channel to frequency
    # This assumes a wide (500 MHz) band
    chan_to_freq = .500/nchan

    # Conversion from slope to delay
    # GHz*ns = 1 turn of phase
    # 360 deg/(GHz*ns) 
    slope_to_delay = 360.

    # Define half-way point in 3.5m band, based on existing correlator bands
    mid_band = nchan*nband/2

    # Flag LSB when using 3mm data, since 3mm data reside only in USB of 3.5m antennas
    # Flag USB data when using 1cm data, since 1mm data reside only in LSB of 3.5m antennas
    # This results in flagging of half the data for 6/10m antennas, but should not pose a problem
    if fit_choice=='OFFSET' : 
        # Flag LSB, use USB
        print 'OFFSET (3mm): flagging LSB, using USB...\n'
        sh('uvflag vis=%s line=chan,%d,1,1,1 flagval=flag'%(vis_file_temp,mid_band)) 
        sh('mfcal vis=%s refant=%d line=chan,%d,%d,1,1 select="-auto,-source(noise)" interval=0.1'%(vis_file_temp,refant,mid_band,mid_band+1)) 
    else : 
        print 'RX DIFF (RX1CM, RX1MM_POL1, or RX1MM_POL2): flagging USB, using LSB...\n'
        # Flag USB, use LSB
        sh('uvflag vis=%s line=chan,%d,%d,1,1 flagval=flag'%(vis_file_temp,mid_band,mid_band+1)) 
        sh('mfcal vis=%s refant=%d line=chan,%d,1,1,1 select="-auto,-source(noise)" interval=0.1'%(vis_file_temp,refant,mid_band)) 

    # Define temporary log file for phase vs. channel information
    log_file = dir+'phase_vs_channel.log'
    # Delete any previous temporary phase vs. channel log file
    sh('rm -f %s'%(log_file)) 

    # Create phase vs. channel log file
    sh('smagpplt vis=%s options=bandpass,nofit,wrap xaxis=chan yaxis=phase log=%s'%(vis_file_temp,log_file))     
    # Plot phase vs. channel data
    if plot:
        sh('smagpplt vis=%s options=bandpass,nofit,wrap xaxis=chan yaxis=phase device=/xs nxy=5,3 yrange=-180,180'%(vis_file_temp)) 

    # Read in phase vs. channel data
    # with open(log_file,'r') as f: -- can't be used until Python v2.6
    f = open(log_file,'r')
    data = []
    for line in ifilterfalse(iscomment,f):        
        # Read 7 fixed-width numbers, 10 elements at a time
        row = [line[i*10:(i+1)*10] for i in range(7)]
        # Take care of either empty strings or spaces 
        def temp (x):
            if isfloat(x):
                return float(x)
            return 0.
        row = map (temp, row)
        # row = map(lambda x:float(x) if isfloat(x) else 0., row) -- can't be used until Python v2.5 (?)
        data.append(row)

    # Reshape the data into an array of [24 telescopes, nband/2 bands, nchan frequencies]
    # Half of the bands have been flagged, hence nband=nband/2
    phase_data = np.array(data)[:,1:].reshape(nband/2,nchan,telescope_num).T.swapaxes(1,2)

    # Create array of channel numbers
    freq = []
    for f in np.array(data)[:,0]:    
        if f > 0:
             freq.append(f)
    freq = np.array(freq)

    # Unwrap phases in each band (i.e. remove discontinuities), fit slopes to each wrap, take the median of the slopes
    # Converts from channel to frequency
    median_slopes=[]
    delays=[]
    for i,telescope in enumerate(phase_data):
        slopes = []
        resid = []
        for j,band in enumerate(telescope):
            phases = np.unwrap(np.radians(band))
            frequencies = freq[(nchan)*j:(nchan)*(j+1)]
            fit = np.polyfit(frequencies, np.degrees(phases), 1, full=True)
            slopes.append((1/chan_to_freq)*fit[0][0])
            # Find average residual for each band; polyfit reports the sum of the squares of the residuals
            resid.append(np.sqrt(fit[1][0]/len(phases)))

        # Takes median of measured slopes, and converts to a delay
        median_slopes.append(np.median(slopes))
        delays.append(np.median(slopes)/slope_to_delay)

        # Abort script if any antenna has an average residual of > 20 deg
        if np.median(resid) > scatter :
            raise Exception('The phase scatter in antenna C{0} is > {1} deg.  Delays will not be applied.\n \
To apply other delays, flag the offending antenna(s) and run fitDelays on the flagged file.'.format(i+1, scatter))

    absent = []
    for antenna in range(23) :
        if antenna == refant :
            print'\n\n\nRefant = C%d'%refant
        elif abs(delays[antenna]) == 0 and antenna+1 != refant :
            absent.append(antenna+1)
    print '\nAbsent antennas:'
    print absent
    print ''

    apply = False
    while True :
        if not apply :
            print 'ANTENNA       BEFORE       CHANGE      AFTER              CHOICE'
            print '----------------------------------------------------------------'

        for antenna in range(23):
            # This corrects and applies OFFSETS
            if fit_choice == 'OFFSET' and abs(delays[antenna]) > 0 :
                # Query delay offsets
                if local :
                    delay_current = 999
                else :
                    delay_current = SAC.queryDouble('DelayEngine.DelayData%d.delayOffset'%(antenna+1), 20)
                # Need to negate sign for OFFSET before subtracting fitted delays from current delays, since we're using USB data 
                delay_new = delay_current -(-delays[antenna])
                if apply:
                    # Apply new delay offsets
                    print 'New C%s %s = %.3f ns'%(antenna+1, fit_choice, delay_new )
                    SAC.delay(delay_new, antenna+1)
                else : print '%2d  %15.3f ns  %8.3f ns  %7.3f ns  %15s'%(antenna+1,delay_current,-(-delays[antenna]),delay_new,fit_choice)

            # This corrects and applies DIFFS to 1mm POL1 (LCP)
            if fit_choice == 'RX1MM_POL1' and abs(delays[antenna]) > 0 :
                # Query delay diffs
                if local :
                    diff_current = 999
                else :
                    diff_current = SAC.queryDouble('Control.Antenna%d.rxDelay1mmPol1'%(antenna+1), 20)
                # Subtract fitted delays from current delays
                diff_new = diff_current - delays[antenna]
                if apply:
                    # Apply new delay diffs
                    print 'New C%s %s = %.3f ns'%(antenna+1, fit_choice, diff_new)
                    SAC.delaydiff(diff_new, antenna+1, SAC.RX1MM, SAC.POL1)
                else : print '%2d  %15.3f ns  %8.3f ns  %7.3f ns  %15s'%(antenna+1,diff_current,-delays[antenna],diff_new,fit_choice)

            # This corrects and applies DIFFS to 1mm POL2 (RCP)
            if fit_choice == 'RX1MM_POL2' and abs(delays[antenna]) > 0 :
                # Query delay diffs
                if local :
                    diff_current = 999
                else :
                    diff_current = SAC.queryDouble('Control.Antenna%d.rxDelay1mmPol2'%(antenna+1), 20)
                # Subtract fitted delays from current delays
                diff_new = diff_current - delays[antenna]
                if apply:
                    # Apply new delay diffs
                    print 'New C%s %s = %.3f ns'%(antenna+1, fit_choice, diff_new)
                    SAC.delaydiff(diff_new, antenna+1, SAC.RX1MM, SAC.POL2)
                else : print '%2d  %15.3f ns  %8.3f ns  %7.3f ns  %15s'%(antenna+1,diff_current,-delays[antenna],diff_new,fit_choice)

            # This corrects and applies DIFFS to 1cm
            if fit_choice == 'RX1CM' and abs(delays[antenna]) > 0 :
                # Query delay diffs
                if local :
                    diff_current = 999
                else :
                    diff_current = SAC.queryDouble('Control.Antenna%d.rxDelay1cmPol1'%(antenna+1), 20)
                # Subtract fitted delays from current delays
                diff_new = diff_current - delays[antenna]
                if apply:
                    # Apply new delays diffs
                    print 'New C%s %s = %.3f ns'%(antenna+1, fit_choice, diff_new)
                    SAC.delaydiff(diff_new, antenna+1, SAC.RX1CM, SAC.POL1)
                else : print '%2d  %15.3f ns  %8.3f ns  %7.3f ns  %15s'%(antenna+1,diff_current,-delays[antenna],diff_new,fit_choice)
        if not apply :
            print '----------------------------------------------------------------'
        else : break

        # Choose whether or not to apply
        select = raw_input('\nDo you want to apply the %s corrections?  Enter "yes" if so.\n'%fit_choice)
        if select.lower() == 'yes' :
            print '\n%s corrections WILL be applied.\n'%fit_choice
            apply = True
        else :
            print '\n%s corrections WILL NOT be applied.\n'%fit_choice
            break

    print '\n'
    # Remove temporary files
    for file in [vis_file_temp, log_file, log_file_2, log_file_3] :
            sh('rm -rf %s'%(file)) 
    
    return
