# LineLengthDiagnostics.py
# all the old linelength test routines are saved here

import sys
import carmaIni
import carma
import Subarray
import subarrayCommands as SAC
import math as m
import time
import datetime
import fpformat
import numpy

nchans = 16

def ttherm(volts) :
    """Returns temperature in C, given volts on YSI 44016 thermistor"""
    a = 0.0011303
    b = 0.0002339
    c = 8.863e-8
    ohms = 10000./(5./volts - 1.)
    if (ohms > 100.) :
       lnr = numpy.log(ohms)
       overt = a + b * lnr + c * numpy.power(lnr,3)
       return 1./overt - 273.16
    else :
       return 0.

def setSynthFreq( MHz, synthNumber=1) :
    """Sets synthesizer frequency, dumps out frequency reported back"""
    s = carmaIni.getObj("carma.loref.loReferenceControl")
    s.setFrequency( synthNumber, 1.e6 * MHz )           
    print "Set synth #%d frequency to %.6f MHz" % (synthNumber, MHz)
    freqrpt = 0.
    secs = 0.
    while ( (freqrpt != MHz) & (secs < 10.) ) :
        time.sleep(0.5) 
        secs += 0.5
        freqrpt = SAC.queryDouble("LoRef.LoRefSynthesizer"+str(synthNumber)+".synthFreqRpt", 20) / 1.e6
        print "... %5.1f secs, readback = %10.6f" % (secs, freqrpt) 

def setSynthPwr( dBm, synthNumber=1) :
    """Sets synthesizer power in dBm"""
    s = carmaIni.getObj("carma.loref.loReferenceControl")
    s.setPower( synthNumber, dBm )              
    print "Set synth #%d power to %.3f dBm" % (synthNumber, dBm)

def fscan( fstart=1100., \
           fstep=1, \
           nsteps=160, \
           stepfactor=1., \
           waitsecs=2.5, \
           outfile="/tmp/lstep.dat", \
           synthNumber=1, \
           writemode='a', \
           verbose=1 ) :
    """Steps synthesizer, takes linelength data, appends to file """
    try:
        SAC.doppler('none')  # hopefully turns off the background task that screws up the synthesizer
    except:
        print "doppler command not accepted"
    currentSynthFreq = SAC.queryDouble("LoRef.LoRefSynthesizer"+str(synthNumber)+".synthFreqRpt", 20)
    s = carmaIni.getObj("carma.loref.loReferenceControl")
    file = open( outfile, writemode )
    file.write( "# " + time.ctime() + "\n" )
    synthPwr = SAC.queryDouble("LoRef.LoRefSynthesizer"+str(synthNumber)+".synthAmpRpt", 20)
    file.write("# synth power = %.2f dBm\n" % synthPwr)
    file.flush()
#   sys.stdout.flush()
    freq = fstart
    for i in range(nsteps) :
        s.setFrequency(synthNumber,freq*1.e6)             # set freq in Hz
        file.write("# set synth %d frequency to %.6f MHz\n" % (synthNumber, freq) )
        if ( verbose > 0 ) :
            print "Set synth #%d frequency to %.6f MHz" % (synthNumber, freq)
        freqrpt = 0.
        secs = 0.
        while ( (freqrpt != freq) & (secs < waitsecs) ) :
            time.sleep(0.5) 
            secs += 0.5
            freqrpt = SAC.queryDouble("LoRef.LoRefSynthesizer"+str(synthNumber)+".synthFreqRpt", 20) / 1.e6
            file.write("# ... %5.1f secs, readback = %10.6f\n" % (secs, freqrpt) )
            file.flush()
            if ( verbose > 0) :
                print "... %5.1f secs, readback = %10.6f" % (secs, freqrpt) 
                sys.stdout.flush()
            if ( (secs > 4.) & (secs < 5.) ) :
                s.setFrequency(synthNumber,freq*1.e6)             # set freq again
                file.write("# ... RESET synth #%d frequency to %.6f MHz\n" % (synthNumber, freq))
                file.flush() 
        file.write("%11.6f  " % freq)
        phstring = "LineLength.PhaseRef1"
        rmstring = "LineLength.PhaseRefRms1"
        file.write(" %9.6f" % SAC.queryDouble(phstring,20))
        file.write(" %8.6f" % SAC.queryDouble(rmstring,20))
        for ch in range(1,nchans+1):
            phstring = "LineLength.Phase" + str(ch)
            rmstring = "LineLength.PhaseRms" + str(ch)
            file.write(" %9.6f" % SAC.queryDouble(phstring,20))
            file.write(" %8.6f" % SAC.queryDouble(rmstring,20))
        file.write("\n");
        file.flush();
        freq += fstep
        fstep *= stepfactor
    file.close()
    s.setFrequency( synthNumber, currentSynthFreq )
    if (verbose > 0) :
        print "Restoring synthesizer to initial frequency %.6f MHz" % (currentSynthFreq/1.e6)
        sys.stdout.flush()

def quickscan() :
    """Measure linelength at 4 frequencies - suitable for quickly fitting the lengths"""
    try:
        b = carmaIni.getObj("carma.linelength.lineLengthControl")
        ut = SAC.getMiriadUTStamp()
        fscan(fstart=1120., \
              fstep=0.002, \
              nsteps=4, \
              stepfactor=-250., \
              waitsecs=10., \
              outfile="/array/rt/linelength/LLdata."+ut, \
              writemode="w", \
              verbose=0 )
        fit(infile="/array/rt/linelength/LLdata."+ut, \
            outfile="/array/rt/linelength/LLfit."+ut, \
            verbose=0 ) 
    except:
        print "   Warning: Linelength initialization failed <-----"

def pscan(pstart,pstep,nsteps,waitsecs) :
    """Steps synthesizer power, takes linelength data, appends to file '/tmp/pstep.dat'"""
    currentSynthFreq = SAC.queryDouble("LoRef.LoRefSynthesizer1.synthFreqRpt", 20)
    synth = carmaIni.getObj("carma.loref.loReferenceControl")
    file = open( "/tmp/pstep.dat", "a" )
    file.write("# step synthesizer power at freq %.6f MHz\n" % (currentSynthFreq/1.e6) )
    file.flush()
    sys.stdout.flush()
    pwr = pstart
    for i in range(nsteps) :
        print "set synth power to %.3f dBm" % pwr
        synth.setPower(1,pwr)           
        sys.stdout.flush()
        time.sleep(waitsecs)
        file.write("%5.2f  " % pwr)
        phstring = "LineLength.PhaseRef1"
        rmstring = "LineLength.PhaseRefRms1"
        file.write(" %9.6f" % SAC.queryDouble(phstring,20))
        file.write(" %8.6f" % SAC.queryDouble(rmstring,20))
        for ch in range(1,nchans+1):
            phstring = "LineLength.Phase" + str(ch)
            rmstring = "LineLength.PhaseRms" + str(ch)
            file.write(" %9.6f" % SAC.queryDouble(phstring,20))
            file.write(" %8.6f" % SAC.queryDouble(rmstring,20))
        file.write("\n");
        file.flush();
        pwr += pstep
    file.close()
    sys.stdout.flush()

def loop( avg=10., file='/tmp/loop.dat', synthNumber=1 ):
    """Endlessly takes linelength data, appends to file 'tmp/loop.dat'"""

    file = open( file, "a" )
    file.flush()
    sys.stdout.flush()
    now = time.time()

    while (1) :
#       ph = numpy.zeros(9)
        length = numpy.zeros(nchans+1)
        imag = numpy.zeros(nchans+1)
        real = numpy.zeros(nchans+1)
        az = numpy.zeros(nchans+1)
        el = numpy.zeros(nchans+1)
        agcref3 = 0.
        nsteps = 0
        while (nsteps < int(2.*avg)) :
            nsteps += 1
            time.sleep(0.5)
            agcref3 += SAC.queryDouble("LineLength.AgcRef3",20)         # thermistor
            xx = SAC.queryComplex("LineLength.PhaseComplexRef1",20)
            real[0] += xx[0]
            imag[0] += xx[1]

            real[0] += SAC.queryDouble("LineLength.RePhaseRef1",20)
            imag[0] += SAC.queryDouble("LineLength.ImPhaseRef1",20)
            for ch in range(1,nchans+1) :
#               ph[ch] += SAC.queryDouble("LineLength.Phase"+str(ch),20)
                az[ch] += getAz(ch)
                el[ch] += getEl(ch)
                length[ch] += SAC.queryDouble("LineLength.LineLength"+str(ch),20)
                xx = SAC.queryComplex("LineLength.PhaseComplex"+str(ch),20)
                real[ch] += xx[0]
                imag[ch] += xx[1]
        dtime = time.time() - now
        file.write("%9.1f " % dtime)
        temperature = ttherm(agcref3/nsteps + 0.024)
        file.write("%8.4f " % temperature)
        freq=SAC.queryDouble("LoRef.LoRefSynthesizer"+str(synthNumber)+".synthFreqRpt", 20)
        file.write("%11.6f " % (freq/1.e6) )
        file.write("  %9.6f " % numpy.arctan2(imag[0],real[0]) )
        for ch in range(1,nchans+1) :
#           ph[ch] /= nsteps
            length[ch] /= nsteps
            az[ch] /= nsteps
            el[ch] /= nsteps
            file.write("  %9.6f %7.2f %6.2f" % (numpy.arctan2(imag[ch],real[ch]),az[ch],el[ch]) )
        file.write("\n")
        file.flush()


def qloop( outfile='/tmp/qloop.dat', writemode='a' ) :
    """Dumps linelength data every 0.5 sec"""

    file = open( outfile, writemode )
    file.write( "# " + time.ctime() + "\n" )
    synthFreq = SAC.queryDouble("LoRef.LoRefSynthesizer1.synthFreqRpt", 20)
    file.write("# synth freq = %.6f MHz\n" % (synthFreq/1.e6) )
    synthPwr = SAC.queryDouble("LoRef.LoRefSynthesizer1.synthAmpRpt", 20)
    file.write("# synth power = %.2f dBm\n" % synthPwr)
    file.flush()
    now = time.time()

    while (True) :
        time.sleep(0.5)
        dtime = time.time() - now
        file.write("%9.1f " % dtime)
        phstring = "LineLength.PhaseRef1"
        rmstring = "LineLength.PhaseRefRms1"
        agcstring = "LineLength.AgcRef1"
        file.write(" %9.6f" % SAC.queryDouble(phstring,20))
        file.write(" %8.6f" % SAC.queryDouble(rmstring,20))
        file.write(" %9.6f" % SAC.queryDouble(agcstring,20))
        for ch in range(1,nchans+1) :
            phstring = "LineLength.Phase" + str(ch)
            rmstring = "LineLength.PhaseRms" + str(ch)
            agcstring = "LineLength.Agc" + str(ch)
            file.write(" %9.6f" % SAC.queryDouble(phstring,20))
            file.write(" %8.6f" % SAC.queryDouble(rmstring,20))
            file.write(" %9.6f" % SAC.queryDouble(agcstring,20))
        file.write("\n");
        file.flush();

def getEl( ant ) :
    if (ant < 7) :
       el = SAC.queryDouble( "Ovro"+str(ant)+".AntennaCommon.Drive.Track.actualElevation", 20)
    elif (ant < 16) :
       el = SAC.queryDouble( "Bima"+str(ant-6)+".AntennaCommon.Drive.Track.actualElevation", 20)
    else :
       el = 0.
    return el

def getAz( ant ) :
    if (ant < 7) :
       az = SAC.queryDouble( "Ovro"+str(ant)+".AntennaCommon.Drive.Track.actualAzimuth", 20)
    elif (ant < 16) :
       az = SAC.queryDouble( "Bima"+str(ant-6)+".AntennaCommon.Drive.Track.actualAzimuth", 20)
    else :
       az = 0.
    return az 


def fit( infile="/tmp/lstep.dat", outfile="/tmp/LLfit.dat", verbose=0  ) :
    phs = []                                            # empty list to contain phases
    freq = []                                           # empty list to contain freqs
    fin = open(infile, "r")
    fout = open(outfile, "a")
    fout.write("--------------------------------------------------------------\n")
    fout.write("input file: %s\n" % infile)
    for line in fin :
        if line.startswith("#") :
            fout.write( line ) 
        else: 
            a = line.split()                            # split line into string tokens
            freq.append(float(a[0])/1.e3)               # convert freq from MHz to GHz
            for i in range(1,len(a)) :
                phase = float(a[i])/(2. * m.pi)         # convert phase from radians to cycles
                if (phase < 0.):
                    phase += 1.                         # force phases to the interval (0,1)
                phs.append(phase)
 
    fin.close()
    nrows = len(freq)
    ncols = len(phs)/nrows
    if (verbose > 0) :
        print infile, "  nrows =",nrows,"  ncols =",ncols
    fout.write("nrows = %d, ncols = %d\n" % (nrows,ncols) )


    A = numpy.ones([nrows,2])                           # create the nlines x 2 'design matrix'
    A[:,1] = numpy.transpose(freq[0:])                  # 2nd column of design matrix is freqGHz
    y = numpy.zeros(nrows)                              # create the measurement vector

    a = carmaIni.getObj("carma.linelength.lineLengthControl")
    nchans = (ncols)/2
    for nchan in range(nchans) :
        if ( verbose > 1) :
           print " "
        for i in range(nrows) :
            y[i] = phs[ i*ncols + 2*nchan ]             # copy phases for channel nchan to y
        lguess = 1000.                                  # guess 1000 nsec length
        for i in range(1,nrows) :                       # extend the phases
            phguess = y[i-1] + lguess * (freq[i] - freq[i-1])   # predicted phase for y[i]
            nguess = int(phguess)                       # whole number of turns (any integer)
            if (phguess < 0) :
                nguess -= 1                             
            fguess = numpy.mod(phguess,1)               # fractional number of turns [0,1] interval
            if (verbose > 1) :
                print "lguess = %8.2f  meas = %5.3f  phguess = %8.3f  f = %5.3f  n = %5d" % (lguess,y[i],phguess,fguess,nguess),
            if ( fguess > (y[i] + 0.5) ) :
                 nguess += 1
            if ( fguess < (y[i] - 0.5) ) :
                 nguess -= 1
            y[i] += float(nguess)
            if (numpy.fabs(freq[i]-freq[0]) > 1.e-6) :
                lguess = (y[i] - y[0])/(freq[i] - freq[0])      # update lguess
            if ( verbose > 1) :
                print "-> %5d  new lguess = %8.2f" % (nguess,lguess)  
        
        c,resid,rank,sigma = numpy.linalg.lstsq(A,y)
        length = 0.5 * c[1]             # one-way length of fiber
        if (length < 0) :
           length = 0.
        offset = numpy.mod( c[0], 1. )  # fractional turns offset
        if ( verbose > 1) :
           print "chan %2d  c0 = %10.4f  c1 = %10.4f  length = %9.4f  offset = %6.4f  resid = %.2e" % \
               (nchan, c[0], c[1], length, offset, resid )
        elif ( verbose > 0 ) :
           print "chan %2d: length %10.4f nsec   offset %6.4f cycles   resid = %.2e" % ( nchan, length, offset, resid )
        fout.write("chan %2d  c0 = %10.4f  c1 = %10.4f  length = %9.4f  offset = %6.4f  resid = %.2e\n" % \
            (nchan, c[0], c[1], length, offset, resid) )
        if (nchan > 0):
            a.setNominalLineLengthByIndex( (nchan-1), length )
            a.setOffsetPhaseByIndex( (nchan-1), offset )
        ffit = open('/tmp/chan'+str(nchan)+'.fit', 'w')
        ffit.write("# chan %2d  c0 = %10.4f  c1 = %10.4f  length = %9.4f  offset = %6.4f  resid = %.2e\n" % \
            (nchan, c[0], c[1], length, offset, resid) )
        ffit.write("#  synthMHz   measCycles   fitCycles  diffCycles\n")
        for i in range(0,nrows) :                       # compute the residuals
            fit = c[0] + c[1]*freq[i]
            ffit.write(" %10.6f  %11.6f  %11.6f  %10.6f\n" % (freq[i], y[i], fit, y[i]-fit))
        ffit.close()    
    fout.close()

# the following routine is used to process raw data

def rawfit( infile, column,  outfile ) :

   nlines = 40          # fit one cycle at a time    
   nchans = 9           # fit ref channel plus 8 regular channels
   a = []

   ph = numpy.zeros( nlines)
   s = numpy.zeros( nlines )
   c = numpy.zeros( nlines )
   for i in range(nlines) :
        t = float(i)/2000.
        ph[i] = 2.* m.pi * 50. * t              # phase in radians
        c[i] = numpy.cos(ph[i])                         # mathematically perfect cos
        s[i] = numpy.sin(ph[i])                 # mathematically perfect sin

   fin = open(infile, "r")
   fout = open(outfile, "w")

   ncycles = 0
   nl = 0
    
   phase = numpy.zeros( nchans )
   phavg = numpy.zeros( nchans )
   raw = numpy.zeros( [nlines,nchans] )

   while (1) :
    nl += 1
    re = numpy.zeros( nchans )
    im = numpy.zeros( nchans )
    av = numpy.zeros( nchans )
    for i in range(nlines) :                    # read in one cycle (40 lines) of raw data
        line = fin.readline()
        if (not line.startswith("#")) :
            a = line.split()                    # split line into string tokens
            for n in range(nchans) :
                raw[i,n] = float(a[n])
                re[n] += c[i] * raw[i,n]                # compute real and imaginary parts
                im[n] += s[i] * raw[i,n]
                av[n] += raw[i,n]

    ncycles += 1
    fout.write("%4d " % nl)
    for n in range(nchans) :
        re[n] /= nlines/2.
        im[n] /= nlines/2.
        av[n] /= nlines
        phase[n] = numpy.arctan2(im[n],re[n])
        amp = numpy.sqrt(re[n]*re[n] + im[n]*im[n])
        phavg[n] += phase[n]
        fout.write(" %9.6f" % phase[n])
        ffit = open('/tmp/chan'+str(n)+'.rawfit', 'a')
        for i in range(nlines) :
           fit = av[n] + amp * numpy.cos(ph[i] - phase[n])
           ffit.write("%3d  %5d  %9.2f %9.2f\n" % (i,raw[i,n],fit,raw[i,n]-fit) )
        ffit.close()
    fout.write("\n")
    fout.flush()

    if ((ncycles % 25) == 0) :
        variance = numpy.zeros( nchans )
        for n in range(nchans) :
           phavg[n] /= ncycles 
           for k in range(ncycles) :
              variance[n] += (phase[n]-phavg[n])*(phase[n]-phavg[n])
           variance[n] /= (ncycles - 1)
           print " %8.5f" % numpy.sqrt(variance[n]),
           phavg[n] = 0.
        print " "

def cableloop( avg=10., file='/tmp/loop.dat', synthNumber=1 ):
    """Endlessly takes linelength data, appends to file 'tmp/loop.dat'"""

    file = open( file, "a" )
    file.write( "# restarting cableloop\n" )
    file.flush()
    
    while (1) :
        length = numpy.zeros(nchans+1)
        az = numpy.zeros(nchans+1)
        el = numpy.zeros(nchans+1)
        agcref3 = 0.
        tamb = 0
        nsteps = 0
        while (nsteps < int(2.*avg)) :
            nsteps += 1
            time.sleep(0.5)
            try:
                tamb += SAC.queryDouble("Weather.ambientTemperature", 3 )  # ambient temperature
            except:
                tamb = 0. 
            agcref3 += SAC.queryDouble("LineLength.AgcRef3",20)         # thermistor
            for ch in range(1,nchans+1) :
                az[ch] += getAz(ch)
                el[ch] += getEl(ch)
                length[ch] += SAC.queryDouble("LineLength.LineLength"+str(ch),20)

        tm = time.gmtime()
        file.write("%3d %7.4f" % ( tm[7], tm[3] + tm[4]/60. + tm[5]/3600. ) ) 
                # day-of-year, fractional hours
        tlab = ttherm(agcref3/nsteps + 0.024)
        file.write(" %7.4f %7.4f " % (tamb/nsteps, tlab) )
                # outdoor temp, fiber room temperature, C
        freq=SAC.queryDouble("LoRef.LoRefSynthesizer"+str(synthNumber)+".synthFreqRpt", 20)
        file.write(" %11.6f " % (freq/1.e6) )
                # synth freq, MHz  
        for ch in range(1,nchans+1) :
            length[ch] /= nsteps
            az[ch] /= nsteps
            el[ch] /= nsteps
            file.write("  %9.6f %7.2f %5.2f" % ( length[ch], az[ch], el[ch]) )
                # cable length, az, el
        file.write("\n")
        file.flush()

def quickinit() :
    """Measure linelength at 4 frequencies - suitable for quickly fitting the lengths"""
    try:
        SAC.doppler("NONE")
    except :
        print "   Warning: Linelength intialization failed to turn off doppler tracking"
    try:
        b = carmaIni.getObj("carma.linelength.lineLengthControl")
            # exit immediately if we can't get handle to linelength control system
        ut = SAC.getMiriadUTStamp()
        scanfit( outfile="/array/rt/linelength/LLinit."+ut )
    except:
        print "   Warning: Linelength initialization failed <-----"

def scanfit( fstart=1120., \
          fstep=0.002, \
          nsteps=4, \
          stepfactor=-250, \
          waitsecs=8., \
          synthNumber=1, \
          outfile="none" ) :
    """Steps synthesizer, takes linelength data, fits linelengths """

    phs = []                                            # empty list to contain phases
    freq = []                                           # empty list to contain freqs

    currentSynthFreq = SAC.queryDouble("LoRef.LoRefSynthesizer"+str(synthNumber)+".synthFreqRpt", 20)
    s = carmaIni.getObj("carma.loref.loReferenceControl")

    writefile = 0
    if ( outfile != "none" ) :
      try:
        file = open( outfile, "w" )
        writefile = 1
        file.write( "# " + time.ctime() + "\n" )
        synthPwr = SAC.queryDouble("LoRef.LoRefSynthesizer"+str(synthNumber)+".synthAmpRpt", 20)
        file.write("# synth power = %.2f dBm\n" % synthPwr)
        file.flush()
      except:
        print "    LineLength initialization unable to open file... proceeding anyway"
        sys.stdout.flush()
        writefile = 0       # in case file cannot be opened

    frq = fstart
    for i in range(nsteps) :
        s.setFrequency( synthNumber, frq*1.e6 )             # set freq in Hz
        if (writefile) :
            file.write("# set synth %d frequency to %.6f MHz\n" % (synthNumber, frq) )
            file.flush()
        freqrpt = 0.
        secs = 0.
        while ( (secs < 3.5) | ((freqrpt != frq) & (secs < waitsecs)) ) :
            time.sleep(0.5)
            secs += 0.5
            freqrpt = SAC.queryDouble("LoRef.LoRefSynthesizer"+str(synthNumber)+".synthFreqRpt", 20) / 1.e6
            if (writefile) :
                file.write("# ... %5.1f secs, readback = %10.6f\n" % (secs, freqrpt) )
                file.flush()
        if (writefile) :
            file.write("\n%11.6f  " % frq)
        freq.append(frq/1.e3)                       # convert freq from MHz to GHz

        phstring = "LineLength.PhaseRef1"
        ph = SAC.queryDouble(phstring,20)
        if (writefile) :
            file.write(" %9.6f" % ph )
            rmstring = "LineLength.PhaseRefRms1"
            file.write(" %8.6f" % SAC.queryDouble(rmstring,20))
 
        ph = ph/(2. * m.pi)         # convert phase from radians to cycles
        if (ph < 0.):
            ph += 1.                         # force phases to the interval (0,1)
        phs.append(ph)
        
        for ch in range(1,nchans+1):
            phstring = "LineLength.Phase" + str(ch)
            ph = SAC.queryDouble(phstring,20)
            if (writefile) :
                file.write(" %9.6f" % ph )
                rmstring = "LineLength.PhaseRms" + str(ch)
                file.write(" %8.6f" % SAC.queryDouble(rmstring,20))
            ph = ph/(2. * m.pi)         # convert phase from radians to cycles
            if (ph < 0.):
                ph += 1.                         # force phases to the interval (0,1)
            phs.append(ph)

        if (writefile) :
            file.write("\n\n");
            file.flush();

        frq += fstep
        fstep *= stepfactor

    s.setFrequency( synthNumber, currentSynthFreq)
    if (writefile) :
        file.write("restoring synthesizer %d to initial frequency %.6f MHz\n\n" % (synthNumber, currentSynthFreq/1.e6))
        file.flush();
        
    nrows = len(freq)
    ncols = len(phs)/nrows
    if (writefile) :
        file.write("nrows = %d, ncols = %d\n" % (nrows,ncols) )
        file.flush();

    A = numpy.ones([nrows,2])                           # create the nlines x 2 'design matrix'
    A[:,1] = numpy.transpose(freq[0:])                  # 2nd column of design matrix is freqGHz
    y = numpy.zeros(nrows)                              # create the measurement vector

    a = carmaIni.getObj("carma.linelength.lineLengthControl")
    for nchan in range(nchans) :
        for i in range(nrows) :
            y[i] = phs[ i*ncols + nchan ]             # copy phases for channel nchan to y
        lguess = 1000.                                  # guess 1000 nsec length
        for i in range(1,nrows) :                       # extend the phases
            phguess = y[i-1] + lguess * (freq[i] - freq[i-1])   # predicted phase for y[i]
            nguess = int(phguess)                       # whole number of turns (any integer)
            if (phguess < 0) :
                nguess -= 1                             
            fguess = numpy.mod(phguess,1)               # fractional number of turns [0,1] interval
            if ( fguess > (y[i] + 0.5) ) :
                 nguess += 1
            if ( fguess < (y[i] - 0.5) ) :
                 nguess -= 1
            y[i] += float(nguess)
            if (numpy.fabs(freq[i]-freq[0]) > 1.e-6) :
                lguess = (y[i] - y[0])/(freq[i] - freq[0])      # update lguess
        
        c,resid,rank,sigma = numpy.linalg.lstsq(A,y)
        length = 0.5 * c[1]             # one-way length of fiber
        offset = numpy.mod( c[0], 1. )  # fractional turns offset
        if (writefile) :
            file.write("chan %2d  c0 = %10.4f  c1 = %10.4f  length = %9.4f  offset = %6.4f  resid = %.2e\n" % \
                (nchan, c[0], c[1], length, offset, resid) )
            file.flush();
        if ( (length < 0) | (length > 25000.) ) :
           length = 0.
           if (writefile) :
                file.write("#  WARNING: length outside range 0 - 25000 nsec, set to 0.0 nsec\n")           
                file.flush();
        if (nchan > 0):
            a.setNominalLineLengthByIndex( (nchan-1), length )
            a.setOffsetPhaseByIndex( (nchan-1), offset )

    if (writefile) :
        file.close()

def SZAinit() :
    a = carmaIni.getObj("carma.linelength.lineLengthControl")
    for szaAnt in range(8):
      a.setAntennaLORef( szaAnt+15, 1 )
      a.setNominalLineLengthByIndex( szaAnt+15, 1000. )
    a.setLORefFreqByIndex( 1, 998000000. )                
     
