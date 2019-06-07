# LineLength.py version 4
# vim: set ts=4 sts=4 sw=4 et:
#
# This is a completely rewritten version of the original LineLength.py script
# which contains only the routines necessary to measure and initialize the
# absolute cable lengths.
#
# Other diagnostic routines are now in LineLengthDiagnostics.py

import subarrayCommands as SAC
import math as m
import time
import numpy
import device

MIN_SECS = 3.5
MAX_SECS = 8.0

# pre-initialize some DO handles
linelength = device.getLinelength()
loref = device.getLoRef()

def quickinit():
    """Measure linelength at 4 frequencies - suitable for quickly fitting the lengths"""
    try:
        SAC.doppler("NONE")
    except :
        print "   Warning: Linelength intialization failed to turn off doppler tracking"

    try:
        global linelength
        global loref

        # Re-grab the references for the linelength and loref DO's to fix
        # the situation where a system restart has happened without restarting
        # the SAC where this is imported
        linelength = device.getLinelength()
        loref = device.getLoRef()

        frequencies = (1120.0, 1120.002, 1119.502, 1244.502)
        subarrayNo = SAC.subarrayNo
        antennas = SAC.currentAntennaNumbers()
        subarrayName = SAC.saName
        ut = SAC.getMiriadUTStamp()
        filename = '/array/rt/linelength/LLinit.%s.%s' % (subarrayName, ut)

        scanfit(subarrayNo, antennas, frequencies, filename)
    except:
        print 'WARNING: LineLength initialization failed!'
        raise

# Simple class to hold sets of phases keyed by antenna
#
# This is basically a dictionary with:
# key: antenna number
# value: list of phases
#
class LineLengthInfo(object):
    def __init__(self):
        self.d = dict()

    def addAntennaPhase(self, ant, phase):
        try:
            self.d[ant].append(phase)
        except:
            self.d[ant] = [phase, ]

    def getAntennaPhases(self, ant):
        phases = self.d[ant]
        return phases[:]

# Simple class to make logging easier
class LineLengthLogger(object):
    def __init__(self, name):
        try:
            self.log = open(name, 'w')
        except:
            self.log = None

    def write(self, s):
        if self.log is not None:
            try:
                self.log.write(s)
                self.log.flush()
            except:
                pass

# Change the LORef and LineLength reference frequency
# for a certain LORef and set of antennas
#
# This will take care of notifying both the LORef and LineLength
# subsystems about the change in frequency. If you will be reading
# data from these subsystems, you should wait until the frequency
# changes have settled.
def changeFrequency(subarrayNo, freq_hz):
    loref.setFrequency(subarrayNo, freq_hz)
    linelength.setLORefFreq(subarrayNo, freq_hz)

def normalizePhase(p):
    # convert phase from radians to cycles
    p = p / (2.0 * m.pi)

    # force phases to the interval (0, 1)
    if (p < 0.0):
        p += 1.0

    return p

# Get the current linelength phase reading for a certain antenna number
#
# This will log the Phase + PhaseRms for the current antenna, then
# return the normalized phase phase
def getAntennaPhase(ant, log):

    # grab the Phase + PhaseRms for this antenna
    mp = 'LineLength.Phase%d' % ant
    phase = SAC.queryDouble(mp, 20)

    mp = 'LineLength.PhaseRms%d' % ant
    rms = SAC.queryDouble(mp, 20)

    # LOG: write the antenna Phase + PhaseRms
    log.write('# Phase%d PhaseRms%d\n' % (ant, ant))
    log.write('%9.6f %8.6f\n' % (phase, rms))

    return phase

def scanfit(subarrayNo, antennas, frequencies, filename):
    # create a file logger object
    log = LineLengthLogger(filename)

    # LOG: the start time
    log.write('# %s\n' % time.ctime())
    log.write('# subarrayNo %d\n' % subarrayNo)
    log.write('# antennas %s\n' % ', '.join([str(e) for e in antennas]))
    log.write('# frequencies %s\n' % ', '.join([str(e) for e in frequencies]))

    # inform the linelength system about which antennas are hooked to
    # which synthesizer (shouldn't change throughout this routine)
    for ant in antennas:
        linelength.setAntennaLORef(ant, subarrayNo)

    # save the original LO Reference Frequency (to be restored later)
    mp = 'LoRef.LoRefSynthesizer%d.synthFreqRpt' % subarrayNo
    origSynthFreq = SAC.queryDouble(mp, 20)

    mp = 'LoRef.LoRefSynthesizer%d.synthAmpRpt' % subarrayNo
    synthPower = SAC.queryDouble(mp, 20)
    log.write('# synth %d power = %.2f dBm\n\n' % (subarrayNo, synthPower))

    # step through each frequency, tune the synthesizer, and gather data
    info = LineLengthInfo()
    for freq in frequencies:

        # set LORef Frequency in Hz
        changeFrequency(subarrayNo, freq * 1.0e6)
        log.write('# Tune to %10.6f MHz\n' % freq)

        # wait for the synthesizer to tune sucessfully
        secs = 0.0
        while secs < MAX_SECS:
            time.sleep(0.5)
            secs += 0.5

            # get the reported frequency in MHz, log it
            mp = 'LoRef.LoRefSynthesizer%d.synthFreqRpt' % subarrayNo
            freqrpt = SAC.queryDouble(mp, 20) / 1.0e6
            log.write('# ... %5.1f secs, readback = %10.6f\n' % (secs, freqrpt))

            # exit the loop if we've waited long enough and
            # hit the requested frequency
            if secs >= MIN_SECS and freqrpt == freq:
                break

        # LOG: error condition (if it happened)
        if freqrpt != freq:
            log.write('# WARNING: requested frequency %10.6f never reached!\n' % freq)
            log.write('# WARNING: final reported frequency %10.6f\n' % freqrpt)

        # LOG: write the reported synth freq
        log.write('\n# Final Synth Frequency\n')
        log.write('%11.6f\n\n' % freqrpt)

        # add the phase for each antenna to the info class
        for ant in antennas:
            info.addAntennaPhase(ant, getAntennaPhase(ant, log))

        # LOG: write a blank line
        log.write('\n')

    # restore the synthesizer to its original frequency
    changeFrequency(subarrayNo, origSynthFreq)
    log.write('# reset synth %d to original frequency %.6f MHz\n' % (subarrayNo, origSynthFreq / 1.0e6))
    log.write('\n\n')

    # do some math to figure out the nominal linelengths
    doMath(subarrayNo, antennas, frequencies, info, log)


def doMath(subarrayNo, antennas, frequencies, info, log):

    # we need frequencies in GHz for this bit of code
    frequencies = [f/1e3 for f in frequencies]
    nrows = len(frequencies)

    # create the nrows x 2 'design matrix'
    A = numpy.ones([nrows, 2])

    # 2nd column of design matrix is freqGHz
    A[:,1] = numpy.transpose(frequencies[:])

    for ant in antennas:
        # convert the raw phase measurements from radians into
        # cycles in the interval (0, 1)
        normphases = [normalizePhase(p) for p in info.getAntennaPhases(ant)]

        # create the measurement vector consisting of each
        # measured phase from the current antenna
        y = numpy.array(normphases)

        # guess 1000 nsec length
        lguess = 1000.0

        for i in xrange(1, nrows):
            # predicted phase for y[i]
            phguess = y[i - 1] + lguess * (frequencies[i] - frequencies[i - 1])

            # whole number of turns (any integer)
            nguess = int(phguess)
            if phguess < 0:
                nguess -= 1

            # fractional number of turns [0, 1] interval
            fguess = numpy.mod(phguess, 1)
            if fguess > (y[i] + 0.5):
                nguess += 1

            if fguess < (y[i] - 0.5):
                nguess -= 1

            y[i] += float(nguess)
            if numpy.fabs(frequencies[i] - frequencies[0]) > 1.0e-6:
                lguess = (y[i] - y[0]) / (frequencies[i] - frequencies[0])

        # least squared fit
        c, resid, rank, sigma = numpy.linalg.lstsq(A, y)

        # one-way length of fiber
        length = 0.5 * c[1]

        # fractional turns offset
        offset = numpy.mod(c[0], 1.0)

        # LOG: lots of data
        log.write('ant %2d c0 = %10.4f c1 = %10.4f length = %9.4f offset = %6.4f resid = %.2e\n' % \
               (ant, c[0], c[1], length, offset, resid))

        # sanity check
        if length < 0.0 or length > 25000.0:
            length = 0.0
            log.write('# WARNING: length outside range [0, 25000] nsec, set to 0.0 nsec\n')

        # set the phase for each antenna
        linelength.setNominalLineLength(ant, length)
        linelength.setOffsetPhase(ant, offset)

