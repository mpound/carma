# vlbiPhaseup.py
#
# this script reads the latest sci1 selfcal gains (16 bands x 15 antennas),
#   applies a passband correction to remove band-to-band phase offsets,
#   then (if apply = True) sends phase corrections to the loberotators
#   to keep the average phase for each antenna near zero
#
# this version of the code does not do certain checks - for example, it
#   does not check that the passband data were obtained with the same
#   correlator setup as the current data - so be careful!
#
# basic operation:
#   sci1> import vlbiPhaseup
#   sci1> vlbiPhaseup.acumPassband( )
#   sci1> vlbiPhaseup.bigLoop( apply=True/False )	  # begin endless loop
#   sci1> CTRL-C to exit
#   sci1> vlbiPhaseup.cleanup()                       # reset loberotators

import subarrayCommands as SAC
import math
import time
import datetime
import device
import cmath
import numpy
import pylab
import sys
import os
import signal
import subprocess

lr = device.getLoberotator()
slpl = device.getSlPipeline()

# =========== edit these variables to control operation ============ #

refant = 8 
  # ... reference antenna for realtime selfcal
  # ... must be a functional antenna!
  # ... restarting with different refant should not cause phase glitch

activeAnts = [2,3,4,5,6,7,8,9,10,11,12,13,14,15]
  # ... adjust loberotator phases on all these antennas
  # ... OK to adjust all, even though all are not used by beamformer
  # ... exception: probably should leave out 'passthrough' or 'comparison'
  #        antennas for which loberotation is disabled during vlbi scans

passbandFile = "/home/obs/plambeck/vlbi/2012/passband.npy" 
  # ... acumulate passband on strong source at the start, store here

#lastVisFile = "/home/obs/plambeck/vlbi/2012/lastVis.npy" 
  # ... contains most recent pbCorrectedVis matrix
#lastTsysFile = "/home/obs/plambeck/vlbi/2012/lastTsys.npy" 
  # ... contains most recent tsys matrix
#lastHeader = "/home/obs/plambeck/vlbi/2012/lastHeader" 
  # ... contains most recent time

phaseHistoryFile = "/home/obs/plambeck/vlbi/2012/phaseHistory.txt"
  # ... diagnostic info

phasedAnts = [2,4,5,6,8,9,13,14]
  # ... these are colored red on the dandelion plot

# ===================================================================#

slpl.setReferenceAnt( refant )   
print "setting reference antenna to %d" % refant

jyPerK = numpy.empty( 15, dtype=float )
jyPerK[0:6] = 65.
jyPerK[6:15] = 145.

# ==== Start of stuff for handling Ctrl-C presses ====
def signal_handler(signal, frame):
  print 'Ctrl-C detected.  Enter 0 for exit with cleanup, 1 for immediate exit:'
  try:
    repeat = int(input())
  except:
    print 'Wrong input! Press Enter to resume (if necessary).'
 
  if (repeat == 1):
    print 'Exiting without further ado.'
    sys.exit(0)
  if (repeat == 0):
    print 'Cleaning up...'
    cleanup()
    print 'Exiting...'
    sys.exit(0)
#  if (repeat == 5):
#    print 'Restarting script.'
#    python = sys.executable
#    os.execl(python, python, * sys.argv)
  print 'Resuming.'
signal.signal(signal.SIGINT, signal_handler)
# ==== End of stuff for handling Ctrl-C presses ====

# --- make list of selfcal and tsys monitor point names to feed into queryMpValues --- #
def makeMpNameLists( ) :
  config = SAC.queryString("SignalPath.Mapping.Astroband1.confName")
  mpNameList = []
  tsysNameList = []
  sb = [".Lsb", ".Usb"]
  for nsb in range (0,2) :
    for nband in range(0,8) :
      for antnum in range (1,16) :
        nlabel = nband + 1
        pol = ".Leftpol"
        if ( (config == "FULLSTOKES") and (nlabel % 2 == 0) ) :  # should add DUALPOL too
          nlabel = nlabel - 1
          pol = ".Rightpol"
        mpNameList.append( "Astro.Antenna" + str(antnum) + ".Band" + str(nlabel) + 
          pol + sb[nsb] +".Selfcal.Antvis" )
        tsysNameList.append( "Astro.Antenna" + str(antnum) + ".Band" + str(nlabel) + 
          pol + sb[nsb] +".Tsys" )
  return [ mpNameList, tsysNameList ]


# --- read the selfcal visibilities (complex, matrix of 16 bands x 15 ants) --- #
def getVisMatrix( mpNameList ) :
  visList = SAC.queryMpValues( mpNameList, nothrow=True )
    # ... retrieve selfcal solutions as a list of [re,im] values
  visComplex = numpy.empty( len(visList), dtype=complex )
  for n in range(0,len(visList)) :
    if (visList[n] == None) :
       visComplex[n] = 0. + 1j * 0.
    else :
       visComplex[n] = float(visList[n][0]) + 1j * float(visList[n][1])
          # [re,im] list -> complex vector; None converted to 0+0j
  return numpy.reshape(visComplex, (16,15) )


# --- read tsys matrix (real, 16 bands x 15 ants) --- #
def getTsysMatrix( tsysNameList ) :
  tsysList = SAC.queryMpValues( tsysNameList, nothrow=True )
  return numpy.reshape( numpy.array( tsysList, dtype=float), (16,15) )
    # ... noHW = None -> nan


# --- (Bool) is this a valid selfcal source, not Noise or Radio Pointing? --- #
def selfcalSource( ) :
  sourceName = SAC.queryString("Control.Subarray1.Source")
  try :
    for n in range (1,33) :
      if ( sourceName == SAC.queryString("Control.SpectralLineCorrelator.Obsblock.ObsObject"+str(n)+".name") ) :
       return SAC.queryBool("Control.SpectralLineCorrelator.Obsblock.ObsObject"+str(n)+".selfCalibratable") 
    return False
  except :
    return False
  

# --- wait until a fresh selfcal solution is available on a valid source --- #
def waitForNewData( ) :
  elapsed = 0.
  # wait until we are integrating on a valid selfcal source (not Noise, not Pointing)
  while not ( selfcalSource( ) and SAC.queryBool("SlPipeline.IntegratorStageContainer.IntegratorStage.integrating") )  : 
    if (elapsed > 20. ) :
      print ".. waiting for integration to begin on a valid selfcal source"
      elapsed = 0. 	# so message is refreshed every 20 sec
    SAC.sleep(1.)
    elapsed = elapsed + 1.
  print " .. waiting for integration to complete"
  elapsed = 0.
  # fresh selfcal data should be available when the integration number changes
  LastIntNumber = SAC.queryInt("SlPipeline.IntegratorStageContainer.IntegratorStage.IntegrationNumber")
  IntNumber = LastIntNumber
  while (IntNumber == LastIntNumber) :
    if (elapsed > 20. ) :
      print " .. waiting for integration to complete"
      elapsed = 0.	# so message is refreshed every 20 sec
    SAC.sleep(0.5)
    elapsed = elapsed + 0.5 
    IntNumber = SAC.queryInt("SlPipeline.IntegratorStageContainer.IntegratorStage.IntegrationNumber")
  SAC.sleep(0.1)	# to be safe, allow extra 0.1 sec to make sure selfcal results are updated
  print " .. fresh selfcal data available"


# --- apply passband, return pbCorrectedVis and normalized antenna vis vector --- #
def applyPb( visMatrix, pbMatrix ) :
  pbCorrectedVis = visMatrix * numpy.conjugate( pbMatrix )
  print "\npassband-corrected phases:"
  print "    C1    C2    C3    C4    C5    C6    C7    C8    C9   C10   C11   C12   C13   C14   C15"
  try :
    print numpy.array_str( numpy.angle(pbCorrectedVis, deg=True ), precision=0, suppress_small=True, max_line_width=200 )
  except :
    print "\nsorry\n"
  for nband in range(0,8) :
    bw = SAC.queryDouble("Control.SpectralLineCorrelator.SlcBand" + str(nband+1) + ".ControlBandPoints.bandwidth")
    if (bw < 125.) :
      pbCorrectedVis[nband][0:15] = 0. 
      pbCorrectedVis[nband+8][0:15] = 0.
        # ... ditch any narrowband windows
  antVec = numpy.nansum( pbCorrectedVis, axis=0 )
  #print "\nvector-averaged antenna phases"
  #print "    C1    C2    C3    C4    C5    C6    C7    C8    C9   C10   C11   C12   C13   C14   C15"
  #print " " + numpy.array_str( numpy.angle(antVec, deg=True ), precision=0, suppress_small=True, max_line_width=200 )
  amps = numpy.abs(antVec)
  for n in range (0,15) :
    if amps[n] == 0. :
       amps[n] = 1.  
  return [ pbCorrectedVis, antVec/amps ]

# --- return offset phases currently in use, as a complex vector; necessary only if script crashes and we restart --- #
def currentOffsetPhaseVec() :
  currentOffsetPhase = numpy.ones( 15, dtype=complex )   # default - all phases are zero
  mpOffsetStateList = []
  mpOffsetPhaseList = []
  for n in range(1,16) :
    mpOffsetStateList.append( "Loberotator.Channel"+str(n)+".offsetPhaseState" )
    mpOffsetPhaseList.append( "Loberotator.Channel"+str(n)+".offsetPhase" )
  OffsetStateList = SAC.queryMpValues( mpOffsetStateList, nothrow=True )
  OffsetPhaseList = SAC.queryMpValues( mpOffsetPhaseList, nothrow=True )
  for n in range(0,15) :
    if OffsetStateList[n] == 0 :
      if OffsetPhaseList[n] != "None" :
        phRadians = math.pi * OffsetPhaseList[n]/180. 
        currentOffsetPhase[n] = math.cos(phRadians) + 1j * math.sin(phRadians)
  return currentOffsetPhase
  

# --- accumulate passband, compute 'merit', save to file (default is passbandFile) --- #
def acumPassband( nacum=5, outfile=passbandFile ):
  [mpNameList, tsysNameList] = makeMpNameLists( )
  vectorSumPbMatrix = numpy.zeros( [16,15], dtype=complex )    # vector sum of passband values
  scalarSumPbMatrix = numpy.zeros( [16,15], dtype=float )	   # scalar (magnitude) passband sum
  npts = 0
  while (npts < nacum) :
    waitForNewData( )
    visMatrix = getVisMatrix( mpNameList )
    vectorSumPbMatrix += visMatrix
    scalarSumPbMatrix += numpy.abs(visMatrix)
    npts += 1
    print "accumulating passband (%d/%d records)" % (npts,nacum)
    meritMatrix = numpy.divide( numpy.abs(vectorSumPbMatrix), scalarSumPbMatrix )   # mag(vector) / sum(mags) 
      # ... some values may be nan if data are missing
    print numpy.array_str( meritMatrix, precision=3, max_line_width=200 )
    maskedMeritMatrix = numpy.ma.array( meritMatrix, mask=(numpy.isnan(meritMatrix) ) )
      # ... mask off the nans
    merit = numpy.ma.mean( maskedMeritMatrix ) 
    print "merit = %.3f" % merit

  # for consistency with mfcal, rotate passband phases to make phase(win1) = 0 for each antenna
  print "\nbefore rotation:"
  print numpy.array_str( numpy.angle(vectorSumPbMatrix, deg=True ), precision=0, suppress_small=True, max_line_width=200 )
  for n in range(0,15) :
    Win1 = vectorSumPbMatrix[0][n]
    if numpy.abs(Win1) > 0. :
      for m in range(0,16) :
        vectorSumPbMatrix[m][n] = vectorSumPbMatrix[m][n] * numpy.conj(Win1)
          # ... don't bother normalizing, as this happens at the very end anyway
  print "\nafter rotation:"
  print numpy.array_str( numpy.angle(vectorSumPbMatrix, deg=True ), precision=0, suppress_small=True, max_line_width=200 )

  print "\nsaving normalized passband to file %s" % outfile
  numpy.save(outfile, vectorSumPbMatrix/numpy.abs(vectorSumPbMatrix) )


# --- compute phase corrections, in radians, based on last 5 measured phases ---- #
def calcPhaseCorr( timeNow, timeHistory, antVecHistory, fudge  ) :
  phaseCorr = numpy.zeros( 15, dtype=float )
  print "\nmeasured antenna phase for last 5 integrations:"
  mtx = []
  for m in range( 0, len(timeHistory) ) :
    mtx.append(antVecHistory[m][0:15])
  mtx = numpy.array_str(numpy.angle(numpy.array(mtx),deg=True), precision=0, suppress_small=True, max_line_width=200 )
  mtx = mtx.split("\n")
  for m in range( 0, len(timeHistory) ) :
    if ( timeHistory[m] > 0. ) :
      #onerow = antVecHistory[m][0:15]
      # print "%8.1f  %s" % ( timeHistory[m]-timeNow, numpy.array_str( numpy.angle(onerow, deg=True), precision=0, 
      print "%8.1f %s" % (timeHistory[m]-timeNow, mtx[m])
  # for now, correction is -0.5 * last measured phase
  mlast = len(timeHistory)
  for n in range(0,15) :
    if numpy.isnan( antVecHistory[mlast-1][n] ) :	
      phaseCorr[n] = 0.
    else :
      phaseCorr[n] = fudge * numpy.angle( antVecHistory[mlast-1][n] )
  # correction = 180.*phaseCorr/math.pi
  # print "correction  %s" % ( numpy.array_str( correction, precision=0, 
  #      suppress_small=True, max_line_width=200 ) )
  return phaseCorr


# --- endless loop of phase corrections; emulating unless apply=True --- #
def bigLoop( apply=False, firstseq=-1, fudge=-0.5 ) :

  if not apply :
    fudge = 0.
  fout = open( phaseHistoryFile, "a" )
  fout.write("\n#### RESTARTING SCRIPT with fudge = %.2f ####\n" % fudge)
  fout.close()
  
  if firstseq == -1 :
    nseq = getLastFrameSeqNo () + 1
  else :
    nseq = firstseq
  print "next movie frame number = ", nseq

  [ mpNameList, tsysNameList ] = makeMpNameLists()
  antVecHistory = numpy.zeros( [5,15], dtype=complex )
    # ... measured antenna phases; 15 complex numbers (normalized to 1) x 5 times
  timeHistory = numpy.zeros( [5], dtype=float )
    # ... times (seconds since the epoch) for antVecHistory entries
  cumOffsetPhase = currentOffsetPhaseVec() 
    # ... 15 complex numbers, normalized to 1
    # ... angles are zero unless we are restarting and phase corrections already are on
  
  print "retrieving passband from file %s" % passbandFile
  pbMatrix = numpy.load( passbandFile )
  #print "\npassband file amplitudes:"
  #print numpy.array_str( numpy.abs(pbMatrix), precision=3, max_line_width=200 )
  print "\npassband file phases:"
  print numpy.array_str( numpy.angle(pbMatrix, deg=True ), precision=0, suppress_small=True, max_line_width=200 )

  while True :

    waitForNewData( )
    tsave = time.time()
    visMatrix = getVisMatrix( mpNameList )
    tsysMatrix = getTsysMatrix( tsysNameList )
    [ pbCorrectedVis, antVec ] = applyPb( visMatrix, pbMatrix )
      # ... antVec = antenna phases after passband correction, as complex numbers
    # numpy.save( lastVisFile, pbCorrectedVis )
    # numpy.save( lastTsysFile, tsysMatrix )
      # ... save current vis,tsys matrices for possible plot
    strtime = time.gmtime()
    timeString = time.strftime("%j  %H:%M:%S",strtime )		# %j = day of year
    utstring = timeString
      # timeString is written on the plot

    # --- save last 5 antVecs  and their times for possible use by fancier correction algorithms --- #
    tmpTimes = numpy.append( timeHistory, tsave )
    timeHistory = numpy.delete( tmpTimes, 0 )
    tmpArray = numpy.append( antVecHistory, [antVec], axis=0 )
    antVecHistory = numpy.delete( tmpArray, 0, 0) 
      # ... add latest values to end of array, delete the first (oldest) entries
    
    phaseCorr = calcPhaseCorr( tsave, timeHistory, antVecHistory, fudge )
	  # ... 15 element float array; phase correction, in radians, computed for each antenna
    if ( apply ) :
      for n in activeAnts :
        cumOffsetPhase[n-1] = numpy.exp(phaseCorr[n-1]*1j) * cumOffsetPhase[n-1]
          # ... rotate cumOffsetPhase vector by phaseCorr; magnitude stays at 1
    cumulative = numpy.angle( cumOffsetPhase, deg=True )
      # ... cumulative phase corrections in degrees
	  # ... note that cumulative remains unchanged if apply=false or ant is inactive

    # --- if apply=True, update loberotator phases --- #
    if (apply) :
      for n in activeAnts :
        lr.setOffsetControl( n, True )
        lr.setOffsetPhase( n, -cumulative[n-1] )

    # --- plot to screen and save frame for movie --- #
    try:
      effic = plotVec( pbCorrectedVis, tsysMatrix, timeString, antList=phasedAnts, nseq=nseq )
      nseq = nseq + 1
    except:
      print "plotting failed" 
      effic = phaseEffic( antVec )

    # --- write diagnostic info to file --- #
    fout = open( phaseHistoryFile, "a" )
    fout.write("\n")
    dailySecs = strtime[3]*3600. + strtime[4]*60. + strtime[5]
	  # ... secs since 0UT, for ease in plotting results
    measPhase = numpy.angle( antVec, deg=True )
    writeLine( fout, utstring, dailySecs, fudge, "meas: ", measPhase, effic )
    writeLine( fout, utstring, dailySecs, fudge, " cum: ", cumulative, -1. )
      # ... note that cumulative phases stay fixed if apply=false or ant is inactive
    fout.close()


# --- this is lame, but I couldn't get numpy.array_str to do what I wanted... --- #
def writeLine( fout, utstring, dailySecs, fudge, label, vec, phasingEffic ) :
  fout.write(" %s %6.0f %4.1f  %s" % (utstring, dailySecs, fudge, label) ) 
  for n in range(0,15) :
    if numpy.isnan(vec[n]) :
      fout.write("  nan")
    else :
      fout.write(" %4d" % vec[n])
  if (phasingEffic >= 0.) :
    fout.write("    %5.3f\n" % phasingEffic )
  else :
    fout.write("\n")

# --- restore normal state, with no loberotator offsets --- #
def cleanup( ):
  for n in range(1,16) :
    lr.setOffsetControl( n, False )
    lr.setOffsetPhase( n, 0. )

# --- compute phasing efficiency from antenna vector --- #
def phaseEffic( antVec, antList=phasedAnts ) :
  scalarSum = 0.
  vectorSum = 0.+0j
  for n in antList :
    scalarSum += numpy.abs(antVec[n-1])
    vectorSum += antVec[n-1]
  return numpy.abs( vectorSum )/scalarSum
    # ... note: phase of vectorSum doesn't matter for vlbi

# create plot of weighted voltage s/n vectors
# for each correlator window, |Vsig| = sqrt(Jy/jyPerK); units sqrt(K)
# what matters is the signal to noise ratio, |Vsig|/sqrt(Tsys); dimensionless
# but the signal/noise vectors are weighted by another 1/sqrt(Tsys) in the beamformer,
#   so plot pbCorrectedVis(Jy)/( |Vsig| * Tsys ); divide by 16 to avg across windows
# this routine also returns the phasing efficiency, defined as abs(vector sum)/scalar sum
#   of the weighted s/n vectors

def plotVec( pbCorrectedVis, tsysMatrix, timeString, antList=phasedAnts, nseq=0 ) :
  hue = ['b','g','r','c','m','y','b','g','r','c','m','y','b','g','r']
  vecList = numpy.zeros( [15,17], dtype=complex )
	# ... list of (x,y) values, as complex numbers, for each vector, beginning with (0,0)
  sourceName = SAC.queryString("Control.Subarray1.Source")
  pylab.ion()
  pylab.clf()

  # create vecList for each antenna
  maxScalarSum = 0. 
  grandScalarSum = 0.   
  grandVectorSum = 0.+0.j

  for n in range(0,15) :
    scalarSum = 0.
    for m in range (0,16) :
      if (numpy.isnan( pbCorrectedVis[m][n] )) or (numpy.abs( pbCorrectedVis[m][n] ) < 1.e-10 ) :
        vecList[n][m+1] = vecList[n][m]    # if nan or 0, repeat last value
      else : 
        scalingFactor = math.sqrt(numpy.abs(pbCorrectedVis[m][n]) * jyPerK[n]) * tsysMatrix[m][n] * 16.
        vecList[n][m+1] = vecList[n][m] + pbCorrectedVis[m][n] / scalingFactor 
        scalarSum += numpy.abs( pbCorrectedVis[m][n] ) / scalingFactor 

    #print "\nvecList for ant %d:" % (n+1)
    #print numpy.array_str( vecList[n], precision=2, max_line_width=200 )

    if (scalarSum > maxScalarSum) :
      maxScalarSum = scalarSum	
        # ... will be used to set plot scale

    if (n+1) in antList :
      grandScalarSum = grandScalarSum + scalarSum
      grandVectorSum = grandVectorSum + vecList[n][16]
         # ... will be used to compute overall phasing effic
    
  if maxScalarSum < .00001 :
    maxScalarSum = .00001
  pylab.axis( [-1.05*maxScalarSum,1.05*maxScalarSum,-1.05*maxScalarSum,1.05*maxScalarSum] )

  # vectorSum = 0.+0.j
  # scalarSum = 0.
  for n in range(0,15) :
    x = numpy.real( vecList[n] )
    y = numpy.imag( vecList[n] )
    color = 'm'
    if (n+1) in antList :
      color = 'r'
      # vectorSum = vectorSum + vecList[n][16]
      # scalarSum = scalarSum + numpy.abs( vecList[n][16] )
    pylab.plot( x, y, color=color, linestyle='solid', linewidth=4 )
    pylab.annotate(str(n+1), [x[15],y[15]] )
  pylab.grid(True)
  pylab.axes().set_aspect('equal')
  # effic = numpy.abs(vectorSum) / scalarSum 
  effic = numpy.abs(grandVectorSum) / grandScalarSum 
  pylab.text( -.92*maxScalarSum, 0.9*maxScalarSum, timeString, size="x-large" ) 
  pylab.text( 0.6*maxScalarSum, 0.9*maxScalarSum, str(effic)[0:5], size="x-large" ) 
  pylab.text( -.92*maxScalarSum, 0.78*maxScalarSum, sourceName, size="x-large" ) 
  pylab.draw()
  plotname = "/home/obs/plambeck/vlbi/2013/movie/d%05d.png" % nseq
  pylab.savefig(plotname)
  return effic

# --- get last frame number in movie directory --- #
def getLastFrameSeqNo () :
  p = subprocess.Popen('ls /home/obs/plambeck/vlbi/2012/movie/*.png | tail -1', \
     stdout=subprocess.PIPE, stdin=subprocess.PIPE, stderr=subprocess.STDOUT, shell=True)
  result = p.communicate()[0]
  l = len(result)
  try :
    lastSeqNo = int(result[l-10:l-5])
  except :
    lastSeqNo = 0
  return lastSeqNo
