import matplotlib
#matplotlib.use('Agg')
import carma
import Subarray
import subarrayControl
import subarrayCommands as SAC
import subprocess
import shlex
import sys
import radioPoint as rp
import numpy
import time
import pylab
from scipy.optimize import curve_fit

# refocus antennas
# begin at present focus
# move steps/2 * stepsize lower, integrate, save, find max

#f0 =  [  1.914,  5.158, -1.304, 3.233, -8.413, 7.292, -6.826, 2.793, -15.403, -16.007, 4.603, 9.508, -2.786, -1.289,  -1.492 ]
      # these are the values we started with on 19 mar 2013
# these are the nominal positions on 20 mar 2013
f0 =  numpy.array( [  1.91,  5.16, -1.30, 3.23, -8.41, 6.50, -6.83, 2.80, -15.40, -15.00, 4.20, 9.51, -2.79, -1.29,  -1.49 ], dtype=float)
print f0

def gaussian(x, mean, spread, amplitude):
  return amplitude * numpy.exp(-(x-mean)**2. / (2. * spread**2.))

def plotamps(zTarg, amps,antennas,fitGaussian=False,plotCurves=True,returnSolutions=False):
  print "Plotting stuff."
  # zTarg is a 2D array holding all focus positions (9 values for all antennas), dimension (9,15).
  # amps is a 2D array holding all actually measured amplitudes (same dimensions as zTarg).
  # fitgaussian is a boolean indicating whether or not we want to fit gaussian curves to the data points yet.

  ##########################################################################
  # NOTE: this function assumes 15 antennas and 9 focus positions for now! #
  ##########################################################################

  # The xVals and yVals lists will be filled with the gaussian data, suitable for plotting.
  yVals = []
  xVals = []
  bestPositions = numpy.ones(len(zTarg[0,:])) * numpy.nan # Initialize the array with NaNs to avoid the values being used without having been set properly...
  if (fitGaussian):
    for i in range(0,len(zTarg[0,:])):
      # Initialize first guess for parameters (mean, spread and amplitude)
      p0 = numpy.array([zTarg[4,i],1.,numpy.max(amps[:,i])])
      try:
        # Fitting takes place here!
        ctemp,vtemp = curve_fit(gaussian, zTarg[:,i], amps[:,i], p0=p0)
        bestPositions[i] = ctemp[0]
        # Fill arrays for possible plotting later on
        xnums = numpy.arange(numpy.min(zTarg[:,i])-2., numpy.max(zTarg[:,i])+2.,(numpy.max(zTarg[:,i]) - numpy.min(zTarg[:,i]))/100.)
        ynums = gaussian(xnums, ctemp[0], ctemp[1], ctemp[2])
        yVals.append(ynums)
        xVals.append(xnums)
      except:
        print "Curve fit failed, ignoring antenna %2d..." % i
        # When fitting does not work, append empty arrays to xVals and yVals - this preserves the order of antennas for the plots.
        yVals.append([])
        xVals.append([])
  if (plotCurves):
    pylab.close('all')
    pylab.ion()
    ffig = pylab.figure(figsize=(12,8))
    for i in range(0,len(zTarg[0,:])):
      xpos = i % 5
      ypos = i / 5
      # Initialize subplot structure here
      arrtemp = ffig.add_subplot(3, 5, i+1)
      arrtemp.set_autoscaley_on(False)
      arrtemp.set_autoscalex_on(False)
      arrtemp.set_xlim([numpy.min(zTarg[:,i])-1.,numpy.max(zTarg[:,i])+1.])
      arrtemp.set_ylim([numpy.min(amps[:,i])-1.,numpy.max(amps[:,i])+1.])
      arrtemp.plot(zTarg[:,i],amps[:,i],'o-')
      avgfoc = numpy.average(zTarg[:,i])
      arrtemp.plot([avgfoc,avgfoc],[-1,1000],'r')
      pylab.title("Antenna " + str(antennas[i]))
      if (fitGaussian):
        arrtemp.plot(xVals[i],yVals[i])
        if (bestPositions[i] != numpy.nan):
          arrtemp.plot([bestPositions[i],bestPositions[i]],[-1,30],'g')
    pylab.draw()
  # Don't return results without having calculated them first...
  if (fitGaussian and returnSolutions):
    return bestPositions

def refocus( antList, outfile, startNominal=True, apply=False ) :
  """
  Syntax:
    refocus( antList, outfile, startNominal=True, apply=False )

  Performs refocusing on all antennas included in 'antList'.
  Allows outputting the results to a text file 'outfile'. 
  If startNominal is chosen 'False', the script starts from the
  most recent focus values (the nominal focus values are fixed,
  hardcoded numbers for the moment).
  If apply=False, measurements will be taken (and output),
  but no changes will be made to the focus of any antenna.

  Example:
    refocus([1,2,3,10,15],"refocus-2013-03-19.txt",startNominal=False,apply=True)
  """
  outfile="/home/obs/plambeck/Focus/focusData"
  offsets = numpy.array( [ -.9, -.6, -.3, 0., .3, .6, .9 ] )
  offsets = numpy.array( [ -3., -2., -1.5, -1., -.5, 0., .5, 1., 1.5, 2.0, 3.0 ] )
  timeout = 20.
  numpy.set_printoptions(precision=2)

# for ease in querying focus values and focus status, make list of monitor point names
  focusNameList = []
  focusStatusNameList = []
  focusOK = []
  ampNameList = []
  for ant in antList :
    if (ant < 7) :
      focusNameList.append( "Ovro%d.Secondary.zPosition" % ant )
      focusStatusNameList.append( "Ovro%d.Secondary.zMovementStatus" % ant )
      focusOK.append( 1 )
    else :
      focusNameList.append( "Bima%d.AntennaCommon.Secondary.focusZ" % (ant-6) )
      focusStatusNameList.append( "Bima%d.AntennaCommon.Secondary.focusState" % (ant-6) )
      focusOK.append( 0 )
    ampNameList.append( 'Astro.Antenna%d.MedianAmp' % ant )

  focusOK = numpy.array( focusOK )
  #print focusNameList
  #print focusStatusNameList
  #print ampNameList
  #print focusOK

# save starting focus values for all ants in subarray
  if startNominal :
    startFocus = numpy.zeros( len(antList), dtype=float )
    j = 0
    for ant in antList:
      startFocus[j] = f0[ant-1]
      j = j + 1
  else :
    startFocus = numpy.array( SAC.queryMpValues( focusNameList, nothrow=True ) )
  print "startfocus : ", startFocus

  fout=open( outfile, "a" )
  fout.write("\n# %s\n" % (time.strftime( "%Y-%b-%d %H:%M", time.gmtime())) )
  elev = SAC.queryDouble("Ovro1.AntennaCommon.Drive.Track.actualElevation", 20)
  fout.write("# elev = %.2f\n" % elev)
  source = SAC.queryString("Control.Subarray1.source", 20)
  fout.write("# source = %s\n" % source)

  lo1 = SAC.queryDouble("Control.Subarray1.loFreq", 20)
  fout.write("# LO1 = %.2f\n#\n" % lo1)
  fout.write("#    ");
  for ant in antList :
    fout.write(" %7d" % ant)
  fout.write("\n")

# now generate array of target focus positions
  zTarg = numpy.empty( [len(offsets), len(antList)], dtype=float )
  amps = numpy.zeros( [len(offsets), len(antList)], dtype=float )

# Preload all focus positions
  for i in range(0, len(offsets)) :
    j = 0
    for ant in antList:
      zTarg[i][j] = startFocus[j] + offsets[i]
      j = j + 1

# now step through the focus ranges, recording selfcal amps
  for i in range(0, len(offsets)) :
    j = 0
    for ant in antList:
      #zTarg[i][j] = startFocus[j] + offsets[i]
      #print "focus %d to %.3f" % (ant, zTarg[i][j])
      SAC.focus( None, None, zTarg[i][j], ant )
      j = j + 1

    print "focus to:", zTarg[i]

    t0 = time.time()
    stillTuning = True
    while ( (time.time() - t0) < timeout) and stillTuning :
      time.sleep(4.)
      zState = numpy.array( SAC.queryMpValues( focusStatusNameList, nothrow=True ), dtype=int )
      print "waiting for : ", (zState - focusOK)
      if numpy.array_equiv( zState, focusOK ) : stillTuning = False

    print "begin integration"
    SAC.integrate( integTime=10., reps=1 )
    time.sleep(1)
    amps[i] = SAC.queryMpValues( ampNameList, nothrow=True )
    print amps
    ######## Plot amps retrieved so far ###########
    plotamps(zTarg,amps,antList)
    ###############################################
    fout.write(" %5.2f" % offsets[i] )
    j = 0
    for ant in antList :
      fout.write(" %7.3f" % amps[i][j] )
      j = j + 1
    fout.write("\n")
 
  bestGaussians = plotamps(zTarg, amps, antList, fitGaussian=True, plotCurves=True, returnSolutions=True)
  print bestGaussians
  bestFocus = numpy.zeros( [len(antList)], dtype=float )

  fout.write("#\n# orig")
  j = 0
  for ant in antList: 
    fout.write(" %7.3f" % startFocus[j] )
    j = j+1
  fout.write("\n")

  fout.write("# gfit")
  j = 0
  for ant in antList: 
    fout.write(" %7.3f" % bestGaussians[j] )
    j = j+1
  fout.write("\n")

  fout.write("# best")
  j = 0
  for ant in antList: 
    bestFocus[j] = numpy.average( zTarg[:,j], weights=amps[:,j] )
    # We now have the weighted average of amplitudes for a best focus estimate,
    # as well as the gaussian fits. Time to judge the gaussian fits and see if they make sense,
    # if so: adopt them if not: stick with the weighted average.
    thresholdFocusChange = 0.7
    if (numpy.abs(bestGaussians[j] - startFocus[j]) < thresholdFocusChange):
      # We'll trust the Gaussian fit
      bestFocus[j] = bestGaussians[j]
    else:
      # Can't trust the Gaussian fit
      print "FOCUS: Antenna: %2d, Current focus: %2.1f, Gaussian fit: %2.1f" % (float(ant),startFocus[j],bestGaussians[j])
      print "FOCUS: Gaussian fit recommends a change in focus above the threshold of %1.1f mm! Not using that value." % thresholdFocusChange
      if (numpy.abs(bestFocus[j] - startFocus[j]) > thresholdFocusChange):
        # Weighted average is too far off as well!
        print "FOCUS: Antenna: %2d, Current focus: %2.1f, Weighted average: %2.1f" % (float(ant),startFocus[j],bestFocus[j])
        print "FOCUS: Weighted average focus is also above threshold!"
        print "FOCUS: Not changing focus."
        bestFocus[j] = startFocus[j]
    if (apply == True):
      SAC.focus( None, None, bestFocus[j], ant )
    fout.write(" %7.3f" % bestFocus[j] )
    j = j+1
  fout.write("\n")

  fout.write("# chng")
  j = 0
  for ant in antList: 
    if (apply == True):
      fout.write(" %7.3f" % (bestFocus[j]-startFocus[j]) )
    else:
      fout.write(" %7.3f" % 0.)
    j = j+1
  fout.write("\n\n")

  fout.close()
  print ""
  print "startFocus = ", startFocus 
  print "bestFocus  = ", bestFocus
  print "change     = ", (bestFocus-startFocus)
    
def restoreFocus( ) :
  antList = SAC.currentAntennaNumbers()
  for ant in antList :
    SAC.focus( None, None, f0[ant-1], ant )
  if focusMoving( antList) :
    print "hey - timed out and a focus is still changing!"  

def pf( source, point=False, startNominal=True, apply=False ) :
  SAC.track( source )
  if point :
    rp.radioPoint( source, type='triangle', timeLimit=5. )
  SAC.intent(source,"S")
  antList = SAC.currentAntennaNumbers()
  SAC.tsys()
  refocus( antList, '/home/obs/plambeck/Focus/focusData', startNominal, apply )
  if not apply :
    restoreFocus( )

# print out which antennas have moving focus motors
# return with code 0 after all are finished
# return with code 1 if an antenna has not finished

def focusMoving( antList ) :
  focusStatusNameList = []
  focusOK = []
  timeout = 20
  for ant in antList :
    if (ant < 7) :
      focusStatusNameList.append( "Ovro%d.Secondary.zMovementStatus" % ant )
      focusOK.append( 1 )
    else :
      focusStatusNameList.append( "Bima%d.AntennaCommon.Secondary.focusState" % (ant-6) )
      focusOK.append( 0 )
  t0 = time.time()
  stillTuning = True
  while ( (time.time() - t0) < timeout) and stillTuning :
    time.sleep(4.)
    zState = numpy.array( SAC.queryMpValues( focusStatusNameList, nothrow=True ), dtype=int )
    print "waiting for : ", (zState - focusOK)
    if numpy.array_equiv( zState, focusOK ) : stillTuning = False
  return stillTuning

