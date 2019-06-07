# vlbiPlot.py
#
#import subarrayCommands as SAC
import math
import time
#import device
import cmath
import numpy
import pylab
import sys

lastVisFile = "/home/obs/plambeck/vlbi/2012/lastVis.npy"
lastTsysFile = "/home/obs/plambeck/vlbi/2012/lastTsys.npy"

activeAnts = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]
phasedAnts = [1,2,3,4,5,6]

jyPerK = numpy.empty( 15, dtype=float )
jyPerK[0:6] = 65.
jyPerK[6:15] = 145.

# create plot of weighted voltage s/n vectors
# for each correlator window, |Vsig| = sqrt(Jy/jyPerK); units sqrt(K)
# what matters is the signal to noise ratio, |Vsig|/sqrt(Tsys); dimensionless
# but the signal/noise vectors are weighted by another 1/sqrt(Tsys) in the beamformer,
#   so plot pbCorrectedVis(Jy)/( |Vsig| * Tsys ); divide by 16 to avg across windows

def plotVec( antList=phasedAnts ) :
  hue = ['b','g','r','c','m','y','b','g','r','c','m','y','b','g','r']
  pbCorrectedVis = numpy.load( lastVisFile )
  tsysMatrix = numpy.load( lastTsysFile )
  vecList = numpy.zeros( [15,17], dtype=complex )
	# ... list of (x,y) values, as complex numbers, for each vector, beginning with (0,0)
  maxScalarSum = 0. 
  pylab.ion()
  pylab.clf()
  for n in range(0,15) :
    scalarSum = 0.
    for m in range (0,16) :
      if ( numpy.isnan( numpy.real( pbCorrectedVis[m][n] ) ) or \
           numpy.isnan( numpy.imag( pbCorrectedVis[m][n] ) ) ) :
        vecList[n][m+1] = vecList[n][m]    # if nan, repeat last value
      else : 
        scalingFactor = math.sqrt(numpy.abs(pbCorrectedVis[m][n]) * jyPerK[n]) * tsysMatrix[m][n] * 16.
        #print m,n,pbCorrectedVis[m][n],jyPerK[n],tsysMatrix[m][n],scalingFactor
        vecList[n][m+1] = vecList[n][m] + pbCorrectedVis[m][n] / scalingFactor 
        scalarSum += numpy.abs( pbCorrectedVis[m][n] ) / scalingFactor 
    if (scalarSum > maxScalarSum) :
      maxScalarSum = scalarSum
    # print "\nvecList for ant %d:" % (n+1)
    # print numpy.array_str( vecList[n], precision=2, max_line_width=200 )

  vectorSum = 0.+0.j
  scalarSum = 0.
  pylab.axis( [-1.05*maxScalarSum,1.05*maxScalarSum,-1.05*maxScalarSum,1.05*maxScalarSum] )
  for n in range(0,15) :
    x = numpy.real( vecList[n] )
    y = numpy.imag( vecList[n] )
    color = 'm'
    if (n+1) in antList :
      color = 'r'
      vectorSum = vectorSum + vecList[n][16]
      scalarSum = scalarSum + numpy.abs( vecList[n][16] )
    pylab.plot( x, y, color=color, linestyle='solid', linewidth=4 )
    pylab.annotate(str(n+1), [x[15],y[15]] )
  pylab.grid(True)
  pylab.axes().set_aspect('equal')
  effic = numpy.abs(vectorSum) / scalarSum 
  pylab.text( -.92*maxScalarSum, 0.82*maxScalarSum, str(effic)[0:5], size="xx-large" ) 
  pylab.draw()
  # pylab.savefig('/home/obs/plambeck/vlbi/2012/dandelion.eps')

def endless() :
  while True :
    plotVec( antList=phasedAnts )
    time.sleep(4.)

