
# @author John Carpenter
# $Id: radioPoint.py,v 1.71 2014/09/29 16:29:33 jmc Exp $
#
# $CarmaCopyright$
#
"""
Perform radio pointing
Required software:
1) mpfit.py
2) numpy python package
"""

import subarrayCommands as commands
import obsdefUtils as utils
import numpy as np
import cmath
import math
import carmaHelpers as helpers
import copy
import time
import mpfit
import datetime as dt
import device
import carma

sp = device.getSignalPath()

CNST = -4.0 * math.log(2.0)
OUTPUT_DIRECTORY = '/home/obs/radioPointData'
RPNT_RESULTS = '%s/rpnt' % OUTPUT_DIRECTORY

def radioPoint(source, type='triangle', tune95=False, timeLimit=7, pntwait=2,
             waitCycles=3, intTime=None, refant=None, stepTriangle=None, stepCross=None, 
             nptsCross=5, antwait=-2, tol=0.1, clear=True, apply=True, fitfwhm=True,
             file=True, antennas=None, indent=1, tmo=500, reconfigure=None):
   """Perform radio pointing on the CARMA antennas
   by grabbing data from the monitor stream. When apply=True (the default)
   the measured offsets are applied incrementally to the mountOffsets.
   Any offsets from the offset() command are cleared at the beginning of
   this function.
   See also: mountoffset, offset

   Params:
    source       : Name of the source to point on. No default.
    type         : Type of pointing pattern. Possible values are "cross" and 
                   "triangle". "cross" splits the array into two sets. 
                   One set stares at the nominal position, while the array set
                   makes an azimuth/elevation cut across thesource. 
                   On a second pass, the two sets are switched.
                   "triangle" moves all antennas togther in a triangle pattern.
                   Default = "triangle".
    tune95       : If true, will tune to 95 GHz for pointing. Default : False.
    timeLimit    : Maximum amount of time in minutes for pointing. This is a bit 
                   misleading since if a pointing iteration is started 
                   before the time limit, then the timeLimit can be exceeded.
    intTime      : Integration in seconds per record. Default = 10 seconds.
    refant       : Reference antenna for the selfcal solution. Default is None,
                   and a suitable antenna is chosen.
    stepTriangle : Pointing offset in arcminutes for triangle pointing offsets.
                   Default = nyquist sample rate of largest antenna.
    stepCross    : Offsets in arceconds between pointing positions on the cross.
                   Default is None and the Nyquist sampling rate is used.
    tol          : The tolerance in fraction FWHM beamwidths needed for 
                   convergence. The offsets between pointing offsets must 
                   change by less than this amount for all antennas. 
                   Default = 0.1.
    nptsCross    : Number of pointings in one leg of the cross. nptsCross should
                   be an odd number. Default = 5
    antwait      : The usual antwait parameter. See "help integrate" for more 
                   information.
    pntwait      : Radio pointing will give up if the last pntwait antennas 
                   do not converge in N integrations, where N = waitCycles.
    waitCycles   : Radio pointing will give up if the last pntwait antennas 
                   do not converge in N integrations, where N = waitCycles.
    clear        : If true, then clear the pointing offsets before starting.
    file         : If != False, then the results are saved to a file. 
                   If file == True, the results are saved in 
                   RPNT_RESULTS_<ant>.dat, where RPNT_RESULTS is defined below.
                   If file is a string, then "file" is interpreted as the 
                   root file name and the output file will be file_<ant>.dat.
    antennas     : A list of antennas to point. 0 -> all antennas in subarray.
    reconfigure  : If True, then maximize the number of widebands for pointing.
                   If False, then do not reset the correlator.
                   If None, then determine if the correlator should be reset 
                   based on the flux density of the pointing source.

    Examples:
     radioPoint('3c273')
     radioPoint('3c273', type='triangle', intTime=4.0)
     radioPoint('3c273', type='cross', tol=0.05) """

   # Track source
   tstart = time.time()
   commands.track(source)
   if clear: commands.offset(0, 0)

   # Keep track of current settings
   subArrayNo = commands.s.getSubarrayNo()
   mp = 'Fault.Subarray%s.driveErrorPreference' % (commands.s.getSubarrayNo(), )
   driveErrorPrefOrig = commands.queryString(mp)
   commands.driveErrorPreference(commands.PREF_NONE)

   # get current block name so we can restore it at the end
   blockName_old = dict()

   corDes = commands.assignedCorrelator(subArrayNo);

   if ( corDes == 'SPECTRAL' ):
       temp = commands.queryString('Control.SpectralLineCorrelator.obsBlockId',5).split('.')
       blockName_old['SL'] = temp
   elif ( corDes == 'WIDEBAND') :
       temp = commands.queryString('Control.WidebandCorrelator.obsBlockId',5).split('.')
       blockName_old['WB'] = temp
   elif (corDes == 'SPECTRAL+WIDEBAND'):
      temp = commands.queryString('Control.SpectralLineCorrelator.obsBlockId',5).split('.')
      blockName_old['SL'] = temp
      temp = commands.queryString('Control.WidebandCorrelator.obsBlockId',5).split('.')
      blockName_old['WB'] = temp
   else :
      raise Exception,'No correlator or unrecognized correlator associated with this subarray: %s' % corDes

   
   #blockName_old = commands.queryString('Control.Subarray%s.obsBlockId' % (commands.s.getSubarrayNo()),5).split('.')
   commands.s.resetProjectAndObsblock()

   # Make sure noise source is off
   commands.noiseoff() 

   # load project correlator config so we can restore it later if we 
   # are not in wideband mode
   oldConfigCorr = utils.getConfigAstroband()

   # get incoming frequency setup in case we need to restore it later
   fsetup  = commands.freqSetup()
   doppler = commands.queryString('Control.Subarray%d.dopplerSource' % commands.subarrayNo)

   # set the frequency to 95 GHz if required
   ttune = -time.time()
   if tune95 : 
      commands.trackMessage('tuning to 3mm for pointing',indent=indent)
      commands.freq(97.5,commands.USB,2.50,'none')

   # Determine number of wideband 
   # nwide and maxwide should always be > 0, but there was a bug
   # in numberofWideBands that made this possible. That is why
   # the error checking is below.
   nwide, maxwide = numberOfWideBands(oldConfigCorr)
   if nwide == 0 or maxwide == 0:
      print 'config = ',oldConfigCorr
      print 'nwide = ',nwide
      print 'maxwide = ',maxwide
      raise Exception,'Error setting nwide or maxwide in radioPoint'

   # Do we need to reconfigure the correlator?
   # Only retune if we need the sensitivity given the flux of the
   # pointing source, and the number of exiting widebands in the corelator
   # configuration.
   isCarma23,astrobands = utils.getCorrelatorMode()

   # EML: nwide numberOfWideBands() returns only bands for the
   # spectral correlator if both correlators are in use, however
   # astrobands returned by getCorrelatorMode() always includes all
   # currently configured correlator bands.  That means that maxsens
   # scripts will always try to reconfigure the correlator, since
   # nastrobands/nwide will always be greater than 1.  

   nslbands = len(sp.getActiveAstroBands(carma.util.CORR_SPECTRAL))
   nwbbands = len(sp.getActiveAstroBands(carma.util.CORR_WIDEBAND))

   if nslbands > 0 and nwbbands > 0:
      nastrobands = nslbands
   else:
      nastrobands = len(astrobands)

   if nastrobands == 0:
      raise Exception, 'No existing correlator config.'
   flux = utils.getSourceFlux(source)
   snro = 2.0 * math.sqrt(nastrobands*500.0)
   snr = flux * math.sqrt(nwide * maxwide)
   needWideBands = (reconfigure == True and nwide < nslbands) or \
                   (snr < snro and math.sqrt(500.0*nastrobands/(nwide*maxwide)) > 1.10)
   if needWideBands:
      if reconfigure == False:
         print 'No resetting correlator since reconfigure = False'
         needWideBands = False
      else:
         print 'We need more widebands for sensitivity'
   elif not tune95:
      print 'Current correlator setup looks fine for pointing'

   # Get a wideband configuration to match the current setup and load it
   doReconfig = tune95 or needWideBands
   if doReconfig:
       print 'Reconfiguring correlator bands'
       if isCarma23:
          commands.configwideastroband( "CARMA23" )
       else:
          commands.configwideastroband( "LL" )
       commands.checkbands()

   ttune += time.time()

   # Determine which antennas to point
   ants = antennas
   if antennas == None or antennas == 0: 
      ants = commands.currentAntennaNumbers()
   else:
      ants = utils.makeListObsdef(antennas)
      c = commands.currentAntennaNumbers()
      for a in ants[:]:  # A [:] is needed ants is modified in the for-loop
         if a not in c: ants.remove(a)

   # Check receiver tuning
   utils.relockReceivers(indent=indent)

   # Point 
   tpoint = -time.time()
   pointingResults = point(source, type=type, timeLimit=timeLimit, intTime=intTime, 
                   refant=refant, stepTriangle=stepTriangle, tol=tol,
                   stepCross=stepCross, nptsCross=nptsCross, antwait=antwait,
                   clear=clear, apply=apply, fitfwhm=fitfwhm, pntwait=pntwait,
                   waitCycles=waitCycles, file=file, antennas=ants, tmo=tmo)
   tpoint += time.time()

   # restore the old correlator configuration
   ttune -= time.time()
   if tune95 :
       if (fsetup[2] > fsetup[0]) :
           commands.freq(fsetup[0],commands.LSB,fsetup[1],doppler)
       else:
           commands.freq(fsetup[0],commands.USB,fsetup[1],doppler)
   if doReconfig:
      print 'Resetting correlator configuration back to original value'
      utils.setConfigband(oldConfigCorr)
   ttune += time.time()

   # Reset project
   if len(blockName_old) == 2 :
       if len(blockName_old['SL']) == 4 :
          commands.s.setAllObsblocks(blockName_old['SL'][0],blockName_old['SL'][1],blockName_old['SL'][2],int(blockName_old['SL'][3]),blockName_old['WB'][0],blockName_old['WB'][1],blockName_old['WB'][2],int(blockName_old['WB'][3]))
       elif len(blockName_old['SL']) == 3 :
          commands.s.setAllObsblocks(blockName_old['SL'][0],blockName_old['SL'][1],"",int(blockName_old['SL'][2]),blockName_old['WB'][0],blockName_old['WB'][1],"",int(blockName_old['WB'][2]))
       else:
          raise Exception, 'Error resetting obsblock name %s in radioPoint. Expecting either 3 or 4 fields, but found %d.' % \
                           (blockName_old['SL'], len(blockName_old['SL']))
   else:
      if 'SL' in blockName_old :
         corr = 'SL'
      else :
         corr = 'WB'

      if len(blockName_old[corr]) == 4 :
         commands.s.setObsblock(blockName_old[corr][0],blockName_old[corr][1],blockName_old[corr][2],int(blockName_old[corr][3]))
      elif len(blockName_old[corr]) == 3 :
         commands.s.setObsblock(blockName_old[corr][0],blockName_old[corr][1],"",int(blockName_old[corr][2]))
      else:
          raise Exception, 'Error resetting obsblock name %s in radioPoint. Expecting either 3 or 4 fields, but found %d.' % \
                           (blockName_old[corr], len(blockName_old[corr]))


   # Return antenna drive error preference to its original state
   commands.driveErrorPreference(driveErrorPrefOrig)

   # Done
   print ''
   print 'That took %.1f seconds' % (time.time() - tstart)
   if doReconfig:
      print '   ... %7.1f sec for pointing' % tpoint
      print '   ... %7.1f sec for tuning/correlator setup' % ttune

   # Return pointing results
   return pointingResults

def grabAstroData(data, antennasAll, sourceInfo, offaz, offel, intTime, 
                  refant=None, niter=None):
   """ Grab visibility data from the Astro based monitor stream. """
   # Helper functions for grabAstroData
   def getBandwidths( astrobands ):
      """ For given astrobands return a list of bandwidths. """

      slPrefix = "Control.SpectralLineCorrelator.Slcband"
      wbPrefix = "Control.WidebandCorrelator.Wbcband"
      bwSuffix = ".ControlBandPoints.bandwidth"
      spmMpPrefix = "SignalPath.Mapping.Astroband%d.Input1" 
      corrBandMpName = spmMpPrefix + ".corrBandNo"
      bwMpNames = []
      for astroband in astrobands: 
         corrDesStr = commands.astrobandCorrelator( astroband )
         corrBandNo = commands.queryInt( corrBandMpName%astroband )
         if corrDesStr == 'SPECTRAL':
            bwMpNames.append( "%s%d%s"%(slPrefix,corrBandNo,bwSuffix) )
         elif corrDesStr == 'WIDEBAND':
            bwMpNames.append( "%s%d%s"%(wbPrefix,corrBandNo,bwSuffix) )
         else:
            raise Exception, 'Invalid corrDesStr: '+ corrDesStr
        
      bwMpValues = commands.queryMpValues( bwMpNames )
      return bwMpValues[:]

   # Initialize
   vis = dict()
   commands.sleep(1)

   # Grab active astrobands currently in this subarray.  
   astrobands = utils.getCurrentAstrobands()

   # Set which antennas to use
   antennas = antennasAll[:]

   sidebands = ['Lsb', 'Usb']
   polarizations = ['Left', 'Right']

   # Set the number of windows to the maximum grid size, anything later found
   # to be unused (e.g. ssb and/or single pol) has it's 'use' flag set to False.
   nwindows = len( astrobands ) *  len( sidebands ) * len( polarizations )

   # Determine maximum bandwidth
   bw = getBandwidths( astrobands )
   maxbandwidth = max( bw )

   # Read monitor points
   for itel in range(len(antennas)):
      # Skip antenna if it is not currently in the subarray
      tel = antennas[itel]
      if tel not in commands.currentAntennaNumbers(): continue

      # Initialize
      window = -1
      diameter = getDiameter(antennas[itel])
      amp  = np.zeros(nwindows)
      pha  = np.zeros(nwindows)
      use  = [False] * nwindows
      wt   = np.zeros(nwindows)

      # Loop over bands
      for ib in range(len(astrobands)):
         # Valid bandwidth?
         ab = astrobands[ib]
         if bw[ib] < maxbandwidth - 1.0: continue
         bandwidth = bw[ib]
            
         mpAstroPrefix = 'Astro.Antenna%d.Band%d'%(tel,ab)

         # Loop over sidebands
         for sb in sidebands:

            for pol in polarizations:

                # Increment counter
                window += 1

                mpFullPrefix = '%s.%sPol.%s' % (mpAstroPrefix, pol, sb)

                # First check to see if valid MP is itself valid, if not 
                # valid or subsequent value itself is false, mark 'use' false.
                mpValid = '%s.SelfCal.valid' % (mpFullPrefix)
                validMp = commands.queryMonitorPoint( mpValid )
                if not validMp[0].isValid() or validMp[0].value() == False:
                   use[window] = False 
                   continue

                # Should have good valid mps for real windows from here on out

                mpTsys = '%s.Tsys' % (mpFullPrefix)
                mpVis = '%s.SelfCal.antVis' % (mpFullPrefix)

                # Read tsys and antenna-based visibility from monitor system.
                tsys = commands.queryDouble(mpTsys)
                v  = commands.queryComplex(mpVis)
                vc = complex(v[0], v[1])

                # Save data
                if tsys > 0.0 and tsys < 10000.0 and abs(vc) > 0.0:
                   sigma = 0.03 * tsys / diameter**2 * \
                           math.sqrt(500.0/bandwidth) * math.sqrt(60.0/intTime)
                   wt[window] = 1.0/(sigma*sigma)
                   use[window] = True
                   amp[window] = abs(vc)
                   pha[window] = math.atan2(vc.real, vc.imag) / math.pi * 180.0

      # Save data for this antenna
      result = dict()
      result['amp']  = amp
      result['pha']  = pha
      result['wt']   = wt
      result['use']  = use[:]
      result['offaz'] = offaz[itel]
      result['offel'] = offel[itel]
      result['niter'] = niter
      if not data.has_key(itel): data[itel] = list()
      data[itel].append(result)

def getTimeStamps():
   """ Get various time stamps needs for the output files """

   # Initialize
   results = dict()

   # UT time
   ut = utils.getUT(pointing=True).split()
   results['utday'] = ut[0]
   results['ut'] = float(ut[1])

   # year/month/day/second
   utStamp = time.gmtime()
   utHour  = maybeAddAZero(utStamp[3])
   utMin   = maybeAddAZero(utStamp[4])
   utSec   = maybeAddAZero(utStamp[5])
   results['timeLab'] = ''.join([commands.yearMonthDay(),'_',utHour,utMin,utSec])

   # Done
   return results


def recordMountOffsetsSession(results, timestamps, antennas):
   # keep a record of the corected data: get a time label
   fd = open(''.join([OUTPUT_DIRECTORY,'/Offsets/mountOffset.',timestamps['timeLab']]),'w')
   for ant in range(1,24):
       mazOff = 0.0
       melOff = 0.0
       if ant in antennas:
           if not results.has_key(ant): 
              mazOff, melOff = grabMountOffsets(ant)
              mazOff = mazOff[0]
              melOff = melOff[0]
           else : 
              mazOff = results[ant][2]
              melOff = results[ant][3]
       fd.write("%d %f %f \n" % (ant, mazOff, melOff))
   fd.close()


def recordMountOffsetsHistory(results, timestamps, sourceInfo):
   # Open file
   fd = open(''.join([OUTPUT_DIRECTORY,'/carma_pointing.mount']),'a')

   # Save good fits
   for ant, values in results.items():
      # Get az/el for antenna
      mpAz = utils.getAntennaMp(ant) + ".AntennaCommon.Drive.Track.actualAzimuth"
      mpEl = utils.getAntennaMp(ant) + ".AntennaCommon.Drive.Track.actualElevation"
      antaz = commands.queryDouble(mpAz)
      antel = commands.queryDouble(mpEl)

      # Record results
      fd.write("%7s%9.3f%10.3f%10.3f%10.3f%10.3f%10.3f %2d %-8s %5.1f\n" % \
             (timestamps['utday'], timestamps['ut'], antaz, antel, 
              values[2], values[3], 0.0, ant, sourceInfo['name'].upper(), 
              sourceInfo['lofreq']))

   # close file
   fd.close()


def recordPointingOffsetsHistory(results, timestamps, sourceInfo):
   # Open file
   fd = open(''.join([OUTPUT_DIRECTORY,'/carma_pointing.auto']),'a')

   # Save good fits
   for ant, values in results.items():
      # Get az/el for antenna
      mpAz = utils.getAntennaMp(ant) + ".AntennaCommon.Drive.Track.actualAzimuth"
      mpEl = utils.getAntennaMp(ant) + ".AntennaCommon.Drive.Track.actualElevation"
      antaz = commands.queryDouble(mpAz)
      antel = commands.queryDouble(mpEl)

      # Record results
      fd.write("%7s%9.3f%10.3f%10.3f%10.3f%10.3f%10.3f %2d %-8s\n" % \
             (timestamps['utday'], timestamps['ut'], antaz, antel, 
              values[0], values[1], 0.0, ant, sourceInfo['name'].upper()))

   # close file
   fd.close()


def numberOfWideBands(config=None):
    """ Returns number of wide bands and the maximum bandwidth
        in the current configuration.

        config: Correlator configuration. If None, then use current
                correlator configuration returned in the format
                of getConfigAstroband().

        config = getConfigband()
        nwide, maxbandwidth = numberofWideBands(config)
    """
    # Get correlator configuration
    c = config
    if c == None: 
        c = utils.getConfigAstroband()

    # Determine if we have both wideband and spectral line astrobands. 
    # If we do, we return nwide & maxbandwidth for sl only since 
    # this is the correlator which will be attached to all ants.
    astrobands = [ abc[0] for abc in c ]
    if len( astrobands ) == 0:
        raise Exception, "No existing astroband configuration."
    if max( astrobands ) > 8 and min( astrobands ) < 9:  
        astrobands = [ ab for ab in astrobands if ab < 9 ]

    # Check bandwidth
    nwide = 0
    maxbandwidth = 0
    for t in c:
       astroband = t[0]
       # Skip band if it is not being used or is not in astroband list above.
       mp = commands.queryString('SignalPath.Mapping.Astroband%d.confTag' % (astroband) )
       if mp == 'NONE' or astroband not in astrobands: continue

       # Get bandwidth
       if t[2] == commands.BW500:
          bw = 500
       elif t[2] == commands.BW250:
          bw = 250
       elif t[2] == commands.BW125:
          bw = 125
       elif t[2] == commands.BW62:
          bw = 62
       elif t[2] == commands.BW31:
          bw = 31
       elif t[2] == commands.BW8:
          bw = 8
       elif t[2] == commands.BW2:
          bw = 2
       else:
          raise Exception, 'Could not find bandwith for '+str(t[2])

       # Maximum?
       if bw > maxbandwidth: 
          maxbandwidth = bw
          if utils.isDualPol( astroband ):
             nwide = 2 
          else:
             nwide = 1
       elif bw == maxbandwidth:
          if utils.isDualPol( astroband ):           
             nwide += 2 
          else:
             nwide += 1

    return nwide, maxbandwidth


def getArea(antenna):
   """ Returns effective area of an antenna in meter^2,
       including an aperture efficiency.
   """
   diameter = getDiameter(antenna)
   if antenna >=1 and antenna <= 6: 
      efficiency = 0.5
   elif antenna >=7 and antenna <= 15: 
      efficiency = 0.6
   elif antenna >=16 and antenna <= 23: 
      efficiency = 0.6
   else:
      raise Exception,'Unregonized antenna type'
   area = math.pi * diameter*diameter / 4.0 * efficiency

   return area


def getDiameter(antenna):
   """ Returns the diameter of an antenna in meters """
   if antenna >=1 and antenna <= 6: 
      diameter = 10.4
   elif antenna >=7 and antenna <= 15: 
      diameter = 6.1
   elif antenna >=16 and antenna <= 23: 
      diameter = 3.5
   else:
      raise Exception,'Invalid antenna number : '+str(antenna)
   return diameter


def getFWHM(antenna, freq):
   """ Returns FWHM in arcminutes of an antenna for the specified frequency in GHz. """
   diameter = getDiameter(antenna)
   lam = 299792458.0 / (freq * 1e9)
   fwhmo = lam / math.pi * 180.0 * 60.0
   fwhm = 1.22 * fwhmo / diameter
   return fwhm


def makeRow(step,points) :
   """ Generates pointing pattern for the cross """
   edge_ = float((points-1.0)/2.0)
   row_ = []
   for j in range(points) :
       rowt = []
       for i in range(len(step)) :
           rowt.append(float(-edge_*step[i]+j*step[i]))
       row_.append(rowt)
   return row_


def maybeAddAZero(xS) :
    if (xS < 10) : return '0'+str(xS)
    else : return str(xS)


def calibrateData(data, cal, antennas, sourceInfo, file=True, niter=None):
   """ Takes spectra and produces band-averaged visibilities """
   # Loop over data
   for iant, dant in data.items():
      # Write results to a file
      writeOutputFile = False
      if file != False and file <> None:
         # Set file name
         writeOutputFile = True

         # date the output file to avoid having to parse huge files later on
         today = dt.date.today()
         dateStr = "%i%02i%02i" % (today.timetuple()[0], today.timetuple()[1], today.timetuple()[2]) 
         
         if file == True:
            outputFileRoot = '%s_%.2d_%s.dat' %  (RPNT_RESULTS, antennas[iant], dateStr)
         else:
            outputFileRoot = "%s_%.2d_%s.dat" % (file, antennas[iant], dateStr)

         # Open file
         fout = open(outputFileRoot, "a")
         fout.write("# Pointing data for antenna %d : %s\n" % (antennas[iant], time.asctime()))
         f=commands.freqSetup()
         fout.write("# Rest Frequency : %d\n" % f[0])
         fout.write("# UT : %s\n" % utils.getUT(timestamp=True))
         fout.write("# Source %s\n" % sourceInfo['name'])
         fout.write("#\n");
         fout.write("# Iter  offset(az) offset(el)      Amp         sigma          Az          El\n");
         fout.write("#         (arcmin)   (arcmin)      (Jy)         (Jy)       (deg)       (deg)\n");

         # Get az/el
         mpAz = utils.getAntennaMp(antennas[iant]) + ".AntennaCommon.Drive.Track.actualAzimuth"
         mpEl = utils.getAntennaMp(antennas[iant]) + ".AntennaCommon.Drive.Track.actualElevation"
         antaz = commands.queryDouble(mpAz)
         antel = commands.queryDouble(mpEl)

      # Initialize
      cal[iant] = list()

      # Compute mean amplitude
      for d in dant:
         # Initialize
         sum  = 0.0
         sumw = 0.0
         nwindows = len(d['use'])
         weights = np.zeros(nwindows)

         # Compute weighted average
         x = []
         for i in range(nwindows):
            if d['use'][i]:
               sum  += d['amp'][i] * d['wt'][i]
               sumw += d['wt'][i]
               x.append(d['amp'][i])

         # Save data
         result = dict()
         if sumw > 0.0:
            # result['amp']   = sum / sumw
            x = sorted(x)
            n1 = len(x) / 2
            n2 = (len(x)-1)/ 2
            result['amp'] = 0.5 * (x[n1] + x[n2])
            result['fwhm']  = getFWHM(antennas[iant], sourceInfo['lofreq'])
            result['offaz'] = d['offaz']
            result['offel'] = d['offel']
            result['sigma'] = 1.0 / math.sqrt(sumw)
            cal[iant].append(result)

            # Write data
            if writeOutputFile and (niter == None or niter == d['niter']):
               fout.write("%6s %10.3f  %10.3f  %10.3f  %10.3f  %10.3f  %10.3f\n" % \
                  (str(d['niter']), result['offaz'], result['offel'], result['amp'], result['sigma'], antaz, antel))

      # Close file
      fout.close()


def grabPointingOffsets(antennas, clear=False):
   """ Grabs the current pointing offsets from the monitor stream """
   # Initialize
   offsetsAz = [0.0] * len(antennas)
   offsetsEl = [0.0] * len(antennas)
   if clear: return offsetsAz, offsetsEl

   # Read monitor points
   for i in range(len(antennas)):
      mpAz = utils.getAntennaMp(antennas[i]) + ".AntennaCommon.Drive.Point.offsetAz"
      mpEl = utils.getAntennaMp(antennas[i]) + ".AntennaCommon.Drive.Point.offsetEl"
      offsetsAz[i] = commands.queryDouble(mpAz)
      offsetsEl[i] = commands.queryDouble(mpEl)

   # Done
   return offsetsAz[:], offsetsEl[:]


def grabMountOffsets(antennas):
   """ Grabs the current pointing offsets from the monitor stream """
   # Initialize
   ant = utils.makeListObsdef(antennas)
   mountAz = [0.0] * len(ant)
   mountEl = [0.0] * len(ant)

   # Read monitor points
   for i in range(len(ant)):
      mpAz = utils.getAntennaMp(ant[i]) + ".AntennaCommon.Drive.Point.mountOffsetAz"
      mpEl = utils.getAntennaMp(ant[i]) + ".AntennaCommon.Drive.Point.mountOffsetEl"
      mountAz[i] = commands.queryDouble(mpAz)
      mountEl[i] = commands.queryDouble(mpEl)

   # Done
   return mountAz[:], mountEl[:]


def observeOffsets(antennas, sourceInfo, offsetsAz, offsetsEl, 
                   data, xoff, yoff, intTime, antwait=-2, niter=None):
   """ Observers a triangle pointing pattern. """
   # Loop over offsets
   for i in range(len(xoff)):
      # Set offsets for all antennas
      offaz = [0.0] * len(antennas)
      offel = [0.0] * len(antennas)
      for j in range(len(offsetsAz)):
         offaz[j] = offsetsAz[j] + xoff[i][j]
         offel[j] = offsetsEl[j] + yoff[i][j]

      # Increment offsets.
      for i in range(len(antennas)):
         if antennas[i] in commands.currentAntennaNumbers():
            commands.offset(offaz[i], offel[i], antennas[i])

      # Integrate
      commands.integrate(intTime, 1, antwait=antwait)

      # Grab the data
      grabAstroData(data, antennas, sourceInfo, offaz, offel, intTime, niter=niter)

   # Go back to original offsets
   for itel in range(len(antennas)):
      if antennas[itel] in commands.currentAntennaNumbers():
         commands.offset(offsetsAz[itel], offsetsEl[itel], antennas[itel])


def takeData(data, type, antennas, sourceInfo, offsetsAz, offsetsEl, 
             intTime=None, nptsCross=5, stepTriangle=None, stepCross=None, 
             antwait=-2, center=False, niter=None):
   """ Generates pointing offsets for each of the pointing patterns. """
   if type == 'cross':
      # Set offset 
      if stepCross == None: 
         step = np.zeros(len(antennas))
         for i in range(len(antennas)):
            step[i] = 0.5 * getFWHM(antennas[i], sourceInfo['lofreq'])
      else:
         step = np.array([stepCross] * len(antennas))

      # Set offsets along a row
      row = makeRow(step, nptsCross)

      # Create X row
      xoff = []
      yoff = []
      for r in row:
         xoff.append(np.zeros(len(antennas)))
         yoff.append(np.array(r))
      for r in row:
         xoff.append(np.array(r))
         yoff.append(np.zeros(len(antennas)))
   elif type == 'triangle':
      # Set offset 
      if stepTriangle == None: 
         step = np.zeros(len(antennas))
         for i in range(len(antennas)):
            step[i] = 0.5 * getFWHM(antennas[i], sourceInfo['lofreq'])
      else:
         step = np.array([stepTriangle] * len(antennas))

      # Set position of triangle points
      xoff = []
      yoff = []
      if center:
        xoff.append(0.0 * step)
        yoff.append(0.0 * step)
      for i in range(0,3):
         angle = (90.0 + 120.0 * i) / 180.0 * math.pi
         xoff.append(step * math.cos(angle))
         yoff.append(step * math.sin(angle))
   else:
      raise Exception,'Unknown pointing pattern: '+type

   # Make data
   observeOffsets(antennas, sourceInfo, offsetsAz, offsetsEl, data, xoff, yoff, intTime, antwait=antwait, niter=niter)


def antennaResponse(peak, xoff, yoff, fwhm):
   """ Returns response of the antenna for a source with PEAK
       flux with pointing offsets XOFF and YOFF with beam FWHM.
   """
   return peak * np.exp(CNST / (fwhm*fwhm) * (xoff*xoff + yoff*yoff)) 


def myfunct(p, fjac=None, data=None):
   """ Function for mpfit that generates the difference between the
       observed fluxes and model flux.
   """
   # Create model
   obs   = np.zeros(len(data))
   err   = np.zeros(len(data))
   model = np.zeros(len(data))
   i = 0
   for d in data:
      # Indices for offsets
      model[i] = antennaResponse(p[0], d['offaz']-p[1], d['offel']-p[2], p[3])
      obs[i]   = d['amp']
      err[i]   = d['sigma']
      i += 1

   # Compute deviates
   status = 0
   return [status, (obs-model)/err]


def point(source, type='cross', timeLimit=7, intTime=None, refant=None, 
          stepTriangle=None, stepCross=None, nptsCross=5, antwait=-2, tol=0.1,
          clear=True, apply=True, fitfwhm=True, pntwait=2, waitCycles=3,
          file=True, antennas=None, tmo=500):
   """ Driver that iterates on the pointing.
       The derived offsets must be SUBTRACTED from the mount offsets.
   """
   # Set source information
   sourceInfo = dict()
   sourceInfo['lofreq'] = commands.freqSetup()[2]
   sourceInfo['name'] = source
   sourceInfo['flux'] = utils.getSourceFlux(source, freq=sourceInfo['lofreq'])
   if sourceInfo['flux'] == None or sourceInfo['flux'] <= 0.0: 
      sourceInfo['flux'] = 1.0

   # Set integration time, if needed
   if intTime == None:
      intTime = 10.0
      if sourceInfo['flux'] > 5.0 and sourceInfo['lofreq'] < 150.0: intTime = 4

   # Make initial guess to parameters
   # Offsets are increments to the mount offsets
   p0 = [sourceInfo['flux'], 0.0, 0.0, 1.0]

   # Set parameters
   parbase={'value':0., 'fixed':0, 'limited':[0,0], 'limits':[0.,0.]}
   parinfo = []
   for i in range(len(p0)):
      parinfo.append(copy.deepcopy(parbase))
   for i in range(len(p0)):
      parinfo[i]['value'] = p0[i]

   # Before proceeding, make sure we are on source
   commands.wait(commands.TRACK, tmo=tmo, waiton=commands.ALL)

   # Initialize. The initial pointing offsets are grabbed as well.
   # Do not trust "clear=True" to actually set the pointing offsets
   # to zero. There is a delay.
   converged = [False] * len(antennas)
   niter = 0
   tstart = time.time()
   offsetsAz, offsetsEl = grabPointingOffsets(antennas,clear=clear)
   data = dict()

   # Observe
   continuePointing = True
   ncyclesWait = 0
   while continuePointing and not min(converged) and \
         time.time() - tstart < timeLimit*60.0:
      # Increment
      niter += 1
      print 'Iteration %2d' % niter

      # Uncomment line below to prevent accumulate data from 
      # one iteration to the next
      # data = dict()

      # Observe center position if fitting 4 parameters and doing a triangle
      center = (len(data) == 0 and type == 'triangle' and fitfwhm)

      # Observe
      print '   ... collecting the data'
      t1 = - time.time()
      takeData(data, type, antennas, sourceInfo, offsetsAz, offsetsEl, 
               nptsCross=nptsCross, stepTriangle=stepTriangle, 
               stepCross=stepCross, antwait=antwait, intTime=intTime,
               center=center, niter=niter)
      fa = {'data': data}
      t1 += time.time()
      print '   ... that took %.1f seconds' % t1

      # Calibrate data and return structure that contains data to be fitted
      cal = dict()
      calibrateData(data, cal, antennas, sourceInfo, file=file, niter=niter)

      # Compute new offsets
      new_az = np.zeros(len(antennas))
      new_el = np.zeros(len(antennas))
      d_az   = np.zeros(len(antennas))
      d_el   = np.zeros(len(antennas))
      fwhm   = np.zeros(len(antennas))
      ndata  = np.zeros(len(antennas))
      validFit = [False] * len(antennas)
      for itel, values in cal.items():
         # Set initialize guesses
         maxoffset = 0.5 * getFWHM(antennas[itel], sourceInfo['lofreq'])
         parinfo[1]['value'] = offsetsAz[itel]
         parinfo[2]['value'] = offsetsEl[itel]
         parinfo[3]['value'] = getFWHM(antennas[itel], sourceInfo['lofreq'])
         if not fitfwhm: parinfo[3]['fixed'] = 1
         parinfo[1]['limited'] = [1,1]
         parinfo[2]['limited'] = [1,1]
         parinfo[3]['limited'] = [1,0]
         parinfo[1]['limits'] = [-maxoffset+offsetsAz[itel], maxoffset+offsetsAz[itel]]
         parinfo[2]['limits'] = [-maxoffset+offsetsEl[itel], maxoffset+offsetsEl[itel]]
         parinfo[3]['limits'] = [0.001, 10.0 * parinfo[3]['value']]

         # Fit the data
         ndata[itel] = len(values)  # May be modified below if antenna is now offline

         if len(values) >= len(parinfo):
            fa = {'data': values}
            m = mpfit.mpfit(myfunct, parinfo=parinfo, functkw=fa, quiet=1)
            if m.status <= 0:
               print 'error message = ', m.errmsg
            else:
               validFit[itel] = True
            # elif m.dof > 0:
            #    validFit[itel] = True

         # Store results if valid fit
         if validFit[itel]:
            # Computed reduced chi-squared
            # redchisq = (myfunct(m.params, data=values)[1]**2).sum() / m.dof

            # print '\nFitting for antenna %2d:' % antennas[itel]
            # for v in values:
            #     print '   %8.3f   %8.3f %8.3f' % (v['amp'], v['offaz'], v['offel'])
            # print 'Fit = %.2f %.2f' % (m.params[1], m.params[2])
         
            # Compute change
            new_az[itel] = m.params[1]
            new_el[itel] = m.params[2]
            fwhm[itel]   = m.params[3]
            d_az[itel]   = m.params[1] - offsetsAz[itel]
            d_el[itel]   = m.params[2] - offsetsEl[itel]

            # Save new offsets
            offsetsAz[itel] = m.params[1]
            offsetsEl[itel] = m.params[2]

      # Check convergence
      print '    %4s   %7s %7s   %7s %7s  %7s' % \
            ('Ant', 'fit_az',  'fit_el',  'daz', 'del', 'FWHM')
      print '    %4s   %7s %7s   %7s %7s' % \
            ('', 'arcmin',  'arcmin',  'arcsec', 'arcsec')
      antennasComplete = converged[:]
      for i in range(len(antennas)):
         # Is the antennas in the subarray?
         inSubarray = antennas[i] in commands.currentAntennaNumbers()

         # Check convergence
         beamSize = getFWHM(antennas[i], sourceInfo['lofreq'])
         deltaBeam = np.sqrt(d_az[i]**2 + d_el[i]**2) / beamSize
         deltaFwhm = abs(beamSize - abs(fwhm[i]))
         converged[i] = (deltaBeam <= tol and deltaFwhm < 0.3 * beamSize and 
                         validFit[i] and inSubarray)

         # Antenna is considered done if it is no longer in the subarray
         # I set ndata to -1 to distingsuish it 
         if not inSubarray: ndata[i] = -1
         if not inSubarray or converged[i]: antennasComplete[i] = True

         # Print message
         s = ''
         if validFit[i] and converged[i]: 
            s = '*** converged '
         elif not validFit[i]:
            s = '*** invalid fit '
         if not inSubarray and not converged[i]:
            s = '*** antenna removed from subarray'
         if validFit[i]:
            print '    %4d   %7.2f %7.2f   %7.2f %7.2f  %7.2f  %s' % \
                  (antennas[i], new_az[i], new_el[i], 
                  d_az[i]*60.0, d_el[i]*60.0, fwhm[i]/beamSize, s)
         else:
            print '    %4d   %7.2f %7.2f   %7s %7s  %7s  %s' % \
                  (antennas[i], new_az[i], new_el[i], "", "", "", s)

      # See if we are almost done
      if antennasComplete.count(False) <= pntwait:
         ncyclesWait += 1
      else:
         ncyclesWait = 0

      # Did we reach the time limit?
      if min(antennasComplete) == False and time.time() - tstart > timeLimit*60.0:
         print '\n\n*** Time limit reached on radio pointing ***'
         continuePointing = False
      elif min(antennasComplete) == False and len(np.nonzero(ndata == 0)[0]) == antennasComplete.count(False): 
         print '\n\n*** Stopping pointing since some antennas do not have valid data ***'
         continuePointing = False
      elif ncyclesWait >= waitCycles:
         if antennasComplete.count(False) > 0:
            print '\n\n*** Last %d antennas are having trouble converging - will abort pointing. ***' % antennasComplete.count(False)
         continuePointing = False
 

   # Update the mount offsets in the system
   results = dict()
   mountOffsetAz, mountOffsetEl = grabMountOffsets(antennas)
   print ''
   sup = []
   snoup = []
   for i in range(len(antennas)):
      if converged[i]:
         sup.append(antennas[i])
         aznew = mountOffsetAz[i] + offsetsAz[i]
         elnew = mountOffsetEl[i] + offsetsEl[i]
         if apply and antennas[i] in commands.currentAntennaNumbers(): 
            commands.s.mountOffset(aznew, elnew, antennas[i])
         results[antennas[i]] = [offsetsAz[i], offsetsEl[i], aznew, elnew]
      else:
         snoup.append(antennas[i])
   commands.offset(0, 0)
   print ''
   if not apply:
      print '*** Did not apply offsets since apply=False'
   else:
      if len(sup) > 0:
         print '*** Updated pointing on antennas ',str(sup)
      else:
         print '*** Pointing did not converge on any antennas'
      if len(snoup) > 0:
         print '*** Did not update pointing on antennas ',str(snoup)
      else:
         print '*** Pointing converged on all antennas.'

   # Record various results in files
   resultsTime = getTimeStamps()
   recordMountOffsetsSession(results, resultsTime, antennas)
   recordMountOffsetsHistory(results, resultsTime, sourceInfo)
   recordPointingOffsetsHistory(results, resultsTime, sourceInfo)

   # Return solution
   return results
