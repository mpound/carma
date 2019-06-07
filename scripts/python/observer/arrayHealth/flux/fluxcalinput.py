"""
Flux calibration script input

"""
import subarrayCommands as sac
import obsdef2 as od

#---------------------------------------------------------------------------------
#----- set variables depending on the configurations
maxElevation = 80.0
config = sac.queryString('Control.Subarray1.configName')
if config == 'E':
    minElevation = 35.0
elif config in ['A','B','C','D']:
    minElevation = 30.0
else:
    raise Exception, "Array configuration wasn't looked up sucessfully"

#---------------------------------------------------------------------------------
# #################################################
# Set variables
# #################################################
# Pointing options
pointing = {
   'doPointNight' :  True, # If True, then perform pointing during nightime
   'doPointDay'   :  True, # If True, then perform pointing during daytime
   'doOptPoint'   :  True, # If True, then perform optical instead of radio pointing
   'intervalNight':   4.0, # [hours]  How often to point during nighttime
   'intervalDay'  :   2.0, # [hours]  How often to point during daytime
   'intervalOpt'  :     0, # [hours]  How often to perform optical pointing
   'minflux'      :  2.00, # [Jy]     Minimum pointing flux
   'maxsep'       :    50, # [degree] Maximum distance from science target
   'preferred'    :  None, # Preferred pointing source
   'nrepInt'      :     1, # Number of repeat observations per position
   'mapPoints'    :     5, # Make a strip in az/el with mapPoints positions
   'tune95'       : False, # Tune to 95 GHz before pointing
   'sunriseset'   :  True, # If True, point immediately after sunrise/sunset
  }

# Set various limits for observations
# minElevationCal  : Minimum elevation for flux/passband/pointing sources
# maxElevationCal  : Maximum elevation for flux/passband/pointing sources
# trackingThreshold: Data are flagged if tracking error is larger than limit.
# record           : Record length for integrations (except noise source)
# tmoTrack         : Timeout when acquiring a source
# tsys             : Time between full tsys measurements
# antwait          : Number of antennas to arriveon source before integrating
limits = {
   'minElevationCal'   :  35.0, # [degrees]
   'maxElevationCal'   :  80.0, # [degrees]
   'trackingThreshold' :   0.1, # [beamwidths]
   'record'            : 10.00, # [seconds]
   'tmoTrack'          : 500.0, # [seconds]
   'tsys'              :  10.0, # [minutes]
   'antwait'           :    -2,
  }

# Noise source parameters
noise = {
   'tint'   :    2, # [seconds] Integration time for noise source
   'record' :    2, # [seconds] Record length for noise integration only
  }
#---------------------------------------------------------------------------------
#---- These variables won't affect running the script, just for setting ----------
# #################################################
# Set fake variables
# #################################################
# Tuning options
tuning = {
   'restfreq' : 87.65,  # [GHz] Line rest frequency
   'sideband' : sac.USB,    # Sideband for first LO (LSB or USB)
   'IFfreq'   : 2.65,   # [GHz] IF frequency
  }

# Correlator configuration for calibrators
# reconfig: Sets correlator configuration for calibration observations
#     None            : same as for science targets
#     od.CORR_BW500LO6: 500 MHz, non-overlapping bands in BIMA IF
#     od.CORR_BW500LO : 500 MHz, non-overlapping bands (not recommended for 3mm)
#     od.CORR_BW500   : change bands to 500 MHz without changing IF
# hybrid:   Correlator configurations to calibrate band offsets (see FAQ page!).
#           Needed only if all bands have width < 500 MHz.
# tintHybrid : integration time for each of the hybrid correlator modes
correlator = {
   'reconfig'   : None,
   'hybrid'     : None,
   'tintHybrid' : 5.0,
  }

#mes for phase calibrators and science targets.
# Enter multiple science targets as 'target' : 'ABAur, GMAur',
# Enter multiple phase calibrators as 
#     'phaseCal' : [ '3c111     1:00:00  4:00:00',
#                    '0530+135  3:30:00  7:00:00'],
# tintTarget: Integration time per cycle **per pointing** on science targets
# tintPhase : Phase calibrator integration time per cycle
# callist   : Used to search for bright phase calibrators. See online manual
sources = {
   'target'       : 'mwc349',
   'phaseCal'     : None,
   'tintTarget'   : 5.00, # [minutes] time per cycle *** per pointing ***
   'tintPhaseCal' :  0.00, # [minutes]
   'callist'      : None,
  }

# Mosaic options
mosaic = {
   'doMosaic'   : False, # If True, make a mosaic
   'startpos'   :     1, # Starting mosaic position 
   'nphase'     :     0, # Number of positions to observe between phase cal
                         # observations. 0 -> Observe all positions.
   'arcminUnits':  True, # If True, offsets are in arcminutes. If False,
                         # offsets are in units of the Nyquist sampling rate
                         # of the large antenna in the subarray
   'offsetFile' :  None, # Offset data file (e.g. 'm51_mosaic.dat')
   'offsets'    :  None, # List containing the offsets.
                         # This can be used instead of offsetFile.
                         # e.g. [ [-0.5,0.5], [0.5,-0.5], [-0.5,-0.5]]
  }

# Passband options
passband = {
   'doPassband' : False, # If True, observe passband calibrator
   'doPoint'    : False, # Point up on passband calibrator if needed
   'forcePoint' : False, # Force pointing if source is available
   'tint'       : 1.00 , # [minutes] Passband calibrator integration time
   'minflux'    : 4.00 , # [Jy]      Minimum flux density for passband cal
   'preferred'  :  None, # Preferred passband calibrator
   'middle'     : False, # OK to observe in middle of phase/source cycle?
   'ncal'       :     1, # Maximum number of calibrators to observe per track
   'interval'   :  None, # [hours]   How frequently to perform passband cal
  }

# Flux calibration options
fluxcal = {
   'doPrimary'    :  True, # If True, observe primary flux calibrators
   'doSecondary'  :  True, # If True, observe secondary calibrator if primary
                           # calibrator is not available
   'doBoth'       : False, # Observe both primary/secondary cal in each cycle
   'doPoint'      :  True, # Point up on flux calibrator if needed
   'forcePoint'   : False, # Force pointing if source is available
   'tint'         :  5.00, # [minutes] Integration time per calibrator
   'preferredPri' :  None, # Preferred primary calibrators
   'preferredSec' :  None, # Preferred secondary calibrators
   'ncal'         :     1, # Number of calibrators to observe per cycle
   'ncycles'      :     1, # Maximum number of calibration cycles in track
   'interval'     :   2.0, # [hours] How often to repeat calibration cycles
   'middle'       : False, # OK to observe in middle of phase/source cycle?
  }

# Indicate, True or False, which primary flux calibrators are ok to use.
# Add sources to the list as needed.
primaryFluxCalibrators = {
   'jupiter'  : False,
   'mars'     : True,
   'neptune'  : True,
   'uranus'   : True,
   'mercury'  : False,
   'mwc349'   : False,
  }
#---------------------------------------------------------------------------------
