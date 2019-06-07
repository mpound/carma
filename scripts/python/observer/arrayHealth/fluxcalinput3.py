"""
Flux calibration script input
"""
import subarrayCommands as sac
import obsdef3 as od

#---------------------------------------------------------------------------------
#----- set variables depending on the configurations
maxElevation = 80.0
config = sac.queryString('Control.Subarray1.configName')
if config == 'E':
    minElevation = 30.0
elif config in ['A','B','C','D']:
    minElevation = 25.0
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
   'doOptPoint'   : False, # If True, then perform optical instead of radio pointing
   'intervalNight':     2, # [hours]  How often to point during nighttime
   'intervalDay'  :     1, # [hours]  How often to point during daytime
   'intervalOpt'  :   0.5, # [hours]  How often to perform optical pointing
   'minflux'      :     3, # [Jy]     Minimum pointing flux
   'maxsep'       :    50, # [degree] Maximum distance from science target
   'preferred'    :  None, # Preferred pointing source
   'tune95'       : False, # Tune to 95 GHz before pointing
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
   'minElevationCal'   :  25.00, # [degrees]
   'maxElevationCal'   :  80.0, # [degrees]
   'trackingThreshold' :   0.1, # [beamwidths]
   'record'            :  15.0, # [seconds]
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
   'restfreq' : 93.00,  # [GHz] Line rest frequency
   'sideband' : sac.USB,    # Sideband for first LO (LSB or USB)
   'IFfreq'   : 3.0,   # [GHz] IF frequency
}

# Correlator configuration.
# For best sensitivity, a band should be placed at an IF frequency > 2 GHz.
# Also, the band should not overlap IF=5 GHz.
# NOTE: The configastroband() commands should all be indented by the same amount!
def setCorrelator(tuning):
   clearastroband(0)
   lo1 = 90.
   configastroband(1, "LL", BW500, lo1 - 1.5, bits=CORR_4BIT)
   configastroband(2, "LL", BW500, lo1 - 2.0, bits=CORR_4BIT)
   configastroband(3, "LL", BW500, lo1 - 2.5, bits=CORR_4BIT)
   configastroband(4, "LL", BW500, lo1 - 4.5, bits=CORR_4BIT)
   configastroband(5, "LL", BW500, lo1 - 5.5, bits=CORR_4BIT)
   configastroband(6, "LL", BW500, lo1 - 6.0, bits=CORR_4BIT)
   configastroband(7, "LL", BW500, lo1 - 7.5, bits=CORR_4BIT)
   configastroband(8, "LL", BW500, lo1 - 8.0, bits=CORR_4BIT)


# Names for phase calibrators and science targets.
# Enter multiple science targets as 'target' : 'ABAur, GMAur',
# Enter multiple phase calibrators as 
#     'phaseCal' : [ '3c111     1:00:00  4:00:00',
#                    '0530+135  3:30:00  7:00:00'],
# tintTarget: Integration time per cycle **per pointing** on science targets
# tintPhase : Phase calibrator integration time per cycle
# callist   : Used to search for bright phase calibrators. See online manual
sources = {
   'target'       : 'mwc349',
   'mosaicTarget' : False,
   'tintTarget'   : 5.0, # [minutes] time per cycle *** per pointing ***
   'tintPhaseCal' : 0.0, # [minutes]
   'callist'      : None,
}


# Mosaic options
mosaic = {
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
   'tint'       :    10, # [minutes] Passband calibrator integration time
   'minflux'    :     4, # [Jy]      Minimum flux density for passband cal
   'preferred'  :  None, # Preferred passband calibrator
}


# Flux calibration options
fluxcal = {
   'doPrimary'    :  True, # If True, observe primary flux calibrators
   'doSecondary'  :  True, # If True, observe secondary calibrator if primary
                           # calibrator is not available
   'doBoth'       : False, # Observe both primary/secondary cal in each cycle
   'doPoint'      :  True, # Point up on flux calibrator if needed
   'tint'         :     5, # [minutes] Integration time per calibrator
   'preferredPri' :  None, # Preferred primary calibrators
   'preferredSec' :  None, # Preferred secondary calibrators
}


# Indicate, True or False, which primary flux calibrators are ok to use.
# Add sources to the list as needed.
primaryFluxCalibrators = {
   'mercury'  : False,
   'venus'    : False,
   'mars'     :  True,
   'jupiter'  : False,
   'uranus'   :  True,
   'neptune'  :  True,
   'mwc349'   :  True,
}

#---------------------------------------------------------------------------------
