# $Id: carmaConstants.py,v 1.2 2008/11/11 17:25:10 lamb Exp $
# Defininition of physical and CARMA-specific constants that
# are commonly used in Python scripts
#
#    It is recommended that these be imported as
#
#        import carmaConstants as CONSTS
#
#    and used as, e.g.,
#
#        lambda = CONSTS.C / (112.0 * CONSTS.GHZ)
#
#    Many of these constants are defined in the CARMA control system in C++
#    classes. Care should be taken to ensure consistency between the two
#    sets of information.
#
# @Author James W. Lamb, Caltech
#
# History
# 11-Nov-2008: JWL      Added more units
# 02-Sep-2008: Original version
#

import math

#------------------------------------------------------------------------------
# Physical constants

# Data from "The NIST Reference on Constants, Units, and Uncertainty, from
# CODATA Recommended Values of the Fundamental Constants" - 2002, by Peter
# J. Mohr and Barry N. Taylor, National Institute of Standards and Technology.
#
# Unless otherwise noted, all constants are in SI units

# Speed of light
# Units: m s^-1
C = 299792458.0

# Newton's gravitational constant
# Units: m^3 kg^-1 s^-1
G = 6.6742E-11

# Boltzmann's constant
# Units: J K^-1
K = 1.3806505E-23

# Planck's constant
# Units: J s
H = 6.6260693E-34

# Absolute zero
# Units: degrees Celsius
ABS_ZERO = -273.15

# Individual gas constant for water
# Units: J kg^-1 K^-1
R_WATER = 461.5

# jansky
# W m^-2 Hz^-1
JY = 1.0E-26

# Conversion of nanoseconds of delay to meters of delay
NANOSEC_PER_METER = 3.33564095198

#------------------------------------------------------------------------------
# Unit conversions

# Length
# Base unit: m
METER  = 1.0
MM = 1000.0
CM = 100.0
KM = 0.001

# Frequency
# Base unit: Hz
HERTZ  = 1.0
KHZ = 1.0E3
MHZ = 1.0E6
GHZ = 1.0E9

# Angles
# Base unit: radian
RADIAN = 1.0
DEG = math.pi / 180.0
ARCMIN = math.pi / 10800.0
ARCSEC = math.pi / 648000.0

# Time
# Base unit: s
SECOND = 1.0
MINUTE = 60
HOUR = 3600
DAY = 86400

#------------------------------------------------------------------------------
# Quantities

# The maximum number of antennas in CARMA
MAX_ANT = 23

# The total number of OVRO antennas
N_OVRO_ANT = 6

# The total number of BIMA antennas
N_BIMA_ANT = 9

# The total number of SZA antennas
N_SZA_ANT = 8

# The total number of wideband system bands
N_WB_BANDS = 16

# The total number spectral-line system bands
N_SL_BANDS = 8

# The total number of wideband correlator stations.
N_WB_STATIONS = 8

# The total number of spectral-line correlator stations
N_SL_STATIONS = 15

#------------------------------------------------------------------------------
# Antenna Parameters

# Aperture diameters
# Units: m
OVRO_DIA = 10.4
BIMA_DIA = 6.1
SZA_DIA  = 3.5

# end carmaConstants.py