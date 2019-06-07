// $Id: ephemFunctions.h,v 1.1 2008/09/30 17:51:25 teuben Exp $
#ifndef CARMA_SERVICES_EPHEM_FUNCTIONS_H
#define CARMA_SERVICES_EPHEM_FUNCTIONS_H
#include <string>

namespace carma {
namespace services {
  
  /**
   *  find out if a source is in an ephemeris table derived
   *  from JPL's HORIZON's system. These tables are typically
   *  valid in a short (few months/years) time interval, e.g.
   *  comets, a sunspot etc. and are stored as <sourceName>.ephem
   *  files in /array/rt/catalogs, [$CARMA/conf/catalogs/observer] 
   *  $CARMA/conf/catalogs and the current directory (useful for testing).
   *  Normally the sourceName is tried in upper case , but the given
   *  case is tried as well.
   *  Ephem files need to be in VECTORS format, though some support for
   *  RADEC style tables is present but not in use.
   *
   *  @return string with full path of the ephem file
   */
  std::string EphemFile(const std::string& sourceName);
  
  /**
   *  return if a sourcename is an ephemeris source. these
   *  are assumed generated from an ephemeris service and
   *  are solar system bodies (or things like sunspots)
   */
  bool isEphem(const std::string& sourceName);

  /**
   *  return if source is a proper solar system body. Apart
   *  from doppler tracking differences, Planets also have
   *  size properties which Ephem sources do not have, hence
   *  two different functions for solar system objects.
   */
  bool isPlanet(const std::string& sourceName);

  /**
   *  return if source is a supported fixed source (e.g. transmitter)
   *  Fixed source need a Long,Lat,Elev from the source catalog from which
   *  a nominal Az,El can be computed, and would need a Ephemeris object
   *  to achieve this.
   */

  bool isFixed(const std::string& sourceName);
}
}
#endif // CARMA_SERVICES_EPHEM_FUNCTIONS_H
