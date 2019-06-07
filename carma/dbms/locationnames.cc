/**
 *
 * Location names
 *
 * This data is used to initialize the PhysicalDeviceIDAuthority class and
 * to initialize the database Locations table.  ADD NEW LOCATION NAMES ONLY
 * TO THE END OF THE LIST AND INCREMENT THE COUNTER.
 *
 * @author: Dave Mehringer
 *
 * $Id: locationnames.cc,v 1.1 2010/02/18 17:00:07 abeard Exp $
 * $CarmaCopyright$
 *
 */

#include "carma/dbms/PhysicalDeviceIDAuthority.h"

using namespace ::std;
using namespace carma;
using namespace carma::dbms;

const char * const PhysicalDeviceIDAuthority::locationNames_[ ] = {
    "NOLOCATION",             // 0
    "BIMA1",                  // 1
    "BIMA2",                  // 2
    "BIMA3",                  // 3
    "BIMA4",                  // 4
    "BIMA5",                  // 5
    "BIMA6",                  // 6
    "BIMA7",                  // 7
    "BIMA8",                  // 8
    "BIMA9",                  // 9
    "OVRO1",                  // 10
    "OVRO2",                  // 11
    "OVRO3",                  // 12
    "OVRO4",                  // 13
    "OVRO5",                  // 14
    "OVRO6",                  // 15
    "SZA1",                   // 16
    "SZA2",                   // 17
    "SZA3",                   // 18
    "SZA4",                   // 19
    "SZA5",                   // 20
    "SZA6",                   // 21
    "SZA7",                   // 22
    "SZA8",                   // 23
    "WBDC_HOST",              // 24
    "SL_CORREL_BAND1",        // 25
    "SL_CORREL_BAND2",        // 26
    "SL_CORREL_BAND3",        // 27
    "SL_CORREL_BAND4",        // 28
    "SL_CORREL_BAND5",        // 29
    "SL_CORREL_BAND6",        // 30
    "SL_CORREL_BAND7",        // 31
    "SL_CORREL_BAND8",        // 32
    "SLDC_HOST",              // 33
    "LOBEROTATOR_CONTROLLER", // 34
    "WB_CORREL_BAND1",        // 35
    "WB_CORREL_BAND2",        // 36
    "WB_CORREL_BAND3",        // 37
    "WB_CORREL_BAND4",        // 38
    "WB_CORREL_BAND5",        // 39
    "WB_CORREL_BAND6",        // 40
    "WB_CORREL_BAND7",        // 41
    "WB_CORREL_BAND8",        // 42
    "WB_CORREL_BAND9",        // 43
    "WB_CORREL_BAND10",       // 44
    "WB_CORREL_BAND11",       // 45
    "WB_CORREL_BAND12",       // 46
    "WB_CORREL_BAND13",       // 47
    "WB_CORREL_BAND14",       // 48
    "WB_CORREL_BAND15",       // 49
    "WB_CORREL_BAND16",       // 50
    "LAB",                    // 51
    "PHASEMONITOR"            // 52
};


// ... AND INCREMENT THIS COUNTER

const int PhysicalDeviceIDAuthority::locationCount_ =
    (sizeof( PhysicalDeviceIDAuthority::locationNames_ ) /
     sizeof( PhysicalDeviceIDAuthority::locationNames_[ 0 ] ));
