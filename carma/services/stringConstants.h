#ifndef CARMA_SERVICES_STRINGCONSTANTS_H
#define CARMA_SERVICES_STRINGCONSTANTS_H
#include <string>

namespace carma {
    namespace services {
	static const std::string CARMA_OBSERVATORY = "carma";
	static const std::string MAINTENANCE = "MAINTENANCE";
	static const std::string NO_CATALOG  = "NONE";
	static const std::string NO_PROJECT  = "NONE";
	static const std::string NO_SOURCE   = "NONE";
	static const std::string REFERENCE   = "reference";
	static const std::string STANDBY     = "STANDBY";
	static const std::string TRANSMITTER = "trans";
	static const std::string UNKNOWN     = "UNKNOWN";
	static const std::string WEATHER     = "WEATHER";
	// various units for use with CQs
	// eventually i should get rid of Angle:: static const strings
	static const std::string DEGREES     = "degrees";
	static const std::string RADIANS     = "radians";
	static const std::string KMS         = "km/s";
	static const std::string ARCSEC      = "arcseconds";
	static const std::string ARCMIN      = "arcminutes";
	static const std::string MPH         = "miles/hour";
	static const std::string GHZ         = "GHz";
    // Believe it or not there is a low level macro in linux which defines HZ
    // and when using TAO, it can get replaced here with it's value.  To 
    // avoid this, I undefine and then redefine it.
    #ifdef HZ
        #define SYSHZ HZ
        #undef HZ
    #endif
	static const std::string HZ          = "Hz";
    #ifdef SYSHZ
        #define HZ SYSHZ
    #endif
    }
}

#endif // CARMA_SERVICES_STRINGCONSTANTS_H
