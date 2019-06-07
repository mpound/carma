#ifndef CARMA_SERVICES_TELESCOPE_STATUS_H
#define CARMA_SERVICES_TELESCOPE_STATUS_H

#include "carma/services/Types.h"
#include <string>

namespace carma {
  namespace services {

/**
 * Holds information about the telescope status with
 * respect to azimuth wrap and upper elevation
 * limits.  Decisions on how long to track on 
 * the current az wrap can be made by querying the
 * TelescopeStatus member variable of the SourceChecker.
 */
class TelescopeStatus {
    friend class SourceChecker;
    public: 
	TelescopeStatus();
	virtual ~TelescopeStatus();

	/**
	 * TelescopeLimits enum value for given source
	 */
	TelescopeLimitsType getLimits() const
	{
	    return limits_;
	}
	
	::std::string getLimitsString() const
	{
	    switch ( limits_ ) {
		default:
		case NO_LIMIT:
		    return ::std::string("NO_LIMIT");
		case LIMIT_NEVER_RISES :
		    return ::std::string("LIMIT_NEVER_RISES");
		case LIMIT_HORIZON_STOP :
		    return ::std::string("LIMIT_HORIZON_STOP");
		case LIMIT_AZ_HORIZON_STOP :
		    return ::std::string("LIMIT_AZ_HORIZON_STOP");
		case LIMIT_AZ_STOP:
		    return ::std::string("LIMIT_AZ_STOP");
	    }
	}

	/**
	 * Is source currently passing through the zenith blind spot? 
	 */
	bool isInZenithBlindSpot() const {
	    return blinded_;
	}

	/**
	 * Can the source pass through the zenith blind spot? 
	 */
	bool canEnterZenithBlindSpot() const {
	    return canBlind_;
	}

    private:
        TelescopeLimitsType limits_;
        bool blinded_;
        bool canBlind_;
 };

 }
}

#endif // CARMA_SERVICES_TELESCOPE_STATUS_H
