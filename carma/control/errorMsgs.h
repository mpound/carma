#ifndef CARMA_CONTROL_ERR_MSGS_H
#define CARMA_CONTROL_ERR_MSGS_H
/**
 *
 * String messages for common errors that can arise from several 
 * places in SubarrayControl.
 *
 * @author: Marc Pound
 *
 * $Id: errorMsgs.h,v 1.1 2006/06/22 14:47:37 mpound Exp $
 *
 * $CarmaCopyright$
 *
 */



#include <string>

namespace carma {
namespace control { // anonymous
    const std::string ZERO_DISALLOWED =
          "Input antenna number did not map to a single antenna (zero disallowed on this command)." ;
    const std::string NULL_MONSYS =
	  "Input antenna number mapped to a NULL antenna monitor subsystem." ;
    const std::string NULL_ANTENNA =
	  "Input antenna number mapped to a NULL AntennaControls object." ;
}
}

#endif //CARMA_CONTROL_ERR_MSGS_H
