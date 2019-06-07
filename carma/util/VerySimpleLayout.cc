/* $Id: VerySimpleLayout.cc,v 1.1 2006/06/20 08:53:35 colby Exp $
 * 
 * VerySimple Layout (format) for CARMA Logging messages.
 * This is specifically intended for short trace messages
 */
#include <iostream>
#include "carma/util/Logger.h"
#include "carma/util/Time.h"

using namespace std;
using namespace log4cpp;

namespace carma
{
  namespace util
  {

    VerySimpleLayout::VerySimpleLayout() { }
    
    VerySimpleLayout::~VerySimpleLayout() { }

    /**
     * The format of messages from a VerySimpleLayout
     */
    std::string VerySimpleLayout::format(const LoggingEvent& event)
    {
	carma::util::Time t;
        std::ostringstream message;

	int colon = event.message.find_first_of( ":" );
	int slash = event.message.find_last_of( "/", colon ) + 1;

        message << event.message.substr( slash, event.message.size() - slash )
	  << endl;

        return message.str();
    } // VerySimpleLayout
  } // util
} // carma
