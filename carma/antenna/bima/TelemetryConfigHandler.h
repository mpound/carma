/** @file 
 * SAX Handler for parsing BIMA Telemetry configuration file
 * 
 * <dl><dt><b>Author </b></dt><dd>Colby Gutierrez-Kraybill</dl>
 * $Revision: 1.8 $
 * $Date: 2006/03/14 22:42:52 $
 * $Id: TelemetryConfigHandler.h,v 1.8 2006/03/14 22:42:52 colby Exp $
 */

#ifndef CARMA_ANTENNA_BIMA_TELEMETRYCONFIGHANDLER_H
#define CARMA_ANTENNA_BIMA_TELEMETRYCONFIGHANDLER_H



#include <xercesc/sax/AttributeList.hpp>
#include <xercesc/sax/HandlerBase.hpp>

#include <string>
#include <vector>
#include <map>

#include <stdlib.h>

#include "carma/canbus/Utilities.h"
#include "carma/util/StrX.h"
#include "carma/util/Trace.h"
#include "carma/util/ErrorException.h"

#include "carma/antenna/bima/TelemetryInfo.h"

XERCES_CPP_NAMESPACE_USE

typedef std::map<carma::canbus::msgType, carma::antenna::bima::TelemetryInfo *> tmMap;

namespace carma
{
  namespace antenna
    {
      namespace bima
	{
	  class TelemetryConfigHandler : public HandlerBase
	    {
	      
	    private:
	       tmMap _info;
               std::vector<short> _addrCheck;
               bool _check;
	       std::string _version;
	      
	    public:
	      TelemetryConfigHandler( bool check = false );

	      ~TelemetryConfigHandler();

	      TelemetryInfo *getInfo( carma::canbus::msgType mid )
		{
		  if ( _info.find( mid ) != _info.end() )
		    {
		      return _info[mid];
		    }
		  else
		    {
		      return (TelemetryInfo *)0;
		    }
		}

	      
	      tmMap::iterator begin() { return _info.begin(); }
	      tmMap::iterator end() { return _info.end(); }

	      std::string getTelemetryVersion();

	      // startDocument DocumentHandler overload
	      void startDocument();

	      // startElement DocumentHandler overload
	      void startElement(const XMLCh* const name, AttributeList& attributes);

	      // endElement DocumentHandler overload
	      void endElement(const XMLCh* const name);

	      // endDocument DocumentHandler overload
	      void endDocument();
    
	      // warning SAX ErrorHandler implementation
	      void warning(const SAXParseException& exception);
    
	      // error SAX ErrorHandler implementation
	      void error(const SAXParseException& exception);

	      // fatalError SAX ErrorHandler implementation
	      void fatalError(const SAXParseException& exception);

	    }; // class TelemetryConfigHandler
	} // namespace bima
    } // namespace antenna
} // namespace carma

#endif // CARMA_ANTENNA_BIMA_TELEMETRYCONFIGHANDLER_H
