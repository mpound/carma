#include "carma/monitor/MonitorSystemSelector.h"

#include "carma/monitor/MonitorSystem.h"
#include "carma/util/ErrorException.h"

using namespace ::std;
using namespace carma;
using namespace carma::monitor;
using namespace carma::util;


CmsSelector
carma::monitor::convertStringToCmsSelector( const string & s )
{
    if ( s == "raw" )
        return RAW_CMS_SELECTOR;

    if ( s == "intermediate" )
        return INTERMEDIATE_CMS_SELECTOR;

    if ( s == "final" )
        return FINAL_CMS_SELECTOR;

    const string msg =
        "unrecognized cms selector string \"" + s + "\"";
        
    throw CARMA_ERROR( msg );
}

CmsAP
carma::monitor::makeCms( const CmsSelector selector,
                         string &          outName )
{
    CmsAP result;
    string name;

    switch ( selector ) {
        case RAW_CMS_SELECTOR:
            result = CmsAP( new RawCarmaMonitorSystem );
            name = "raw";
            break;

        case INTERMEDIATE_CMS_SELECTOR:
            result = CmsAP( new CarmaMonitorSystem );
            name = "intermediate";
            break;

        case FINAL_CMS_SELECTOR:
            result = CmsAP( new FinalCarmaMonitorSystem );
            name = "final";
            break;
    }

    if ( result.get() == 0 )
        throw CARMA_ERROR( "Unknown cms selector value" );

    outName = name;

    return result;
}

