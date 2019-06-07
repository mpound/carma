#include "carma/util/xercesUtils.h"

#include <string>
#include <sstream>

#include "carma/util/ErrorException.h"
#include "carma/util/programLogging.h"
#include "carma/util/PthreadMutex.h"
#include "carma/util/ScopedLock.h"

using namespace ::std;
using namespace ::xercesc;
using namespace carma;
using namespace carma::util;


namespace {

typedef ScopedLock< ::pthread_mutex_t > ScopedGuardLock;

::pthread_mutex_t gGuard = PTHREAD_MUTEX_INITIALIZER;

XercesPanicHandler * gPanicHandler = 0;

}  // namespace < anonymous >


void
XercesPanicHandler::panic( const PanicReasons reason )
{
    string msg;
    {
        ostringstream oss;
    
        oss << "Xerces panic with reason " << reason;
        
        msg = oss.str();
    }
    
    programLogErrorIfPossible( msg );
    
    throw CARMA_ERROR( msg );
}


void
carma::util::xercesTerm( )
{
    const ScopedGuardLock lock( gGuard );
    
    XMLPlatformUtils::Terminate();
}


void
carma::util::xercesInit( )
{
    const ScopedGuardLock lock( gGuard );
    
    if ( gPanicHandler == 0 )
        gPanicHandler = new XercesPanicHandler;
    
    XMLPlatformUtils::Initialize( XMLUni::fgXercescDefaultLocale,
                                  0,
                                  gPanicHandler );
}


void
carma::util::xercesInit( xercesc::PanicHandler * const panicHandler )
{
    const ScopedGuardLock lock( gGuard );
    
    XMLPlatformUtils::Initialize( XMLUni::fgXercescDefaultLocale,
                                  0,
                                  panicHandler );
}

