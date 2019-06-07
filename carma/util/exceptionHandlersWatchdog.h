#ifndef CARMA_UTIL_EXCEPTION_HANDLERS_WATCHDOG_H
#define CARMA_UTIL_EXCEPTION_HANDLERS_WATCHDOG_H


namespace carma {
namespace util {


void startExceptionHandlersWatchdog( );

void stopExceptionHandlersWatchdog( );


class AutoExceptionHandlersWatchdog {
    public:
        AutoExceptionHandlersWatchdog( );
        
        ~AutoExceptionHandlersWatchdog( );
};


}  // namespace carma::util
}  // namespace carma


inline
carma::util::AutoExceptionHandlersWatchdog::AutoExceptionHandlersWatchdog( )
{
    startExceptionHandlersWatchdog( );
}


inline
carma::util::AutoExceptionHandlersWatchdog::~AutoExceptionHandlersWatchdog( )
try {
    stopExceptionHandlersWatchdog( );
} catch ( ... ) {
    // Just stifle any exception
    
    return;
}


#endif
