/**
 *
 * Carma control LO reference interface implementation.
 *
 * @author: Amar Amarnath
 *
 * $Id: LOrefHandle.cc,v 1.21 2012/04/03 21:54:56 scott Exp $
 *
 * $CarmaCopyright$
 *
 */


#include <cstdlib>

#include "carma/control/RemoteObjHandleT.h"
#include "carma/control/LOrefHandle.h"
#include "carma/control/SubarrayControlImpl.h"
#include "carma/corba/corba.h"
#include "carma/monitor/MonitorSystem.h"
#include "carma/monitor/MonitorSubsystem.h"
#include "carma/monitor/LoRefSubsystem.h"
#include "carma/monitor/ControlSubsystemExt.h"
#include "carma/monitor/ControlSubsystem.h"
#include "carma/util/programLogging.h"

using namespace ::std;
using namespace carma;
using namespace carma::util;
using namespace carma::control;
using namespace carma::loref;
using namespace carma::monitor;


LOrefHandle::LOrefHandle( MonitorSystem &                   carmaMonitor,
                          ControlSubsystemBase::Reachable & reachable ) :
LOrefControlRemoteObjHandle( LOREF_NAME,
                             &(reachable.loRef()),
                             &(carmaMonitor.loRef( )),
                             &carmaMonitor,
                             true,
                             false ),
synthIndex_(99) // Intentionally bogus default value!
{
}


LOrefHandle::~LOrefHandle( )
try {
} catch ( ... ) {
    // Just stifle any exception
    
    return;
}


void
LOrefHandle::setFrequencyPower( const double frequency,
                                const double power ) {
    if (synthIndex_ > 10) {
        programLogError("setFrequencyPower() ignored because synthIndex not set");
        return;
    }
    if ( isObjReachable( ) ) {
        string remoteCallString;
        {
            ostringstream oss;
            
            oss << setiosflags(ios::fixed)
                << "LOReferenceControl::setFrequencyPower("
                << "synthIndex=" << synthIndex_
                << setprecision(0) << " frequency(Hz)=" << frequency
                << " power(dBm)=" << power
                << ")";
                
            remoteCallString = oss.str( );
        }
    
        try {
            //const double sendTime = Time::MJD();
            
            remoteObj()->setFrequencyPower( synthIndex_, frequency, power );
            SubarrayControlImpl::loginfo() << remoteCallString;
            //logSentCommandIfNeeded( remoteCallString, sendTime );
        } catch ( const CORBA::Exception & ex ) {
            processException( remoteCallString, ex );
        }
    }
}


void
LOrefHandle::setFrequency( const double frequency ) {
    if (synthIndex_ > 10) {
        programLogError("setFrequency() ignored because synthIndex not set");
        return;
    }
    if ( isObjReachable( ) ) {
        string remoteCallString;
        {
            ostringstream oss;
            
            oss << "LOReferenceControl::setFrequency("
                << "synthIndex = " << synthIndex_
                << " frequency (Hz) = " << frequency
                << ")";
                
            remoteCallString = oss.str( );
        }
    
        try {
            const double sendTime = Time::MJD();
            
            remoteObj( )->setFrequency( synthIndex_, frequency );

            logSentCommandIfNeeded( remoteCallString, sendTime );
        } catch ( const CORBA::Exception & ex ) {
            processException( remoteCallString, ex );
        }
    }
}


void
LOrefHandle::setPower( const double power ) {
    if (synthIndex_ > 10) {
        programLogError( "setPower() ignored because synthIndex not set");
        return;
    }
    if ( isObjReachable( ) ) {
        string remoteCallString;
        {
            ostringstream oss;
            
            oss << "LOReferenceControl::setPower("
                << "synthIndex = " << synthIndex_
                << " poower (dBm) = " << power
                << ")";
                
            remoteCallString = oss.str( );
        }
    
        try {
            const double sendTime = Time::MJD();
            
            remoteObj( )->setPower( synthIndex_, power );

            logSentCommandIfNeeded( remoteCallString, sendTime );
        } catch ( const CORBA::Exception & ex ) {
            processException( remoteCallString, ex );
        }
    }
}

void
LOrefHandle::assignSynth(const int synthIndex) 
{
    synthIndex_ = synthIndex;
}

