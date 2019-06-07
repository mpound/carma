#include "carma/antenna/common/CMFrontEndControlImpl.h"
#include "carma/antenna/common/CMReceiver.h"
#include "carma/util/compileTimeCheck.h"
#include "carma/util/ErrorException.h"
#include "carma/util/programLogging.h"
#include "carma/util/UserException.h"

#include <sstream>
#include <string>

using namespace carma::antenna::common;
using namespace carma::util;
using namespace std;

namespace {

    void 
    throwUserException( const string & str )
    {
        throw CARMA_EXCEPTION( carma::util::UserException, str.c_str() );
    }

} // namespace < unnamed >

CMFrontEndControlImpl::CMFrontEndControlImpl( CMReceiver & cmrx ) :
    cmRx_( cmrx )
{
    // Nothing
}

CMFrontEndControlImpl::~CMFrontEndControlImpl( ) 
{
    // Nothing
}

void
CMFrontEndControlImpl::setFrequency( const CORBA::Double gHz )
{
    throwUserException( "setFrequency has no cm implementation." );
}

void
CMFrontEndControlImpl::setSISVj( const CORBA::Float milliVolts )
{
    throwUserException( "setSISVj has no cm implementation." );
}

void
CMFrontEndControlImpl::setSISIj( const CORBA::Float microAmps )
{
    throwUserException( "setSISIj has no cm implementation." );
}

void 
CMFrontEndControlImpl::doIVcurve( const CORBA::Float startVjInMv,
                                  const CORBA::Float stopVjInMv,
                                  const CORBA::Float stepVjInMv,
                                  const CORBA::UShort deltaInMs,
                                  const CORBA::Boolean doPower,
                                  const CORBA::ULong seqNo ) 
{
    throwUserException( "doIVCurve has no cm implementation." );
}

IVCurve *
CMFrontEndControlImpl::getIVCurve( )
{
    throwUserException( "doIVCurve has no cm implementation." );
    return 0;
}

void
CMFrontEndControlImpl::setVG( const FrontEndControl::Amp amplifier,
                              const FrontEndControl::Stage feStage,
                              const CORBA::Float volts )
{
    throwUserException( "setVG has no cm implementation." ); 
}

void
CMFrontEndControlImpl::setVD( const FrontEndControl::Amp amplifier,
                              const FrontEndControl::Stage feStage,
                              const CORBA::Float volts )
{
    // Make sure nobody toys with enums as these get converted to ints.
    compileTimeCheck< FrontEndControl::FIRST == 0 >();
    compileTimeCheck< FrontEndControl::SECOND == 1 >();
    compileTimeCheck< FrontEndControl::THIRD == 2 >();
    compileTimeCheck< FrontEndControl::FOURTH == 3 >();

    ostringstream log;
    log << "CMFrontEndControlImpl::setVD( ";

    const unsigned short stage = feStage + 1;

    switch ( amplifier ) {
        case FrontEndControl::RF:
            log << "RF, stage=" << stage << ", volts=" << volts << " )";
            cmRx_.set30GHzDrainVoltage( stage, volts );
            break;
        case FrontEndControl::IF:
            throwUserException( "setVD has no cm implementation for IF amp." );
            break;
        default:
            throwUserException( "Input amplifier does not exist." );
            break;
    }

    programLogInfoIfPossible( log.str() ); 
}

void
CMFrontEndControlImpl::setID( const FrontEndControl::Amp amplifier,
                              const FrontEndControl::Stage feStage,
                              const CORBA::Float milliAmps )
{
    ostringstream log;
    log << "CMFrontEndControlImpl::setID( ";

    const unsigned short stage = feStage + 1;

    switch ( amplifier ) {
        case FrontEndControl::RF:
            log << "RF, stage=" << stage << ", milliAmps=" << milliAmps << " )";
            cmRx_.set30GHzDrainCurrent( stage, milliAmps );
            break;
        case FrontEndControl::IF:
            log << "IF, stage=" << stage << ", milliAmps=" << milliAmps << " )";
            cmRx_.set30GHzIFDrainCurrent( milliAmps );
            break;
        default:
            throwUserException( "Input amplifier does not exist." );
            break;
    }
    
    programLogInfoIfPossible( log.str() ); 
}

void
CMFrontEndControlImpl::setMixer( const CORBA::Float volts )
{
    throwUserException( "setMixer has no cm implementation." );
}
