#include "carma/control/SwitchyardHandles.h"
#include "carma/monitor/MonitorSystem.h"
#include "carma/monitor/SignalPathSubsystem.h"

#include "carma/util/corbaSequenceUtils.h"
#include "carma/util/ScopedLogNdc.h"
#include "carma/util/Time.h"

#include <iostream>
#include <sstream>

using namespace carma;
using namespace carma::control;
using namespace carma::util;

IFSwitchyardHandle::IFSwitchyardHandle( 
        carma::monitor::MonitorSystem & monitorSystem,
        carma::monitor::ControlSubsystemBase::Reachable & reachable ) :
    SwitchyardRemoteObjHandle( switchyard::IFSWITCHYARDCONTROL_NAME,
                               &( reachable.ifSwitchyard() ),
                               &( monitorSystem.signalPath() ),
                               &monitorSystem,
                               true,
                               true )
{
    // Nothing
}
        
IFSwitchyardHandle::~IFSwitchyardHandle( ) 
{
    // Nothing
}

/**.......................................................................
 * Assert a set of switch positions
 */
void IFSwitchyardHandle::setSwitches(std::vector<carma::switchyard::SwitchPosition>& swVec)
{
  ScopedLogNdc ndc("IFSwitchyardHandle::setSwitches");

  if ( isObjReachable( ) ) {

    std::string remoteCallString;
    std::ostringstream os ;
    os << "( < " << swVec.size() << " elements >)";
    remoteCallString = os.str();

    try {

      const double sendTime = Time::MJD( );
      carma::switchyard::SwitchPositionSeq swSeq = convertVectorToSequence<carma::switchyard::SwitchPositionSeq>(swVec);
      remoteObj( )->setSwitches(swSeq);
      logSentCommandIfNeeded( remoteCallString, sendTime );

    }  catch ( const CORBA::Exception & ex )  {
      processException( remoteCallString, ex );
    }

  }
}

LOSwitchyardHandle::LOSwitchyardHandle( 
        carma::monitor::MonitorSystem & monitorSystem,
        carma::monitor::ControlSubsystemBase::Reachable & reachable ) :
    SwitchyardRemoteObjHandle( switchyard::LOSWITCHYARDCONTROL_NAME,
                               &( reachable.loSwitchyard() ),
                               &( monitorSystem.signalPath() ),
                               &monitorSystem,
                               true,
                               true )
{
    // Nothing
}
        
LOSwitchyardHandle::~LOSwitchyardHandle( ) 
{
    // Nothing
}

/**.......................................................................
 * Assert a set of switch positions
 */
void LOSwitchyardHandle::setSwitches(std::vector<carma::switchyard::SwitchPosition>& swVec)
{
  ScopedLogNdc ndc("LOSwitchyardHandle::setSwitches");

  if ( isObjReachable( ) ) {

    std::string remoteCallString;
    std::ostringstream os ;
    os << "( < " << swVec.size() << " elements >)";
    remoteCallString = os.str();

    try {

      const double sendTime = Time::MJD( );
      carma::switchyard::SwitchPositionSeq swSeq = convertVectorToSequence<carma::switchyard::SwitchPositionSeq>(swVec);
      remoteObj( )->setSwitches(swSeq);
      logSentCommandIfNeeded( remoteCallString, sendTime );

    }  catch ( const CORBA::Exception & ex )  {
      processException( remoteCallString, ex );
    }

  }
}

LLSwitchyardHandle::LLSwitchyardHandle( 
        carma::monitor::MonitorSystem & monitorSystem,
        carma::monitor::ControlSubsystemBase::Reachable & reachable ) :
    SwitchyardRemoteObjHandle( switchyard::LLSWITCHYARDCONTROL_NAME,
                               &( reachable.llSwitchyard() ),
                               &( monitorSystem.signalPath() ),
                               &monitorSystem,
                               true,
                               true )
{
    // Nothing
}
        
LLSwitchyardHandle::~LLSwitchyardHandle( ) 
{
    // Nothing
}

/**.......................................................................
 * Assert a set of switch positions
 */
void LLSwitchyardHandle::setSwitches(std::vector<carma::switchyard::SwitchPosition>& swVec)
{
  ScopedLogNdc ndc("LLSwitchyardHandle::setSwitches");

  if ( isObjReachable( ) ) {

    std::string remoteCallString;
    std::ostringstream os ;
    os << "( < " << swVec.size() << " elements >)";
    remoteCallString = os.str();

    try {

      const double sendTime = Time::MJD( );
      carma::switchyard::SwitchPositionSeq swSeq = convertVectorToSequence<carma::switchyard::SwitchPositionSeq>(swVec);
      remoteObj( )->setSwitches(swSeq);
      logSentCommandIfNeeded( remoteCallString, sendTime );

    }  catch ( const CORBA::Exception & ex )  {
      processException( remoteCallString, ex );
    }

  }
}

DCLOSwitchyardHandle::DCLOSwitchyardHandle( 
        carma::monitor::MonitorSystem & monitorSystem,
        carma::monitor::ControlSubsystemBase::Reachable & reachable ) :
    SwitchyardRemoteObjHandle( switchyard::DCLOSWITCHYARDCONTROL_NAME,
                               &( reachable.dcLoSwitchyard() ),
                               &( monitorSystem.signalPath() ),
                               &monitorSystem,
                               true,
                               true )
{
    // Nothing
}
        
DCLOSwitchyardHandle::~DCLOSwitchyardHandle( ) 
{
    // Nothing
}

/**.......................................................................
 * Assert a set of switch positions
 */
void DCLOSwitchyardHandle::setSwitches(std::vector<carma::switchyard::SwitchPosition>& swVec)
{
  ScopedLogNdc ndc("DCLOSwitchyardHandle::setSwitches");

  if ( isObjReachable( ) ) {

    std::string remoteCallString;
    std::ostringstream os ;
    os << "( < " << swVec.size() << " elements >)";
    remoteCallString = os.str();

    try {

      const double sendTime = Time::MJD( );
      carma::switchyard::SwitchPositionSeq swSeq = convertVectorToSequence<carma::switchyard::SwitchPositionSeq>(swVec);
      remoteObj( )->setSwitches(swSeq);
      logSentCommandIfNeeded( remoteCallString, sendTime );

    }  catch ( const CORBA::Exception & ex )  {
      processException( remoteCallString, ex );
    }

  }
}
