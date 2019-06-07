/**
 *
 * Carma control Alarm reference interface implementation.
 *
 * @author: Amar Amarnath
 * @author: Colby Gutierrez-Kraybill
 *
 * $Id: AlarmHandle.cc,v 1.9 2010/07/13 18:46:11 abeard Exp $
 *
 * $CarmaCopyright$
 *
 */


#include <cstdlib>

#include "carma/corba/corba.h"

#include "carma/monitor/MonitorSystem.h"
#include "carma/monitor/MonitorSubsystem.h"
#include "carma/monitor/AlarmSubsystem.h"
#include "carma/monitor/ControlSubsystemExt.h"
#include "carma/monitor/ControlSubsystem.h"
#include "carma/control/RemoteObjHandleT.h"
#include "carma/control/AlarmHandle.h"
#include "carma/util/Time.h"

using namespace ::std;
using namespace carma;
using namespace carma::util;
using namespace carma::control;
using namespace carma::alarm;
using namespace carma::monitor;


AlarmHandle::AlarmHandle( MonitorSystem &                   carmaMonitor,
                          ControlSubsystemBase::Reachable & reachable ) :
  AlarmControlRemoteObjHandle(
      ALARM_NAME,
      &(reachable.alarm()),
      &(carmaMonitor.alarm()),
      &carmaMonitor,
      true,
      false )
{
  CPTRACE( Trace::TRACE3, "   AlarmHandle c'tor" );
}


AlarmHandle::~AlarmHandle( )
try {
} catch ( ... ) {
    // Just stifle any exception
    
    return;
}


void AlarmHandle::turnOn(
    const char *alarmName,
    const char *mp,
    const char *reason,
    bool repeat )
{
  CPTRACE( Trace::TRACE3, "   AlarmHandle::turnOn( "
      << " alarmName=" << alarmName
      << ", mp=" << mp
      << ", reason=" << reason
      << ", repeat=" << (boolalpha) << repeat << " )" );

  if ( isObjReachable( ) )
  {
    CPTRACE( Trace::TRACE4, "    AlarmHandle obj is reachable!" );

    string remoteCallString;
    {
      ostringstream oss;

      oss << setiosflags(ios::fixed)
	<< "AlarmControl::turnOn("
	<< "alarmName=" << alarmName
	<< ", mp=" << mp
	<< ", reason=" << reason
	<< ", repeat=" << repeat
	<< ")";

      remoteCallString = oss.str( );
    }

    CPTRACE( Trace::TRACE4, "    AlarmHandle remoteCallString: "
	<< remoteCallString );

    try
    {
      const double sendTime = Time::MJD();

      remoteObj( )->turnOn( alarmName, mp, reason, repeat );

      logSentCommandIfNeeded( remoteCallString, sendTime );
    }
    catch ( const CORBA::Exception & ex )
    {
      processException( remoteCallString, ex );
    }
  }
  else
    CPTRACE( Trace::TRACE4, "    AlarmHandle obj is not reachable!" );
}


void AlarmHandle::turnOff()
{
  if ( isObjReachable( ) )
  {
    string remoteCallString;
    {
      ostringstream oss;

      oss << setiosflags(ios::fixed)
	<< "AlarmControl::turnOff()";

      remoteCallString = oss.str( );
    }

    try
    {
      const double sendTime = Time::MJD();

      remoteObj( )->turnOff();

      logSentCommandIfNeeded( remoteCallString, sendTime );
    }
    catch ( const CORBA::Exception & ex )
    {
      processException( remoteCallString, ex );
    }
  }
}


void AlarmHandle::updateDeadMan()
{
  if ( isObjReachable( ) )
  {
    string remoteCallString;
    {
      ostringstream oss;

      oss << setiosflags(ios::fixed)
	<< "AlarmControl::updateDeadMan()";

      remoteCallString = oss.str( );
    }

    try
    {
      const double sendTime = Time::MJD();

      remoteObj( )->updateDeadMan();

      logSentCommandIfNeeded( remoteCallString, sendTime );
    }
    catch ( const CORBA::Exception & ex )
    {
      processException( remoteCallString, ex );
    }
  }
}


void AlarmHandle::enableAlarm(const bool state)
{
  if ( isObjReachable( ) ) {
    string remoteCallString;
    {
      ostringstream oss;
      oss << setiosflags(ios::fixed)
          << "AlarmControl::anable(" << boolalpha << state << ")";
      remoteCallString = oss.str( );
    }

    try {
      const double sendTime = Time::MJD();
      remoteObj( )->enableAlarm(state);

      logSentCommandIfNeeded( remoteCallString, sendTime );
    }
    catch ( const CORBA::Exception & ex ) {
      processException( remoteCallString, ex );
    }
  }
}


