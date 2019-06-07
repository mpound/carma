
/**@file
 * Class implementation for TelemetryClient on the BIMA antennas.
 *
 * <dl><dt><b>Author </b></dt><dd>Colby Gutierrez-Kraybill </dl>
 * $Revision: 1.5 $
 * $Date: 2006/12/02 08:04:22 $
 * $Id: AntennaIFClient.cc,v 1.5 2006/12/02 08:04:22 colby Exp $
 */


#include "carma/antenna/bima/AntennaIFClient.h"

#include <cmath>

#include <sys/time.h>

using namespace std;
using namespace log4cpp;
using namespace carma::util;
using namespace carma::canbus;
using namespace carma::antenna::bima;

AntennaIFClient::AntennaIFClient( Configuration& config )
  : SharedMemory( config.getAntenna().c_str() ),
    _config( config )
{
  string configFileName( _config.getTelemConfFile() );
  int retrycnt = 0;

  while ( true )
  {
    try
    {
      if ( _config.isEmulating() == false )
      {
	CPTRACE( Trace::TRACE1, "Aquiring handle to IFIPQ" );
	_aifWriter = new IPQwriter<TelemetryCommand>( IFIPQ, false, IPQLEN );
      }
      else
	CPTRACE( Trace::TRACE1, "Emulation mode, ignoring attempt to acquire "
	   << "handle to IFIPQ" );

      break;
    }
    catch ( carma::util::ErrorException &eex )
    {
      if ( retrycnt++ > 10 )
      {
        ostringstream oss;
        string msg;
        oss << "Unable to aquire AntennaIF IPQ! Is bimaIFHost running? "
            << eex.what();
        msg = oss.str();
        throw CARMA_ERROR( msg );
      }
    }

    sleep(1);
  }

}

AntennaIFClient::~AntennaIFClient()
{
  // no-op
}


void AntennaIFClient::command( const unsigned short command, short value )
{
  if ( _config.isEmulating() == false )
  {
    _aifWriter->set( command, value );
    _aifWriter->write();
    CPTRACE( Trace::TRACE6, "IPQ write to AntennaIF, command: "
	<< (hex) << command << " value: " << value );
  }
  else
    CPTRACE( Trace::TRACE6, "IPQ write to AntennaIF, command: "
	<< (hex) << command << " value: " << value << " EMULATED" );
}

void AntennaIFClient::command( const unsigned short command, float value )
{
  if ( _config.isEmulating() == false )
  {
    _aifWriter->set( command, value );
    _aifWriter->write();
    CPTRACE( Trace::TRACE6, "IPQ write to AntennaIF, command: "
	<< (hex) << command << " value: " << value );
  }
  else
    CPTRACE( Trace::TRACE6, "IPQ write to AntennaIF, command: "
	<< (hex) << command << " value: " << value << " EMULATED" );
}


