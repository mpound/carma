

/**@file
 *
 * <dl><dt><b>Author </b></dt><dd>Colby Gutierrez-Kraybill </dl>
 * $Revision: 1.2 $
 * $Date: 2006/06/01 01:32:17 $
 * $Id: RxClient.cc,v 1.2 2006/06/01 01:32:17 colby Exp $
 */


#include "carma/antenna/bima/RxClient.h"

using namespace std;
using namespace carma::util;
using namespace carma::antenna::bima;

RxClient::RxClient( Configuration& config ):
  _config( config )
{
  int retrycnt = 0;

  while ( true )
  {
    try
    {
      if ( _config.isEmulating() == false )
      {
	CPTRACE( Trace::TRACE1, "Aquiring handle to RXIPQ" );

	auto_ptr< carma::util::IPQwriter<RxCommand> > tempPtr( 
	    new IPQwriter<RxCommand>( RXIPQ, false, IPQLEN ) );

	_rxWriter = tempPtr;
	CPTRACE( Trace::TRACE1, "Got handle to RXIPQ" );
      }
      else
      {
	CPTRACE( Trace::TRACE1, "Emulation mode, ignoring attempt to aquire" 
	    << " handle to RXIPQ");
      }

      break;
    }
    catch ( ErrorException &eex )
    {
      if ( retrycnt++ > 10 )
      {
	ostringstream oss;
	oss << "Unable to aquire RX IPQ('" << RXIPQ << "')!"
	  << " Is bimaRxMgr running? "
	  << eex.what();

	throw CARMA_ERROR( oss.str() );
      }
    }

    sleep(1);
  }
}

RxClient::~RxClient()
{
  CPTRACE( Trace::TRACE1, "Destroying RxClient" );
}

void RxClient::rxWrite()
{
  _rxWriter->write();
}

