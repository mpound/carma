/**@file
 * IFPlate routines...
 *
 * <dl><dt><b>Author </b></dt><dd>Colby Gutierrez-Kraybill</dl>
 * $Revision: 1.2 $
 * $Date: 2005/08/02 23:00:57 $
 * $Id: IF.cc,v 1.2 2005/08/02 23:00:57 colby Exp $
 */


// CARMA includes
#include "carma/antenna/bima/IF.h"

using namespace std;
using namespace log4cpp;
using namespace carma::util;
using namespace carma::services;
using namespace carma::antenna::bima;


IF::IF( Configuration &config )
     : TelemetryClient( config ), _config( config )
{
  _name = string( "ifplate" );
}

float IF::getTemp()
{
  return (float)thermistor(( atodin( "TPLTIF" ) * THERMI ));
}

float IF::getHeater()
{
  return (float)( atodin( "IFHPLTMN" ) * VOLTS10 );
}

