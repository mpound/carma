/**@file
 * SecondaryPlate routines...
 *
 * <dl><dt><b>Author </b></dt><dd>Colby Gutierrez-Kraybill</dl>
 * $Revision: 1.8 $
 * $Date: 2006/04/26 22:47:22 $
 * $Id: Secondary.cc,v 1.8 2006/04/26 22:47:22 colby Exp $
 */


// CARMA includes
#include "carma/antenna/bima/Secondary.h"

using namespace std;
using namespace log4cpp;
using namespace carma::util;
using namespace carma::services;
using namespace carma::antenna::bima;


Secondary::Secondary( Configuration &config )
     : TelemetryClient( config ), _config( config )
{
  _name = string( "secondary" );
  _focus = new Motor( string( "focus" ), _config );
  setState( ACQUIRED );
}


void Secondary::setFocus( float mm )
{
  // Flip sign to switch around how the observer sees
  // moves and reported positions for the
  // BIMA secondary.
  // This means when increasing the focus the secondary
  // will now move AWAY from the dish.
  mm = -mm;

  unsigned short pos = (short)(mm * 1106) + 32768;
  ostringstream oss;

  try
  {
    if ( fabs( mm ) <= 25.0 )
    {
      setState( MOVING );
      _focus->moveToTarget( pos );
      setState( ACQUIRED );
    }
    else
    {
      oss << "Focus requested (" << mm
	<< ") mm is out of range [-25,25] mm";
      throw CARMA_ERROR( oss.str() );
    }
  }
  catch ( ... )
  {
    setState( FAILED );
    _logger << Priority::WARN << "Unknown exception caught in setFocus";
  }
}

// Returns mm
float Secondary::getFocus()
{
  // Note that this is now inverting the actual readout from the
  // focus position.  See note above in setFocus() for more info.
  return (float)( -((((unsigned short)_focus->position())-32768) / 1106.) ); 
}

// Returns raw counts
short Secondary::getRawFocus()
{
  return ((unsigned short)_focus->position())-32768; 
}

Secondary::FocusState Secondary::getState()
{
  int status;
  getData( "FOCSTATUS", &status );
  return static_cast<FocusState>(status);
}


void Secondary::setState( Secondary::FocusState fs )
{
  int status = (int)fs;
  putData( "FOCSTATUS", &status );
}


