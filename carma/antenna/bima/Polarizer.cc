/**@file
 *
 * <dl><dt><b>Author </b></dt><dd>Colby Gutierrez-Kraybill</dl>
 * $Revision: 1.2 $
 * $Date: 2007/03/03 01:29:21 $
 * $Id: Polarizer.cc,v 1.2 2007/03/03 01:29:21 colby Exp $
 */


#include <stdlib.h>

#include <cmath>
#include <vector>

// CARMA includes
#include "carma/antenna/bima/Polarizer.h"

using namespace std;
using namespace log4cpp;
using namespace carma::util;
using namespace carma::services;
using namespace carma::antenna::bima;

Polarizer::Polarizer( Configuration& config )
     : TelemetryClient( config )
{
  _polarizer = new Motor( "polarizer", config );
  loadConfFile();
  _band = 'A';
}

void Polarizer::loadConfFile()
{
  Table poltab( _config.getPolConfFile() );
  vector<string> names = poltab.getColumn( "name" );

  vector<string>::iterator i;
  unsigned short *polBands;
  int entry = 0;
  for ( i = names.begin(); i != names.end(); ++i )
  {
    polBands = new unsigned short[5]; // memory leak!

    polBands[0] = (unsigned short)poltab.getIntColumn("a").at( entry );
    polBands[1] = (unsigned short)poltab.getIntColumn("b").at( entry );
    polBands[2] = (unsigned short)poltab.getIntColumn("c").at( entry );
    polBands[3] = (unsigned short)poltab.getIntColumn("d").at( entry );

    _positions[*i] = polBands;

    entry++;
  }

  _rcp = _positions[string("rcp")];
  _lcp = _positions[string("lcp")];
  _clear = _positions[string("clear")];
}

Polarizer::PolPos Polarizer::getCanonicalPosition()
{
  unsigned int cur = _polarizer->position();

  int bits;
  char bands[4] = { 'A', 'B', 'C', 'D'};

  getData( "BITSTO60", &bits, 1 );
  _band = bands[ bits & 0x03 ];

  int polidx = _band - 65;

  if ( abs((long int)(_rcp[polidx] - cur)) < _tolerance )
    return RCP;
  else if ( abs((long int)(_lcp[polidx] - cur)) < _tolerance )
    return LCP;
  else if ( abs((long int)(_clear[polidx] - cur)) < _tolerance )
    return CLEAR;
  else
    return BAD;
}

int Polarizer::getInstantPosition()
{
  return (int)_polarizer->position();
}

