/*
 * $Id: AntennaParameters.cc,v 1.5 2013/02/06 20:07:29 abeard Exp $
 */

#include "carma/phasemonitor/AntennaParameters.h"

#include "carma/services/Table.h"
#include "carma/util/ErrorException.h"
#include "carma/util/Trace.h"

using namespace std;
using namespace carma::util;
using namespace carma::services;
using namespace carma::phasemonitor;

AntennaParameters::AntennaParameters( string filename )
{
  _loaded = false;
  load( filename );
}

void AntennaParameters::load( string filename )
{

  CPTRACE( Trace::TRACE2, "Loading configuration file: " << filename );

  _filename = filename;

  Table t( filename );
  vector<int> allchans = t.getIntColumn( "chan" );

  if ( allchans.size() != channels )
  {
    ostringstream errs;
    errs << "Number of rows in " << filename
      << " should match " << channels << " rows = " << allchans.size();
    throw CARMA_ERROR( errs.str() );
  }

  vector<int>::iterator i;
  for ( i = allchans.begin(); i != allchans.end(); ++i )
  {
    CPTRACE( Trace::TRACE7, "  Channel: " << *i );
    
    _offV[*i]   = (float)t.getDoubleColumn( "offV" ).at(*i);
    _rotCos[*i] = (float)t.getDoubleColumn( "cos" ).at(*i);
    _rotSin[*i] = (float)t.getDoubleColumn( "sin" ).at(*i);
    _scale[*i]  = (float)t.getDoubleColumn( "scale" ).at(*i);

    CPTRACE( Trace::TRACE7, "    offV/cos/sin/scale: "
	<< _offV[*i] << "/" << _rotCos[*i] << "/"
	<< _rotSin[*i] << "/" << _scale[*i] );
  }

  _loaded = true;

}

void AntennaParameters::getParameters(
    float* offV, float* cosses,
    float* sins, float* scale )
{
  if ( ! _loaded )
    throw CARMA_ERROR( "getParameters called without file load first!" );

  for ( unsigned int i = 0; i < channels; i++ )
  {
    offV[i]   = _offV[i];
    cosses[i] = _rotCos[i];
    sins[i]   = _rotSin[i];
    scale[i]  = _scale[i];
  }
}
    
std::vector< float > 
AntennaParameters::getVoltageOffsets( ) const
{
    if ( ! _loaded )
        throw CARMA_ERROR( "getVoltageOffsets called without file load first!" );

    return vector< float >( &_offV[0], &_offV[channels] );
}

std::vector< float > 
AntennaParameters::getCosTerms( ) const
{
    if ( ! _loaded )
        throw CARMA_ERROR( "getVoltageOffsets called without file load first!" );

    return vector< float >( &_rotCos[0], &_rotCos[channels] );
}

std::vector< float >
AntennaParameters::getSinTerms( ) const
{
    if ( ! _loaded )
        throw CARMA_ERROR( "getVoltageOffsets called without file load first!" );

    return vector< float >( &_rotSin[0], &_rotSin[channels] );
}

std::vector< float > 
AntennaParameters::getScales( ) const
{
    if ( ! _loaded )
        throw CARMA_ERROR( "getVoltageOffsets called without file load first!" );
    
    return vector< float >( &_scale[0], &_scale[channels] );
}

string AntennaParameters::getFilename()
{
  return _filename;
}

ostream& operator<<( ostream& os, AntennaParameters& ap )
{
  os << "  AntennaParamters read from file: ";
  os << "'" << ap.getFilename() << "' ";

  int len = AntennaParameters::channels;

  float offV[len], rotCos[len], rotSin[len], scale[len];
  ap.getParameters( offV, rotCos, rotSin, scale );

  os << endl << "   offsetV(";
  for ( int i = 0; i < len; i++ )
    os << ( i > 0 ? "," : "" ) << offV[i];
  os << ") " << endl;

  os << "   cos(rot)(";
  for ( int i = 0; i < len; i++ )
    os << ( i > 0 ? "," : "" ) << rotCos[i];
  os << ") " << endl;

  os << "   sin(rot)(";
  for ( int i = 0; i < len; i++ )
    os << ( i > 0 ? "," : "" ) << rotSin[i];
  os << ") " << endl;

  os << "   scale(";
  for ( int i = 0; i < len; i++ )
    os << ( i > 0 ? "," : "" ) << scale[i];
  os << ")";

  return os;
}

