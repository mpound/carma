/**@file
 *
 * <dl><dt><b>Author </b></dt><dd>Colby Gutierrez-Kraybill</dl>
 * $Revision: 1.21 $
 * $Date: 2013/02/20 18:07:18 $
 * $Id: Configuration.cc,v 1.21 2013/02/20 18:07:18 friedel Exp $
 */


// CARMA includes
#include "carma/antenna/bima/Configuration.h"

using namespace std;
using namespace carma::util;
using namespace carma::services;
using namespace carma::antenna::bima;

Configuration::Configuration( string antenna, string confDir, bool emulate )
  : _antenna( antenna ), _confDir( confDir + string( "antenna/bima" ) )
{
  CPTRACE( Trace::TRACE7, "Configuration loading..." );

  setTelemConfigHandlerP( (TelemetryConfigHandler *)0 );
  setSemOpP( (SemaphoreOperator *)0 );

  _emulate = emulate;

  // Figure out anttype and antno
  int start;
  string antnostr, anttype;
  start = _antenna.find_first_of( "123456789" );

  if ( (string::size_type)start == string::npos )
  {
    ostringstream oss;
    oss <<  "Malformed antenna name \"" << _antenna <<  "\""
      << " Must be in form nameN where N is [1,9]";
    throw CARMA_ERROR( oss.str() );
  }

  anttype = _antenna.substr( 0, start );
  antnostr = _antenna.substr( start, start+1 );
  CPTRACE( Trace::TRACE6, "antnostr: " << antnostr );
  _antno = atoi( antnostr.c_str() );


  _descTableFile = string( _confDir + string("/") + string("desc.tab") );
  _descTable = new Table( _descTableFile );
  vector<string> names = _descTable->getColumn( "name" );

  CPTRACE( Trace::TRACE7, "Opened " << _descTableFile );
  CPTRACE( Trace::TRACE7, "Looking for: " << _antenna );

  vector<string>::iterator i;
  int entry = 0;
  for ( i = names.begin(); i != names.end(); ++i )
  {
    CPTRACE( Trace::TRACE7, "  Looking at: " << *i );
    if ( i->compare( _antenna ) == 0 )
    {
      CPTRACE( Trace::TRACE7, "   Found it!" );
      _dewarName = _descTable->getColumn( "dewar" ).at( entry );
      _oscADname = _descTable->getColumn( "oscAD" ).at( entry );
      _oscADfile = _confDir + string( "/rx/osc/" ) + _oscADname + string( ".tab" );
      _oscBname  = _descTable->getColumn( "oscB" ).at( entry );
      _oscBfile = _confDir + string( "/rx/osc/" ) + _oscBname + string( ".tab" );
      _modB = _descTable->getBoolColumn( "modB" ).at( entry );
      _modD = _descTable->getBoolColumn( "modD" ).at( entry );
      _psiMod = _descTable->getIntColumn( "psimod" ).at( entry );
      _cmOptics = _descTable->getBoolColumn( "cmOptics" ).at( entry );
      _cmDewarName = _descTable->getColumn( "cmdewar" ).at( entry );

      // cout << "dewarname = " << _dewarName << endl ;
      // cout << "cmOptics = " << _cmOptics << endl ;
      CPTRACE( Trace::TRACE7, "   Dewarname: " << _dewarName );
      CPTRACE( Trace::TRACE7, "   oscAD: " << _oscADname );
      CPTRACE( Trace::TRACE7, "   oscADfile: " << _oscADfile );
      CPTRACE( Trace::TRACE7, "   oscB: " << _oscBname );
      CPTRACE( Trace::TRACE7, "   oscBfile: " << _oscBfile );
      CPTRACE( Trace::TRACE7, "   modB: " << (boolalpha) << _modB );
      CPTRACE( Trace::TRACE7, "   modD: " << (boolalpha) << _modD );
      CPTRACE( Trace::TRACE7, "   psiMod: " << _psiMod );
      CPTRACE( Trace::TRACE7, "   newCal: " << (boolalpha) << _cmOptics );
      CPTRACE( Trace::TRACE7, "   cmDewar: " << _cmDewarName );
      break;
    }

    entry++;
  }

  if ( i == names.end() )
    throw CARMA_ERROR( "Unable to find " + _antenna
	+ " in " + _descTableFile );

  _drivesConfFile = string( _confDir + string( "/drives.tab" ) );
  _calwheelConfFile = string( _confDir + string( "/calwheel.tab" ) );
  _dewarConfFile = string( _confDir + string( "/rx/dewars.tab" ) );
  _cmDewarConfFile = string( _confDir + string( "/rx/cmDewars.tab" ) );
  _cmFocusConfFile = string( _confDir + string( "/focus/cmFocus.tab" ) );
  _dewarTempDir = string( _confDir + string( "/rx/tempsensors/" ) );
  _polConfFile = string( _confDir + string( "/rx/polplates.tab" ) );
  _telemConfFile = string( _confDir + string( "/telemetry.xml" ) );
}

string Configuration::getAntenna()
{
  return _antenna;
}

int Configuration::getAntennaNo()
{
  return _antno;
}

string Configuration::getDewarName()
{
  return _dewarName;
}

string Configuration::getoscADName()
{
  return _oscADname;
}

string Configuration::getoscBName()
{
  return _oscBname;
}

string Configuration::getDewarConfFile()
{
  return _dewarConfFile;
}

string Configuration::getTelemConfFile()
{
  return _telemConfFile;
}

string Configuration::getoscADFile()
{
  return _oscADfile;
}

string Configuration::getoscBFile()
{
  return _oscBfile;
}

string Configuration::getDewarTempDir()
{
  return _dewarTempDir;
}

string Configuration::getConfDir()
{
  return _confDir;
}

string Configuration::getDescTableFile()
{
  return _descTableFile;
}

string Configuration::getCmDewarName()
{
  return _cmDewarName;
}

string Configuration::getCmDewarConfFile()
{
  return _cmDewarConfFile;
}

string Configuration::getCmFocusConfFile()
{
  return _cmFocusConfFile;
}

ostream& operator<<( ostream& os, Configuration& config )
{
  os << "  Configuration directory:" << config.getConfDir() << endl;
  os << "  Telemetry file:" << config.getTelemConfFile() << endl;
  os << "  Description table filename: " << config.getDescTableFile() << endl;
  os << "   Dewar name: " << config.getDewarName() << endl;
  os << "   Dewar file: " << config.getDewarConfFile() << endl;
  os << "    Dewar temp dir: " << config.getDewarTempDir() << endl;
  os << "   oscAD name: " << config.getoscADName() << endl;
  os << "    oscAD file: " << config.getoscADFile() << endl;
  os << "   oscB name: " << config.getoscBName() << endl;
  os << "    oscB file: " << config.getoscBFile() << endl;
  os << "   modB: " << (boolalpha) << config.hasModulatorB() << endl;
  os << "   modD: " << (boolalpha) << config.hasModulatorD() << endl;
  os << "   psiMod: " << config.getPSIMod() << endl;
  os << "  Drives name: " << config.getAntenna() << endl;
  os << "   Drives file: " << config.getDrivesConfFile() << endl;
  os << "  Calwheel file: " << config.getCalwheelConfFile() << endl;
  os << "  cmDewar name: " << config.getCmDewarName() << endl;
  os << "  cmDewar File: " << config.getCmDewarConfFile() << endl;

  return os;
}

bool Configuration::isEmulating()
{
  return _emulate;
}

void Configuration::setEmulate( bool status )
{
  _emulate = status;
}

std::string Configuration::getDrivesConfFile()
{
  return _drivesConfFile;
}

std::string Configuration::getPolConfFile()
{
  return _polConfFile;
}

std::string Configuration::getCalwheelConfFile()
{
  return _calwheelConfFile;
}

