/**@file
 * Class definition for Temperature  on BIMA systems.
 *
 * <dl><dt><b>Author </b></dt><dd>Colby Gutierrez-Kraybill</dl>
 * $Revision: 1.22 $
 * $Date: 2012/12/05 00:19:15 $
 * $Id: Dewar.cc,v 1.22 2012/12/05 00:19:15 eml Exp $
 */


// CARMA includes
#include "carma/antenna/bima/Dewar.h"

#include <cmath>

using namespace std;
using namespace carma::util;
using namespace carma::services;
using namespace carma::antenna::bima;


Dewar::Dewar( Configuration& config )
     : TelemetryClient( config ), _config( config )
{
  _name = _config.getDewarName();
  _cmName = _config.getCmDewarName();

  lookUpDewar();
  loadDiodeCurves();
}

void Dewar::lookUpDewar()
{
  Table t( _config.getDewarConfFile() );

  CPTRACE( Trace::TRACE3, "Scanning " << _config.getDewarConfFile()
                          << " for '" << _name << "'" );

  vector<string> dewars = t.getColumn( "name" );

  vector<string>::iterator i;
  int entry = 0;
  for ( i = dewars.begin(); i != dewars.end(); ++i )
  {
    CPTRACE( Trace::TRACE3, "Dewar '" << *i << "'" );

    if ( i->compare( _name ) == 0 )
    {
      CPTRACE( Trace::TRACE3, "Found it!" );

      _stageFiles.push_back( _config.getDewarTempDir() 
                             + t.getColumn( "stage1" ).at( entry )
                             + string( ".tab" ) );
      _stageFiles.push_back( _config.getDewarTempDir()
                             + t.getColumn( "stage2" ).at( entry )
                             + string( ".tab" ) );
      _stageFiles.push_back( _config.getDewarTempDir() 
                             + t.getColumn( "stage3" ).at( entry )
                             + string( ".tab" ) );
      _stageFiles.push_back( _config.getDewarTempDir()
                             + t.getColumn( "stage4" ).at( entry )
                             + string( ".tab" ) );
      _stageFiles.push_back( _config.getDewarTempDir()
                             + t.getColumn( "stage5" ).at( entry )
                             + string( ".tab" ) );

      _sisMixerBName = t.getColumn( "mixerB" ).at( entry ); 
      _sisMixerBConfFile = _config.getConfDir() + string( "/rx/sis/" )
                          + _sisMixerBName + string( ".tab" );
      _gateB = t.getDoubleColumn( "gateB" ).at( entry );

      _sisMixerDName = t.getColumn( "mixerD" ).at( entry ); 
      _sisMixerDConfFile = _config.getConfDir() + string( "/rx/sis/" )
                          + _sisMixerDName + string( ".tab" );
      _gateD = t.getDoubleColumn( "gateD" ).at( entry );

      _powexp = t.getDoubleColumn( "powexp" ).at( entry );
      _safeatten = t.getIntColumn( "safeatten" ).at( entry );

      _gainp = t.getDoubleColumn( "gainp" ).at( entry );
      _gaini = t.getDoubleColumn( "gaini" ).at( entry );

      _hmincnt = t.getIntColumn( "hmincnt" ).at( entry );
      _hmaxcnt = t.getIntColumn( "hmaxcnt" ).at( entry );

      _tuningmode = t.getColumn( "mode" ).at( entry );

      break;
    }
 
    entry++;
  }

  if ( i == dewars.end() )
    throw CARMA_ERROR( "Unable to find " + _name + " in "
	+ _config.getDewarConfFile()
	+ " (First check dewar conf file and then check entry "
	+ "for '" + _config.getAntenna() + "' in "
	+ _config.getDescTableFile() + ")" );

  // get the CM Rx stuff
  Table ct( _config.getCmDewarConfFile() );

  CPTRACE( Trace::TRACE3, "Scanning " << _config.getCmDewarConfFile()
                          << " for '" << _cmName << "'" );

  vector<string> cmDewars = ct.getColumn( "name" );
  CPTRACE( Trace::TRACE3, "HAVE NAMES");

  vector<string>::iterator ci;
  CPTRACE(Trace::TRACE3,"iterator");
  entry = 0;
  for ( ci = cmDewars.begin(); ci != cmDewars.end(); ++ci )
  {
    CPTRACE( Trace::TRACE3, "CmDewar '" << *ci << "'" );

    if ( ci->compare( _cmName ) == 0 )
    {
      CPTRACE( Trace::TRACE3, "Found it!" );
      for(unsigned int j = 0; j < 4; j++){
	ostringstream vstage,istage;
	vstage << "Vd" << j+1;
	istage << "Id" << j+1;
	double gateVoltage = ct.getDoubleColumn( vstage.str() ).at( entry );

	// EML: Don't convert to mV at this point -- Doug does this
	// again later in the call to set30GHzGateVoltage()

	_gateVoltage[j] = gateVoltage;
	double drainCurrent = ct.getDoubleColumn( istage.str() ).at( entry );

	// EML: Don't convert to 0.01 mA at this point -- Doug does this
	// again later in the call to set30GHzDrainCurrente()

	_drainCurrent[j] = drainCurrent;
      }

      // EML: Don't convert to 0.01 mA at this point -- Doug does this
      // again later in the call to set30GHzIFDrainCurrent()

      _ifCurrent = ct.getDoubleColumn( "Ig" ).at( entry );

      break;
    }
 
    entry++;
  }
  if ( ci == cmDewars.end() )
    throw CARMA_ERROR( "Unable to find " + _cmName + " in "
	+ _config.getCmDewarConfFile()
	+ " (First check dewar conf file and then check entry "
	+ "for '" + _config.getAntenna() + "' in "
	+ _config.getDescTableFile() + ")" );

}

void Dewar::loadDiodeCurves()
{
  _t = loadACurve( _config.getDewarTempDir() + string( "t.tab" ) );
  _curve10 = loadACurve( _config.getDewarTempDir() + string( "curve10.tab" ) );
  _stage1Curve = loadACurve( _stageFiles.at(0) );
  _stage2Curve = loadACurve( _stageFiles.at(1) );
  _stage3Curve = loadACurve( _stageFiles.at(2) );
  _stage4Curve = loadACurve( _stageFiles.at(3) );
  _stage5Curve = loadACurve( _stageFiles.at(4) );
}

vector<double> Dewar::loadACurve ( string filename )
{
  Table *t;
  ostringstream oss;

  try
  {
    t = new Table( filename );
  } 
  catch ( carma::util::FileNotFoundException &fnfex )
  {
    oss << fnfex.what() << ".  Check entry for '"
      << _config.getDewarName() << "' in file "
      << _config.getDewarConfFile();
    throw CARMA_ERROR( oss.str() );
  }

  return t->getDoubleColumn(0);
}

double Dewar::tempConvert( std::vector<double> *tryThisCurve, double volts )
{
  std::vector<double> *curve;
  double temp;

  if ( volts < 1.17 )
  {
    curve = &_curve10;
  }
  else
    curve = tryThisCurve;

  int i = 10;

  while ( ( (int)( curve->size() - 1 ) > i ) &&  ( curve->at(i+1) > volts ) )
    i++;

  while ( ( curve->at(i) < volts ) && ( i > 0 ) )
    i--;

  if ( i == 0 )
  {
    temp = 2.0;
  }
  else if ( i == (int)_curve10.size() - 1 )
  {
    temp = 475.0;
  }
  else
  {
    temp = (double)( _t.at(i) + ( volts - curve->at(i) )
                     * ( _t.at(i+1) - _t.at(i) )
                     / ( curve->at(i+1) - curve->at(i) ) );
  }
  
  return temp;
}

double Dewar::stage1temp()
{
  return ( tempConvert( &_stage1Curve, diodeRawToVolts( atodin( "SENSOR1" ) ) ) );
}

double Dewar::stage2temp() // Tempature in Kelvins on stage4
{
  return ( tempConvert( &_stage2Curve, diodeRawToVolts( atodin( "SENSOR2" ) ) ) );
}

double Dewar::stage3temp() // Tempature in Kelvins on stage4
{
  return ( tempConvert( &_stage3Curve, diodeRawToVolts( atodin( "SENSOR3" ) ) ) );
}

double Dewar::stage4temp() // Tempature in Kelvins on stage4
{
  return ( tempConvert( &_stage4Curve, diodeRawToVolts( atodin( "SENSOR4" ) ) ) );
}

double Dewar::stage5temp() // Tempature in Kelvins on stage5
{
  return ( tempConvert( &_stage5Curve, diodeRawToVolts( atodin( "SENSOR5" ) ) ) );
}

double Dewar::cmStage1temp()
{
  return ( tempConvert( &_stage1Curve, diodeRawToVolts( atodin( "CMSENSOR1" ) ) ) );
}

double Dewar::cmStage2temp() // Tempature in Kelvins on stage4
{
  return ( tempConvert( &_stage2Curve, diodeRawToVolts( atodin( "CMSENSOR2" ) ) ) );
}

double Dewar::cmStage3temp() // Tempature in Kelvins on stage4
{
  return ( tempConvert( &_stage3Curve, diodeRawToVolts( atodin( "CMSENSOR3" ) ) ) );
}

double Dewar::cmStage4temp() // Tempature in Kelvins on stage4
{
  return ( tempConvert( &_stage4Curve, diodeRawToVolts( atodin( "CMSENSOR4" ) ) ) );
}

double Dewar::getHeater3mW()
{
  double mA = atodin( "HEATER3READ" ) * 4.516129/32768.*10.;
  // HEATER3SET is no longer returned in the telemetry, so
  // it is retreived from shared mem directly without
  // conversion
  //double v  = atodin( "HEATER3SET" ) * 12.1/2048.; 
  double v;
  getData( "HEATER3SET", &v );

//  return ( mA * v );
  return mA ;
}

void Dewar::setHeater3V( double volts )
{
 unsigned short hexval = (unsigned short)(2048*volts/12.1);
 tpoke( "HEATER3SET", hexval );
 putData( "HEATER3SET", &volts );
}


string Dewar::getSISMixerBName()
{
  return _sisMixerBName;
}

string Dewar::getSISMixerBConfFile()
{
  return _sisMixerBConfFile;
}

string Dewar::getSISMixerDName()
{
  return _sisMixerDName;
}

string Dewar::getSISMixerDConfFile()
{
  return _sisMixerDConfFile;
}


double Dewar::getCompressorInletTemp()
{
  return thermistor(( atodin( "TINHECP" ) * THERMI ));
}

double Dewar::getCompressorDischTemp()
{
  return thermistor(( atodin( "TDISHECP" ) * THERMI ));
}

double Dewar::getCompressorExchTemp()
{
  return thermistor(( atodin( "TEXCHHEC" ) * THERMI ));
}

double Dewar::getCompressorSumpTemp()
{
  return thermistor(( atodin( "TSUMPHEC" ) * THERMI ));
}

double Dewar::getHeSupplyP()
{
  return psi(atodin( "HESUPPRS" ));
}

double Dewar::getHeReturnP()
{
  return psi( atodin( "HERETPRS" ));
}

double Dewar::getGainI()
{
  return _gaini;
}

void Dewar::setGainI( double set )
{
  _gaini = set;
}

double Dewar::getGainP()
{
  return _gainp;
}

void Dewar::setGainP( double set )
{
  _gainp = set;
}

int Dewar::getHeaterMinCnt()
{
  return _hmincnt;
}

int Dewar::getHeaterMaxCnt()
{
  return _hmaxcnt;
}

std::string Dewar::getSISTuningMode()
{
  return _tuningmode;
}

double Dewar::getPowExp()
{
  return _powexp;
}

int Dewar::getSafeAtten()
{
  return _safeatten;
}

double Dewar::getGateVoltage(unsigned short deviceID){
  return _gateVoltage[deviceID - 1];
}

double Dewar::getDrainCurrent(unsigned short deviceID){
  return _drainCurrent[deviceID - 1];
}

double Dewar::getIFCurrent(){
  return _ifCurrent;
}

ostream& operator<<( ostream& os, Dewar& dewar )
{
  os << "  Dewar configuration info:" << endl;
  os << "    SIS Mixer B Name: " << dewar.getSISMixerBName() << endl;
  os << "    SIS Mixer B Conf File: " << dewar.getSISMixerBConfFile() << endl;
  os << "    SIS Mixer D Name: " << dewar.getSISMixerDName() << endl;
  os << "    SIS Mixer D Conf File: " << dewar.getSISMixerDConfFile() << endl;
  os << "    SIS Tuning Mode: " << dewar.getSISTuningMode() << endl;
  return os;
}

