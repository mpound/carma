/**@file
 * Class definition for Stepper Motors for BIMA systems.
 * This class is derived from existing BIMA code to
 * generally describe motor parameters.
 * The original structure was defined in inc/nrcvr.h
 *
 * <dl><dt><b>Author </b></dt><dd>Colby Gutierrez-Kraybill</dl>
 * $Revision: 1.21 $
 * $Date: 2013/02/20 18:07:18 $
 * $Id: Configuration.h,v 1.21 2013/02/20 18:07:18 friedel Exp $
 */



#ifndef CARMA_ANTENNA_BIMA_CONFIGURATION_H
#define CARMA_ANTENNA_BIMA_CONFIGURATION_H

// C++
#include <vector>
#include <string>


// CARMA includes
#include "carma/util/Program.h"
#include "carma/util/Trace.h"
#include "carma/util/ErrorException.h"
#include "carma/services/Table.h"

#include "carma/antenna/bima/TelemetryConfigHandler.h"
#include "carma/antenna/bima/SemaphoreOperator.h"

#if 0
#define CPTRACE( A, B )
#endif 

#define IPQLEN	2000

namespace carma
{
  namespace antenna
  {
    namespace bima
    {
      class Configuration
      {

	public:
	  Configuration( std::string antenna, std::string confDir, bool emulate = false );

	  std::string getAntenna();
	  int getAntennaNo();
	  bool isEmulating();
	  void setEmulate( bool status );
	  std::string getConfDir();
	  std::string getDewarName();
	  std::string getoscADName();
	  std::string getoscADFile();
	  std::string getoscBName();
	  std::string getoscBFile();
	  std::string getDewarTempDir();
	  std::string getDewarConfFile();
	  std::string getTelemConfFile();
	  std::string getDescTableFile();
	  std::string getDrivesConfFile();
	  std::string getPolConfFile();
	  std::string getCalwheelConfFile();
	  std::string getMotorsName();
	  std::string getMotorsConfFile();
	  bool hasModulatorB() { return _modB; }
	  bool hasModulatorD() { return _modD; }
	  bool cmOpticsInstalled() { return _cmOptics; }
	  std::string getCmDewarName();
	  std::string getCmDewarConfFile();
	  std::string getCmFocusConfFile();
	  int getPSIMod() { return _psiMod; }

	  void setTelemConfigHandlerP( TelemetryConfigHandler *tmCfg )
	  { _tmConfig = tmCfg; };
	  TelemetryConfigHandler *getTelemConfigHandlerP()
	  { return _tmConfig; };

	  void setSemOpP( SemaphoreOperator *semOp )
	  { _semOp = semOp; };
	  SemaphoreOperator *getSemOpP()
	  { return _semOp; };

	  char mytoupper( char c );

	  const static std::string _dewarConfSuffix;
	  const static std::string _dewarTempMeat;
	  const static std::string _telemConfSuffix;

	private:
	  int _antno;
	  std::string _descTableFile;
	  std::string _dewarName;
	  std::string _antenna;
	  std::string _confDir;
	  std::string _oscADname;
	  std::string _oscADfile;
	  std::string _oscBname;
	  std::string _oscBfile;
	  std::string _dewarTempDir;
	  std::string _dewarConfFile;
	  std::string _telemConfFile;
	  std::string _drivesConfFile;
	  std::string _polConfFile;
	  std::string _calwheelConfFile;
	  std::string _motorsName;
	  std::string _motorsConfFile;
	  bool _modB, _modD, _cmOptics;
	  std::string _cmDewarName;
	  std::string _cmDewarConfFile;
	  std::string _cmFocusConfFile;
	  int _psiMod;


	  carma::services::Table* _descTable;

	  bool _emulate;

	  TelemetryConfigHandler *_tmConfig;
	  SemaphoreOperator *_semOp;

      }; // class Configuration

    } // namespace bima
  } // namespace antenna
} // namespace carma

::std::ostream& operator<<( ::std::ostream& os,
    ::carma::antenna::bima::Configuration& config );

#endif // CARMA_ANTENNA_BIMA_CONFIGURATION_H
