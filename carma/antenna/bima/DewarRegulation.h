/**@file
 *
 * <dl><dt><b>Author </b></dt><dd>Colby Gutierrez-Kraybill</dl>
 * $Revision: 1.9 $
 * $Date: 2006/04/28 21:19:35 $
 * $Id: DewarRegulation.h,v 1.9 2006/04/28 21:19:35 colby Exp $
 */



#ifndef CARMA_ANTENNA_BIMA_DEWARREGULATION_H
#define CARMA_ANTENNA_BIMA_DEWARREGULATION_H

#include <stdio.h>

#include <list>
#include <string>

// System includes
#include <sys/time.h>
#include <unistd.h>

// CARMA includes
#include "carma/util/Program.h"
#include "carma/util/Logger.h"
#include "carma/services/Table.h"
#include "carma/antenna/bima/TelemetryClient.h"
#include "carma/antenna/bima/Dewar.h"
#include "carma/antenna/bima/Configuration.h"

#include "carma/util/ErrorException.h"
#include "carma/util/ExceptionUtils.h"


namespace carma
{
  namespace antenna
  {
    namespace bima
    {
      class TimedValue
      {
	public:
	  TimedValue( time_t t, float value )
	  {
	    _t = t;
	    _v = value;
	  }

	  time_t _t; // time in seconds from epoch
	  float _v;  
      };

      class DewarRegulation : public SharedMemory
      {

	public:
	  DewarRegulation(
	      Configuration& config,
	      bool server,
	      bool emulate );

	  ~DewarRegulation();

	  bool alreadyRunning();
	  bool isOk();
	  void updateWatchdog();
	  void setPoint( double temp );
	  double getPoint();
	  double getPointMJD();
	  void setHeater( double temp );
	  void diagnosticLog( double temp, double min, double max );
	  void startDiagLog( bool start );

	  // Calls regulate
	  static void thread( DewarRegulation &This );
	  void regulate();

	  bool isOn();
	  void on();
	  void off();
	  void defrost();
	  void cancelDefrost();
	  bool isDefrosting();

	  void setRunningV( float volts );
	  void setMaxV( float volts ); 
	  void setMinV( float volts ); 
	  float getRunningV();
	  float getMaxV();
	  float getMinV();
	  void setAvgTemp( float t );
	  void setMaxAvgTemp( float t );
	  void setMinAvgTemp( float t );
	  void computeAvgTempSetPointRMS( std::list<TimedValue> &aList );
	  float getTempSetPointRMS();
	  float getAvgTemp();
	  float getMaxAvgTemp();
	  float getMinAvgTemp();

	  void cullEntriesOlderThan( std::list<TimedValue> &aList, time_t t );
	  void insertAndCullMaxMin( std::list<TimedValue> &aList, float value );
	  Dewar & getDewar() { return _dewar; }
	  double getStage1Temp() { return _dewar.stage1temp(); };
	  double getStage2Temp() { return _dewar.stage2temp(); };
	  double getStage3Temp() { return _dewar.stage3temp(); };
	  double getStage4Temp() { return _dewar.stage4temp(); };
	  double getStage5Temp() { return _dewar.stage5temp(); };

	  void setStage( int stage );
	  double (DewarRegulation::*getTemp)();

	private:
	  static const std::string _poaName;
	  Dewar _dewar;
	  Configuration &_config;
	  log4cpp::Category &_log;
	  double _setPoint, _oldPoint;
	  bool _emulate;
	  time_t _defrostStart, _lastRMS;

	  std::list<TimedValue> _maxminT; // avg'd temps
	  std::list<TimedValue> _maxminV; // voltages
	  std::list<TimedValue> _tempList; // running avg of temps for rms

      }; // class DewarRegulation
    } // namespace bima
  } // namespace antenna
} // namespace carma

#endif // CARMA_ANTENNA_BIMA_DEWARREGULATION_H
