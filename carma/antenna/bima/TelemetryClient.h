

/**@file
 * Class definition for TelemetryClient on the BIMA antennas.
 *
 * <dl><dt><b>Author </b></dt><dd>Colby Gutierrez-Kraybill </dl>
 * $Revision: 1.26 $
 * $Date: 2008/02/27 02:27:18 $
 * $Id: TelemetryClient.h,v 1.26 2008/02/27 02:27:18 colby Exp $
 */


#ifndef CARMA_BIMA_TELEMETRYCLIENT_H
#define CARMA_BIMA_TELEMETRYCLIENT_H

#include <iosfwd>
#include <string>
#include <map>
#include <vector>

#include <sys/poll.h>

#include <xercesc/parsers/SAXParser.hpp>
#include <xercesc/validators/DTD/DTDValidator.hpp>

#include "carma/antenna/bima/TelemetryConfigHandler.h"
#include "carma/antenna/bima/TelemetryCommand.h"
#include "carma/antenna/bima/Telemetry.h"
#include "carma/antenna/bima/SharedMemory.h"

#include "carma/util/ScopedPthreadMutexLock.h"
#include "carma/util/IPQwriter.h"
#include "carma/util/ErrorException.h"
#include "carma/util/Logger.h"
#include "carma/util/Trace.h"

// Constants defined to compute conversion factors
// for units...
// These come directly from hatcreek/subs/misc/atod.c
#define VOLTS15 4.516129/32768.*3.608
#define VOLTS10 4.516129/32768.*2.697
#define VOLTS10P 4.516129/32768.*2.758 
#define VOLTS5 4.516129/32768.*1.347
#define VOLTS4 4.516129/32768.
#define VOLTS1P2 4.516129/32768.*.280
#define VOLTS1 4.516129/32768.*.250
#define VOLTSP4 4.516129/32768.*.100
// Milivolts mv
#define MVOLTS 4.516129/32768.*5.
// microAmps uA 402 ohm sense resistor
#define UAMPS 4.516129/32768.*124.38
// Temp sensors volts
#define TVOLTS 4.516129/32768./2.5
#define THERMI 4.516129/32768.

#define BIMASTATUSINFO(T,S) \
{ \
  std::ostringstream bsioss; \
  bsioss << S ; \
  T.setStatusInfo( bsioss.str() ); \
}

namespace carma
{
  namespace antenna
  {
    namespace bima
    {
      class TelemetryClient : public SharedMemory
      {
	public:
	  TelemetryClient( Configuration& config, bool checkTelemHost = true );
	  TelemetryClient();
	  ~TelemetryClient();

	  bool status();

	  void tpoke( const char *ccname, unsigned char value = 1);
	  void tpoke( const char *ccname, unsigned short value = 1 );
	  void tpoke( const char *ccname, unsigned long value = 1 );

	  void tpeekWriteSpace( const char *ccname, unsigned short *value );
	  void tpeek( const char *ccname, unsigned short *value );
	  void tpeek( const char *ccname, unsigned long *value );

	  int atodin ( const char *ccname );
	  int atodin ( int value )
	  {
	    if ( value & 0x8000 )
	      value |= 0xffff0000;

	    return -value;
	  };
	  double thermistor( double volts );
	  double psi( int value );
	  void limit( double& value, double min, double max );

	  bool atodSaturated();

	  void setbits( const char *bitsin, unsigned char value, unsigned char mask );

	  void cpoke( unsigned short msgId, std::vector<carma::canbus::byteType> data );

	  void enableToggleBit( const char *name, int mask, int update );
	  void disableToggleBit( void );
	  void enableMonitorPackets( void );
	  void disableMonitorPackets( void );
	  void serialEnable( void );
	  void serialDisable( void );

	  Configuration& getConfig() { return _config; }

	  unsigned char getStatusByteA();
	  unsigned char getStatusByteB();
	  unsigned char getStatusByteC();

	  // Status Control Box bit info
	  bool isElLim();
	  bool isElULim();
	  bool isAzLim();
	  bool isAzULim();
	  bool isKey();
	  bool isWtrPrssrNotNorm();
	  bool isCollisionDectOff();
	  bool isCollision();
	  bool isAzDrvTempNorm();
	  bool isElDrvTempNorm();
	  bool isRxTempNorm();
	  bool isCabTempNorm();
	  bool isSpareTempNorm();
	  bool isCameraSafe();
	  bool isComputerCtl();
	  bool isCamFlapOpen();
	  bool isCamFlapBypass();
	  bool isCabPwrOff();

	  int getNumByteAErrs();
	  int getNumByteBErrs();
	  int getNumByteCErrs();

	  // Status string info for some useful info to be
	  // passed back to RTD windows mostly...
	  void setStatusInfo( std::string msg );
	  std::string getStatusInfo();

	  std::string getFirmwareVersion();
	  std::string getTelemetryVersion();
	  void setTelemetryVersion( std::string ver );

	protected:
	  Configuration &_config;

	private:
	  static TelemetryConfigHandler *_tmConfig;
	  static SemaphoreOperator *_semOp;
	  static ::carma::util::PthreadMutex mConfigGuard; 
	  static std::map<std::string, unsigned short> _nameMap;
	  ::carma::util::IPQwriter<TelemetryCommand> *_tcWriter;

      };
    }
  }
}



#endif // CARMA_BIMA_TELEMETRYCLIENT_H
