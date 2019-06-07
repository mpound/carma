/**@file
 * 
 *
 * <dl><dt><b>Author </b></dt><dd>Colby Gutierrez-Kraybill</dl>
 * $Revision: 1.35 $
 * $Date: 2013/02/13 15:48:17 $
 * $Id: Telemetry.h,v 1.35 2013/02/13 15:48:17 friedel Exp $
 */

#ifndef CARMA_ANTENNA_BIMA_TELEMETRY_H
#define CARMA_ANTENNA_BIMA_TELEMETRY_H

// C++ Standard library includes
#include <map>
#include <string>
#include <vector>
#include <list>
#include <iostream>
#include <ostream>
#include <queue>

// C includes
#include <time.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/time.h>

// Xerces includes
#include <xercesc/parsers/SAXParser.hpp>
#include <xercesc/validators/DTD/DTDValidator.hpp>

// CARMA includes
#include "carma/util/Program.h"
#include "carma/util/Trace.h"
#include "carma/antenna/bima/Configuration.h"
#include "carma/antenna/bima/SemaphoreOperator.h"
#include "carma/antenna/bima/TelemetryConfigHandler.h"
#include "carma/antenna/bima/TelemetryCommand.h"
#include "carma/antenna/bima/SharedMemory.h"
#include "carma/util/IPQreader.h"
#include "carma/util/IPQwriter.h"
#include "carma/services/Global.h"
#include "carma/util/Logger.h"
#include "carma/util/ErrorException.h"
#include "carma/util/Time.h"
#include "carma/canbus/Utilities.h"
#include "carma/canbus/exceptions.h"
#include "carma/canbus/CanDio.h"

// This needs to be defined in some sorta globals area..  See Drives.h
#define TILTSAMPLES	100

#ifndef BIMA_DEBUG
#define BIMA_DEBUG 9
#endif

#define RESOLVERSAMPLES	12
#define AZRESOLVER	"AZRESOLV"
#define AZRESOLVSAMPLES	"AZRSMPLS"
#define ELRESOLVER	"ELRESOLV"
#define ELRESOLVSAMPLES	"ELRSMPLS"

#define ENCODERSAMPLES	12
#define AZCOSENC	"AZCOSEN"
#define AZCOSENCSAMPLES "AZCESMPLS"
#define ELCOSENC	"ELCOSEN"
#define ELCOSENCSAMPLES "ELCESMPLS"
#define AZSINENC	"AZSINEN"
#define AZSINENCSAMPLES "AZSESMPLS"
#define ELSINENC	"ELSINEN"
#define ELSINENCSAMPLES "ELSESMPLS"

#define AZRESOLVOUT	"AZROUTLRS"
#define ELRESOLVOUT	"ELROUTLRS"
#define AZCOSENCOUT	"AZCEOUTLRS"
#define AZSINENCOUT	"AZSEOUTLRS"
#define ELCOSENCOUT	"ELCEOUTLRS"
#define ELSINENCOUT	"ELSEOUTLRS"

/*
  define MASSAGEENCVALUE( a )	\
          if ( (a & 0x8000) != 0 ) a = ~(a & 0x7fff)+1; \
*/
// Disable MASSAGENCVALUE FOR NOW...
#define MASSAGEENCVALUE( a ) ;


/** @class carma::antenna::bima::Telemetry
 * Server stub for Telemetry
 * Documentation for the Telemetry API may be found at
 * <A HREF="tbd">tbd</A>
 *
 * The canbus interface for the Telemetry systems is documented in the
 * <A HREF="http://www.mmarray.org/project/WP/">
 * CANBus API No. 144.</A>
 */

namespace carma {
  
  namespace antenna {

    namespace bima {
    
    
    /**
     * Telemetry Device class implementation.
     * This class implements the carma::canbus::Device base class for
     * <A HREF="http://www.mmarray.org/project/WP/LobeRotator/hw/">
     * Carma CAN API 144</A> describing the Telemetry CAN device.
     */
    class Telemetry
      {
      public:
	
	/** 
	 * Constructor.
	 * Creates a Telemetry device with the given node id.
	 * @param node Node id of device.
	 * @param io Reference to CanOutput class.
	 */
	Telemetry( int canbusBoardNo, int canbusSlotNo, Configuration &config, SharedMemory *shm );

	/**
	 * Destructor
	 */
	virtual ~Telemetry();
	
	/**
	 * Retrieve a map of this devices half second monitor points.
	 * The monitor points returned from this routine will be
	 * simulated if the device is in the OFFLINE state.
	 * @return map of string description of half second monitors
	 * keyed by msgType (message id).
	 */
	std::map<carma::canbus::msgType, std::string> getHalfSecMonitors() const;
	
	/**
	 * Retrieve a map of this devices slow monitor points.
	 * These monitor points will be simulated every 5 seconds if
	 * the device is in the OFFLINE state.
	 * @return map of string description of slow (5 second) monitors
	 * keyed by msgType (message id).
	 */
	std::map<carma::canbus::msgType, std::string> getSlowMonitors() const;
	
	/**
	 * Process a CAN message.
	 * This routine is responsible for processing all CAN messages
	 * addressed to this device.
	 * @param mid the 10bit message id (carma::canbus::msgType)
	 * @param data reference to the byte vector containing the raw data.
	 */
	void processMsg(carma::canbus::msgType mid,
            double rxMJD, std::vector<carma::canbus::byteType> data, bool sim );
	
	// useful utility function for asserting data payload validity
	bool assertDataAndSize( std::vector<carma::canbus::byteType> &data );

	/**
	 * Produce a simulated CAN message for a given msgType.
	 * This routine creates a Message with simulated data for the 
	 * input message id.  The returned message is automatically
	 * placed in the CAN message queue for retrieval and processing
	 * by the Master class.  When this device is OFFLINE, it is called for
	 * each message type returned from Loberotator::getSlowMonitors and
	 * Loberotator::getHalfSecMonitors and thus tests the processMsg()
	 * method and monitor system. Note that this routine is called by
	 * the carma::canbus::Master base class automatically.
	 * @param mid Message id of CAN message to simulate.
	 * @return Simulated CAN message.
	 */
	carma::canbus::Message simulateMsg(carma::canbus::msgType mid);
	
	/**
	 * Staticly retrieve the API Id.
	 * This is a helper routine and does the same thing as the getApi
	 * member function with the exception that it can be called staticly.
	 * @return api id of device.
	 */
	static carma::canbus::apiType getApiId();
        
	
	/**
	 * Engineering commands
	 */
	void resetNode( );

	void run( void );
	static void *startReaderThread( void *arg );
	static void *startWriterThread( void *arg );
	static void *startEmulateReaderThread( void *arg );
	static void *startEmulateWriterThread( void *arg );
	void readerThread( void );
	void writerThread( void );
	void emulateReaderThread( void );
	void emulateWriterThread( void );

	// Commands
	static const carma::canbus::msgType RESET             = 0x000;
	static const carma::canbus::msgType VMEWRITE          = 0x001;
	static const carma::canbus::msgType TOGGLEBIT         = 0x002;
	static const carma::canbus::msgType WRITESPACEQRY     = 0x003;
	static const carma::canbus::msgType READSPACEQRY      = 0x004;
	static const carma::canbus::msgType TMTYDISABLE       = 0x005;
	static const carma::canbus::msgType TMTYENABLE        = 0x006;
        static const carma::canbus::msgType SETBITS           = 0x007;
	static const carma::canbus::msgType QUERYLOG          = 0x3F0;
	static const carma::canbus::msgType SERIALENABLE      = 0x3F1;
	static const carma::canbus::msgType SERIALDISABLE     = 0x3F2;
	static const carma::canbus::msgType CLEARLOG          = 0x3F4;
	static const carma::canbus::msgType DOWNLOAD          = 0x3FD;

      private:
	
	static const carma::canbus::apiType API_ID                   = 144;

        // Response to Commands
	static const carma::canbus::msgType ERROR_RECORD_1           = 0x1F0;
	static const carma::canbus::msgType ERROR_RECORD_2           = 0x1F1;
	static const carma::canbus::msgType WRITESPACERPY            = 0x1F2;
	static const carma::canbus::msgType READSPACERPY             = 0x1F3;

	// Fast packets
	static const carma::canbus::msgType FAST_PACKET_01           = 0x100;
	static const carma::canbus::msgType FAST_PACKET_02           = 0x101;
	static const carma::canbus::msgType FAST_PACKET_03           = 0x102;
	static const carma::canbus::msgType FAST_PACKET_04           = 0x103;
	static const carma::canbus::msgType FAST_PACKET_05           = 0x104;
	static const carma::canbus::msgType FAST_PACKET_06           = 0x105;
	static const carma::canbus::msgType FAST_PACKET_07           = 0x106;
	static const carma::canbus::msgType FAST_PACKET_08           = 0x107;
	static const carma::canbus::msgType FAST_PACKET_09           = 0x108;
	static const carma::canbus::msgType FAST_PACKET_10           = 0x109;
	static const carma::canbus::msgType FAST_PACKET_11           = 0x10A;
	static const carma::canbus::msgType FAST_PACKET_12           = 0x10B;
	static const carma::canbus::msgType FAST_PACKET_13           = 0x10C;
	static const carma::canbus::msgType FAST_PACKET_14           = 0x10D;
	static const carma::canbus::msgType SLOW_PACKET_01           = 0x120;
	static const carma::canbus::msgType SLOW_PACKET_02           = 0x121;
	static const carma::canbus::msgType SLOW_PACKET_03           = 0x122;
	static const carma::canbus::msgType SLOW_PACKET_04           = 0x123;
	static const carma::canbus::msgType SLOW_PACKET_05           = 0x124;
	static const carma::canbus::msgType SLOW_PACKET_06           = 0x125;
	static const carma::canbus::msgType SLOW_PACKET_07           = 0x126;
	static const carma::canbus::msgType SLOW_PACKET_08           = 0x127;
	static const carma::canbus::msgType SLOW_PACKET_09           = 0x128;
	static const carma::canbus::msgType SLOW_PACKET_10           = 0x129;
	static const carma::canbus::msgType SLOW_PACKET_11           = 0x12A;
	static const carma::canbus::msgType SLOW_PACKET_12           = 0x12B;
	static const carma::canbus::msgType SLOW_PACKET_13           = 0x12C;
	static const carma::canbus::msgType SLOW_PACKET_14           = 0x12D;

	// Looking for other CANBus msgType definitions?  Go look in telemetry.xml
	
	// Copy and assignment not permitted for this class.
	Telemetry( const Telemetry & );
	Telemetry &operator=( const Telemetry & );
	
	// Methods to process individual CAN messages.  These routines
	// are called by processMsg.
	void processFastPacket( std::vector<carma::canbus::byteType> &data );

	// Methods to produce simulated CAN messages.  These routines are
	// called by simulateMsg and then placed in the CAN message queue
	// where they will eventually be processed by processMsg above.


	std::map<carma::canbus::msgType, TelemetryInfo> _msgMap;
        std::map<unsigned short, std::string> _idMap;

	carma::util::Time _time;
        bool _emulate;
	log4cpp::Category &_logger;

	int _node;
	static carma::canbus::CanDio *_canBus; // shared between threads
	TelemetryConfigHandler *_tmConfig;
        SemaphoreOperator *_semOp;
	SharedMemory *_bimaShm;
	carma::util::IPQreader<TelemetryCommand> *_tcReader;
        std::ostringstream _errMsg;
        int *_tilt1samples;
        int _tilt1len;
        int *_tilt2samples;
        int _tilt2len;

        int *_azrsamples;
        int _azrlen;
        int *_elrsamples;
        int _elrlen;
        int *_azcsamples;
        int _azclen;
        int *_elcsamples;
        int _elclen;
        int *_azssamples;
        int _azslen;
        int *_elssamples;
        int _elslen;

	std::string getTelemetryVersion();
	void setTelemetryVersion( std::string ver );


	int _lastByteA, _lastByteB, _lastByteC,
	    _lastByteAI, _lastByteBI, _lastByteCI,
	    _lastByteAC, _lastByteBC, _lastByteCC;

      };  

    } // Namespace bima
  } // Namespace antenna
} // Namespace carma 


#endif // CARMA_ANTENNA_BIMA_TELEMETRY_H
