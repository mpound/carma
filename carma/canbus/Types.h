/** @file
 * Declarations of carma::canbus types. 
 * 
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.44 $
 * $Date: 2012/09/05 20:32:40 $
 * $Id: Types.h,v 1.44 2012/09/05 20:32:40 abeard Exp $
 */
#ifndef CARMA_CANBUS_TYPES_H
#define CARMA_CANBUS_TYPES_H

// C++ Standard Library includes 
#include <map>
#include <string>
#include <vector>

/** 
 * @ifnot carma
 * @mainpage 
 * @else
 * @page Canbus Carma CANBus Library 
 * @endif
 * 
 * This documentation describes the Carma CANbus Library. 
 *
 * All classes, structures, typedefs and global functions are documented in
 * the carma::canbus namespace.  Good starting points for learning
 * how to use the library are the carma::canbus::Device and 
 * carma::canbus::Master classes.
 * <A HREF="http://www.mmarray.org/project/system/API/canfaq.html">
 * The CARMA CANbus FAQ</A> documents common questions.
 *
 * Several implementations of this framework now exist and provide useful 
 * working examples.  These include the @ref WbdcHost, Loberotator and 
 * MasterClock applications as well as portions of SZA Antenna code.  
 * Code for the @ref WbdcHost application is extensively documented
 * under the carma::downconverter namespace and exists in CVS under 
 * carma/downconverter.
 *
 * In addition to the above starting points, implementors should look at the
 * carma::canbus::devices::CorbaXacDevice class which provides a base Device
 * class implementation for XAC based CAN devices which follow the standard
 * carma CANbus API template.  
 *
 * External links to canbus related documentation.
 * - <A HREF="http://www.mmarray.org/project/WP/CANbus/sw/"> 
 * Design docs for this library. </A>
 * - <A HREF="http://www.mmarray.org/project/WP/CANbus/hw/JANZ_OVRO_CAN/cmod comb.pdf"> Description of Janz hardware, pin mappings and
 *TTL board jumper settings - important! </A>
 * - <A HREF="http://www.mmarray.org/project/docs/Software/canbus/VMODican23u17USERSMANUAL.pdf"> Janz VMOD-ICAN CAN card users manual </A>
 * - <A HREF="http://www.mmarray.org/project/docs/Software/canbus/VMODICAN3mican3_12HWMANUAL.pdf"> Janz VMOD-ICAN CAN card hw manual </A>
 * - <A HREF="http://www.mmarray.org/project/docs/Software/canbus/VMOD-TTL.pdf"> Janz Digital IO card hw manual </A>
 * - <A HREF="http://www.mmarray.org/project/docs/Software/canbus/Manual_CMOD-IO_v2_0.pdf"> Janz Modulbus carrier board manual </A>
 *  
 */

namespace carma {
namespace canbus {
	
    typedef unsigned int idType; /**< Type for full 29 bit CAN id. */
    typedef unsigned char byteType; /**< Base raw CAN byte. */
	typedef unsigned short portType; /**< Port id type for multi-port boards.*/
    
	typedef unsigned short apiType; /**< Carma API id type. */
	typedef unsigned short boardType; /**< Carma Board Type id type. */
    typedef unsigned short nodeType; /**< Carma Node Type id type. */
	typedef unsigned short serialNumberType; /**< Carma Serial Number type.*/
    typedef unsigned int keyType; /**< Unique key identifier for each device.*/
	typedef unsigned short msgType;  /**< Carma Message id type. */
    typedef unsigned short busIdType; /**< Carma Bus Id type */
    /** Alias for CAN data. */
    typedef ::std::vector< ::carma::canbus::byteType> DataVector; 
    /** Alias for message descriptions keyed by message id. */
    typedef ::std::map< ::carma::canbus::msgType, ::std::string > MsgBriefMap;
    
    /**
     * Dummy packet api. 
     * This api id is reserved for dummy messages.
     */
    const apiType DUMMY_PKT_API = 0xFF;

    /**
     * Dummy packet message id. 
     * This message id is reserved for dummy messages.
     */
    const msgType DUMMY_PKT_MID    = 0x3FF;

    /**
     * Specifies that a message should be sent to ALL_BUSSES.
     */
    const busIdType ALL_BUSSES = 0xffff;
    
    /**
     * Mode type.
     * The mode type is extracted from the CAN id.
     * It is a single bit and designates how the application should
     * interpret the 29 bit CAN id.  
     */
    typedef enum modeTypeEnum {
		ENGINEERING = 1,  /**< Decode CAN Id in serial num, board type form */
		APPLICATION = 0,  /**< Decode CAN id in API, Node form */
	} modeType;
	
    /**
     * Bus State type.
     * The Bus states are standard CAN bus states which are
     * defined by the number of rx or tx errors recorded by 
     * the CAN controller.
     */
	typedef enum busStateEnum {
        NO_ERRORS = 0,             /**< Rx and tx error count = 0 */
	    ERROR_ACTIVE = 1,          /**< Rx and tx error count < 127 */
	    ERROR_PASSIVE = 2,         /**< Rx and tx error count < 255 */
	    BUS_OFF = 3,               /**< Rx and tx error count = 255 */
    } busStateType;

    /**
     * Bus Status type.
     * Bus status contains variables useful for determining
     * the health of the bus.  They are maintained by 
     * CanIo
     * @see carma::canbus::CanIo
     */
	typedef struct busStatusStruct {
		busStateType state;            /**< State of bus */
        volatile double rxMsgRate;    /**< Half sec averaged rx msg rate */ 
        volatile double txMsgRate;   /**< Half sec averaged tx msg rate */
        volatile double oneMinRxMsgRate; /**< 1 Min averaged rx msg rate */
        volatile double oneMinTxMsgRate; /**< 1 Min averaged tx msg rate */
		volatile unsigned short rxErrors; /**< Current CAN rx errors */ 
		volatile unsigned short txErrors; /**< Current CAN tx errors */ 
        /** 
         * Count of Janz slow msgs lost (slow msgs are messages
         * to communicate with the Janz board only).  
         */
		volatile unsigned int slowMsgsLost;
        /**
         * Count of Janz fast msgs lost (fast msgs are CAN messages
         * only).
         */
		volatile unsigned int fastMsgsLost; 
        
        /** 
         * Time elapsed from the time timestamp is created to when it is 
         * echoed back to the host and processed (in microseconds).  
         */
        volatile int tsEchoLatency;
	} busStatusType;
            
    /**
     * Map to hold bus status for multiple busses.
     */
    typedef std::map< busIdType, busStatusType > BusStatusMap;

    /** 
     * @enum deviceStateEnum 
     * Device state type enumeration.
     * Defines the various states of a CAN device.   
     */

    /**
     * @var deviceStateEnum OFFLINE
     * Monitor packets are not being received from the module.
     */

    /**
     * @var deviceStateEnum STARTING
     * The module has sent out a single slow monitor packet to identify 
     * itself.  The module should begin sending normal blanking frame packets
     * after about five seconds.
     */

    /**
     * @var deviceStateEnum ONLINE
     * Host is receiving CAN messages on a half second time scale.
     */

    /**
     * @var deviceStateEnum SIMULATED
     * Host is simulating the receipt of monitor packets for this module.
     */
    
    typedef enum deviceStateEnum {
        OFFLINE, 
        STARTING,
        ONLINE,
        SIMULATED,
    } deviceStateType;

    /**
     * CAN Tx priority type.
     * CAN messages are sent via prioritized queues. 
     */
    typedef enum {
        HIGH    = 1,
        NORMAL  = 2,
        LOW     = 3,
    } txPriorityType;

    // Useful constants.
    const int NANOSECS_PER_SEC = 1000000000;

    // IPQ name string constants.  The CAN_INPUT_IPQ is the name
    // of the ipq that will be written by DirectCan, read from CanIo
    // and rewritten to the CANbus.  The CAN_OUTPUT_IPQ is the name
    // of the ipq that will be read by DirectCan, written to by 
    // CanIo with messages read from the CANbus.
    const std::string CAN_INPUT_IPQ = "/can-input.ipq";  
    const std::string CAN_OUTPUT_IPQ = "/can-output.ipq";
    // The IPQ is very large in order to accomodate an entire remote firmware
    // upload (roughly 40,000 messages and 3x as many dummy packets).  This 
    // allows us to very rapidly download firmware.
    const long IPQ_BUFFER_SIZE = 150000;  /**< DirectCan IPQ buffer sizes. */

} // namespace canbus
} // namespace carma
#endif
