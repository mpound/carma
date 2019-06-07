/** @file
 * $Id: Clock.h,v 1.18 2011/08/25 20:54:37 abeard Exp $
 *
 * Clock class
 * - The Clock class is responsible for processing CAN messages
 * fed to it by the Master for the Lobe Rotator Computer.
 *
 * $Author: abeard $
 * Version: $Revision: 1.18 $
 * $Date: 2011/08/25 20:54:37 $
 *
 */

#ifndef CARMA_CLOCK_CLOCK_H
#define CARMA_CLOCK_CLOCK_H

// CARMA includes
#include "carma/canbus/devices/XacDevice.h"
#include "carma/clock/ClockControl.h"
#include "carma/corba/corba.h"
#include "carma/monitor/MasterClockSubsystem.h"

namespace carma {

  /**
   * Contains all code related to Master clock
   */

namespace clock {

/**
 * @class Clock
 * The Clock class is an instantiation of the
 * carma::canbus::Device class for the Master Clock.  This device has
 * sends monitor packets on a 1/2 second time scale as well as
 * allowing control functions for initialization and aligning the
 * offset between the hardware output and gps signal.  @see
 * carma::canbus::Device @see POA_carma::clock::ClockControl
 */

class Clock : public carma::canbus::devices::XacDevice {
public:
  /**
   * Clock constructor
   * @param node: unique location id on CAN bus
   * @param io: reference to CanOutput class from Master
   */
  Clock(carma::canbus::nodeType node,
	carma::canbus::CanOutput &io,
	carma::monitor::MasterClockSubsystem *masterClockSubsystem);

  // virtual destructor
  virtual ~Clock();

  /**
   * return CANbus API
   */
  static carma::canbus::apiType getApiId();

private:
  /**
   * @return std::map of device controls
   */
  virtual std::map<carma::canbus::msgType, std::string>
    getControls() const;

  /**
   * @return std::map of half-second monitor points
   */
  virtual std::map<carma::canbus::msgType, std::string>
    getHalfSecMonitors() const;

  /**
   * No system monitor points defined for Clock
   * @return std::map of half-second monitor points
   */
  virtual std::map<carma::canbus::msgType, std::string>
    getSlowMonitors() const;

  /**
   * @param messageId 10-bit message ID
   * @return simulated CAN messages
   */
  virtual carma::canbus::Message
    simulateMsg(carma::canbus::msgType messageId);

  // simulation of CAN messages corresponding to monitor points
  carma::canbus::Message simulateBlankingFramePacket1();
  carma::canbus::Message simulateBlankingFramePacket2();
  carma::canbus::Message simulateBlankingFramePacket3();
  carma::canbus::Message simulateBlankingFramePacket4();
  carma::canbus::Message simulateBlankingFramePacket5();
  carma::canbus::Message simulateBlankingFramePacket6();
  carma::canbus::Message simulateBlankingFramePacket7();
  carma::canbus::Message simulateBlankingFramePacket8();
  carma::canbus::Message simulateBlankingFramePacket9();

  /**
   * Process messages from the CAN bus
   * @param messageId 10-bit message ID
   * @param data Reference to vector containing raw data
   */
  virtual void processMsg(carma::canbus::msgType messageId,
			  std::vector<carma::canbus::byteType>& data,
			  bool sim);

  // half-second monitor packet processing routines
  /**
   * Packet 1:
   * - difference between NTP and distributed 1 pps (microseconds),
   * - difference between external 10MHz and distributed 1 pps (microseconds)
   */
  void processBlankingFramePacket1(std::vector<carma::canbus::byteType> &data);
  /**
   * Packet 2:
   * - difference between primary GPS and distributed 1 pps (microseconds)
   * - difference between secondary GPS and distributed 1 pps (microseconds)
   */
  void processBlankingFramePacket2(std::vector<carma::canbus::byteType> &data);
  /**
   * Packet 3:
   * - primary GPS (GPS A == 0, GPS B == 1, Not Used == anything else)
   * - 10 MHz source (internal Rb == 0, external CW == 1,
   *   Not Used == anything else)
   * - 1 pps source (Rb == 0, GPS == 1, external 10MHz == 2, other 10MHz == 3,
   *   Not Used == anything else)
   */
  void processBlankingFramePacket3(std::vector<carma::canbus::byteType> &data);
  /**
   * Packet 4:
   * - duration of last heartbeat before 1 pps (microseconds)
   * - status of heartbeat generator (OK == 0, halted/error == 1)
   * - enable/disable of walsh state timing synchronization
   */
  void processBlankingFramePacket4(std::vector<carma::canbus::byteType> &data);
  /**
   * Packet 5:
   * - temperature of CAN module (in units of 0.01 deg Celsius)
   * - delay of the 1pps A relative to the 10MHz clock (nanosec)
   * - delay of the 1pps B relative to the 10MHz clock (nanosec)
   */
  void processBlankingFramePacket5(std::vector<carma::canbus::byteType> &data);
  /**
   * Packet 6:
   * - Rb locked status (10 MHz locked to 1 pps == 0, not locked == 1)
   * - Rb oscillation mode (free running == 0, phase locked to GPS == 1)
   * - Rb status (online == 0, online w/ errors == 1, offline == 2,
   *   not present or inoperable == 3, not used == anything else)
   * - Error Code (no errors == 0, warming up == 1, power supply == 2,
       physics package == 3, temp control == 4, RF == 5, Freq Loop == 6,
       GPS Freq lock == 7, Fatal PRS10 == 8, N/A == anything else)
   * - OXCO temperature of Rb oscillator (degrees in 0.01 Celsius)
   * - physics temperature of Rb oscillator (degrees in 0.01 Celsius)
   */
  void processBlankingFramePacket6(std::vector<carma::canbus::byteType> &data);
  /**
   * Packet 7:
   * - +24V power supply voltage (mV)
   * - +12V power supply voltage (mV)
   * - +5V analog power supply voltage (mV)
   * - +5V digital power supply voltage (mV)
   */
  void processBlankingFramePacket7(std::vector<carma::canbus::byteType> &data);
  /**
   * Packet 8:
   * - 5VDIGITALPS(A) (mV)
   * - 5VDIGITALPS(B) (mV)
   */
  void processBlankingFramePacket8(std::vector<carma::canbus::byteType> &data);
  /**
   * Packet 9:
   * - Power Flags
   * - RF Flags
   * - Temp Flags
   * - Freq Lock Flags
   * - GPS Flags
   * - System Flags
   */
  void processBlankingFramePacket9(std::vector<carma::canbus::byteType> &data);

public:

  // CORBA control routines
  /**
   * set 1pps to come from GPS or Rb oscillator counter
   * @param ppsMode - {RBPRS10, GPSPPS, TENMHZPPS}
   */
  void setPpsMode(carma::clock::ppsModeType ppsMode);

  /**
   * set Rb oscillation mode to be free-running or to phase lock to GPS
   * @param rbMode - {FREE, GPS000424, GPS001648, GPS003336, GPS010712,
   *                  GPS021424, GPS042848, GPS085736, GPS175502}
   */
  void setRbMode(carma::clock::rbModeType rbMode);

  /**
   * set primary GPS source
   * @param gpsSource - {GPSA or GPSB}
   */
  void setGpsSource(carma::clock::gpsSourceType gpsSource);

  /**
   * set 10MHz source to be from the internal Rb oscillator or some
   * external source
   * @param tenMHzSource {INTERNAL or EXTERNAL}
   */
  void set10MHzSource(carma::clock::tenMHzSourceType tenMHzSource);

  /**
   * set heartbeat delay
   * @param delay hearbeat delay
   * @param register delay register {REGISTER1, REGISTER2}
   */
  void setHbDelay(CORBA::UShort delay, carma::clock::delayRegister reg);

  /**
   * synchronize 1pps from Rubidium oscillator with GPS-derived 1 pps signal
   * @param walshSync sync state of oscillator {ENABLE, DISABLE}
   */
  void resync10MHzGps(carma::clock::walshSyncType walshSync);

  /**
   * reset PRS10 Rb Time Standard and resync to primary GPS
   */
  void resetRb();

  /**
   * Initialize master clock
   * @param ppsMode - {GPSPPS or TENMHZPPS}
   * @param rbMode - {FREE or GPSLOCKED}
   * @param walshPeriod - desired Walsh period of master clock
   * @param gpsSource - {GPSA or GPSB}
   * @param tenMHzSource {INTERNAL or EXTERNAL}
   */
  void initialize(carma::clock::ppsModeType ppsMode,
		  carma::clock::rbModeType rbMode,
		  carma::clock::gpsSourceType gpsSource,
		  carma::clock::tenMHzSourceType tenMHzSource,
		  carma::clock::walshSyncType walshSync);

private:

  void updateFrameData( );

  // reference to monitor subsystem
  carma::monitor::MasterClockSubsystem::Clock *mon_;

};  // class Clock

}  // namespace carma::clock
}  // namespace carma

#endif
