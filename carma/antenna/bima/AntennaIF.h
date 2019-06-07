/** @file
 * CAN Device implementation for Antenna IF Module.
 * Derived from OVRO Antenna IF implementation
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * <dl><dt><b>Author </b></dt><dd>Colby Gutierrez-Kraybill</dl>
 * $Revision: 1.16 $
 * $Date: 2012/02/21 21:06:56 $
 * $Id: AntennaIF.h,v 1.16 2012/02/21 21:06:56 abeard Exp $
 *
 * $CarmaCopyright$
 */
#ifndef CARMA_ANTENNA_BIMA_ANTENNAIF_H
#define CARMA_ANTENNA_BIMA_ANTENNAIF_H

// System includes
#include <sys/time.h>

// Carma includes
#include "carma/corba/corba.h"
#include "carma/antenna/common/AntennaIF.h"
#include "carma/canbus/Types.h"

namespace log4cpp {
    class Category;
} // End namespace log4cpp

namespace carma
{

  namespace monitor {
    class AntennaIF;
    class StateMonitorPointEnum;
    class Xac;
  } // End namespace monitor

  namespace antenna
  {
    namespace bima
    {

        // Forward Declarations
        class Rx;
        class SharedMemory;
/**
 * Antenna IF CAN module device class.
 */
class AntennaIF :
   public carma::antenna::common::AntennaIF
{
public:

    /**
     * Constructor
     * @param node Location id of this instance (node location id).
     * @param io Reference to CanOutput class.
     * @param monsubsys Reference monitor subsystem for Antenna IF
     */
    AntennaIF(carma::canbus::nodeType node, carma::canbus::CanOutput &io,
              Rx &rx, 
              carma::monitor::StateMonitorPointEnum & state,
              carma::monitor::AntennaIF &ifMon,
              carma::monitor::Xac &xacMon);

    /**
     * Destructor
     */
    ~AntennaIF();

    // Public control commands.
    /**
     * Select band
     * Select a particular band as an input to the PAM, by setting the
     * position of the IF switch.
     * @param band IF switch position (1,2,3 or 4).
     */
    void selectRx();

    void selectBand( ::CORBA::UShort band );

    /**
     * Set IF total attenuation.
     * Set the IF attenuator to a nominal value.
     * @param atten Attenuator setting, 0-63 dB in 0.5 dB steps.
     */
    void setAtten(::CORBA::Float atten);

    /**
     * Set IF level.
     * Set the IF attenuator so that the IF total power detector returns the
     * closest value to the input parameter.  Current PAM will only be able to
     * produce about +8dBm.
     * @param pow Target IF total power, in mW.
     */
    void setPower(::CORBA::Float pow);

    /**
     * Set output power to preset.
     * Go to preset IF level.  Current preset IF level is 0 dBm. Thus it is
     * functionally equivaletn to setIFlevel command with a power level of
     * 1.0 mW.
     */
    void setPresetPower();

    /**
     * Reset the module
     */ void reset( );

    void startFastSample( unsigned short chan );
    void stopFastSample();

protected:

    // No protected members or data

private:

    // Disallow assignment and copy construction.
    AntennaIF(const AntennaIF &);
    AntennaIF &operator=(const AntennaIF &);

    virtual void blankingFrameHook1( float ifoutTotPower, float pamTemp );
    virtual void blankingFrameHook2( float attenSet,
            unsigned char pamStat,
            unsigned char ifSwitchStat,
            unsigned char laserStat,
            unsigned char nErrors );
    virtual void blankingFrameHook3( float laserOpticalPow,
            float laserTemp );
    virtual void blankingFrameHook4( float inputAttenSet,
            float outputAttenSet );

    void processPAMStatusOnChange(carma::canbus::DataVector &data);
    void processSwitchStatusOnChange(carma::canbus::DataVector &data);
    void processTotalPowerResponse(carma::canbus::DataVector &data);
    void processFastTotalPowerPacket(carma::canbus::DataVector & data);

    void processFastChannel1Packet( carma::canbus::DataVector &data );
    void processFastChannel2Packet( carma::canbus::DataVector &data );

    void updateIFTimer( void );
    static const int NMAX=300;    // note: must match MAX_SCAN_STEPS in Rx.h
    float Rpow[NMAX];
    float Lpow[NMAX];

    // Member variables
    carma::antenna::bima::Rx& rx_;
    log4cpp::Category &log_;
    SharedMemory *_bimaShm;
    struct timeval _tm;
    struct timezone _tz;

};
}}} // End namespace carma::antenna::bima
#endif
