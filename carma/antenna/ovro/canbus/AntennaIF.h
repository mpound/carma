/** @file
 * CAN Device implementation for Antenna IF Module.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.13 $
 * $Date: 2008/08/27 19:12:37 $
 * $Id: AntennaIF.h,v 1.13 2008/08/27 19:12:37 colby Exp $
 *
 * $CarmaCopyright$
 */
#ifndef CARMA_ANTENNA_OVRO_ANTENNAIF_H
#define CARMA_ANTENNA_OVRO_ANTENNAIF_H

#include "carma/antenna/common/AntennaIF.h"
#include "carma/canbus/Types.h"
#include "carma/util/PthreadMutex.h"

#include <vector>

namespace log4cpp {
    // Forward declaration
    class Category;
} // End namespace log4cpp

namespace carma {

namespace monitor {
    class OvroSubsystem;
} // namespace monitor

namespace antenna {
namespace ovro {

/**
 * IF Total Power vector in milliWatts.
 */
typedef ::std::vector< float > IFTotalPowerVec;

/** 
 * Antenna IF CAN module device class.
 */
class AntennaIF : public carma::antenna::common::AntennaIF {
public:

    /**
     * Constructor
     * @param node Location id of this instance (node location id).
     * @param io Reference to CanOutput class.
     * @param mon Reference to OvroSubsystem instance.
     */
    AntennaIF( carma::canbus::nodeType node,
               carma::canbus::CanOutput & io,
               carma::monitor::OvroSubsystem & mon );

    /**
     * Destructor
     */
    ~AntennaIF();

    /** 
     * Retrieve last valid IF total power readout.
     * If none exists, an empty vector is returned.
     * Generally this occurs as part of an IV curve.
     * @return IF total power vector in milliWatts.
     */
    IFTotalPowerVec getIFTotalPower( ) const;
    
    /**
     * Internally create simulated total power vector.
     * @param nsamps Size of simulated total power vector.
     */
    void simTotalPower( unsigned int nsamps );

protected:
    

private:

    // Disallow assignment and copy construction.
    AntennaIF(const AntennaIF &);
    AntennaIF &operator=(const AntennaIF &);

    void processTotalPowerResponse(carma::canbus::DataVector &data);
    void processFastTotalPowerPacket(carma::canbus::DataVector & data);
    void processSwitchStatusOnChange(carma::canbus::DataVector &data);
    void processPAMStatusOnChange(carma::canbus::DataVector &data);

    IFTotalPowerVec totalPower;
    mutable carma::util::PthreadMutex totalPowerMutex_;

       
    void processFastChannel1Packet( carma::canbus::DataVector &data );
    void processFastChannel2Packet( carma::canbus::DataVector &data );
};
}}} // End namespace carma::antenna::ovro
#endif
