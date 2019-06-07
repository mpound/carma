#ifndef CARMA_MONITOR_SIGNAL_PATH_MAPPING_H_
#define CARMA_MONITOR_SIGNAL_PATH_MAPPING_H_

#include "carma/monitor/CorrDesignation.h"
#include "carma/monitor/MonitorSystem.h"
#include "carma/monitor/SignalPathCommonMonitorPoints.h"

#include <vector>
#include <set>

namespace carma {
namespace monitor {

typedef PolarizationMonitorPointEnum PolMPE;
typedef PolarizationMonitorPointEnum::POLARIZATION PolType;
typedef std::pair< int, PolType > AntPolPair;
typedef std::pair< int, PolType > BandPolPair;

/**
 * Class containing convenient accessors to the signal path mapping.
 */
class SignalPathMapping {
public:

    /**
     * Constructor
     * @param Reference to monitor system instance.
     * @param corrDes Correlator designation (SPECTRAL, WIDEBAND etc ).
     * @throw IllegalArgumenException if corrDes == NONE. 
     */
    SignalPathMapping( const MonitorSystem & monitorSystem,
                       MonitorCorrelatorDesignation corrDes );

    /**
     * Destructor
     */
    ~SignalPathMapping( );

    /**
     * Update
     * Checks current frame of monitor system for signal path mapping changes.
     * Note this routine does not call read or readNewest on the monitor system.
     * @return bool True if mapping has changed, false otherwise.
     */
    bool update( );

    /**
     * Changed
     * Returns true if the signal path mapping has changed since the last
     * call to update.  Useful for N const references to this class but only
     * a single non-const caller to update (see rtdsignalpath).
     * @return bool True if mapping changed since last update, false otherwise.
     */
    bool changed( ) const;

    /**
     * Retrieve ordered astro band numbers for which a signal path is defined.
     * @return Possibly empty vector of astro band numbers.
     */
    std::vector< int > getMappedAstroBandNumbers( ) const;

    /**
     * Retrieve ordered antenna numbers for which a signal path is defined.
     * @return Possibly empty vector of antenna numbers.
     */
    std::vector< int > getMappedAntennaNumbers( ) const;

    /**
     * Retrieve ordered antenna numbers for which a signal path is defined.
     * @return Possibly empty vector of antenna numbers.
     */
    std::set< int > getMappedAntennaNumbers( const int astroBandNo ) const;

    /**
     * For given astroband retrieve pols for which a signal path is defined.
     * @param astroBandNo Astro band number. 
     * @return Possibly empty vector of polarizations.
     */
    std::vector< PolType > getMappedPolarizations( int astroBandNo ) const;

    /**
     * For given astroband get ant-pol pairs for which a signal path is defined.
     * @param astroBandNo Astro band number. 
     * @return Possibly empty vector of AntPolPairs.
     */ 
    std::vector< AntPolPair > getMappedAntPolPairs( int astroBandNo ) const;

    /**
     * Retrieve ordered band-pol pairs for which a signal path is defined.
     * @return Possible empty vector of BandPolPairs.
     */
    std::vector< BandPolPair > getMappedBandPolPairs( ) const;

    /** 
     * Determine if a particular astro band is mapped.
     * @param astroBandNo Astro band number. 
     */
    bool signalPathMapped( int astroBandNo ) const;
    
    /** 
     * Determine if a particular astro band, ant-pol pair is mapped.
     * @param astroBandNo Astro band number. 
     */
    bool signalPathMapped( int astroBandNo, const AntPolPair & antPol ) const;

private:

    struct Pimpl;
    std::auto_ptr< Pimpl > pimpl_;

}; // class SignalPathMapping

}} // namespace carma::monitor

#endif // CARMA_MONITOR_SIGNAL_PATH_MAPPING_H_
