#ifndef CARMA_MONITOR_PIPELINE_MONITOR_INPUT_H
#define CARMA_MONITOR_PIPELINE_MONITOR_INPUT_H

#include "carma/util/types.h"

#include "carma/monitor/CorrDesignation.h"
#include "carma/monitor/MonitorSystem.h"
#include "carma/monitor/SignalPathSubsystem.h"
#include "carma/util/types.h"

#include <map>
#include <vector>
#include <set>


namespace carma {
namespace monitor {

class MonitorSystem;
class AntennaCommon;
class ControlBandPoints;
class MonitorPointFloat;
class NoiseStatusMonitorPointEnum;

typedef PolarizationMonitorPointEnum PolMPE;
typedef PolarizationMonitorPointEnum::POLARIZATION PolType;
typedef std::pair< int, PolType > AntPolPair;
typedef std::pair< int, PolType > BandPolPair;
typedef std::pair< int, int > BandInputPair;

class PipelineMonitorInput {
public:

    /** 
     * Constructor
     * Accessors are limited to specified mapped correlator.
     * @param corrDes Correlator designation (SPECTRAL, WIDEBAND, etc).
     * @throw IllegalArgumenException if corrDes == NONE. 
     */
    PipelineMonitorInput( MonitorCorrelatorDesignation corrDes );
    
    /**
     * Destructor
     */
    ~PipelineMonitorInput( );

    /** 
     * Wait for pipeline monitor input for specified frame.
     * Waits until monitor data for specified frame becomes available or is 
     * known to be unavailable.  If successful, accessors will will refer to
     * the specified frame, if not, accessors will refer to an undefined frame
     * in the past.
     * @param frame Frame time of data to wait for.
     * @return True if input available, false on timeout, error, whatever.
     */
    bool waitForInput( carma::util::frameType frame );

    /**
     * Retrieve ordered astro band numbers which contain a valid mapping. 
     * Mapping is only valid for current frame. 
     */
    std::vector< int > getMappedAstroBandNumbers( ) const;

    /**
     * Retrieve ordered antenna numbers mapped to specified correlator.
     */
    std::set< int > getMappedAntennaNumbers( ) const;

    /**
     * Retrieve ordered input numbers mapped to specified correlator.
     */
    std::set< int > getMappedInputNumbers( ) const;

    /**
     * Retrieve multimap containing polarization(s) keyed by astro band number.
     * Mapping is only valid for current frame.
     */
    std::set< PolType > getMappedPols( int astroBandNo ) const;

    /**
     * Retrieve ant-pol pairs which are mapped for given astroband.
     * Mapping is only valid for current frame.
     */ 
    std::vector< AntPolPair > getMappedAntPolPairs( int astroBandNo ) const;

    /**
     * Retrieve ordered band-pol pairs which are mapped to specified correlator.
     */
    std::vector< BandPolPair > getMappedBandPolPairs( ) const;

    /**
     * Retrieve astroband-astroinput pairs which are mapped to given antenna.
     */
    std::vector< BandInputPair > 
    getMappedAstroBandInputPairs( int carmaAntNo ) const;

    /** 
     * Determine if a particular astro band is mapped.
     */
    bool signalPathMapped( int astroBandNo ) const;
    
    /** 
     * Determine if a particular astroBand, ant-pol pair is mapped.
     */
    bool signalPathMapped( int astroBandNo, const AntPolPair & antPol ) const;

    /** 
     * For a given astroBand & astroInput, get the corresponding ant-pol pair.
     * If not signalPathMapped, return AntPolPair( -1, UNKNOWN ).
     */
    AntPolPair getAntPolPair( int astroBandNo, int astroInputNo ) const;

    /**
     * For a given astroBand & antPol, get the corresponding astroInputNo.
     * Requests for components which are not signal path mapped will throw.
     */
    int getAstroInputNo( int astroBandNo, const AntPolPair & antPol ) const;

    /** 
     * Retrieve the current frame count of the underlying data.
     */
    carma::util::frameType frameCount( ) const;

    /**
     * Based on current LO frequency determine if rx hardware is available.
     * This routine will return false if signal path is not mapped for input
     * astroBandNo & antPol pair.
     * @return pair of bools indicating mp validity first and rx in sb second.
     */
    std::pair< bool, bool > isRxInSideband( int astroBandNo, 
                                            const AntPolPair & antPol,
                                            bool usb ) const; 

    /** 
     * Retrieve particular monitor system components by astroBand & AntPol.
     * Requests for components which are not signal path mapped will throw.
     * Thus users should either iterate over above returned vectors or 
     * check that the signal path is mapped for given components via
     * signalPathMapped routines.
     */
    const AntennaCommon & antennaCommon( int carmaAntNo ) const; 
    const ControlBandPoints & controlBandPoints( int astroBandNo ) const;
    const MonitorPointFloat & totalPower( int astroBandNo, 
                                          const AntPolPair & antPol) const;
    const NoiseStatusMonitorPointEnum & noiseStatus( 
        MonitorCorrelatorDesignation corrDes ) const;
    const MonitorPointByte & blankStatus(
        int astroBandNo,
        const AntPolPair & antPol ) const;

    const WeatherSubsystem & weather( ) const;
    const ControlSubsystem & control( ) const;
    BandInputPair getCorrBandInputPair( int astroBandNo, 
                                        const AntPolPair & antPol ) const;

    /**
     * Retrieve antenna correlator designator as gleaned from the 
     * signal path mapper.  If an antenna is mapped to both correlators,
     * this will return CorrDesignation::ANY.
     */
    MonitorCorrelatorDesignation getAntCorrDes( int carmaAntNo ) const;

    /**
     * Retrieve antenna's owning subarray correlator designation.
     * This differs from getAntCorrDes which will return ANY if 
     * an antennas signal path is mapped to two correlators in
     * differing subarrays.  Rather this routine will return only 
     * the subarray's owned correlator and ANY only if it owns both.
     */
    MonitorCorrelatorDesignation getAntSubarrayCorrDes( int carmaAntNo ) const;

    /** 
     * Retrieve expected channel counts.
     * @param astroBandNo
     */
    int getExpectedChannels( int astroBandNo ) const;

    /**
     * Is astroband online?
     * @param astroBandNo
     */
    bool astroBandOnline( int astroBandNo ) const;

    /**
     * Return the corr designation, band number and input number fo
     * the given astroband number and input
     */
    void getCorrInfo(int astroBandNo, int astroBandInput, 
		     MonitorCorrelatorDesignation& corrDes, int& corrBandNo, int& corrBandInputNo);

    bool isManuallyFlagged(int astroBandNo, int astroBandInput1, int astroBandInput2);

    /**
     * Get const reference to underlying CarmaMonitorSystem instance.
     */
    const carma::monitor::CarmaMonitorSystem & getCarmaMonitorSystem() const;

private:

    struct MapDetails;

    // Too many different maps here - consolidate this into the SPM class.
    typedef std::map< int, MapDetails > AstroInputMap;
    typedef std::map< int, AstroInputMap > AstroBandInputMap;
    
    // This is a reverse multimap to get fast lookups on ant/pol
    // This can go away if we modify the above to be pure input based.
    typedef std::multimap< AntPolPair, int > AntPolInputMultimap;
    typedef std::map< int, AntPolInputMultimap > AstroBandAntPolInputMap;

    // Keyed on absolute carma antenna number.
    typedef std::map< int, std::vector< BandInputPair > > AntAstroPairs;

    void internalSpmUpdate();
    void internalAstrobandUpdate( 
        const SignalPathSubsystem::Astroband & astroband,
        const int astroBandNo );

    void flagInputs(MonitorCorrelatorDesignation corrDes, int bandNo, int inputNo1, int inputNo2, int nMaxBand);
    void rebuildControlMap(MonitorCorrelatorDesignation corrDes) ;
    void rebuildControlMaps() ;
    void clearControlMap(MonitorCorrelatorDesignation corrDes);
    void clearControlMaps() ;
    void internalControlUpdate(); 

    const MapDetails & getMapDetails( int astroBandNo, 
                                      const AntPolPair & antPol ) const;
    
    const MapDetails & getMapDetails( int astroBandNo,  
                                      int astroInputNo ) const;

    AstroBandInputMap astroMapping_;   
    AstroBandAntPolInputMap antPolMapping_;
    std::map< int, std::set< PolType > > astroBandPolMapping_;
    std::set< int > mappedAnts_;
    std::set< int > mappedInputs_;
    AntAstroPairs mappedAntAstroPairs_;
    
    typedef std::map< int, std::set< int > > AstroBandCorrBandMap;
    AstroBandCorrBandMap astroBandCorrBands_;

    int lastMappingUpdate_;

    const MonitorCorrelatorDesignation corrDes_; 

    mutable carma::monitor::CarmaMonitorSystem cms_;

    std::map<MonitorCorrelatorDesignation, std::map<int, std::map<int, std::map<int, bool> > > > flagMap_;
};

}} // namespace carma::monitor
#endif
