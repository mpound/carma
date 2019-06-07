#ifndef CARMA_CONTROL_SIGNALPATHMAPPERHANDLE_H
#define CARMA_CONTROL_SIGNALPATHMAPPERHANDLE_H

/**
 * @file
 * Carma control interface to the SignalPathMapper DO
 *
 * $CarmaCopyright$
 *
 * @author Marc Pound
 */
#include "carma/control/CorrDefs.h"
#include "carma/control/RemoteObjHandleT.h"
#include "carma/control/CorrelatorInfo.h"
#include "carma/monitor/ControlSubsystem.h"

#include "carma/signalpath/SignalPathMapperControl.h"
#include "carma/switchyard/SwitchyardControl.h"
#include "carma/util/SeqTypedefs.h"
#include "carma/util/corrUtils.h"

#include <string>
#include <vector>

namespace carma {

namespace monitor {
    class MonitorSystem;
} // namespace monitor

namespace control {

typedef 
RemoteObjHandleT< carma::signalpath::SignalPathMapperControl> 
                  SignalPathMapperRemoteObjHandle;

//! @brief Manages control's reference to SignalPathMapper DO 
// It is a thin handle that calls through to the DO and contains
// no local state.
class SignalPathMapperHandle: public SignalPathMapperRemoteObjHandle {
public:
    
    /**
     * Constructor
     *
     * @param carmaMonitor carma::monitor::MonitorSystem&
     *              monitor system, which allows Alarm handle to get a 
     *              reference to its own monitor stream.
     * @param const carma::monitor::ControlSubsystemBase:Reachable&
     *        reachable
     *              monitor system, which allows Alarm handle to set 
     *              monitor points for the subarray within control monitor 
     *              subsystem .
     */
    SignalPathMapperHandle( 
        carma::monitor::MonitorSystem & monitorSystem,
        carma::monitor::ControlSubsystemBase::Reachable & reachable );

    virtual ~SignalPathMapperHandle( );

      /**
       * Initialize a cable map, specifying switchyard hardware connections
       * 
       * @param fileName The name of the file containing the cable map 
       *
       * @throw UserException if the file does not contain a valid
       * mapping
       */
    void initializeCableMap( const ::std::string & fileName );

  /**
   * Load a new configuration from a file
   *
   * @param fileName The name of the file containing the configuration
   * @param confName The name to assign to the configuration
   * @param astroBandConfName The name of the astroband configuration to 
   * associate with this switchyard configuration
   * 
   * @throw UserException if the file does not contain a valid
   * configuration
   */
    void loadConfiguration( const ::std::string & fileName, 
                            const ::std::string & confName, 
                            const ::std::string & astroBandConfName );

  /**
   * Configure an astro band
   * 
   * @param bandNo   The Astroband number to configure
   * @param confName The name of the configuration to assign to this astro band
   * @param type     The correlator type owned by the caller
   *
   * @throw UserException if the configuration conflicts with
   * another configured astro band, if the configuration is not
   * possible, or if the caller is not allowed to modify the
   * specified astro band
   */
    void configureAstroBand(unsigned short bandNo, 
			    const ::std::string & confName, 
			    unsigned short subarrayNo,
			    ControlCorrelatorDesignation type);

    void checkConfigurationValidity(unsigned short bandNo, 
				    const ::std::string & confName, 
				    unsigned short subarrayNo,
				    ControlCorrelatorDesignation type);

   void checkConfigurationSuccess(unsigned short bandNo);

   /**
    * Query antennas
    *
    * @param astroBandNo Astro band number to query
    */
   std::vector<carma::signalpath::SignalPathMapperControl::Antenna>
     getAntennas(unsigned short astroBandNo);
     
   /**
    * Query block downconverter settings
    *
    * @param astroBandNo Astro band number to query (0 == all)
    */
   std::vector<carma::signalpath::SignalPathMapperControl::BlockDownconverterSetting>
     getBdcSettings(unsigned short astroBandNo);
     
  /**
   * Query correlator band input mapping
   *
   * @param band Correlator band to query
   */
   ::std::vector<carma::signalpath::SignalPathMapperControl::CorrelatorBandInput> 
       getCorrelatorBandInputMap( const carma::signalpath::SignalPathMapperControl::CorrelatorBand & band );

  /**
   * Query active astro bands managed by this correlator
   *
   * @param type Correlator type to query (CORR_SPECTRAL, CORR_WIDEBAND)
   */
   ::std::vector<carma::signalpath::SignalPathMapperControl::AstroBand> 
       getActiveAstroBands( ControlCorrelatorDesignation type );

   /**
    *
    * Query astro bands for which a mapping is specified by the
    * requested configuration
    *
    * @param confName   The name of the configuration to assign to this astro band
    * @param subarrayNo The calling subarray number
    * @param type       Correlator type to query (CORR_SPECTRAL, CORR_WIDEBAND)
    */
   ::std::vector<carma::signalpath::SignalPathMapperControl::AstroBand> 
       getAstroBandsForConfiguration( const ::std::string & confName, 
				      unsigned short subarrayNo,
				      ControlCorrelatorDesignation type);
   
  /**
   * Query active correlator bands managed by this correlator
   *
   * @param type Correlator type to query (CORR_SPECTRAL, CORR_WIDEBAND)
   */
   ::std::vector<carma::signalpath::SignalPathMapperControl::CorrelatorBand> 
       getActiveCorrelatorBands( ControlCorrelatorDesignation type );

   /**
    * Query active correlator bands in this AstroBand
    * @param astroBandNo The astroband number  to query
    */
   ::std::vector<carma::signalpath::SignalPathMapperControl::CorrelatorBand> 
       getCorrelatorBands(unsigned astroBandNo);

   /**
    * @return a Sequence of correlatorband numbers associated with this
    * astroband
    * @param astroBandNo The astroband number  to query
    */
   carma::util::SeqShort * getCorrelatorBandNoSeq(unsigned astroBandNo);

   /**
    * @return a Sequence of Astroband numbers associated with this
    * correlator
    * @param astroBandNo The astroband number  to query
    */
   carma::util::SeqShort * getActiveAstroBandNoSeq( ControlCorrelatorDesignation type );

   ::std::vector<short>    getActiveAstroBandNoVec( ControlCorrelatorDesignation type );

   carma::util::CorrelatorFpgaModeType getFpgaMode(unsigned astroBandNo);

   /**
    * Query switch settings
    *
    * @param astroBandNo Astro band number to query (0 == all)
    */
   std::vector<carma::switchyard::SwitchPosition>
     getIFSwitchSettings(unsigned short astroBandNo);

   std::vector<carma::switchyard::SwitchPosition>
     getDCLOSwitchSettings(unsigned short astroBandNo);

   std::vector<carma::switchyard::SwitchPosition>
     getLOSwitchSettings(unsigned short astroBandNo);

   std::vector<carma::switchyard::SwitchPosition>
     getLLSwitchSettings(unsigned short astroBandNo);

   /**
    * @return the number of active correlator bands of a specific type
    * @param type Correlator type to query (CORR_SPECTRAL, CORR_WIDEBAND)
    */
   unsigned int getNumActiveCorrelatorBands( ControlCorrelatorDesignation type );

   /**
    * @return the number of active astrobands of a specific type
    * @param type Correlator type to query (CORR_SPECTRAL, CORR_WIDEBAND)
    */
   unsigned int getNumActiveAstroBands( ControlCorrelatorDesignation type );

  /**
   * Clear an astro band configuration
   * 
   * @param bandNo   The Astroband number to configure
   * @param type     The correlator type owned by the caller
   *
   * @throw UserException if the caller is not allowed to clear
   * this astro band
   */
   void clearAstroBandConfiguration(unsigned short bandNo, 
				    unsigned short subarrayNo,
				    ControlCorrelatorDesignation type);
		
   /**
   * Set a walsh column explicitly for an antenna
   *
   * @param wca   The Walsh column assignemnt to make. This is a struct of an antenna number and
   * Walsh table column number.
   *
   * @throw UserException if walsh column assignment conflicts
   * with current signalpath configuration
   */
   void assignWalshColumn(carma::signalpath::SignalPathMapperControl::WalshColumnAssignment wca);

   /**
    * Clear a walsh column assignment
    *
    * @param antNo The antenna number whose Walsh column we wish to clear
    * 
    * @throw UserException only if an invalid antenna was specified
    */
   void clearWalshColumnAssignment(unsigned short antNo);

   /**
    * Query walsh function assignments.
    * 
    * @param antNo Antenna to query, or 0 to query all
    */
   ::std::vector<carma::signalpath::SignalPathMapperControl::WalshColumnAssignment> 
          getWalshColumnAssignment(unsigned short antNo);

   /**
    * @return the configuration as a printable string
    */
   ::std::string queryConfiguration();

   /**
    * Methods to add/remove an antenna from a subarray
    */
   void addAntenna(unsigned short antNo, unsigned short subarrayNo);
   void removeAntenna(unsigned short antNo, unsigned short subarrayNo);

   void addCorrelator(ControlCorrelatorDesignation type, unsigned short subarrayNo);

   void removeCorrelator(ControlCorrelatorDesignation type, unsigned short subarrayNo);

   /**
    * @return True if the input astroband number is a valid astroband for
    * the input correlator type.
    */
   bool isValidAstroBand( unsigned astroBandNo, ControlCorrelatorDesignation type);

   /**
    * @return the configuration name of the input astroband
   *  @param astrobandNo   The Astroband number to query
    */
   ::std::string getConfname( unsigned short astroBandNo );

   /**
    * @return the Astroband number to which the input correlator band
    * is assigned, or -1 if unassigned.
    * @param corrBandNo The correlator band number to query
    * @param type The correlator type enum, e.g. CORR_SPECTRAL
    */
   int getAstroBandForCorrelatorBand( unsigned short corrBandNo,
                                            ControlCorrelatorDesignation type );

   /**
    * Query the correlator type for a given astroband.
    * @param astroBandNo The Astroband number to configure
    * @return The correlator type enum, e.g. CORR_SPECTRAL
    */
   ControlCorrelatorDesignation getCorrTypeForAstroBand( unsigned short astroBandNo );
};

// Optimally, this should be implemented in 
// signalpath/SignalPathMapperControlImpl.cc
// so that less data are transmitted across the wire.
// But for now, this suffices.
inline unsigned int
  carma::control::SignalPathMapperHandle::getNumActiveCorrelatorBands(ControlCorrelatorDesignation type) 
{
    if ( type == carma::util::CORR_NONE ) return 0;

    return getActiveCorrelatorBands( type ).size();
}


}} // namespace carma::control
#endif
