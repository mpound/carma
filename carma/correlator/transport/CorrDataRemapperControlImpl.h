// $Id: CorrDataRemapperControlImpl.h,v 1.4 2014/06/20 15:56:19 mpound Exp $

#ifndef CARMA_CORRELATOR_CORRDATAREMAPPERCONTROLIMPL_H
#define CARMA_CORRELATOR_CORRDATAREMAPPERCONTROLIMPL_H

/**
 * @file CorrDataRemapperControlImpl.h
 * 
 * Tagged: Tue Feb 15 10:47:29 PST 2011
 * 
 * @version: $Revision: 1.4 $, $Date: 2014/06/20 15:56:19 $
 * 
 * @author username: Command not found.
 */
#include <vector>
#include <map>
#include <boost/shared_ptr.hpp>

#include "carma/correlator/lib/CorrelatorData.h"

#include "carma/correlator/obsRecord2/CorbaCorrProducer.h"

#include "carma/correlator/transport/CorrDataRemapperControl.h"
#include "carma/correlator/transport/CorrDataRemapper.h"

#include "carma/monitor/SlRemapperSubsystem.h"
#include "carma/monitor/WbRemapperSubsystem.h"
#include "carma/monitor/C3gRemapperSubsystem.h"

#include "carma/szautil/Mutex.h"

#include "carma/util/Orb.h"

// memory managed pointers, no need to delete them in class destructor
typedef boost::shared_ptr<carma::monitor::SlRemapperSubsystem> slmon_ptr;
typedef boost::shared_ptr<carma::monitor::WbRemapperSubsystem> wbmon_ptr;
typedef boost::shared_ptr<carma::monitor::C3gRemapperSubsystem> c3gmon_ptr;

namespace carma {
  namespace correlator {

    namespace lib {
      class CorrelatorData;
    }

    class CorrDataRemapper;

    class CorrDataRemapperControlImpl {
    public:

      enum Sideband {
        SB_AUTO,
        SB_LSB,
        SB_USB
      };

      //------------------------------------------------------------
      // A struct for handling debug information
      //------------------------------------------------------------

      struct Debugger {

        // If true, print debugging output

        bool debug_;

        // Band number ro print

        unsigned astroBandNo_;

        // Sideband to print

        Sideband sideband_;

        // First and second input number to print

        unsigned inputNo1_;
        unsigned inputNo2_;

        Debugger();
        void printDebugInfo(carma::correlator::lib::CorrelatorData* cd);
        void printSideband(const carma::correlator::lib::CorrelatorSideband& sb);
      };
      
      //=======================================================================
      // Define a class for producing data via notification
      //=======================================================================
      
      class Producer : public carma::correlator::obsRecord2::CorbaCorrProducer {
      public:
      
      Producer(std::string notificationChannelName, carma::util::Orb & orb );
      ~Producer();
      
      std::string ncName_;

      void sendCorrelatorData(carma::correlator::lib::CorrelatorData* data);

    };

      /**
       * Constructor.
       */
      CorrDataRemapperControlImpl(bool produce, std::string imrSrc, std::string imrDest, bool noPCS);

      /**
       * Destructor.
       */
      virtual ~CorrDataRemapperControlImpl();

      //------------------------------------------------------------
      // IDL interface
      //------------------------------------------------------------

      void clearAstroBandInputMap(CORBA::UShort& astroBandNo);

      void updateAstroBandInputMap(CORBA::UShort& astroBandNo,
                   const CorrDataRemapperControl::AstroBandInputSeq& astroBandInputs);

      //------------------------------------------------------------
      // Non-IDL methods
      //------------------------------------------------------------

      // Initialize data connections

      void createProducers();

      // Create correlator-data listener resources

      void createCorrBandListeners();
      void createAstroBandListeners();

      // Spawn correlator-data listeners 

      void spawnListeners();
      void spawnProducers();

      void addListener(std::string objectName, const unsigned bandNo);

      /** Publish remapped data for an astroband
       * @param cd The correlator data for the band
       * @param astroBandNo The astroband number associated with the band data.
       * @return true if publish was successful, false if it failed or the remapper is set to not produce data. 
       */
      bool publishData(carma::correlator::lib::CorrelatorData* cd, unsigned astroBandNo);

      Debugger& getDebugger();

      void setAstroBandInputMapsToDefaults();

      // Methods to set the underlying monitor point values via callback
      // from an instantiated CorrDataRemapper listener.  
      // First parameter is the notification channel name which is used to
      // look up the associated monitor subsystem from the std::map.
      // Second parameter is the monitor point value to set.

      /**
       * Set the monitor point indicating the monitor subsystem is
       * online.
       * @param ncName The notification channel name for the band
       * @param conjugate True if the baselines have been conjugated.
       */
      void setOnlineMP( const std::string & ncName, const bool online );

      /**
       * Set the monitor point indicating the 
       * number of valid baselines seen by the remapper 
       * for a given band.
       * @param ncName The notification channel name for the band
       * @param num The number of valid baselines.
       */
      void setNumValidBaselinesMP( const std::string & ncName, const unsigned num );

       // There is no way for the remapper to know the number of invalid baselines
       // because the correlator data receieved already has the invalid baselines
       // removed.
      /**
       * 
       * Set the monitor point indicating the 
       * number of invalid baselines seen by the remapper 
       * for a given band.
       * @param ncName The notification channel name for the band
       * @param num The number of invalid baselines.
      void setNumInvalidBaselinesMP( const std::string & ncName, const unsigned num );
       */

      /**
       * Set the monitor point indicating the 
       * astroband number for a given band.
       * @param ncName The notification channel name for the band
       * @param abNo The astroband
       */
      void setAstrobandNoMP( const std::string & ncName, const unsigned abNo );

      /**
       * Set the monitor point indicating the 
       * the number of baselines in a given band that the remapper 
       * has conjugated.
       * @param ncName The notification channel name for the band
       * @param num The number of conjugated baselines.
       */
      void setNumConjugatedMP( const std::string & ncName, const unsigned num);

      /**
       * Set the monitor point indicating the 
       * the name of the published object for the remapped band.
       * @param ncName The notification channel name for the band
       * @param objName The object name
       */
      void setPublishedObjectMP( const std::string & ncName, const std::string & objName );

      /**
       * Set the monitor point indicating the 
       * the name of the published object for the remapped band to
       * the default value for a given astroband; i.e.
       * carma.correlator.astrobandN.data.
       * @param ncName The notification channel name for the band
       * @param astroBandNo The astroband number
       */
      void setDefaultPublishedObjectMP( const std::string & ncName, const unsigned astroBandNo);

      /**
       * Set the monitor point indicating the 
       * the time at which a band's data were received from the CorrDataUpdater.
       * @param ncName The notification channel name for the band
       * @param mjd The modified Julian day
       */
      void setReceivedTimeMP( const std::string & ncName, const double mjd);

      /**
       * Set the monitor point indicating the 
       * the time at which a band's data were published the remapper.
       * @param ncName The notification channel name for the band
       * @param mjd The modified Julian day
       */
      void setPublishedTimeMP( const std::string & ncName, const double mjd);

    private:

      // IMR from which we are getting our data

      std::string imrSrc_;

      // IMR on which we are publishing our data

      std::string imrDest_;

      // Debugger object for printing out information on visibility data

      Debugger debugger_;

      // If true, we will connect to the actual data stream and produce data
      // If false, we will connect to the astroband data stream and print it

      bool produce_;

      // True if we are NOT using Program's corba::Server as the orb.
      // In this case no monitor system can be written.
      bool noPCS_;

      // A vector of listeners for collecting data from correlator
      // band servers

      // @todo consider using shared_ptr here.
      std::vector<CorrDataRemapper*> listeners_;

      // A vector of producers for publishing data on notification
      // channels

      // @todo consider using shared_ptr here.
      std::vector<Producer*> producers_;
      // names of published remapped objects indexed by astroband number
      std::map<unsigned, std::string> producerNames_;

      carma::util::Orb orb_; 

      // A cleverer person would do this with a single map
      // and a template.
      std::map<std::string, slmon_ptr > slMonsys_;
      std::map<std::string, wbmon_ptr > wbMonsys_;
      std::map<std::string, c3gmon_ptr > c3gMonsys_;


    }; // End class CorrDataRemapperControlImpl

  } // End namespace correlator
} // End namespace carma



#endif // End #ifndef CARMA_CORRELATOR_CORRDATAREMAPPERCONTROLIMPL_H
