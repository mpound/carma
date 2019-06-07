// $Id: CorrDataRemapper.h,v 1.6 2013/06/25 18:54:18 mpound Exp $

#ifndef CARMA_CORRELATOR_CORRDATAREMAPPER_H
#define CARMA_CORRELATOR_CORRDATAREMAPPER_H

/**
 * @file CorrDataRemapper.h
 * 
 * Tagged: Mon Feb 14 17:11:13 PST 2011
 * 
 * @version: $Revision: 1.6 $, $Date: 2013/06/25 18:54:18 $
 * 
 * @author Erik Leitch
 */
#include "carma/correlator/obsRecord2/CorbaCorrConsumer.h"

#include "carma/correlator/lib/CorrelatorBaseline.h"
#include "carma/correlator/lib/CorrelatorPolarization.h"

#include "carma/signalpath/SignalPathMap.h"
#include "carma/signalpath/SignalPathMapperControl.h"

#include "carma/szautil/Mutex.h"
#include "carma/szautil/RunnableTask.h"

namespace carma {
  namespace correlator {

    namespace lib {
      class CorrelatorData;
    }

    class CorrDataRemapperControlImpl;

    //-----------------------------------------------------------------------
    // Define a class for remapping correlator data from input-order
    // to antenna-order.
    //-----------------------------------------------------------------------
    
    class CorrDataRemapper : 
    public sza::util::RunnableTask, 
      public carma::correlator::obsRecord2::CorbaCorrConsumer::Listener {
    public:

      // A struct for handling the mapping between astrobands and antenna IFs

      struct AstroBandInputMap {
        unsigned astroBandNo_;
        std::map<unsigned, carma::signalpath::SignalPathMapperControl::AntennaIF> antennaIfMap_;
      };

      struct AstroBandMap {
        sza::util::Mutex guard_;
        bool hasChanged_;
        std::map<unsigned, AstroBandInputMap*> astroBands_;

        AstroBandMap();
        ~AstroBandMap();

        void operator=(AstroBandMap& astroBandMap);
        void operator=(const AstroBandMap& astroBandMap);
      };

      /**
       * Constructor.
       */
      CorrDataRemapper(CorrDataRemapperControlImpl* parent, 
               std::string notificationChannelName,
               std::string imrSrc);

      /**
       * Destructor.
       */
      virtual ~CorrDataRemapper();

      // Re-populate the mapping for an astroband

      void repopulateAstroBandInputMap(CorrDataRemapper::AstroBandInputMap& astroBandInputMap);

    private:

      std::string imrSrc_;

      // The name of the correlator control DO name.  The notification
      // channel name is obtained via a call on this DO down in the
      // CorbaCorrConsumer class

      std::string notificationChannelName_;

      // The parent that instantiated this object

      CorrDataRemapperControlImpl* parent_;

      // The mapping between astroband inputs and antenna IFs

      CorrDataRemapper::AstroBandMap astroBandMap_;
      CorrDataRemapper::AstroBandMap stagedAstroBandMap_;

      // Update the astroband input mapping if it has changed

      void updateAstroBandMap();

      // Process data received for a correlator band
      // This method is called at regular intervals by 
      // the obsRecord2::CorbaCorrConsumer instantiated in the run() method.
      void processData(carma::correlator::lib::CorrelatorData * cd);

      // Remap correlator data

      void remapCorrData(carma::correlator::lib::CorrelatorData * cd, unsigned& astroBandNo);

      // Retrieve antenna numbers

      void getAntennaNos(carma::correlator::lib::CorrelatorBaseline& baseline, unsigned astroBandNo, 
             unsigned& antennaNo1, unsigned& antennaNo2, 
             bool& requiresConjugation);

      void getAntPols(carma::correlator::lib::CorrelatorBaseline& baseline, unsigned astroBandNo, 
              unsigned& antennaNo1, unsigned& antennaNo2, 
              carma::correlator::lib::Polarization& pol1,
              carma::correlator::lib::Polarization& pol2, 
              bool& requiresConjugation);

      // Conjugate all data for this baseline

      void conjugate(carma::correlator::lib::CorrelatorBaseline& baseline);
  
      // Republish remapped data on the appropriate channel

      void republishCorrData(carma::correlator::lib::CorrelatorData * cd, unsigned astroBandNo);

      // Run this thread
 
      void run();

    public:

      void spawn();
      unsigned getThreadId();

    }; // End class CorrDataRemapper

  } // End namespace correlator
} // End namespace carma

#endif // End #ifndef CARMA_CORRELATOR_CORRDATAREMAPPER_H
