// $Id: CorrelatorDataTestSZA.h,v 1.8 2005/11/28 23:35:28 rick Exp $

#ifndef CORRELATORDATATESTSZA_H
#define CORRELATORDATATESTSZA_H

#include "carma/correlator/lib/CorrelatorConfigChecker.h"
#include "carma/correlator/lib/CorrelatorData.h"

/**
 * @file CorrelatorDataTestSZA.h
 * 
 * Started: Mon Mar  8 15:14:07 PST 2004
 *
 * @version $Revision: 1.8 $, $Date: 2005/11/28 23:35:28 $
 * 
 * @author Rick Hobbs
 */
namespace carma {
  namespace correlator {
    namespace lib {

      /**      
       * Test class used to assign test data to a CorrelatorData object.
       */
      class CorrelatorDataTestSZA:
        public carma::correlator::lib::CorrelatorData {
        public:
        /**
         * Constructor
         */
        CorrelatorDataTestSZA();

        /**
         * Constructor. Assigns bandNumber to this band
         */
        CorrelatorDataTestSZA(int bandNumber);
	
        /**
         * Destructor
         */
        virtual ~CorrelatorDataTestSZA();
	
        /**
         *  Set the bandNumber for this band.
         */
        void setBandNumber(int bandNumber);
	
        private:
        static const std::string className_;
        carma::correlator::lib::CorrelatorConfigChecker* ccc_;
        void createTestData();
		void simData(int band, int an1, int an2, int numChans, int sideband,
                     std::vector<std::complex<float> > &data);
		void simData2(int band, int an1, int an2, int numChans, int sideband,
                     std::vector<std::complex<float> > &data);
		int bandNumber_;
		int getId(int a1, int a2);
      }; // End class CorrelatorDataTestSZA
      
    }; // End namespace lib
  }; // End namespace correlator
}; // End namespace carma

#endif // End #ifndef 
