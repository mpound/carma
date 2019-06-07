// $Id: CorrelatorDataTestWb.h,v 1.1 2005/08/27 15:43:54 rick Exp $

#ifndef CORRELATORDATATESTWB_H
#define CORRELATORDATATESTWB_H

#include "carma/correlator/lib/CorrelatorConfigChecker.h"
#include "carma/correlator/lib/CorrelatorData.h"

/**
 * @file CorrelatorDataTestWb.h
 * 
 * Started: Mon Mar  8 15:14:07 PST 2004
 *
 * @version $Revision: 1.1 $, $Date: 2005/08/27 15:43:54 $
 * 
 * @author Rick Hobbs
 */
namespace carma {
  namespace correlator {
    namespace lib {

      /**      
       * Test class used to assign test data to a CorrelatorData object.
       */
      class CorrelatorDataTestWb:
        public carma::correlator::lib::CorrelatorData {
        public:
        /**
         * Constructor
         */
        CorrelatorDataTestWb();

        /**
         * Constructor. Assigns bandNumber to this band
         */
        CorrelatorDataTestWb(int bandNumber);
	
        /**
         * Destructor
         */
        virtual ~CorrelatorDataTestWb();
	
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
		int bandNumber_;
		int getId(int a1, int a2);
      }; // End class CorrelatorDataTestWb
      
    }; // End namespace lib
  }; // End namespace correlator
}; // End namespace carma

#endif // End #ifndef 
