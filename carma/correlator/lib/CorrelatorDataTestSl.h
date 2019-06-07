// $Id: CorrelatorDataTestSl.h,v 1.3 2006/12/11 19:42:48 abeard Exp $

#ifndef CORRELATORDATATESTSL_H
#define CORRELATORDATATESTSL_H

#include "carma/correlator/lib/CorrelatorConfigChecker.h"
#include "carma/correlator/lib/CorrelatorData.h"

/**
 * @file CorrelatorDataTestSl.h
 * 
 * Started: Mon Mar  8 15:14:07 PST 2004
 *
 * @version $Revision: 1.3 $, $Date: 2006/12/11 19:42:48 $
 * 
 * @author Rick Hobbs
 */
namespace carma {
  namespace correlator {
    namespace lib {

      /**      
       * Test class used to assign test data to a CorrelatorData object.
       */
      class CorrelatorDataTestSl:
        public carma::correlator::lib::CorrelatorData {
        public:
        /**
         * Constructor
         */
        CorrelatorDataTestSl();

        /**
         * Constructor. 
         * Creates test data with a specified number of channels.
         */
        CorrelatorDataTestSl( int channels );
	
        /**
         * Destructor
         */
        virtual ~CorrelatorDataTestSl();
	
        private:
        static const std::string className_;
        carma::correlator::lib::CorrelatorConfigChecker* ccc_;
        void createTestData( int channels );
		void simData(int band, int an1, int an2, int numChans, int sideband,
                     std::vector<std::complex<float> > &data);
      }; // End class CorrelatorDataTestSl
      
    }; // End namespace lib
  }; // End namespace correlator
}; // End namespace carma

#endif // End #ifndef 
