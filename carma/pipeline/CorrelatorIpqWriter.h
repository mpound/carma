#ifndef CARMA_PIPELINE_CORRELATORIPQWRITER_H
#define CARMA_PIPELINE_CORRELATORIPQWRITER_H

#include "carma/util/IPQbasicTypeBuffer.h"
#include "carma/correlator/lib/CorrelatorListener.h"

#include <vector>

namespace carma {
  namespace pipeline {
      /**
       * Concrete class for writing out Correlator Data to a File
       */
      class CorrelatorIpqWriter : public carma::correlator::lib::CorrelatorListener {
      public:

        /**
         * Constructor. Specifies output filename
         */
        CorrelatorIpqWriter(const std::string& filename,
                            unsigned int ipqMaxsize,
                            unsigned int ipqNumberOfElements);

        /**
         * Destructor
         */
        virtual ~CorrelatorIpqWriter();

        /**
         *  Called whenever new data is ready to be processed.
         */
        void processData(carma::correlator::lib::CorrelatorData* cd);

        /**
         *  Return class name
         */
        const std::string& getName() const;

      private:

        const std::string outputFilename_;
        const unsigned int ipqMaxsize_;
        const unsigned int ipqNumberOfElements_;

        mutable std::vector< char > byteArray_;
        mutable carma::util::IPQbasicTypeBuffer ipq_;

      };

  } // End namespace pipeline
} // End namespace carma

#endif //CARMA_PIPELINE_CORRELATORIPQWRITER_H
