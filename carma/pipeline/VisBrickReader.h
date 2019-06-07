
// $Id: VisBrickReader.h,v 1.1 2011/08/18 23:25:52 abeard Exp $

#ifndef CARMA_PIPELINE_VISBRICKREADER_H
#define CARMA_PIPELINE_VISBRICKREADER_H

#include "carma/util/Time.h" // For frameType

#include <fstream>
#include <map>

namespace carma {

  namespace correlator {
    namespace lib {
      
      class CorrelatorData;

    } // namespace lib
  } // namespace correlator 
  
  namespace pipeline {
        
      typedef 
      ::std::map< carma::util::frameType, 
                  carma::correlator::lib::CorrelatorData * > RecordsByFrameMap;

      /**
       * Reads Correlator Data from a specifiec file
       */
      class CorrelatorVisBrickReader {
      public:

        /**
         * Constructor. 
         * @param filename Specifies input visbrick filename.
         * @param continueOnFileErrors Extract as many records as possible 
         *        from visbrick and continue (if possible) while logging 
         *        error to logs.
         */
        explicit CorrelatorVisBrickReader( const std::string & filename,
                                           bool continueOnFileErrors = false );

        /**
         * Destructor
         */
        virtual ~CorrelatorVisBrickReader();

        /**
         *  Read a single record from file
         *  @throw EOFException When read fails due to end-of-file.
         *  @throw exception when there is an error reading file
         *  @deprecated Use getRecordsKeyedByFrame
         */
        const carma::correlator::lib::CorrelatorData & readOne( );

        /**
         * Retrieve a map of all records keyed by frame time.
         */
        RecordsByFrameMap getRecordsKeyedByFrame( );

        /**
         * Was a file error detected while extracting correlator data records?
         * @see CorrelatorVisBrickReader::CorrelatorVisBrickReader
         */
        bool fileErrorDetected( ) const;

      private:

        void extractRecords( );

        const std::string inputFilename_;
        const std::string className_;
        std::ifstream fp_;
        RecordsByFrameMap records_;
        RecordsByFrameMap::iterator recordIterator_;
        const bool continueOnFileErrors_;
        bool fileErrorDetected_;
      };

  } // End namespace pipeline
} // End namespace carma

#endif //CARMA_PIPELINE_CORRELATORVISBRICKREADER_H
