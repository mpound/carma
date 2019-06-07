#ifndef CARMA_PIPELINE_DATACONTAINER_H
#define CARMA_PIPELINE_DATACONTAINER_H

#include "carma/util/PthreadMutex.h"
#include "carma/util/types.h"

#include <map>

namespace carma {

namespace correlator {
namespace lib {

class CorrelatorData;

}  // namespace carma::correlator::lib
}  // namespace carma::correlator

namespace pipeline {

class DataContainer {
    public:

        /**
         * Constructor
         */
        explicit DataContainer( );

        /**
        * Destructor
        */
        virtual ~DataContainer( );

        /**
         * Retrieve copy of (possibly empty) CorrelatorData for a given frame.
         * Trying to retrieve correlatorData for frames which have already been
         * cleared via clearCorrelatorData will result in an exception.
         * @param cd Pointer to CorrelatorData which copy will be placed into.
         * @param frame Frame identifier for data.
         * @throw IllegalArgumentException on attempt to retrieve expired data. 
         * @see clearCorrelatorData
         */
        void getCorrelatorData( carma::correlator::lib::CorrelatorData * cd,
                                carma::util::frameType frame ) const;

        /**
         * Fill internal CorrelatorData objects with bands from input object.
         * Bands are not copied in any particular order.
         * @param cd Input CorrelatorData object to copy bands from. 
         */
        void fillCorrelatorData( 
            const carma::correlator::lib::CorrelatorData & cd );

        /**
         * Set the expected number of bands to reserve space for bands 
         * when creating new CorrelatorData.
         * @param expectedNumberOfBands Expected number of bands.
         */ 
        void setExpectedNumberOfBands( int expectedNumberOfBands );

        /**
         * Delete internal CorrelatorData older than specified frame.
         * Subsequent data received which is older than specifed frame will be
         * logged as late.  
         * @param olderThan Delete data older than frame.
         * @see getCorrelatorData
         */
        void clearCorrelatorData( carma::util::frameType olderThan );

        /**
         * Typedef for late band count, keyed on band number.
         */
        typedef ::std::map< int, int >                 LateBandMap;

        /**
         * Get a copy of the late band count map.
         */
        LateBandMap getLateBandMap( ) const;

        /**
         * Typedef for late band count, keyed on band number.
         */
        typedef ::std::map< int, int >                 DupBandMap;

        /**
         * Get a copy of duplicate band count.
         * A duplicate band is one which is received that contains
         * already added baselines.  We shouldn't get them ever and
         * doing so is a serious bug upstream.
         */
        DupBandMap getDuplicateBandMap( ) const;

    private:

        DataContainer( const DataContainer & rhs );
        DataContainer & operator=( const DataContainer & rhs );

        mutable carma::util::PthreadMutex              guard_;
        typedef ::std::map< 
            carma::util::frameType,
            carma::correlator::lib::CorrelatorData * > CorrelatorDataMap;
        CorrelatorDataMap                              correlatorData_;
        int                                            expectedNumberOfBands_;
        carma::util::frameType                         oldestValidFrame_;
        
        mutable carma::util::PthreadMutex              lateBandMapGuard_;
        LateBandMap                                    lateBandMap_;
        
        mutable carma::util::PthreadMutex              dupBandMapGuard_;
        DupBandMap                                     dupBandMap_;
};


} // End namespace pipeline
} // End namespace carma


#endif
