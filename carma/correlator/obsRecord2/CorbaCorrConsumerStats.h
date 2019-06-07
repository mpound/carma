#ifndef CARMA_CORRELATOR_OBSRECORD2_CORBACORRCONSUMERSTATS_H
#define CARMA_CORRELATOR_OBSRECORD2_CORBACORRCONSUMERSTATS_H

#include <string>

namespace carma {
namespace correlator {
namespace obsRecord2 {

//! @brief Structure to hold corr consumer statistics for profiling.
struct CorbaCorrConsumerStats {

    CorbaCorrConsumerStats( );

    CorbaCorrConsumerStats( const std::string & ncName );

    CorbaCorrConsumerStats( const CorbaCorrConsumerStats & rhs );

    CorbaCorrConsumerStats & operator=( const CorbaCorrConsumerStats & rhs );

    bool active;
    std::string notificationChannelName;
    int deserializationErrorCount;
    bool errorOnLastDeserialization;
    double assemblyLatencyInMs;
    double transmitLatencyInMs;
    double receiveLatencyInMs;
    double corbaDemarshalingTimeInMs;
    double deserializationTimeInMs;
    double totalProcTimeInMs;
};

} // End namespace obsRecord2
} // End namespace correlator
} // End namespace carma
#endif
