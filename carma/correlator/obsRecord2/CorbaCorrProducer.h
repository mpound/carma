#ifndef CARMA_CORRELATOR_OBSRECORD2_CORBACORRPRODUCER_H
#define CARMA_CORRELATOR_OBSRECORD2_CORBACORRPRODUCER_H

//! @file
//! @brief Interface file for the CorbaCorrProducer class.
//! @author Rick Hobbs

#include "carma/correlator/obsRecord2/CorDataBase.h"
#include "carma/util/PthreadMutex.h"

#include <memory>
#include <orbsvcs/CosNotificationC.h>
#include <string>


namespace carma {

  namespace corba {
    class Client;
  }

  namespace util {
    class Orb;
  }

namespace correlator {

namespace lib {
    class CorrelatorData;
}

namespace obsRecord2 {


//! @brief Implements interface for serving correlator data using CORBA.
class CorbaCorrProducer {
    public:

        //! @brief Constructor.
        CorbaCorrProducer( const ::std::string & channelName );

        //! @brief Constructor.
        CorbaCorrProducer( carma::util::Orb* localOrb,
                           const ::std::string & channelName );
        
        //! @brief Destructor.
        virtual ~CorbaCorrProducer( );
        
        //! @brief Send data through Notification Channel
        //! @param cd correlator data to send
        //! @param corbaSendMicros If not NULL then the value pointed to will
        //!                        will be filled in with the latency of the
        //!                        raw corba send action in microseconds
        void sendCorData(
            const carma::correlator::lib::CorrelatorData & cd,
            double *                                       corbaSendMicros,
            size_t *                                       corbaSendBytes );
        
    private:
        // No copying
        CorbaCorrProducer( const CorbaCorrProducer & rhs );
        CorbaCorrProducer & operator=( const CorbaCorrProducer & rhs );
        

        const ::std::string notificationChannelName_;
        
        carma::util::Orb * localOrb_;
        const bool useLocalOrb_;

        CosNotification::StructuredEvent_var event_;
        carma::correlator::obsRecord2::CorData_s cordata_;
        carma::util::PthreadMutex containerMutex_;

        carma::corba::Client * client_;

};


}  // namespace carma::correlator::obsRecord2
}  // namespace carma::correlator
}  // namespace carma


#endif
