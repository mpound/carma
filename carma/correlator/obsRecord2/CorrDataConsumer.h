#ifndef CARMA_CORRELATOR_OBSRECORD2_CORRDATACONSUMER_H
#define CARMA_CORRELATOR_OBSRECORD2_CORRDATACONSUMER_H

#include "carma/correlator/obsRecord2/CorbaCorrConsumerStats.h"

#include <string>
#include <memory>

//! @file CorrDataConsumer.h
//!
//! @author Rick Hobbs
//!
namespace carma {

namespace corba {

  class Server;

}

namespace correlator {

namespace lib {

class CorrelatorData;

}

namespace obsRecord2 {

//! @brief Implements CorrConsumer interface for CORBA.
class CorrDataConsumer {
    public:
        class Listener;
    
        //! Constructor.
        CorrDataConsumer( carma::corba::Server & server,
                          const std::string & notificationChannelName, 
                          Listener & listener );
      
        //! Destructor.
        virtual ~CorrDataConsumer( );
      
        //! @brief Start collecting data.
        //!
        //! This method will block. The listener given in the constructor
        //! will be called back when data is ready.
        virtual void getData( );
    
        //! @brief Get Deserialization stats
        //! 
        //! Retrieve deserialization stats for last event.
        CorbaCorrConsumerStats getCorbaCorrConsumerStats( );

    private:
        // No copying
        CorrDataConsumer( const CorrDataConsumer & rhs );
        CorrDataConsumer & operator=( const CorrDataConsumer & rhs );

        class Impl;
        
        ::std::auto_ptr< Impl > impl_;

}; // End class CorrDataConsumer


//! @brief Abstract base class for listener called back by CorrDataConsumer
class CorrDataConsumer::Listener {
    public:
        virtual ~Listener( ) {  }
        
        virtual void
        processData( carma::correlator::lib::CorrelatorData * cd ) = 0;
};

} // End namespace obsRecord2
} // End namespace correlator
} // End namespace carma
#endif
