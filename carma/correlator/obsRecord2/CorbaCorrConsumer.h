#ifndef CARMA_CORRELATOR_OBSRECORD2_CORBACORRCONSUMER_H
#define CARMA_CORRELATOR_OBSRECORD2_CORBACORRCONSUMER_H

#include "carma/correlator/obsRecord2/CorbaCorrConsumerStats.h"

#include <string>
#include <memory>

//! @file CorbaCorrConsumer.h
//!
//! @author Rick Hobbs
//!
namespace carma {

  namespace util {
    class Orb;
  }

namespace correlator {

namespace lib {

class CorrelatorData;

}

namespace obsRecord2 {

//! @brief Implements CorrConsumer interface for CORBA.
class CorbaCorrConsumer {
    public:
        class Listener;
    
        CorbaCorrConsumer( carma::util::Orb * localOrb,
                           const std::string & notificationChannelName, 
                           Listener & listener );
      
        //! Destructor.
        virtual ~CorbaCorrConsumer( );
      
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
        CorbaCorrConsumer( const CorbaCorrConsumer & rhs );
        CorbaCorrConsumer & operator=( const CorbaCorrConsumer & rhs );

        class CorrDataConsumer;
        class Impl;
        
        ::std::auto_ptr< Impl > impl_;

}; // End class CorbaCorrConsumer


//! @brief Abstract base class for listener called back by CorbaCorrConsumer
class CorbaCorrConsumer::Listener {
    public:
        virtual ~Listener( ) {  }
        
        virtual void
        processData( carma::correlator::lib::CorrelatorData * cd ) = 0;
};


} // End namespace obsRecord2
} // End namespace correlator
} // End namespace carma


#endif
