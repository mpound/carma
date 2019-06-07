#include "carma/correlator/obsRecord2/CorbaCorrProducer.h"

#include "carma/corba/Client.h"
#include "carma/correlator/lib/CorrelatorData.h"
#include "carma/util/compileTimeCheck.h"
#include "carma/util/ErrorException.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/Orb.h"
#include "carma/util/Program.h"
#include "carma/util/programLogging.h"
#include "carma/util/ScopedPthreadMutexLock.h"

#include <sys/time.h>

using namespace ::std;
using namespace carma::corba;
using namespace carma::util;
using namespace carma::correlator;
using namespace carma::correlator::lib;
using namespace carma::correlator::obsRecord2;


CorbaCorrProducer::CorbaCorrProducer( const string & channelName ) :
    notificationChannelName_( channelName ),
    localOrb_( 0 ),
    useLocalOrb_( false ),
    event_( ),
    cordata_( ),
    containerMutex_( ),
    client_( &Program::getProgram().getCorbaClient() )
{
    if ( client_ == 0 )
        throw CARMA_ERROR( "Client reference is null." );
}

CorbaCorrProducer::CorbaCorrProducer( carma::util::Orb* localOrb, 
                                      const string & channelName ) :
    notificationChannelName_( channelName ),
    localOrb_( localOrb ),
    useLocalOrb_( true ),
    event_( localOrb->createEventForm( "Correlator Data", "Ok" ) ),
    cordata_( ),
    containerMutex_( ),
    client_( 0 )
{
    event_->filterable_data.length( 1 );
}


CorbaCorrProducer::~CorbaCorrProducer( )
try {
} catch ( ... ) {
    // Just stifle any exceptions
    
    return;
}


void
CorbaCorrProducer::sendCorData( const CorrelatorData & cd,
                                double * const         corbaSendMicros,
                                size_t * const         corbaSendBytes )
{
    compileTimeCheck< (sizeof( CORBA::Octet ) == sizeof( char )) >();

    const size_t dataSeqLength = ::std::max( 0, cd.getTotalSerialBytes() );

    struct ::timeval tvBefore;
    struct ::timeval tvAfter;

    {
        const ScopedPthreadMutexLock lock( containerMutex_ );
        // copy data
        {
            if ( cordata_.correlatorData.length() != dataSeqLength ) {
                if ( dataSeqLength > cordata_.correlatorData.maximum() )
                    cordata_.correlatorData = DataSeq( dataSeqLength );

                cordata_.correlatorData.length( dataSeqLength );
            }

            char * const byteArray =
                reinterpret_cast< char * >(
                        cordata_.correlatorData.get_buffer() );

            int totalSerialBytes = 0;
            cd.serialIntoByteArray( byteArray,
                    dataSeqLength,
                    &totalSerialBytes );
        }

        if (useLocalOrb_) {
            event_->filterable_data[0].name = CORBA::string_dup(
                "Correlator Data");
            event_->filterable_data[0].value <<= cordata_;
        }

        // transport

        if ( corbaSendMicros != 0 )
            ::gettimeofday( &tvBefore, 0 );

        try {

            if(useLocalOrb_) {
                localOrb_->sendNotification( notificationChannelName_,
                        notificationChannelName_,
                        event_ );
            } else {
                client_->sendNotification< CorData_s >( 
                        notificationChannelName_,
                        notificationChannelName_,
                        cordata_ );
            }

        } catch ( const CORBA::Exception & ex ) {
            programLogErrorIfPossible(
                    "Exception while trying to send data - " +
                    getStringForCaught() );
        }

        if ( corbaSendMicros != 0 )
            ::gettimeofday( &tvAfter, 0 );
    }

    if ( corbaSendMicros != 0 ) {
        double micros = static_cast< double >( tvAfter.tv_sec );
        micros -= static_cast< double >( tvBefore.tv_sec );

        micros *= 1.0e+6;

        micros += static_cast< double >( tvAfter.tv_usec );
        micros -= static_cast< double >( tvBefore.tv_usec );

        *corbaSendMicros = micros;
    }

    if ( corbaSendBytes != 0 )
        *corbaSendBytes = dataSeqLength;
}
