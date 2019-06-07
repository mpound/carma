#include "carma/pipeline/DataCollectorN.h"

#include "carma/corba/Server.h"
#include "carma/pipeline/DataContainer.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/ScopedPthreadMutexLock.h"
#include "carma/util/StartPthread.h"

#include <pthread.h>

using namespace ::std;
using namespace carma;
using namespace carma::correlator;
using namespace carma::correlator::lib;
using namespace carma::correlator::obsRecord2;
using namespace carma::pipeline;
using namespace carma::util;


struct DataCollectorN::CollectionThreadArgs {
    DataCollectorN * This;
    string ncName; // notification channel name
    corba::Server & server;

    CollectionThreadArgs( DataCollectorN * mom,
                          const string & channelName,
                          corba::Server & corbaServer ) :
        This( mom ),
        ncName( channelName ),
        server( corbaServer ) { }; 

};


const string DataCollectorN::className_("DataCollectorN");


DataCollectorN::DataCollectorN( const string & ncName,
                                DataContainer & dataContainer,
                                corba::Server & server ) :
    _dataContainer( dataContainer ),
    corrConsumer_( 0 ),
    corrConsumerActive_( false ),
    collectionActive_( false ),
    ncName_( ncName )
{
    CollectionThreadArgs threadArgs( this, ncName, server );

    ScopedPthreadMutexLock scopelock( collectionActiveMutex_ ); 

    StartPthreadWithCopy( dataCollectionThread, 
                          threadArgs, 
                          "DataCollectorN::dataUpdateThread(ncName=" 
                          + ncName,
                          0,
                          false ); // Disable cancellation
    
    // note that I don't care if we are in the CORBA event loop yet or not.
    // Main purpose of this is to detect when we've exited that loop.
    collectionActive_ = true; 
}


DataCollectorN::~DataCollectorN( )
{
    if ( corrConsumer_ != 0 ) 
        delete corrConsumer_;
}

bool
DataCollectorN::isCollectorActive( ) const
{
    ScopedPthreadMutexLock scopelock( collectionActiveMutex_ ); 
    return collectionActive_;
}

void
DataCollectorN::dataCollectionThread( CollectionThreadArgs & args )
try {

    args.This->corrConsumer_ = new CorrDataConsumer( 
                                           args.server,
                                           args.ncName,
                                           *(args.This) );

    { 
        ScopedPthreadMutexLock scopelock( args.This->collectionActiveMutex_ );
        args.This->corrConsumerActive_ = true;
    }

    args.This->corrConsumer_->getData();

    { 
        ScopedPthreadMutexLock scopelock( args.This->collectionActiveMutex_ ); 
        args.This->collectionActive_ = false;
    }

} catch ( ... ) {
    logCaughtAsError( );
    ScopedPthreadMutexLock scopelock( args.This->collectionActiveMutex_ ); 
    args.This->collectionActive_ = false;
}


void
DataCollectorN::processData( CorrelatorData * const cd )
{
    _dataContainer.fillCorrelatorData( *cd );
}

carma::correlator::obsRecord2::CorbaCorrConsumerStats
DataCollectorN::getCorbaCorrConsumerStats( ) const
{
    ScopedPthreadMutexLock scopelock( collectionActiveMutex_ );
    if ( corrConsumerActive_ ) {
        return corrConsumer_->getCorbaCorrConsumerStats();
    } else {
        return CorbaCorrConsumerStats( ncName_ );
    }
}

