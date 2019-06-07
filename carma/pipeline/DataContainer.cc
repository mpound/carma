#include "carma/pipeline/DataContainer.h"

#include <iomanip> // needed for setprecision()
#include <iostream>
#include <sstream>
#include <vector>

#include "carma/correlator/lib/CorrelatorData.h"
#include "carma/util/IllegalArgumentException.h"
#include "carma/util/programLogging.h"
#include "carma/util/PthreadRWLock.h"
#include "carma/util/ScopedExclusiveLock.h"
#include "carma/util/ScopedPthreadMutexLock.h"
#include "carma/util/ScopedSharedLock.h"
#include "carma/util/Time.h"

#include <boost/foreach.hpp>

using namespace ::std;
using namespace log4cpp;
using namespace carma;
using namespace carma::pipeline;
using namespace carma::correlator::lib;
using namespace carma::util;

DataContainer::DataContainer( ) :
    guard_(),
    expectedNumberOfBands_( 0 ),
    oldestValidFrame_( 0 )
{
    // Nothing
}


DataContainer::~DataContainer( )
{
    const CorrelatorDataMap::iterator iBegin = correlatorData_.begin( );
    const CorrelatorDataMap::iterator iEnd = correlatorData_.end( );
    for ( CorrelatorDataMap::iterator i = iBegin; i != iEnd; ++i ) {
        delete i->second;
    }
}

void
DataContainer::setExpectedNumberOfBands( const int expectedNumberOfBands )
{
    expectedNumberOfBands_ = expectedNumberOfBands;
}

void
DataContainer::clearCorrelatorData( const frameType olderThan )
{
    // Loop through data and delete anything older than input frame
    const ScopedPthreadMutexLock lock( guard_ );
            
    oldestValidFrame_ = olderThan;

    const CorrelatorDataMap::iterator iBegin = correlatorData_.begin( );
    const CorrelatorDataMap::iterator iEnd = correlatorData_.end( );
    for ( CorrelatorDataMap::iterator i = iBegin; i != iEnd; ) {
        if ( i->first < olderThan ) {
            delete i->second;
            correlatorData_.erase( i++ ); 
        } else {
            return; 
        }
    }
}

void
DataContainer::getCorrelatorData( CorrelatorData * const cd,
                                  const carma::util::frameType frame ) const
{
    const ScopedPthreadMutexLock lock( guard_ );

    if ( frame < oldestValidFrame_ ) {
        ostringstream msg;
        msg << "DataContainer::getCorrelatorData() - Caller tried to retrieve "
            << "corr data for frame " << frame << " which has already been "
            << "cleared - oldest valid frame=" << oldestValidFrame_ << "."; 
        programLogErrorIfPossible( msg.str( ) );
        throw CARMA_EXCEPTION( IllegalArgumentException, msg.str( ) );
    }

    const CorrelatorDataMap::const_iterator i = correlatorData_.find( frame );

    if ( i != correlatorData_.end( ) ) {
        *cd = *(i->second);
    } else {
        // Return 'empty' correlator data.
        CorrelatorHeader header;
        header.setMJD( Time::MJD( frame ) );
        
        CorrelatorData corrData;
        corrData.setHeader( header );
        *cd = corrData;

        ostringstream msg;
        msg << "DataContainer::getCorrelatorData() - Returning empty "
            << "CorrelatorData for frame " << frame << ".";
        programLogWarnIfPossible( msg.str( ) );
    }
}

void
DataContainer::fillCorrelatorData( const CorrelatorData & cdIn )
{
    const int numBands = cdIn.getNumberOfBands();

    // Use the mjd of the current data to compute a frameType id.
    // WARNING: This ultimately relies on the fact that data is stamped with
    // a timestamp which is pegged to an integral half second.  If the mjd
    // were to ever change to an actual timestamp, this breaks!
    const double mjd = cdIn.getHeader().getMJD();
    const frameType dataTimeId = Time::computeClosestFrame(mjd);
    const vector< CorrelatorBand > & bands = cdIn.getBands();
    
    bool expired = false;
    frameType localOldestValidFrame = 0;

    {
        // Now swap it in while holding the lock
        const ScopedPthreadMutexLock lock( guard_ );
            
        // But only if it isn't too old
        if ( dataTimeId < oldestValidFrame_ ) {

            expired = true;
            localOldestValidFrame = oldestValidFrame_;

        } else {
        
            CorrelatorDataMap::iterator targetCdIterator = 
                correlatorData_.find( dataTimeId );
        
            // Create new corr data if necessary 
            if ( targetCdIterator == correlatorData_.end( ) ) { 

                pair< CorrelatorDataMap::iterator, bool > insertResult;

                CorrelatorHeader header;
                header.setMJD( mjd );
        
                auto_ptr< CorrelatorData > corrData( new CorrelatorData );
                corrData->reserveBands( expectedNumberOfBands_ );
                corrData->setHeader( header );

                insertResult = correlatorData_.insert( 
                    CorrelatorDataMap::value_type( dataTimeId, 
                                                   corrData.release( ) ) );

                if ( !insertResult.second ) {
                    throw CARMA_EXCEPTION( ErrorException,
                                           "Error inserting new cor data." );
                }

                targetCdIterator = insertResult.first;
            }

            CorrelatorData * targetCd = targetCdIterator->second;

            // Add bands to correlator data. 
            for ( int i = 0; i < numBands; ++i ) {
                const int bandNo = bands[i].getBandNumber();
                bool dups( false ); 
                if ( targetCd->hasBand( bandNo ) ) {
                    dups = targetCd->getBand( bandNo ).addInIgnoringDups( 
                        bands[i] );
                } else {
                    targetCd->addBand( bands[i] );
                }

                if ( dups ) {
                    ScopedPthreadMutexLock scopelock( dupBandMapGuard_ );
                    ++dupBandMap_[ bandNo ];
                }
            }

            // Set the target CD header to contain the largest tx & rx mjd 
            // of the various bands being collected.
            const double cdInTxMJD = cdIn.getHeader().getTransmissionMJD( );
            const double targetTxMJD = 
                targetCd->getHeader().getTransmissionMJD( );
            if ( cdInTxMJD > targetTxMJD ) 
                targetCd->setHeaderTransmissionMJD( cdInTxMJD );

            const double cdInRxMJD = cdIn.getHeader().getReceivedMJD( );
            const double targetRxMJD = targetCd->getHeader().getReceivedMJD( );
            if ( cdInRxMJD > targetRxMJD ) 
                targetCd->setHeaderReceivedMJD( cdInRxMJD );

        }

    }
    
    if ( expired ) {

        BOOST_FOREACH( const CorrelatorBand & band, bands ) {

            const int bandNumber = band.getBandNumber();

            { 
                const ScopedPthreadMutexLock lock( lateBandMapGuard_ );
                ++(lateBandMap_[bandNumber]);
            }

            ostringstream oss;
            
            oss << "In DataContainer::fillCorrelatorData: "
                << "LATE bandIdx= " << bandNumber - 1 
                << " dMJD(ms)= " 
                << setprecision(5)
                << fmod(mjd * 86400000., 60000.0)
                << setprecision(12)
                << " cMJD(ms)= " 
                << setprecision(5)
                << fmod(Time::MJD() * 86400000., 60000.0)
                << setprecision(12)
                << " dataFrame=" << dataTimeId
                << " currFrame=" << Time::computeFrame( Time::MJD() )
                << " lastValidFrame=" << localOldestValidFrame;
            
            programLogWarnIfPossible( oss.str() );
        }
    }
}

DataContainer::LateBandMap
DataContainer::getLateBandMap( ) const
{
    const ScopedPthreadMutexLock lock( lateBandMapGuard_ );
    return lateBandMap_;
}

DataContainer::DupBandMap
DataContainer::getDuplicateBandMap( ) const
{
    ScopedPthreadMutexLock scopelock( dupBandMapGuard_ );
    return dupBandMap_;
}
