// $Id: CorrelatorData.cc,v 1.48 2012/12/20 19:09:16 abeard Exp $

#include "carma/correlator/lib/CorrelatorData.h"

#include "carma/correlator/lib/CorrelatorConfigChecker.h"
#include "carma/util/ErrorException.h"
#include "carma/util/NotFoundException.h"
#include "carma/util/programLogging.h"
#include "carma/util/ScopedPthreadMutexLock.h"
#include "carma/util/Trace.h"

#include <boost/foreach.hpp>
#include <ostream>
#include <algorithm>

using namespace ::std;
using namespace carma;
using namespace carma::correlator;
using namespace carma::correlator::lib;
using namespace carma::util;

namespace {

    const Trace::TraceLevel TRACE_CTOR_DTOR = Trace::TRACE1;

} // namespace <unnamed>


int
CorrelatorData::getSizeInBytes( ) const
{
  int size = 0;
  size += header_.getSizeInBytes();
  size += sizeof(int);
  int bsize = band_.size();
  for (int idx = 0; idx < bsize; ++idx)
    size += band_.at( idx ).getSizeInBytes();

  return size;
}


void
CorrelatorData::mySerialize( char * const byteArray,
                             int * const  offset ) const
{
  // CPTRACE( Trace::TRACE5, "packing header...");
  header_.serialize(byteArray, offset);
  const int numberOfBands = band_.size();
  pack(numberOfBands, byteArray, offset);
  for (int idx = 0; idx < numberOfBands; ++idx)
    band_.at( idx ).serialize(byteArray, offset);
}


void
CorrelatorData::deserializeVer0( const char * const byteArray,
                                 int * const        offset,
                                 const int          byteArraySize )
{
  CPTRACE(carma::util::Trace::TRACE4, "Data V0");
  // CPTRACE( Trace::TRACE5, "offset= " << *offset);
  header_.deserializeVer0(byteArray, offset, byteArraySize );
  int numberOfBands = 0;
  unpack(numberOfBands, byteArray, offset, byteArraySize );
  // CPTRACE( Trace::TRACE5, "numberOfBands= " << numberOfBands);
  band_.resize(numberOfBands);
  // CPTRACE( Trace::TRACE5, "resized band_ vector");
  for (int idx = 0; idx < numberOfBands; ++idx)
    band_.at( idx ).deserializeVer0(byteArray, offset, byteArraySize );
}


void
CorrelatorData::deserializeVer1( const char * const byteArray,
                                 int * const        offset,
                                 const int          byteArraySize )
{
  CPTRACE(carma::util::Trace::TRACE4, "Data V1");
  // CPTRACE( Trace::TRACE5, "offset= " << *offset);
  header_.deserializeVer1(byteArray, offset, byteArraySize );
  int numberOfBands = 0;
  unpack(numberOfBands, byteArray, offset, byteArraySize );
  // CPTRACE( Trace::TRACE5, "numberOfBands= " << numberOfBands);
  band_.resize(numberOfBands);
  // CPTRACE( Trace::TRACE5, "resized band_ vector");
  for (int idx = 0; idx < numberOfBands; ++idx)
    band_.at( idx ).deserializeVer1(byteArray, offset, byteArraySize );
}


void
CorrelatorData::deserializeSwapVer0( const char * const byteArray,
                                     int * const        offset,
                                     const int          byteArraySize )
{
  header_.deserializeSwapVer0(byteArray, offset, byteArraySize );
  int numberOfBands = 0;
  unpackSwap(numberOfBands, byteArray, offset, byteArraySize );
  // CPTRACE( Trace::TRACE5, "numberOfBands= " << numberOfBands);
  band_.resize(numberOfBands);
  for (int idx = 0; idx < numberOfBands; ++idx)
    band_.at( idx ).deserializeSwapVer0(byteArray, offset, byteArraySize );
}


void
CorrelatorData::deserializeSwapVer1( const char * const byteArray,
                                     int * const        offset,
                                     const int          byteArraySize )
{
  header_.deserializeSwapVer1(byteArray, offset, byteArraySize );
  int numberOfBands = 0;
  unpackSwap(numberOfBands, byteArray, offset, byteArraySize );
  // CPTRACE( Trace::TRACE5, "numberOfBands= " << numberOfBands);
  band_.resize(numberOfBands);
  for (int idx = 0; idx < numberOfBands; ++idx)
    band_.at( idx ).deserializeSwapVer1(byteArray, offset, byteArraySize );
}


void
CorrelatorData::addBand( const CorrelatorBand & band )
{
    // check to see if this band number has been added
    const int bandNo = band.getBandNumber();

    if ( hasBand( bandNo ) )
        return;
    
    band_.push_back( band );
}


void
CorrelatorData::addBandViaSwap( CorrelatorBand & band )
{
    // check to see if this band number has been added
    const int bandNo = band.getBandNumber();
    
    if ( hasBand( bandNo ) )
        return;
    
    band_.push_back( CorrelatorBand() );
    const size_t j = band_.size();
    band_.at( j - 1 ).swap( band );
}


const CorrelatorBand &
CorrelatorData::getBand( const int bandNumber ) const
{
    BOOST_FOREACH( const CorrelatorBand & band, band_ ) {
        if ( band.getBandNumber() == bandNumber )
            return band;
    }
    
    ostringstream oss;
    
    oss << "Band " << bandNumber << " does not exist.";
    
    throw CARMA_EXCEPTION( NotFoundException, oss.str() );
}


CorrelatorBand &
CorrelatorData::getBand( const int bandNumber ) 
{
    BOOST_FOREACH( CorrelatorBand & band, band_ ) {
        if ( band.getBandNumber() == bandNumber )
                return band;
    }
    
    ostringstream oss;
    
    oss << "Band " << bandNumber << " does not exist.";
    
    throw CARMA_EXCEPTION( NotFoundException, oss.str() );
}


CorrelatorBand &
CorrelatorData::getBandNonconst( const int bandNumber )
{
    BOOST_FOREACH( CorrelatorBand & band, band_ ) {
        if ( band.getBandNumber() == bandNumber )
            return band;
    }
    
    ostringstream oss;
    
    oss << "Band " << bandNumber << " does not exist.";
    
    throw CARMA_EXCEPTION( NotFoundException, oss.str() );
}


bool
CorrelatorData::hasBand( const int bandNumber ) const
{
    BOOST_FOREACH( const CorrelatorBand & band, band_ ) {
        if ( band.getBandNumber() == bandNumber ) 
            return true;
    }

    return false;
}

void
CorrelatorData::addIn( const CorrelatorData & rhs )
{
    const vector< CorrelatorBand > & rhsBands = rhs.getBands();

    BOOST_FOREACH( const CorrelatorBand & rhsBand, rhsBands ) {
        addIn( rhsBand );
    }
}

void
CorrelatorData::addIn( const CorrelatorBand & rhsBand )
{
    const int rhsBandNo = rhsBand.getBandNumber(); 

    BOOST_FOREACH( CorrelatorBand & band, band_ ) {
        if ( band.getBandNumber() == rhsBandNo ) {
            band.addIn( rhsBand );
            return;
        }
    }

    band_.push_back( rhsBand );
}

bool
CorrelatorData::addInIgnoringDups( const CorrelatorBand & rhsBand )
{
    const int rhsBandNo = rhsBand.getBandNumber();

    BOOST_FOREACH( CorrelatorBand & band, band_ ) {
        if ( band.getBandNumber() == rhsBandNo ) {
            return band.addInIgnoringDups( rhsBand );
        }
    }

    band_.push_back( rhsBand );

    return false;
}

void
CorrelatorData::normalize( )
{
    BOOST_FOREACH( CorrelatorBand & band, band_ ) {
        band.normalize();
    }
}

void
CorrelatorData::incrementRefCount( )
{
    const ScopedPthreadMutexLock scopelock( refCountGuard_ );
    
    ++refCount_;
}


bool
CorrelatorData::incrementRefCountIfZero( )
{
    bool result = false;
    
    {
        const ScopedPthreadMutexLock scopelock( refCountGuard_ );
    
        if ( refCount_ == 0 ) {
            refCount_ = 1;
            result = true;
        }
    }
    
    return result;
}


void
CorrelatorData::decrementRefCount( )
{
    const ScopedPthreadMutexLock scopelock( refCountGuard_ );

    --refCount_;

    if ( refCount_ < 0 ) {
        refCount_ = 0;

        const string msg =
            "refCount < 0, Something is amiss. Check all "
            "increment/decrementRefCount calls. refCount is being set to 0";

        programLogErrorIfPossible( msg );

        throw CARMA_ERROR( msg );
    }
}


int
CorrelatorData::getRefCount( ) const
{
    int result;
    {
        const ScopedPthreadMutexLock scopelock( refCountGuard_ );

        result = refCount_;
    }
    
    return result;
}


void
CorrelatorData::setBandMJD( const int    bandNumber,
                            const double mjd )
{
    getBandNonconst( bandNumber ).setMJD( mjd );
}

namespace {
    
    set< int > calculatePhysicallyValidBaselineCounts( ) 
    {
        // We do both with and without autos here and note zero is a valid 
        // baseline count.
        set< int > validBlCounts;
        for ( int ant = 23; ant > 0; --ant ) {
            const int blcount = ( ant ) * ( ant - 1 ) / 2;
            const int blcountPlusAutos = blcount + ant;
            validBlCounts.insert( blcount ); 
            validBlCounts.insert( blcountPlusAutos );
            // Dual pol modes
            if ( ant < 16 ) {
                validBlCounts.insert( blcount * 2 );
                validBlCounts.insert( blcountPlusAutos * 2 );
            }
        }

        return validBlCounts;
    }
}


bool
CorrelatorData::baselineCountsPhysicallyValid( ) const
{
    static const set< int > validBlCounts = 
        calculatePhysicallyValidBaselineCounts( );
    
    BOOST_FOREACH( const CorrelatorBand & band, band_ ) {
        const BaselineVector::size_type blCount = band.getBaselines().size();
        if ( validBlCounts.find( blCount ) == validBlCounts.end() ) 
            return false;
    }
    return true;
}
        
