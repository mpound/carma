#include "carma/correlator/lib/CorrelatorBand.h"

#include <ostream>

#include "carma/util/ExceptionUtils.h"
#include "carma/util/NotFoundException.h"
#include "carma/util/ScopedLogNdc.h"
#include "carma/util/Trace.h"
#include "carma/util/Time.h"

#include <boost/foreach.hpp>

using namespace ::std;
using namespace carma::correlator;
using namespace carma::correlator::lib;
using namespace carma::util;


int
CorrelatorBand::getSizeInBytes( ) const
{
  int size = 0;
  size += sizeof(mjd_);
  size += sizeof(bandNumber_);
  size += sizeof(selfTest_);
  size += sizeof(simulation_);
  size += sizeof(sequenceNumber_);
  size += sizeof(bandwidth_);
  size += sizeof(numberOfAntennas_);
  
  const int numberOfBaselines = baselines_.size();

  size += sizeof(numberOfBaselines);
  size += sizeof(valid_);

  BOOST_FOREACH( const CorrelatorBaseline & baseline, baselines_ ) {
    size += baseline.getSizeInBytes();
  }

  //cerr << "CorrelatorBand size[bytes]= " << size << endl;

  return size;
}


void
CorrelatorBand::mySerialize( char * const byteArray,
                             int * const  offset ) const
{
  pack(mjd_, byteArray, offset);
  pack(bandNumber_, byteArray, offset);
  pack(selfTest_, byteArray, offset);
  pack(simulation_, byteArray, offset);
  pack(sequenceNumber_, byteArray, offset);
  pack(bandwidth_, byteArray, offset);
  pack(numberOfAntennas_, byteArray, offset);

  const int numberOfBaselines = baselines_.size();

  pack(numberOfBaselines, byteArray, offset);
  pack(valid_, byteArray, offset);

  BOOST_FOREACH( const CorrelatorBaseline & baseline, baselines_ ) {
    baseline.serialize(byteArray, offset);
  }
}


void
CorrelatorBand::deserializeVer0( const char * const byteArray,
                                 int * const        offset,
                                 int                byteArraySize )
{
  //CPTRACE(carma::util::Trace::TRACE7, "Band V0");
  unpack(mjd_, byteArray, offset, byteArraySize);
  unpack(bandNumber_, byteArray, offset, byteArraySize);
  unpack(selfTest_, byteArray, offset, byteArraySize);
  unpack(simulation_, byteArray, offset, byteArraySize);
  unpack(sequenceNumber_, byteArray, offset, byteArraySize);
  unpack(bandwidth_, byteArray, offset, byteArraySize);
  unpack(numberOfAntennas_, byteArray, offset, byteArraySize);

  int numberOfBaselines = 0;
  unpack(numberOfBaselines, byteArray, offset, byteArraySize);
  unpack(valid_, byteArray, offset, byteArraySize);

  /*
    CPTRACE( Trace::TRACE5, "  bandNumber_= " << bandNumber_ << endl
           << "  selfTest_= " << selfTest_ << endl
           << "  simulation_= " << simulation_ << endl
           << "  sequenceNumber_= " << sequenceNumber_ << endl
           << "  bandwidth_= " << bandwidth_ << endl
           << "  numberOfInputs_= " << numberOfAntennas_ << endl
           << "  valid_= " << valid_ << endl
           << "  numberOfBaselines= " << numberOfBaselines);
   */
  
  baselines_.resize(numberOfBaselines);
  BOOST_FOREACH( CorrelatorBaseline & baseline, baselines_ ) {
    baseline.deserializeVer0(byteArray, offset, byteArraySize);
  }
}


void
CorrelatorBand::deserializeVer1( const char * const byteArray,
                                 int * const        offset,
                                 int                byteArraySize )
{
  //CPTRACE(carma::util::Trace::TRACE4, "Band V1");
  unpack(mjd_, byteArray, offset, byteArraySize);
  unpack(bandNumber_, byteArray, offset, byteArraySize);
  unpack(selfTest_, byteArray, offset, byteArraySize);
  unpack(simulation_, byteArray, offset, byteArraySize);
  unpack(sequenceNumber_, byteArray, offset, byteArraySize);
  unpack(bandwidth_, byteArray, offset, byteArraySize);
  unpack(numberOfAntennas_, byteArray, offset, byteArraySize);

  int numberOfBaselines = 0;
  unpack(numberOfBaselines, byteArray, offset, byteArraySize);
  unpack(valid_, byteArray, offset, byteArraySize);

  /*
    CPTRACE( Trace::TRACE5, "  bandNumber_= " << bandNumber_ << endl
           << "  selfTest_= " << selfTest_ << endl
           << "  simulation_= " << simulation_ << endl
           << "  sequenceNumber_= " << sequenceNumber_ << endl
           << "  bandwidth_= " << bandwidth_ << endl
           << "  numberOfInputs_= " << numberOfAntennas_ << endl
           << "  valid_= " << valid_ << endl
           << "  numberOfBaselines= " << numberOfBaselines);
   */
  
  baselines_.resize(numberOfBaselines);
  BOOST_FOREACH( CorrelatorBaseline & baseline, baselines_ ) {
    baseline.deserializeVer1(byteArray, offset, byteArraySize);
  }
}


void
CorrelatorBand::deserializeSwapVer0( const char * const byteArray,
                                     int * const        offset,
                                     const int          byteArraySize )
{
  unpackSwap(mjd_, byteArray, offset, byteArraySize);
  unpackSwap(bandNumber_, byteArray, offset, byteArraySize);
  unpackSwap(selfTest_, byteArray, offset, byteArraySize);
  unpackSwap(simulation_, byteArray, offset, byteArraySize);
  unpackSwap(sequenceNumber_, byteArray, offset, byteArraySize);
  unpackSwap(bandwidth_, byteArray, offset, byteArraySize);
  unpackSwap(numberOfAntennas_, byteArray, offset, byteArraySize);

  int numberOfBaselines = 0;
  unpackSwap(numberOfBaselines, byteArray, offset, byteArraySize);
  unpackSwap(valid_, byteArray, offset, byteArraySize);

  /*
    CPTRACE( Trace::TRACE5, "  bandNumber_= " << bandNumber_ << endl
           << "  selfTest_= " << selfTest_ << endl
           << "  simulation_= " << simulation_ << endl
           << "  sequenceNumber_= " << sequenceNumber_ << endl
           << "  bandwidth_= " << bandwidth_ << endl
           << "  numberOfInputs_= " << numberOfAntennas_ << endl
           << "  valid_= " << valid_ << endl
           << "  numberOfBaselines= " << numberOfBaselines);
   */
   
  baselines_.resize(numberOfBaselines);
  BOOST_FOREACH( CorrelatorBaseline & baseline, baselines_ ) {
    baseline.deserializeSwapVer0(byteArray, offset, byteArraySize);
  }
}


void
CorrelatorBand::deserializeSwapVer1( const char * const byteArray,
                                     int * const        offset,
                                     const int          byteArraySize )
{
  unpackSwap(mjd_, byteArray, offset, byteArraySize);
  unpackSwap(bandNumber_, byteArray, offset, byteArraySize);
  unpackSwap(selfTest_, byteArray, offset, byteArraySize);
  unpackSwap(simulation_, byteArray, offset, byteArraySize);
  unpackSwap(sequenceNumber_, byteArray, offset, byteArraySize);
  unpackSwap(bandwidth_, byteArray, offset, byteArraySize);
  unpackSwap(numberOfAntennas_, byteArray, offset, byteArraySize);

  int numberOfBaselines = 0;
  unpackSwap(numberOfBaselines, byteArray, offset, byteArraySize);
  unpackSwap(valid_, byteArray, offset, byteArraySize);

  /*
    CPTRACE( Trace::TRACE5, "  bandNumber_= " << bandNumber_ << endl
           << "  selfTest_= " << selfTest_ << endl
           << "  simulation_= " << simulation_ << endl
           << "  sequenceNumber_= " << sequenceNumber_ << endl
           << "  bandwidth_= " << bandwidth_ << endl
           << "  numberOfInputs_= " << numberOfAntennas_ << endl
           << "  valid_= " << valid_ << endl
           << "  numberOfBaselines= " << numberOfBaselines);
   */
   
  baselines_.resize(numberOfBaselines);
  BOOST_FOREACH( CorrelatorBaseline & baseline, baselines_ ) {
    baseline.deserializeSwapVer1(byteArray, offset, byteArraySize);
  }
}

bool
CorrelatorBand::hasBaseline( const int input1No,
                             const int input2No ) const
{
    BOOST_FOREACH( const CorrelatorBaseline & baseline, baselines_ ) {

        if ( (baseline.getInput1Number() == input1No ) &&
             (baseline.getInput2Number() == input2No ) )
            return true;
    }

    return false;
    
}


void
CorrelatorBand::addBaseline( const CorrelatorBaseline & baseline )
{
    baselines_.push_back( baseline );
}


void
CorrelatorBand::removeBaseline( const int a1,
                                const int a2 )
{
    vector< CorrelatorBaseline >::iterator iSrc = baselines_.begin();
    vector< CorrelatorBaseline >::iterator iDst = iSrc;
    const vector< CorrelatorBaseline >::iterator iEnd = baselines_.end();
    
    for ( ; iSrc != iEnd; ++iSrc ) {
        const bool keep = ((iSrc->getInput1Number() != a1) ||
                           (iSrc->getInput2Number() != a2));
        
        if ( keep ) {
            if ( iSrc != iDst )
                iDst->swap( *iSrc );
                
            ++iDst;
        }
    }
    
    if ( iDst == iEnd ) {
        ostringstream oss;
        
        oss << "Baseline[" << a1 << "-" << a2 << "] does not exist";
        
        throw CARMA_EXCEPTION( NotFoundException, oss.str() );
    }
    
    baselines_.erase( iDst, iEnd );
}

CorrelatorBaseline &
CorrelatorBand::getBaseline( const int i1, const int i2 ) 
{
    BOOST_FOREACH( CorrelatorBaseline & baseline, baselines_ ) {

        if ( (baseline.getInput1Number() == i1) &&
             (baseline.getInput2Number() == i2) )
            return baseline;
    }
    
    ostringstream oss;
    
    oss << "Baseline[" << i1 << "-" << i2 << "] does not exist";
    
    throw CARMA_EXCEPTION( NotFoundException, oss.str() );
}


const CorrelatorBaseline &
CorrelatorBand::getBaseline( const int i1,
                             const int i2 ) const
{
    BOOST_FOREACH( const CorrelatorBaseline & baseline, baselines_ ) {

        if ( (baseline.getInput1Number() == i1) &&
             (baseline.getInput2Number() == i2) )
            return baseline;
    }
    
    ostringstream oss;
    
    oss << "Baseline[" << i1 << "-" << i2 << "] does not exist";
    
    throw CARMA_EXCEPTION( NotFoundException, oss.str() );
}

void
CorrelatorBand::flagData( const unsigned int reason )
{
    BOOST_FOREACH( CorrelatorBaseline & baseline, baselines_ ) 
        baseline.flagData( reason );
}

void
CorrelatorBand::addIn( const CorrelatorBand & rhs )
{
    ScopedLogNdc ndc( "CorrelatorBand::addIn()" );

    if ( getNumberOfBaselines() <= 0 ) 
    {
        *this = rhs;
    } 
    else 
    {
        const vector< CorrelatorBaseline > & rhsBaselines = rhs.getBaselines();

        BOOST_FOREACH( const CorrelatorBaseline & rhsBaseline, rhsBaselines ) {
            addIn( rhsBaseline );
        }
    }
}

void
CorrelatorBand::addIn( const CorrelatorBaseline & rhsBaseline )
{
    const int i1n = rhsBaseline.getInput1Number();
    const int i2n = rhsBaseline.getInput2Number();
        
    ostringstream ndcMsg;
    ndcMsg << "CorrelatorBand::addIn( CorrelatorBaseline( " 
        << i1n << "-" << i2n << " ) )";
    ScopedLogNdc ndc( ndcMsg.str() );

    BOOST_FOREACH( CorrelatorBaseline & baseline, baselines_ ) {

        if ( (baseline.getInput1Number() == i1n) &&
             (baseline.getInput2Number() == i2n) ) {
            baseline.addIn( rhsBaseline );
            return;
        }
    }

    try {
        baselines_.push_back( rhsBaseline );
    } catch (...) {
        logCaughtAsError( );
        throw; // Rethrow 
    }

}


bool
CorrelatorBand::addInIgnoringDups( const CorrelatorBaseline & rhsBaseline )
{
    const int i1n = rhsBaseline.getInput1Number();
    const int i2n = rhsBaseline.getInput2Number();
        
    ostringstream ndcMsg;
    ndcMsg << "CorrelatorBand::addInIgnoringDups( CorrelatorBaseline( " 
        << i1n << "-" << i2n << " ) )";
    ScopedLogNdc ndc( ndcMsg.str() );

    BOOST_FOREACH( CorrelatorBaseline & baseline, baselines_ ) {

        if ( (baseline.getInput1Number() == i1n) &&
             (baseline.getInput2Number() == i2n) ) {
            return true;
        }
    }

    try {
        baselines_.push_back( rhsBaseline );
    } catch (...) {
        logCaughtAsError( );
        throw; // Rethrow 
    }
    
    return false;
}

             
bool 
CorrelatorBand::addInIgnoringDups( const CorrelatorBand & rhs )
{
    ScopedLogNdc ndc( "CorrelatorBand::addInIgnoringDups()" );
    bool dups( false );

    if ( getNumberOfBaselines() <= 0 ) 
    {
        *this = rhs;
    } else {

        const vector< CorrelatorBaseline > & rhsBaselines = rhs.getBaselines();

        BOOST_FOREACH( const CorrelatorBaseline & rhsBaseline, rhsBaselines ) {
            if ( addInIgnoringDups( rhsBaseline ) ) {
                dups = true;
            }
        }
    }

    return dups;
}


void
CorrelatorBand::normalize( )
{
    ScopedLogNdc ndc( "CorrelatorBand::normalize()" );

    BOOST_FOREACH( CorrelatorBaseline & baseline, baselines_ ) {
        baseline.normalize();
    }
}


void
CorrelatorBand::reserveBaselines( const int numBaselines )
{
    if ( numBaselines > 0 )
        baselines_.reserve( numBaselines );
}

string
CorrelatorBand::getSummary() const 
{
    ostringstream oss;
    oss << "Band " << bandNumber_ << ": " 
        << baselines_.size() << " bls (" << numberOfAntennas_ << " ants), " 
        << std::fixed << setprecision(0) << bandwidth_ << "MHz " 
        << (valid_ ? "" : "!") << "valid, "
        << (selfTest_ ? "" : "!") << "test, " 
        << (simulation_ ? "" : "!") << "sim, " 
        << "seq# " << sequenceNumber_  << " " << Time::getTimeString( mjd_, 1 );

    return oss.str();
}
