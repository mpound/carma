#include "carma/correlator/lib/CorrelatorStats.h"
#include "carma/util/Trace.h"
#include "carma/util/complexManip.h"

using namespace ::std;
using namespace carma;
using namespace carma::correlator;
using namespace carma::correlator::lib;
using namespace carma::util;


int
CorrelatorStats::getSizeInBytes( ) const
{
  int size = 0;
  size += sizeof(integrationTime_);
  size += sizeof(numberOfSamples_);
  size += sizeof(avg_);
  size += sizeof(var_);
  size += sizeof(sd_);

  const complex< float > dummy( 0.0, 0.0 );
  
  size += sizeof(dummy);  // dummy up old max_ field
  size += sizeof(dummy);  // dummy up old min_ field
  
  //cerr << "Correlator Stats: size[bytes] = " << size << endl;
  
  return size;
}


void
CorrelatorStats::mySerialize( char * const byteArray,
                              int * const  offset ) const
{
  pack(integrationTime_, byteArray, offset);
  pack(numberOfSamples_, byteArray, offset);
  pack(avg_, byteArray, offset);
  pack(var_, byteArray, offset);
  pack(sd_, byteArray, offset);
  
  const complex< float > dummy( 0.0, 0.0 );
  
  pack(dummy, byteArray, offset);  // dummy up old max_ field
  pack(dummy, byteArray, offset);  // dummy up old min_ field
}


void
CorrelatorStats::deserializeVer0( const char * const byteArray,
                                  int * const        offset,
                                  const int          byteArraySize )
{
  //CPTRACE(carma::util::Trace::TRACE4, "Stats V0");
  unpack(integrationTime_, byteArray, offset, byteArraySize);
  unpack(numberOfSamples_, byteArray, offset, byteArraySize);
  unpack(avg_, byteArray, offset, byteArraySize);
  unpack(var_, byteArray, offset, byteArraySize);
  unpack(sd_, byteArray, offset, byteArraySize);

  complex< float > dummy( 0.0, 0.0 );

  unpack(dummy, byteArray, offset, byteArraySize);  // discard old max_ field
  unpack(dummy, byteArray, offset, byteArraySize);  // discard old min_ field
}

void
CorrelatorStats::deserializeVer1( const char * const byteArray,
                                  int * const        offset,
                                  const int          byteArraySize )
{
  //CPTRACE(carma::util::Trace::TRACE4, "Stats V1");
  unpack(integrationTime_, byteArray, offset, byteArraySize);
  unpack(numberOfSamples_, byteArray, offset, byteArraySize);
  unpack(avg_, byteArray, offset, byteArraySize);
  unpack(var_, byteArray, offset, byteArraySize);
  unpack(sd_, byteArray, offset, byteArraySize);

  complex< float > dummy( 0.0, 0.0 );

  unpack(dummy, byteArray, offset, byteArraySize);  // discard old max_ field
  unpack(dummy, byteArray, offset, byteArraySize);  // discard old min_ field
}


void
CorrelatorStats::deserializeSwapVer0( const char * const byteArray,
                                      int * const        offset,
                                      const int          byteArraySize )
{
  unpackSwap(integrationTime_, byteArray, offset, byteArraySize);
  unpackSwap(numberOfSamples_, byteArray, offset, byteArraySize);
  unpackSwap(avg_, byteArray, offset, byteArraySize);
  unpackSwap(var_, byteArray, offset, byteArraySize);
  unpackSwap(sd_, byteArray, offset, byteArraySize);

  complex< float > dummy( 0.0, 0.0 );

  unpackSwap(dummy, byteArray, offset, byteArraySize);  // discard old max_ field
  unpackSwap(dummy, byteArray, offset, byteArraySize);  // discard old min_ field
}


void
CorrelatorStats::deserializeSwapVer1( const char * const byteArray,
                                      int * const        offset,
                                      const int          byteArraySize )
{
  unpackSwap(integrationTime_, byteArray, offset, byteArraySize);
  unpackSwap(numberOfSamples_, byteArray, offset, byteArraySize);
  unpackSwap(avg_, byteArray, offset, byteArraySize);
  unpackSwap(var_, byteArray, offset, byteArraySize);
  unpackSwap(sd_, byteArray, offset, byteArraySize);

  complex< float > dummy( 0.0, 0.0 );

  unpackSwap(dummy, byteArray, offset, byteArraySize);  // discard old max_ field
  unpackSwap(dummy, byteArray, offset, byteArraySize);  // discard old min_ field
}

string
CorrelatorStats::getSummary( ) const
{
    ostringstream sum;

    sum << "Stats: " 
        << "int time=" << integrationTime_ << "ms, " 
        << "avg (amp, phase) (" << amp( avg_ ) << ", " << phase( avg_ ) << "), "
        << "var (" << amp( var_ ) << ", " << phase( var_ ) << "), "
        << "sd (" << amp( sd_ ) << ", " << phase( sd_ ) << ") "
        << "over " << numberOfSamples_ << " samples.";
    return sum.str();
}
