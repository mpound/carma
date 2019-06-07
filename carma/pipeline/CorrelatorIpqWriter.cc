#include "carma/pipeline/CorrelatorIpqWriter.h"

#include "carma/correlator/lib/CorrelatorData.h"
#include "carma/util/ErrorException.h"
#include "carma/util/Trace.h"

#include <iostream>

#include <sys/time.h>
#define GTOD(argptr) gettimeofday(argptr, NULL)

using namespace carma::correlator::lib;
using namespace carma::pipeline;
using namespace carma::util;
using namespace ::std;

namespace {

const Trace::TraceLevel TRACE_CTOR_DTOR( Trace::TRACE5 );
const Trace::TraceLevel TRACE_DETAILS( Trace::TRACE7 );

double
myDiffTimeMs( const struct timeval & start,
              const struct timeval & end )
{
    long diffSec = end.tv_sec - start.tv_sec;
    double diffMsec = (end.tv_usec - start.tv_usec) / 1000.;
    
    return diffSec * 1000. + diffMsec;
}


} // namespace < anonymous >


CorrelatorIpqWriter::CorrelatorIpqWriter(
    const string &     filename,
    const unsigned int ipqMaxsize,
    const unsigned int ipqNumberOfElements ) :
outputFilename_( filename ),
ipqMaxsize_( ipqMaxsize ),
ipqNumberOfElements_( ipqNumberOfElements ),
byteArray_( ipqMaxsize ),
ipq_( &byteArray_[0], // Warning construction order dependency on byteArray_
      ipqMaxsize,
      filename,
      true,
      ipqNumberOfElements )
{
  CARMA_CPTRACE( TRACE_CTOR_DTOR, "In Constructor:" << endl
         << " outputFilename_= " << outputFilename_ << endl
         << " ipqMaxsize_= " << ipqMaxsize_ << endl
         << " ipqNumberOfElements_= " << ipqNumberOfElements_ << endl );
}

CorrelatorIpqWriter::~CorrelatorIpqWriter( )
{
  CARMA_CPTRACE( TRACE_CTOR_DTOR, "CorrelatorIpqWriter() - Dtor" );
}

void
CorrelatorIpqWriter::processData( CorrelatorData * const cd )
{
  const CorrelatorData::ScopedRef refCountInc( *cd );

  // check to see if data should be processed
    struct timeval startTv, endTv;

    CARMA_CPTRACE( TRACE_DETAILS, "Entering..." );
    
    const int numberOfBands = cd->getNumberOfBands();
    CARMA_CPTRACE( TRACE_DETAILS, "numberOfBands= " << numberOfBands );
    
    GTOD(&startTv);

    // Serialize straight into the byteArray_
    cd->serialIntoByteVec( byteArray_ );

    // Check for vector reallocation
    const vector< char >::size_type byteArraySize( byteArray_.size() );
    if ( byteArraySize > ipqMaxsize_ ) {
        ostringstream oss;
        oss << "CorrelatorIpqWriter::processData() - Serialized correlator "
            << "data size of " << byteArraySize << " bytes exceeds "
            << "ipq maximum element size of " << ipqMaxsize_ << "!";
        throw CARMA_ERROR( oss.str() );
    } 

    GTOD(&endTv);
    CARMA_CPTRACE( TRACE_DETAILS, "time to serialize= " 
                   << myDiffTimeMs( startTv, endTv ) << " msec" );

    CARMA_CPTRACE( TRACE_DETAILS, "finished serilaization: size= "
                   << byteArraySize );

    GTOD(&startTv);

    try {
      ipq_.write();
    } catch ( const exception & err ) {
      CARMA_CPTRACE( TRACE_DETAILS, "Error write ipq. " << err.what() );
    }

    GTOD(&endTv);
    CARMA_CPTRACE( TRACE_DETAILS, "time to write IPQ= "
                   << myDiffTimeMs( startTv, endTv ) << " msec" );
}

const string &
CorrelatorIpqWriter::getName( ) const
{
    static const string className( "CorrelatorIpqWriter" );
    return className;
}
