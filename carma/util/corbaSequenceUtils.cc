#include "carma/util/corbaSequenceUtils.h"

using namespace ::std;
using namespace carma;
using namespace carma::util;


namespace {


vector< double >
makeTestVector( ) {
    vector< double > result;
    
    result.push_back( 1.0 );
    result.push_back( 2.0 );
    result.push_back( 3.0 );

    return result;
}


void
compileTestIt( ) {
    const vector< double > vectorDoubleA = makeTestVector();
    
    SeqDouble seqDoubleA;

    assignVectorToSequence( vectorDoubleA, seqDoubleA );

    const SeqDouble SeqDoubleB =
        convertVectorToSequence< SeqDouble >( vectorDoubleA );
    
    vector< double > vectorDoubleB =
        convertSequenceToVector< double >( SeqDoubleB );
    
    assignSequenceToVector( seqDoubleA, vectorDoubleB );
}


}  // namespace < anonymous >


void carma::util::assignSequenceToVector( 
    const carma::util::SeqString & sequence,
    ::std::vector< std::string > & tVector )
{
    const CORBA::Long count = sequence.length();

    tVector.clear();
    tVector.reserve( count );

    for ( CORBA::Long i = 0; i < count; ++i )
        tVector.push_back( static_cast< const char * >( sequence[i] ) );
}

