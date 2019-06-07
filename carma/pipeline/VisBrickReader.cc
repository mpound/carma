// $Id: VisBrickReader.cc,v 1.1 2011/08/18 23:25:52 abeard Exp $

#include "carma/pipeline/VisBrickReader.h"

#include "carma/correlator/lib/CorrelatorData.h"
#include "carma/util/NotFoundException.h"
#include "carma/util/programLogging.h"
#include "carma/util/EOFException.h"
#include "carma/util/Trace.h"

#include <string.h> // for memcpy
#include <memory>

using namespace std;
using namespace carma::util;
using namespace carma::correlator::lib;
using namespace carma::pipeline;

namespace { 

    const Trace::TraceLevel TRACE_CTOR_DTOR = Trace::TRACE3;
    const Trace::TraceLevel TRACE_FILE_OPS = Trace::TRACE5;

    // Forward dec, defined in anon namespace below.
    void
    checkIstream( const ::std::istream & s,
                  const string &         filename,
                  const string &         messageSuffix );

} // namespace <unnamed>
    

CorrelatorVisBrickReader::CorrelatorVisBrickReader( 
        const string & filename,
        const bool continueOnFileErrors ) :
    inputFilename_( filename ),
    className_( "CorrelatorVisBrickReader" ),
    continueOnFileErrors_( continueOnFileErrors ),
    fileErrorDetected_( false )
{
    CARMA_CPTRACE( TRACE_CTOR_DTOR, "CorrelatorVisBrickReader( filename= "
        + filename + " ) - Constructor." );

    fp_.open( inputFilename_.c_str(), std::ios::in | std::ios::binary );

    checkIstream( fp_, inputFilename_, "Failed to open." );

    fp_.seekg( 0 );

    checkIstream( fp_, inputFilename_, "In contructor.");

    CARMA_CPTRACE( TRACE_FILE_OPS, "CorrelatorVisBrickReader() - "
        "Successfully opened " + inputFilename_ + "." );

    extractRecords( );

    recordIterator_ = records_.begin( );
}

CorrelatorVisBrickReader::~CorrelatorVisBrickReader( )
try {
    if ( fp_.is_open( ) ) {
        fp_.close();
    }

    // Delete CorrelatorData objects
    RecordsByFrameMap::iterator i = records_.begin( );
    const RecordsByFrameMap::const_iterator iEnd = records_.end( );
    for ( ; i != iEnd; ++i ) {
        if ( i->second != 0 ) {
            delete i->second;
        }
    }

} catch ( ... ) {
    // Just stifle any exception
    
    return;
}


namespace {

void
checkIstream( const ::std::istream & s,
              const string &         filename,
              const string &         messageSuffix )
{
    if ( s.good() )
        return;
        
    const ios_base::iostate state = s.rdstate();
    
    if ( (state & ios_base::badbit) == ios_base::badbit ) {
        ostringstream oss;
        
        oss << "Stream bad bit set for file \"" << filename
            << "\" " << messageSuffix;
            
        throw CARMA_EXCEPTION( NotFoundException, oss.str() );
    }
    
    if ( (state & ios_base::failbit) == ios_base::failbit ) {
        if ( (state & ios_base::eofbit) == ios_base::eofbit ) {
            ostringstream oss;

            oss << "Stream fail and EOF bits set for file \""
                << filename << "\" " << messageSuffix;

            throw CARMA_EXCEPTION( EOFException, oss.str() );
        } else {
            ostringstream oss;
            
            oss << "Stream fail bit set without EOF bit for file \""
                << filename << "\" " << messageSuffix;
                
            throw CARMA_EXCEPTION( NotFoundException, oss.str() );
        }
    }
}


}  // namespace < anonymous >


RecordsByFrameMap 
CorrelatorVisBrickReader::getRecordsKeyedByFrame( )
{
    return records_;
}

const CorrelatorData & 
CorrelatorVisBrickReader::readOne( ) 
{
    if ( recordIterator_ != records_.end( ) ) {
   
        CorrelatorData & cd = *( recordIterator_->second );

        ++recordIterator_;

        return cd;

    } else {

        throw CARMA_EXCEPTION( EOFException, "EOFException" );

    }
}

void
CorrelatorVisBrickReader::extractRecords( )
{
    CARMA_CPTRACE( TRACE_FILE_OPS, 
        className_ + " - Reading one correlator datum." );

    bool eof = false;

    vector< char > data;

    while ( !eof ) {
        try {
            int size;

            CARMA_CPTRACE( TRACE_FILE_OPS, 
                    className_ + " - Current read position: " << fp_.tellg() );

            checkIstream( fp_, inputFilename_, " before size read" );

            fp_ >> size;

            checkIstream( fp_, inputFilename_, " after size read" );

            if (size > 0) {

                CARMA_CPTRACE(TRACE_FILE_OPS, "size of record= " << size );

                data.resize( size );

                if ( !fp_.read( &( data.at(0) ), size ) ) {
                    checkIstream( fp_, inputFilename_, "After read." );
                }

            } else {
                ostringstream os;
                os << "Record size < 0. Most likely this is the end of the "
                    << "file: " << inputFilename_;
                throw CARMA_EXCEPTION( NotFoundException, os.str( ) );
            }
            
            auto_ptr< CorrelatorData > cd( new CorrelatorData );

            cd->deserial( data );

            CARMA_CPTRACE( TRACE_FILE_OPS, "Successfully read correlator data "
                    "record from visbrick." );

            const frameType frame = Time::computeClosestFrame( 
                    cd->getHeader( ).getMJD( ) );

            // Insert cd into map and check that it succeeded.
            if ( !( records_.insert( make_pair( frame , cd.get() ) ).second ) )
            {
                ostringstream msg;

                msg << "Insertion of CorrelatorData for frame " << frame 
                    << ", in " << inputFilename_ << " failed.";

                programLogErrorIfPossible( msg.str( ) );
            }

            cd.release( );

        } catch ( const EOFException & ) {
            eof = true;
        } catch ( const NotFoundException & nfe ) {
            
            eof = true;

            if ( continueOnFileErrors_ ) {
                ostringstream errorMsg;
                errorMsg << "File error detected while reading " 
                    << inputFilename_ << " but continuing at users request "
                    << "with " << records_.size() << " presumably valid "
                    << "CorrelatorData records. Error msg is: \"" << nfe 
                    << "\".";
                programLogErrorIfPossible( errorMsg.str() );
                fileErrorDetected_ = true;
            }
        } catch ( ... ) {
            throw;
        }
    }
}

bool
CorrelatorVisBrickReader::fileErrorDetected( ) const 
{
    return fileErrorDetected_;
}
