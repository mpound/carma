// $Id: VisBrickWriterStage.cc,v 1.4 2014/07/23 20:01:19 scott Exp $

#include "carma/pipeline/VisBrickWriterStage.h"

#include "carma/correlator/lib/CorrelatorData.h"
#include "carma/monitor/PipelineSubsystem.h"
#include "carma/util/AutoPthreadQuitAndJoinGroup.h"
#include "carma/util/ByteBuffer.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/FileUtils.h"
#include "carma/util/posixErrors.h"
#include "carma/util/programLogging.h"
#include "carma/util/ScopedPthreadMutexLock.h"
#include "carma/util/StartPthread.h"
#include "carma/util/ThreadQuit.h"
#include "carma/util/Time.h"
#include "carma/util/Trace.h"

#include <cstdio>
#include <fstream>
#include <iostream>

using namespace std;
using namespace boost;
using namespace carma::util;
using namespace carma::monitor;
using namespace carma::correlator::lib;
using namespace carma::pipeline;

namespace {

    const Trace::TraceLevel TRACE_VIS_RENAME = Trace::TRACE6;
    const Trace::TraceLevel TRACE_THREAD_TEARDOWN = Trace::TRACE6;
    const Trace::TraceLevel TRACE_CTOR_DTOR = Trace::TRACE6;
    const Trace::TraceLevel TRACE_WRITE = Trace::TRACE6;

    const string NO_FILENAME( "Not set - waiting for 1st integration" );
    const string suffix( ".write" ); 

    const int VISBRICK_FILESIZE_MB = 64;

    void
    renameOutputFileSansDotWrite( std::string filename )
    {
        const string::size_type suffixIdx = filename.find( suffix );

        if ( suffixIdx == string::npos ) {
            ostringstream msg;
            msg << "VisBrickWriter renameOutputFileSansDotWrite( ) -"
                << " Warning input filename does not have a " << suffix 
                << " suffix.";
            programLogWarnIfPossible( msg.str( ) );
            return;
        }

        // need to keep original since .erase changes current string.
        const string fromFilename = filename;
        const string toFilename = filename.erase( suffixIdx );

        int rtn = ::rename( fromFilename.c_str( ), toFilename.c_str( ) );

        if ( rtn != 0 ) {
            ostringstream msg;
            msg << "VisBrickWriter - error renaming "
                << fromFilename << " to " << toFilename << ": "
                << strerror( errno );
            programLogErrorIfPossible( msg.str( ) );
        }

        // Set permissions to read only
        rtn = ::chmod( toFilename.c_str(), 00444 );
        if ( rtn != 0 )
            logPosixError( errno );

    }

    void
    parseDirectoryAndFileNames( const std::string & dirAndFileName,
                                string & dirname,
                                string & filename )
    {
        const string::size_type idx = dirAndFileName.rfind( '/' );

        if ( idx == string::npos ) {
            dirname = '.';
            filename = dirAndFileName;
        } else {
            dirname = dirAndFileName.substr( 0, idx );
            filename = dirAndFileName.substr( idx + 1 );
        }

    }

    void
    renameOldVisBricks( const std::string & fileDirAndPrefix )
    {
        string dirname, filePrefix;
        parseDirectoryAndFileNames( fileDirAndPrefix, dirname, filePrefix );

        if ( dirname == "" ) {
            dirname = "."; // Force to cwd
        }

        if ( dirname.at( dirname.size( ) - 1 ) != '/' ) {
            dirname.push_back( '/' ); // Append '/' if one doesn't exist.
        }

        vector<string> files = FileUtils::scandir( dirname );

        const vector<string>::iterator fiBegin = files.begin( );
        const vector<string>::const_iterator fiEnd = files.end( );
        for ( vector<string>::iterator fi = fiBegin; fi != fiEnd; ++fi ) {
            
            string::size_type startat = fi->size( ) - suffix.size( );

            CARMA_CPTRACE( TRACE_VIS_RENAME, 
                           "renameOldVisBricks() - Candidate '" << *fi
                           << "' size=" << fi->size( ) << ", with suffix="
                           << suffix.size( ) << ", starting at " 
                           << startat << "." );

            if ( fi->size( ) <= suffix.size( ) ) {
                CARMA_CPTRACE( TRACE_VIS_RENAME, "Candidate failed." );
                continue;
            }

            if ( fi->find( suffix, startat ) != string::npos ) {
                CARMA_CPTRACE( TRACE_VIS_RENAME, "Candidate passed." );
                if ( fi->substr( 0, filePrefix.size() ) == filePrefix ) {
                    renameOutputFileSansDotWrite( dirname + (*fi) );
                }
            }

        }
    }

} // namespace <unnamed>

VisBrickWriter::VisbrickWriteTQRH::VisbrickWriteTQRH(
    VisBrickWriter & dad ) : dad_( dad )
{
    // Nothing
}

VisBrickWriter::VisbrickWriteTQRH::~VisbrickWriteTQRH( )
{ 
    // Nothing
}

void
VisBrickWriter::VisbrickWriteTQRH::HandleQuitRequest( 
    pthread_t thread )
{
    // We pop a dummy request onto the queue to kill this guy
    VisBrickWriter::WriteRequest dummyRqst;
    dummyRqst.filename = "/home/abeard/BIG-ERROR-DUMMY";
    dummyRqst.createNewFile = false;
    dummyRqst.data = CorrelatorDataPtr();

    dad_.writeRequestQueue_.push( dummyRqst );
}

VisBrickWriter::Shared::Shared( ) :
    mutex( ),
    fileWriteTimeAcc( ),
    lastFileWriteTimeMs( 0.0 ),
    lastSerializationTimeMs( 0.0 ),
    fileError( false ),
    isScienceData(true)
{ 
    // Nothing to see here
}

VisBrickWriter::VisBrickWriter(PipelineSubsystem& monitor) : 
    Stage( monitor.getVisBrickStageStats( ), "VisBrickWriter" ),
    shared_( ),
    writeRequestQueue_( ),
    currentFilename_( NO_FILENAME ),
    mbWrittenOrPending_( 0.0 ),
    maxFilesize_( VISBRICK_FILESIZE_MB ),
    outputFilenameBase_( "visbrick" ),
    monitorData_( monitor )
{
    renameOldVisBricks( "" );

    visbrickWriteThreadId_ = 
        StartPthreadWithRef( visbrickWriteThread,
                             *this,
                             "VisBrickWriter::visBrickWriteThread" );

}

VisBrickWriter::VisBrickWriter(
        const string & filename,
        PipelineSubsystem & monitor ) :
    Stage( monitor.getVisBrickStageStats( ), "VisBrickWriter" ),
    shared_( ),
    writeRequestQueue_( ),
    currentFilename_( NO_FILENAME ),
    mbWrittenOrPending_( 0.0 ),
    maxFilesize_( VISBRICK_FILESIZE_MB ),
    outputFilenameBase_( filename ),
    monitorData_( monitor )
{
    renameOldVisBricks( filename );
    
    visbrickWriteThreadId_ = 
        StartPthreadWithRef( visbrickWriteThread,
                             *this,
                             "VisBrickWriter::visBrickWriteThread" );
}

VisBrickWriter::~VisBrickWriter() 
{
    CARMA_CPTRACE( TRACE_CTOR_DTOR, "VisBrickWriter - D'tor." );
    try {
        RequestThreadQuit( visbrickWriteThreadId_ );
        AutoPthreadQuitAndJoinGroup quitter;
        quitter.insert( visbrickWriteThreadId_, 
                        AutoPthreadQuitAndJoinGroup::JOIN_ONLY_ACTION );
    } catch ( ... ) {
        // Stifle
    }
}

void VisBrickWriter::setIsScienceData(bool isScienceData) {
        shared_.isScienceData = isScienceData;
}

void
VisBrickWriter::preprocess( CorrelatorDataPtr cd )
{
    return;
}

void
VisBrickWriter::processBand( CorrelatorBand * cb )
{
    return;
}

CorrelatorDataPtr
VisBrickWriter::postprocess( CorrelatorDataPtr cd )
{
    // First check to see if we need to write a new file...

    // Written with all records is an int with the size - hence the sizeof(int)
    const float mbThisWrite = (sizeof(int) + cd->getTotalSerialBytes()) * 1.e-6; 
    bool createNewFile;
    if ( mbWrittenOrPending_ > maxFilesize_ || currentFilename_ == NO_FILENAME )
    {
        const frameType fc = getDataFrameCount();
        ostringstream os;
        os << outputFilenameBase_ << "_" << fc << ".write";
        currentFilename_ = os.str();

        mbWrittenOrPending_ = mbThisWrite;
        createNewFile = true; 
    } else {
        mbWrittenOrPending_ += mbThisWrite;
        createNewFile = false;
    }
    
    if (shared_.isScienceData ) {       
        WriteRequest request;
        request.filename = currentFilename_; 
        request.createNewFile = createNewFile;
        request.data = cd;
    
        writeRequestQueue_.push( request );
    }

    return CorrelatorDataPtr(); // Safety pipeline termination.
}

void 
VisBrickWriter::fillMonitorData( ) 
{
    VisBrickStage & visBrickMon = monitorData_.getVisBrickStage( );

    visBrickMon.outFileName().setValue( currentFilename_ );
    visBrickMon.bytesWritten().setValue( mbWrittenOrPending_ );
    visBrickMon.maxFilesize().setValue( maxFilesize_ );

    ScopedPthreadMutexLock scopelock( shared_.mutex );

    visBrickMon.fileError( ).setValue( shared_.fileError );
    visBrickMon.fileWriteTime().setValue( shared_.lastFileWriteTimeMs );
    visBrickMon.serializationTime().setValue( shared_.lastSerializationTimeMs );
    visBrickMon.scienceData().setValue(shared_.isScienceData);

    const std::size_t count = accumulators::count( shared_.fileWriteTimeAcc );
    
    visBrickMon.fileWriteCount().setValue( count );
    if ( count > 0 ) {
        visBrickMon.fileWriteTimeMin().setValue( 
                accumulators::min( shared_.fileWriteTimeAcc ) );
        visBrickMon.fileWriteTimeMax().setValue( 
                accumulators::max( shared_.fileWriteTimeAcc ) );
        visBrickMon.fileWriteTimeMean().setValue( 
                accumulators::mean( shared_.fileWriteTimeAcc ) );
        visBrickMon.fileWriteTimeStdDev().setValue( 
                ::sqrt( ::fabs( accumulators::variance( 
                    shared_.fileWriteTimeAcc ) ) ) );
    } 
    else {
        visBrickMon.fileWriteTimeMin().setValidity( 
            MonitorPoint::INVALID_NO_DATA );
        visBrickMon.fileWriteTimeMax().setValidity( 
            MonitorPoint::INVALID_NO_DATA );
        visBrickMon.fileWriteTimeMean().setValidity( 
            MonitorPoint::INVALID_NO_DATA );
        visBrickMon.fileWriteTimeStdDev().setValidity( 
            MonitorPoint::INVALID_NO_DATA ); 
    }
}

bool
VisBrickWriter::checkFileStream(
    const ofstream & fp,
    const string & filename,
    const string & attemptedOpString )
{
    if ( fp.fail( ) ) {
        ostringstream msg;
        msg << "Stream state is 'fail' after attempting to "
            << attemptedOpString << " " << filename 
            << ".  Check that directory exists and permissions are correct.";
        programLogErrorIfPossible( msg.str( ) );

        ScopedPthreadMutexLock scopelock( shared_.mutex );
        shared_.fileError = true;
        return true;
    }
    return false;
}
    

void 
VisBrickWriter::visbrickWriteThread( VisBrickWriter & This )
try {
    VisbrickWriteTQRH tqrh( This );
    ScopedThreadQuitRequestHandlerSelf scopeQuit( tqrh );

    bool fileError = false;
    string lastFilenameWrittenTo;
    ofstream fp;
    carma::util::ByteBuffer dataByteBuffer;

    while ( true ) { // Sit on the request queue until something arrives.

        WriteRequest request;
        This.writeRequestQueue_.wait_and_pop( request );

        ThreadQuitTestSelf();

        if ( fileError ) continue; // We leave it up to humans to fix filesystem

        if ( request.createNewFile && lastFilenameWrittenTo != "" )
            renameOutputFileSansDotWrite( lastFilenameWrittenTo ); 
    
        const double serializeStart = Time::MJD();
        request.data->serialIntoByteBuffer( dataByteBuffer );
        const double serializeEnd = Time::MJD();
        const double serializeMs = 
            ( serializeEnd - serializeStart ) * Time::MILLISECONDS_PER_DAY;

        const int size = dataByteBuffer.size();

        const double writeStart = Time::MJD(); 
        fp.clear( ); // Set good 'bit'
        fp.open( request.filename.c_str(), ios::out | ios::app | ios::binary );

        if ( This.checkFileStream( fp, request.filename, "open" ) ) {
            fileError = true;
            continue;
        }

        fp << size; // write out size of record first

        fp.write( dataByteBuffer.get(), size );

        fp.flush( ); // Attempt to force a write to disk

        if ( This.checkFileStream( fp, request.filename, "write" ) ) {
            fileError = true;
        }

        if ( fp.is_open( ) ) { // Close current file

            fp.clear( ); // Set good 'bit'

            fp.close( );

            if ( This.checkFileStream( fp, request.filename, "close" ) ) {
                fileError = true;
            }
        }
        const double writeEnd = Time::MJD();
        const double writeMs = 
            ( writeEnd - writeStart ) * Time::MILLISECONDS_PER_DAY;

        lastFilenameWrittenTo = request.filename;

        // Finally write out stats and stuff that will go into the monitor sys
        {
            ScopedPthreadMutexLock scopelock( This.shared_.mutex );
            This.shared_.lastSerializationTimeMs = serializeMs;
            This.shared_.lastFileWriteTimeMs = writeMs;
            This.shared_.fileWriteTimeAcc( writeMs ); 
        }
        
        ThreadQuitTestSelf();
    }
} catch (...) {
    if ( CaughtExceptionIsThreadQuitRequestedError( ) ) {
        CARMA_CPTRACE( TRACE_THREAD_TEARDOWN,
                "VisBrickWriter::visbrickWriteThread - "
                "Exiting cleanly via a thread quit request." );
        throw;
    } else {
        logCaughtAsError( );
    }
}
