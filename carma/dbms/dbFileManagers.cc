/**
 * Definition of DB FileManager classes.
 *
 * $Id: dbFileManagers.cc,v 1.5 2012/05/01 21:53:56 abeard Exp $
 */

#include "carma/dbms/dbFileManagers.h"

#include "carma/dbms/DBConfigurator.h"
#include "carma/dbms/dbFFIO.h"
#include "carma/dbms/TagIDAuthority.h"
#include "carma/monitor/AverageAccumulator.h"
#include "carma/util/ErrorException.h"
#include "carma/util/FileUtils.h"
#include "carma/util/Logger.h"
#include "carma/util/Program.h"
#include "carma/util/Trace.h"

#include <cerrno>
#include <cstring>
#include <vector>

// If this is defined, create binary output files. If not, generate
// traditional ASCII output.
#define BINNUMERIC

using namespace carma;
using namespace carma::dbms;
using namespace carma::monitor;
using namespace carma::util;
using namespace std;

namespace {
  
const string WRITESUFFIX(".write");

struct DbFileAndDirectoryInfo {

    DbFileAndDirectoryInfo( dbms::MonitorDataAreaType dataAreaType, 
                            dbms::MonitorAverageType avgType,
                            const dbms::DBConfigurator & dbConf ) :
        shortDir( dbConf.getDataDirectory( dataAreaType, avgType, 
                                           dbms::SHORT_TYPE ) ),
        numericDir( dbConf.getDataDirectory( dataAreaType, avgType,
                                             dbms::NUMERIC_TYPE ) ),
        stringDir( dbConf.getDataDirectory( dataAreaType, avgType,
                                            dbms::STRING_TYPE ) ),
        complexDir( dbConf.getDataDirectory( dataAreaType, avgType,
                                             dbms::COMPLEX_TYPE ) )
    {
        // Nothing
    };

    ~DbFileAndDirectoryInfo( ) 
    { 
        // Nothing 
    };

    const string shortDir;
    const string numericDir;
    const string stringDir;
    const string complexDir;

    string shortFilename;
    string numericFilename;
    string stringFilename;
    string complexFilename;
};

struct DbFFIOFileInfo {

#if defined(BINNUMERIC)
    dbFFIOb shortFile;
    dbFFIOb numericFile;
    dbFFIOb stringFile;
    dbFFIOb complexFile;
#else
    dbFFIOa shortFile;
    dbFFIOa numericFile;
    dbFFIOa stringFile;
    dbFFIOa complexFile;
#endif
    
};
    
struct DbFileInfo {
    FILE * shortFile;
    FILE * numericFile;
    FILE * stringFile;
    FILE * complexFile;

    DbFileInfo( ) : 
        shortFile( 0 ), 
        numericFile( 0 ),
        stringFile( 0 ),
        complexFile( 0 ) { };

};

/**
 * print info about symlink failures
 * FIXME should be in a util class probably
 */
void
LogSymlinkError( const int err, const char * path1, const char * path2 ) {
    CARMA_CPTRACE( util::Trace::TRACE1, "symlink error");

    util::Program::getLogger() << ::log4cpp::Priority::ERROR
                               << "symlink error";

    ostringstream oss;

    switch ( err ) {
        case EACCES:
            oss << "Write access not permitted";
            break;

        case EEXIST:
            oss << "newpath already exists.";
            break;

        case EIO:
            oss << "An I/O error occurred.";
            break;

        case ELOOP:
            oss << "Too many symbolic links were encountered in resolving "
                << "newpath.";
            break;

        case ENAMETOOLONG:
            oss << "Old path or new path is too long";
            break;

        case ENOENT:
            oss << "A directory component in newpath does not exist or is a "
                << "dangling symbolic link, or oldpath is the empty string.";
            break;

        case ENOSPC:
            oss << "The device containing the file has no room for the new "
                << "directory entry.";
            break;

        case ENOTDIR:
            oss << "A component used as a directory in newpath is not, in "
                << "fact, a directory.";
            break;

        case EROFS:
            oss << "newpath is on a read-only filesystem.";
            break;

        case EPERM:
            oss << "Filesystem does not support symlinks";
            break;

        case EFAULT:
            oss << "oldpath or newpath points outside accessible address space";
            break;

        case ENOMEM:
            oss << "Insufficient kernel memory was available.";
            break;

        default:
            oss << "Unkown error code " << err;
            break;
    }

    const string emsg = oss.str( );

    CARMA_CPTRACE( util::Trace::TRACE1, emsg);

    util::Program::getLogger() << ::log4cpp::Priority::ERROR << emsg;
}

// Routine to open files the old way (pre dbFFIO).
/** Open 1 new file.
 * prefix - "/short_", "/numeric_", etc.
 * name - <framecount0><"-"><framecount1><".mpdat">.
 * dir    - Dir containing file.
 * filename- Set to filename string (without WRITESUFFIX).
 * fileptr - Set to the file pointer of the opened file.
 * frameCount - frameCount used for file header.
 * sig     - Signature used for file header.
 */
void Open1NewFile(const string &prefix, const string &name,
                  const string &dir, string & filename, FILE * &fileptr,
                  carma::util::frameType frameCount, const string &sig)
{ 
  int openCount = 0;
  filename = dir + prefix + name;
  string wfilename = filename + WRITESUFFIX;
  fileptr = 0;
        do        // Try up to 10 times to open file.
        {        fileptr = fopen( wfilename.c_str( ), "w" );
                if(fileptr)
                        break;
                sleep (1);
        } while(++openCount < 10);

        // Did it work?
        if ( !fileptr ) {
          string errmsg = strerror(errno);
          throw CARMA_ERROR("Unable to open file " + filename
                            + " for writing: " + errmsg);
        } else // Did it multiple attempts?
        if(openCount)
        {  carma::util::Program::getLogger() 
                << log4cpp::Priority::INFO
                << filename << " took " << openCount
                << " tries to open\n";
        }

        chmod(wfilename.c_str(), S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH);
        // Write the header for the file.
        FileUtils::writeMonitorDataFlatFileHeader(fileptr,frameCount, sig);
}

struct NameFileDbLoadDirInfo {
    string name_;
    FILE * file_;
    string dbLoadDir_;

    NameFileDbLoadDirInfo( const string & name,
                           FILE *         file,
                           const string & dbLoadDir ) :
      name_( name ),
      file_( file ),
      dbLoadDir_( dbLoadDir ) {  }
};

struct NameFileDbLoadDirInfo2 {
    string name_;
    dbFFIO * file_;
    string dbLoadDir_;

    NameFileDbLoadDirInfo2( const string & name,
                           dbFFIO *file,
                           const string & dbLoadDir ) :
      name_( name ),
      file_( file ),
      dbLoadDir_( dbLoadDir ) {  }
};

// Compare the contents of a link with a supplied value. Returns 0
// if they are the same, 1 if they are not or -1 if there was an error.
// (Check errno).
  static int checkLink(const string &link, const string &value)
  { char buf[4096];
    int cnt = readlink(link.c_str(), buf, sizeof(buf)-1);
    if( cnt < 0)
      return cnt;
    buf[cnt] = '\0';
    if(strcmp(buf, value.c_str()) == 0)
      return 0;
    else
      return 1;
  }

// Called when a set of files has been written. This closes them and
// creates the needed links. This version uses FILE pointers.
void
FinishedWithFiles( const vector< NameFileDbLoadDirInfo > & infos ) 
{
    auto_ptr< struct stat > statbuf( new struct stat );

    vector< NameFileDbLoadDirInfo >::const_iterator i = infos.begin( );
    const vector< NameFileDbLoadDirInfo >::const_iterator iEnd = infos.end( );


    while ( i != iEnd ) {
        // If FILE ptr is 0, assume this is transfer or sdp and the file has
        // already been closed.
        bool closed = (i->file_ == 0);
        // bool closed = (fileno(i->file_) < 0);
        if(!closed)
        { int err = fclose(i->file_);
            if(err != 0)
            {        throw CARMA_ERROR( "Unable to close file " + i->name_ );
            }
            else        // Rename file.
            { string writeFileName = i->name_ + WRITESUFFIX;
                int err = rename(writeFileName.c_str(), i->name_.c_str());
                if(err != 0)
                { throw CARMA_ERROR( "Unable to rename write file to "
                        + i->name_ );
                }
            }
        }
        int errnum = stat( i->name_.c_str( ), statbuf.get( ) );

        // If there's no file, there's nothing to link to.  (This happens
        // when a zero length file has been removed and we're making links
        // from the sdp or transfer directories).
        if( (errnum < 0) && (errno == ENOENT))
        {
            CARMA_CPTRACE( util::Trace::TRACE7,
                    "Ignoring non-existent file: " << i->name_);
        } else if ( statbuf->st_size
                <= (off_t)FileUtils::getMonitorDataFlatFileHeaderLength
                (carma::dbms::TagIDAuthority::getAuthority()
                 .tagIDNameMapSha1Sum().length())) {
            CARMA_CPTRACE( util::Trace::TRACE7,
                    "Removing file which contains no "
                    << "monitor data " << i->name_);

            if ( remove( i->name_.c_str( ) ) != 0 ) {
                ostringstream msg;

                msg  << "error deleting 0 size file " << i->name_;

                CARMA_CPTRACE( util::Trace::TRACE1, msg.str( ) );

                throw CARMA_ERROR( msg.str( ) );
            }

            CARMA_CPTRACE( util::Trace::TRACE7, i->name_ << " removed" );
        } else {
            string filename = i->name_;
            // Name of symlink to create.
            const string linkname =
                i->dbLoadDir_ + "/" + basename( filename.c_str() );

            CARMA_CPTRACE( util::Trace::TRACE7, "creating symlink "
                    << linkname << "->" << i->name_);
            if(carma::util::FileUtils::exists(linkname))
            {ostringstream msg;
                msg << linkname <<
                    " existed before symlink attempt.";
                CARMA_CPTRACE( util::Trace::TRACE4, linkname << msg.str());

                util::Program::getLogger()
                    << ::log4cpp::Priority::NOTICE << msg;
            }

            if ( symlink( filename.c_str(), linkname.c_str( ) ) != 0 ) {
                if((errno == EEXIST) && (checkLink(linkname, i->name_)==0))
                {ostringstream msg;
                    msg << linkname <<
                        " already exists but is correct.";
                    CARMA_CPTRACE( util::Trace::TRACE4, linkname << msg.str());
                    util::Program::getLogger()
                        << ::log4cpp::Priority::NOTICE << msg;
                }
                else
                {  LogSymlinkError( errno, (i->name_).c_str( ), linkname.c_str( ) );

                    throw CARMA_ERROR("Unable to create symlink "
                            + linkname + " -> " + i->name_);
                }
            }

            CARMA_CPTRACE( util::Trace::TRACE7, "symlink creation "
                    << "successful");
        }

        ++i;
    }
}

  ///////////////////////////////////////////////---------------

  // Called when a set of files has been written. This closes them and
  // creates the needed links. This version uses the dbFFIO class.
void
FinishedWithFiles( const vector< NameFileDbLoadDirInfo2 > & infos ) 
{
    auto_ptr< struct stat > statbuf( new struct stat );

    vector< NameFileDbLoadDirInfo2 >::const_iterator i = infos.begin( );
    const vector< NameFileDbLoadDirInfo2 >::const_iterator iEnd = infos.end( );
    while ( i != iEnd ) {
        // If file is closed, assume this is transfer or sdp.
        bool closed = !i->file_->isOpen();
        if ( !closed && (! i->file_->close()))
            throw CARMA_ERROR( "Unable to close file " + i->name_ );

        int errnum = stat( i->name_.c_str( ), statbuf.get( ) );

        // If there's no file, there's nothing to link to.
        // (This happens when a zero length file has been removed and
        // we're making links from the sdp or transfer directories).
        if( (errnum < 0) && (errno == ENOENT))
        {
            CARMA_CPTRACE( util::Trace::TRACE7, "Ignoring non-existent file: "
                    << i->name_);
        }
        else
            if ( statbuf->st_size
                    <= (off_t)FileUtils::getMonitorDataFlatFileHeaderLength
                    (carma::dbms::TagIDAuthority::getAuthority()
                     .tagIDNameMapSha1Sum().length())) {
                CARMA_CPTRACE( util::Trace::TRACE7, "Removing file which contains no "
                        << "monitor data " << i->name_);

                if ( remove( i->name_.c_str( ) ) != 0 ) {
                    ostringstream msg;

                    msg  << "error deleting 0 size file " << i->name_;

                    CARMA_CPTRACE( util::Trace::TRACE1, msg.str( ) );

                    throw CARMA_ERROR( msg.str( ) );
                }

                CARMA_CPTRACE( util::Trace::TRACE7, i->name_ << " removed" );
            } else {
                const string linkname =
                    i->dbLoadDir_ + "/" + basename( i->name_.c_str( ) );

                CARMA_CPTRACE( util::Trace::TRACE7, "creating symlink "
                        << linkname << "->" << i->name_);
                if(carma::util::FileUtils::exists(linkname))
                {ostringstream msg;
                    msg << linkname <<
                        " existed before symlink attempt.";
                    CARMA_CPTRACE( util::Trace::TRACE4, linkname << msg.str());

                    util::Program::getLogger() << ::log4cpp::Priority::NOTICE << msg;
                }


                if ( symlink( i->name_.c_str( ), linkname.c_str( ) ) != 0 ) {
                    if((errno == EEXIST) && (checkLink(linkname, i->name_)==0))
                    {ostringstream msg;
                        msg << linkname <<
                            " already exists but is correct.";
                        CARMA_CPTRACE( util::Trace::TRACE4, linkname << msg.str());
                        util::Program::getLogger()
                            << ::log4cpp::Priority::NOTICE << msg;
                    }
                    else
                    {  LogSymlinkError( errno, (i->name_).c_str( ), linkname.c_str( ) );

                        throw CARMA_ERROR("Unable to create symlink "
                                + linkname + " -> " + i->name_);
                    }
                }

                CARMA_CPTRACE( util::Trace::TRACE7, "symlink creation "
                        << "successful");
            }

        ++i;
    }
}

} // namespace < unnamed >

struct DbFileManager::Impl {

    const dbms::MonitorAverageType avgType;
    DbFileAndDirectoryInfo sourceDirs;
    DbFileInfo sourceFiles;
    DbFileAndDirectoryInfo loadDirs;
    vector< DbFileAndDirectoryInfo * > destinationDirs;
    
    Impl( const dbms::MonitorAverageType averageType,
          const dbms::DBConfigurator & dbConf ) :
        avgType( averageType ),
        sourceDirs( dbms::MP_WRITE_AREA, averageType, dbConf ), 
        sourceFiles( ), 
        loadDirs( dbms::MP_LOAD_AREA, averageType, dbConf ),
        destinationDirs( ) 
    {
        // Nothing
    };

};

DbFileManager::DbFileManager( const dbms::MonitorAverageType avgType,
                              const bool dotransfer,
                              const bool dosdp,
                              const dbms::DBConfigurator & dbConf ) :
    impl_( new DbFileManager::Impl( avgType, dbConf ) )
{ 
    if ( dotransfer )
        addMonitorDataArea( dbms::MP_TRANSFER_AREA, dbConf );

    if ( dosdp )
        addMonitorDataArea( dbms::MP_SDP_AREA, dbConf );
}

DbFileManager::~DbFileManager( ) 
{ 
    vector< DbFileAndDirectoryInfo * >::iterator i;
    const vector< DbFileAndDirectoryInfo * >::iterator iEnd =
        impl_->destinationDirs.end( );
    for ( i = impl_->destinationDirs.begin( ); i != iEnd; ++i ) {
        delete ( *i );
    }
}
    
void 
DbFileManager::writeLongAveragesToFile( AverageAccumulator & accumulator,
                                        const long frameCount )
{
    // The only difference between inst and long averages is 
    // that min, max, maxSamples, and nSamples are also 
    // written out for each monitor point
    accumulator.writeLongAveragesToFile( 
            frameCount,
            impl_->sourceFiles.shortFile, impl_->sourceFiles.numericFile,
            impl_->sourceFiles.stringFile, impl_->sourceFiles.complexFile );
}
                                            
void 
DbFileManager::DoneWithFiles( const bool dodbload ) 
{
    vector< NameFileDbLoadDirInfo > infos;

    if ( dodbload ) {
        infos.push_back( 
                NameFileDbLoadDirInfo( 
                    impl_->sourceDirs.shortFilename, 
                    impl_->sourceFiles.shortFile, 
                    impl_->loadDirs.shortDir ) );
        infos.push_back( 
                NameFileDbLoadDirInfo( 
                    impl_->sourceDirs.numericFilename, 
                    impl_->sourceFiles.numericFile, 
                    impl_->loadDirs.numericDir ) );
        infos.push_back( 
                NameFileDbLoadDirInfo( 
                    impl_->sourceDirs.stringFilename, 
                    impl_->sourceFiles.stringFile, 
                    impl_->loadDirs.stringDir ) );
        infos.push_back( 
                NameFileDbLoadDirInfo( 
                    impl_->sourceDirs.complexFilename, 
                    impl_->sourceFiles.complexFile, 
                    impl_->loadDirs.complexDir ) );
    }

    vector< DbFileAndDirectoryInfo * >::iterator i;
    const vector< DbFileAndDirectoryInfo * >::iterator iEnd = 
        impl_->destinationDirs.end();
    for ( i = impl_->destinationDirs.begin( ); i != iEnd; ++i ) {

        infos.push_back( 
                NameFileDbLoadDirInfo( 
                    impl_->sourceDirs.shortFilename, 0,
                    (*i)->shortDir ) );
        infos.push_back( 
                NameFileDbLoadDirInfo( 
                    impl_->sourceDirs.numericFilename, 0,
                    (*i)->numericDir ) );
        infos.push_back( 
                NameFileDbLoadDirInfo( 
                    impl_->sourceDirs.stringFilename, 0,
                    (*i)->stringDir ) );
        infos.push_back( 
                NameFileDbLoadDirInfo( 
                    impl_->sourceDirs.complexFilename, 0,
                    (*i)->complexDir ) );

    }

    FinishedWithFiles( infos );
}

void
DbFileManager::OpenNewFiles( const string & endName, 
                             const carma::util::frameType frameCount)
{
    // Opens files using stdio routines. (The original method).
    string sig = carma::dbms::TagIDAuthority::getAuthority()
        .tagIDNameMapSha1Sum();

    Open1NewFile("/short_", endName, 
            impl_->sourceDirs.shortDir, impl_->sourceDirs.shortFilename, 
            impl_->sourceFiles.shortFile, frameCount, sig);

    Open1NewFile("/numeric_", endName,
            impl_->sourceDirs.numericDir, impl_->sourceDirs.numericFilename, 
            impl_->sourceFiles.numericFile, frameCount, sig);

    Open1NewFile("/string_", endName,
            impl_->sourceDirs.stringDir, impl_->sourceDirs.stringFilename, 
            impl_->sourceFiles.stringFile, frameCount, sig);

    Open1NewFile("/complex_", endName,
            impl_->sourceDirs.complexDir, impl_->sourceDirs.complexFilename, 
            impl_->sourceFiles.complexFile, frameCount, sig);
}

void 
DbFileManager::addMonitorDataArea( dbms::MonitorDataAreaType monitorDataArea,
                                   const dbms::DBConfigurator & dbConf ) 
{
    ::std::auto_ptr< DbFileAndDirectoryInfo > dbFileAndDirInfo(
            new DbFileAndDirectoryInfo( monitorDataArea, 
                impl_->avgType, 
                dbConf ) );
    impl_->destinationDirs.push_back( dbFileAndDirInfo.release( ) );
}

struct DbFFIOFileManager::Impl {

    const dbms::MonitorAverageType avgType;
    DbFileAndDirectoryInfo sourceDirs;
    DbFFIOFileInfo sourceFiles;
    DbFileAndDirectoryInfo loadDirs;

    Impl( const dbms::MonitorAverageType averageType,
          const dbms::DBConfigurator & dbConf ) :
       avgType( averageType ),
       sourceDirs( dbms::MP_WRITE_AREA, averageType, dbConf ),
       sourceFiles( ),
       loadDirs( dbms::MP_LOAD_AREA, averageType, dbConf ) 
    {
        // Nothing
    };

};
    
DbFFIOFileManager::DbFFIOFileManager( const dbms::MonitorAverageType avgType,
                                      const dbms::DBConfigurator & dbConf ) :
    impl_( new DbFFIOFileManager::Impl( avgType, dbConf ) )
{
    // Nothing
}

DbFFIOFileManager::~DbFFIOFileManager( ) 
{
    // Nothing
}

void 
DbFFIOFileManager::writeInstAveragesToFile( AverageAccumulator & accumulator,
                                            const long frameCount )
{
    accumulator.writeInstAveragesToFile( 
            frameCount, 
            impl_->sourceFiles.shortFile, impl_->sourceFiles.numericFile,
            impl_->sourceFiles.stringFile, impl_->sourceFiles.complexFile );
}

void 
DbFFIOFileManager::DoneWithFiles( const bool dodbload ) 
{
    vector< NameFileDbLoadDirInfo2 > infos2;

    if ( dodbload ) {
        infos2.push_back( 
                NameFileDbLoadDirInfo2( 
                    impl_->sourceDirs.shortFilename, 
                    &impl_->sourceFiles.shortFile, 
                    impl_->loadDirs.shortDir ) );
        infos2.push_back( 
                NameFileDbLoadDirInfo2( 
                    impl_->sourceDirs.numericFilename, 
                    &impl_->sourceFiles.numericFile, 
                    impl_->loadDirs.numericDir ) );
        infos2.push_back( 
                NameFileDbLoadDirInfo2( 
                    impl_->sourceDirs.stringFilename, 
                    &impl_->sourceFiles.stringFile, 
                    impl_->loadDirs.stringDir ) );
        infos2.push_back( 
                NameFileDbLoadDirInfo2( 
                    impl_->sourceDirs.complexFilename, 
                    &impl_->sourceFiles.complexFile, 
                    impl_->loadDirs.complexDir ) );
    }

    FinishedWithFiles( infos2 );
}

void 
DbFFIOFileManager::OpenNewFiles ( const string & endName,
                                  const carma::util::frameType frameCount )
{
    // Open files using dbFFIO classes.
    string sig = carma::dbms::TagIDAuthority::getAuthority()
          .tagIDNameMapSha1Sum();

    impl_->sourceDirs.shortFilename = 
        impl_->sourceDirs.shortDir + "/short_" + endName;
    impl_->sourceDirs.numericFilename = 
        impl_->sourceDirs.numericDir + "/numeric_" + endName;
    impl_->sourceDirs.complexFilename = 
        impl_->sourceDirs.complexDir + "/complex_" + endName;
    impl_->sourceDirs.stringFilename = 
        impl_->sourceDirs.stringDir + "/string_" + endName;

#if defined(BINNUMERIC)
    impl_->sourceDirs.numericFilename += ".bin";
    impl_->sourceDirs.shortFilename += ".bin";
    impl_->sourceDirs.stringFilename += ".bin";
    impl_->sourceDirs.complexFilename += ".bin";
#endif
     
    impl_->sourceFiles.shortFile.open(impl_->sourceDirs.shortFilename, true);
    impl_->sourceFiles.shortFile.writeFFHeader(frameCount, sig);

    impl_->sourceFiles.numericFile.open(impl_->sourceDirs.numericFilename, true);
    impl_->sourceFiles.numericFile.writeFFHeader(frameCount, sig);

    impl_->sourceFiles.stringFile.open(impl_->sourceDirs.stringFilename, true);
    impl_->sourceFiles.stringFile.writeFFHeader(frameCount, sig);

    impl_->sourceFiles.complexFile.open(impl_->sourceDirs.complexFilename, true);
    impl_->sourceFiles.complexFile.writeFFHeader(frameCount, sig);
}
