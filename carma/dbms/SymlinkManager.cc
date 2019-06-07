/** * *
 * @author: Marc Pound
 * $CarmaCopyright$
 */

#include <fstream>
#include <sstream>
#include <memory>
#include <cerrno>

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include "carma/dbms/SymlinkInfo.h"
#include "carma/dbms/SymlinkManager.h"
#include "carma/util/Program.h"
#include "carma/util/Logger.h"
#include "carma/util/Trace.h"
#include "carma/util/CommonExceptions.h"
#include "carma/util/ErrorException.h"


using namespace carma::dbms;

SymlinkManager::SymlinkManager() { } 

SymlinkManager::~SymlinkManager() { }

/**
 * print info about symlink failures
 */
void
SymlinkManager::logSymlinkError( const int err, 
	                         const char* oldpath, 
				 const char* newpath) 
{
    log4cpp::Category& myLogger = carma::util::Program::getLogger();
    
    std::ostringstream oss;
    oss << "symlink error on " << newpath << "->" << oldpath << ": ";
				
    switch ( err ) {
	case EACCES:
		oss << "Write access not permitted.";
		break;
	case EEXIST:
		oss << newpath << " already exists.";
		break;
	case EIO:
		oss << "An I/O error occurred.";
		break;
	case ELOOP:
		oss << "Too many symbolic links were encountered in resolving "
		    << newpath;
		break;
	case ENAMETOOLONG:
		oss << "Old path or new path is too long.";
		break;
	case ENOENT:
		oss << "A directory component in "
		    <<  newpath 
		    << " does not exist "
		    << "or is a dangling symbolic link, or "
		    << "old path is the empty string.";
		break;
	case ENOSPC:
		oss << "The device containing the file has no room for the new "
		    << "directory entry.";
		break;
	case ENOTDIR:
		oss << "A component used as a directory in "
		    << newpath 
		    << " is not, in fact, a directory.";
		break;
	case EROFS:
		oss << newpath << " is on a read-only filesystem.";
		break;
	case EPERM:
		oss << "Filesystem does not support symlinks";
		break;
	case EFAULT:
		oss << "Old path or new path points outside accessible "
		    << " address space";
		break;
	case ENOMEM:
		oss << "Insufficient kernel memory was available.";
		break;
	default:
		oss << "Unknown error code " << err;
		break;
    }
    const ::std::string emsg = oss.str( );
    CPTRACE( util::Trace::TRACE1, emsg);
    myLogger.error(emsg);
}

/**
 * open new files
std::vector<SymlinkInfo>
SymlinkManager::createSymlinkInfo(
	const ::std::map<std::string,std::string>& filesAndDirs ) 
{
    std::vector<SymlinkInfo> info;
    std::string name = keymap;
    std
    FILE filep = fopen(name);
    info.push_back(SymlinkInfo(name, filep, filesAndDirs));
    return info;
}
 */

void 
SymlinkManager::openFiles(std::vector<SymlinkInfo>& info, 
	                  const char* mode) {
    ::std::vector< SymlinkInfo >::iterator       i    = info.begin();
    const ::std::vector< SymlinkInfo >::const_iterator iEnd = info.end();

    while ( i != iEnd ) {
	i->file_ = fopen( i->name_.c_str(), mode );
	if ( ! i->file_ )
	    throw CARMA_ERROR( "Unable to open file " + i->name_ );
        // explicitly make world readable so the db ingestor doesn't bomb
        chmod(i->name_.c_str(), S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH);
	++i;
    }
}

/**
 * close files, print error if needed
 */
void
SymlinkManager::finishedWithFiles( const ::std::vector< SymlinkInfo > & info ) 
{
    ::std::auto_ptr< struct stat > statbuf( new struct stat );

    ::std::vector< SymlinkInfo >::const_iterator 
	i    = info.begin( );
    const ::std::vector< SymlinkInfo >::const_iterator 
	iEnd = info.end( ); 
    while ( i != iEnd ) {
	if ( fclose( i->file_ ) != 0 )
	    throw CARMA_ERROR( "Unable to close file " + i->name_ );

	stat( i->name_.c_str( ), statbuf.get( ) );
		
	// If the file has zero size, then just remove it.
	// Don't expose it to the database via symlink.
	if ( statbuf->st_size == 0 ) {
	    CPTRACE( util::Trace::TRACEALL, "Removing 0 size "
		     << "file " << i->name_);
					 
	    if ( remove( i->name_.c_str( ) ) != 0 ) {
		::std::ostringstream msg;
		msg  << "error deleting 0 size file " << i->name_;
		CPTRACE( util::Trace::TRACE1, msg.str( ) );
		throw CARMA_ERROR( msg.str( ) );
	    }

	    CPTRACE( util::Trace::TRACEALL, i->name_ << " removed" );
	} else {
	    const ::std::string linkname =
		i->dbLoadDir_ + "/" + basename( i->name_.c_str( ) );

	    CPTRACE( util::Trace::TRACEALL, "creating symlink "
		     << linkname << "->" << i->name_);
					 
	    if ( symlink( i->name_.c_str( ), linkname.c_str( ) ) != 0 ) {
		logSymlinkError( errno, (i->name_).c_str(), linkname.c_str() );
		throw CARMA_ERROR("Unable to create symlink " 
				  + linkname + " -> " + i->name_);
	    }

	    CPTRACE( util::Trace::TRACEALL, "symlink creation "
		     << "successful");
	}
	++i;
    }
}
