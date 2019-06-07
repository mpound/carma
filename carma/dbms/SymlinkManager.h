#ifndef CARMA_DBMS_SYMLINKMANAGER_H
#define CARMA_DBMS_SYMLINKMANAGER_H
/**
 * @file
 * Support the bulk ingest model of the dbms.  Some process
 * creates a file, writes to it, closes it,  then symlinks
 * it to a location where the database will notice. The dbms
 * process then ingests the file and removes both it and the symlink.
 * @todo move timestamping (framecount) to this class
 */

#include "carma/dbms/SymlinkInfo.h"
#include <vector>

namespace carma {
  namespace dbms {

    class SymlinkManager {

	public:
	SymlinkManager();

	~SymlinkManager();

	/**
	 * convenience method to create a vector of symlinkinfo
	 * objects from a map of files and dirs.
	 * File pointers will be initialized to 0
        std::vector<SymlinkInfo> createSymlinkInfo(
	    const ::std::map<std::string,std::string>& filesAndDirs );
	 */

	/**
	 * open file pointers in info if they aren't already open,
	 * using mode indicated
	 * @param info vector containing Symlink objects that describe 
	 * the files to be created (and later symlinked).
	 * @parma mode Mode with which to open each file, identical to 
	 * mode parameter for <tt>fopen(3)</tt>
	 */
        void openFiles( ::std::vector<SymlinkInfo> & info,
		        const char* mode);

	/**
	 * Close and symlink file. If the file has zero length it
	 * will be removed.
	 * @param info vector containing Symlink objects that describe 
	 * the files to be symlinked
	 */
        void finishedWithFiles( const ::std::vector<SymlinkInfo>& info );

	private:

	/**
	 * print info about symlink failures
	 * @param err Error code returned from <tt>symlink(2)</tt>.
	 * @param oldpath The path to be symlinked.
	 * @param newpath The symlink path to create.
	 * @see <tt>symlink(2)</tt> man page
	 */
	void logSymlinkError( const int err, 
		              const char* oldpath, 
			      const char* newpath );
    };
  }
}

#endif // CARMA_DBMS_SYMLINKMANAGER_H
