#ifndef CARMA_DBMS_SYMLINKINFO_H
#define CARMA_DBMS_SYMLINKINFO_H

#include <string>
#include <unistd.h>

namespace carma {
  namespace dbms {

    struct SymlinkInfo {
	    /**
	     * The file name
	     */
	    ::std::string name_;
	    /**
	     * the file pointer
	     */
	    FILE*         file_;
	    /**
	     * The directory where the database will pick
	     * up this file.  The file will be symlinked 
	     * from its current location so that it appears in 
	     * this directory.
	     */
	    ::std::string dbLoadDir_;
	    
	    SymlinkInfo( const ::std::string & name,
			 FILE* file,
			 const ::std::string & dbLoadDir ) :
	    name_( name ),
	    file_( file ),
	    dbLoadDir_( dbLoadDir ) {  }
    };
  }
}
#endif //CARMA_DBMS_SYMLINKINFO_H
