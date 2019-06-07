

/**
 *
 */

#include <vector>
#include "carma/ui/rtd/common/Version.h"
#include "carma/util/programLogging.h"
#include "carma/util/ErrorException.h"
#include "carma/services/Table.h"


using namespace ::std;
using namespace carma::services;
using namespace carma::util;
using namespace carma::ui::rtd;

Version::Version( const std::string & file )
{
    try {
        Table versionTable;
        versionTable.open( file );
        int lastrow = versionTable.getNrows() - 1;
        vector<string> serverVersions =
            versionTable.getColumn("C++ Server Version");
        vector<string> clientVersions =
            versionTable.getColumn("Java Client Version");
        this->client_ = clientVersions.at( lastrow );
        this->server_ = serverVersions.at( lastrow );
    } catch ( const ErrorException & e ) {
        ostringstream os;
        os << "Caught error reading version table "
            << file << " : "
            << e.getMessage();
        programLogErrorIfPossible( os.str() );
        server_ = string("1.7.0");
        client_ = string("0.1.2");
    } catch ( ... ) {
        ostringstream os;
        os << "Caught unclassified error reading version table " << file ;
        programLogErrorIfPossible( os.str() );
        server_ = string("1.7.0");
        client_ = string("0.1.2");
    }
}

Version::~Version()
{
}

const char *
Version::getLatestClientVersion() const
{
    const char * v = client_.c_str();
    return v;
}

const char *
Version::getServerVersion() const
{
    const char * v = server_.c_str();
    return v;
}
