/**
 * @file testing for FileUtils
 * @author Dave Mehringer
 * @version $Id: tFileUtils.cc,v 1.10 2006/07/19 20:40:33 mpound Exp $
 */

#include <iostream>
#include <unistd.h>
#include "carma/util/FileUtils.h"
#include "carma/util/StringUtils.h"
#include "carma/util/Program.h"
#include "carma/util/Logger.h"

using namespace carma::util;
using namespace std;

//
// @noKeys
//
// @logger TEST_FACILITY carma.test.util.tFileUtils
//

int
carma::util::Program::main() {
    cout << FileUtils::computeSha1Sum("/etc/hosts") << endl;
    cout << FileUtils::computeMessageDigest("/etc/hosts",SHA1) 
         << endl;
    cout << FileUtils::computeMessageDigest("/etc/hosts",MD5) 
         << endl;
    
    cout << " Does /array/rt/catalogs/ exist? "
	<< boolalpha 
	<< FileUtils::exists("/array/rt/catalogs/") 
	<< endl;

    /*
    time_t mtime, ctime;
    mtime = FileUtils::modificationTime("/var/carma/log/environment.log");
    ctime = FileUtils::changeTime("/var/carma/log/environment.log");
    cout << "modification time before: " << mtime
	 << " change time before: " << ctime; 
    setProgramFacilityAndLogname(ENVIRONMENT_FACILITY, "carma.util.Test");
    sleep(2);
    Program::getLogger().notice("Does the modification time change?");
    mtime = FileUtils::modificationTime("/var/carma/log/environment.log");
    ctime = FileUtils::changeTime("/var/carma/log/environment.log");
    cout << " modification time after: " << mtime
	 << " change time after: " << ctime << endl;
    uint linecount = FileUtils::lineCount("/var/carma/log/interferometry.log");
    cout << " line count of interferometry.log is " << linecount << endl;
    */
    return EXIT_SUCCESS;
}
