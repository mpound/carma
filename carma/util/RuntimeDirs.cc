
/*
 *
 * Implementation of class that gets the various runtime directories
 * for a carma Program.
 *
 * @author Steve Scott
 * $id: $
 *
 * $CarmaCopyright$
 *
 */


#include <sstream>
// For debugging messages
#include <iostream>

#include <unistd.h>
#include <sys/wait.h>
#include <libgen.h>
#include <limits.h>

#include <glib.h>

#include "carma/util/ErrorException.h"
#include "carma/util/PthreadMutex.h"
#include "carma/util/RuntimeDirs.h"
#include "carma/util/ScopedLock.h"
#include "carma/util/ScopedLogNdc.h"
#include "carma/util/Trace.h"

using namespace ::std;
using namespace ::log4cpp;
using namespace carma;
using namespace carma::util;


RuntimeDirs::RuntimeDirs()
{
}

string RuntimeDirs::getConfDir(const string& programName)
{
  return getRootDir(programName) + "conf/";
}

string RuntimeDirs::getConfFile(const string& programName,
        const string& filename)
{
    string confFilename = filename;

    if (filename[0] == '/') {
        // Absolute path, don't do anything...
    }
    else if (filename[0] == '-') {
        // Relative path, remove the first character (the '-')
        confFilename = filename.substr(1);
    }
    else {
        confFilename = getConfDir(programName) + filename;
    }
    CARMA_CPTRACE(Trace::TRACE7, "ConfFilename:" << confFilename
        << " Filename:" << filename);

    return confFilename;
}


string
RuntimeDirs::getRootDir( const string & programName )
{
    const ScopedLogNdc ndc( "getRootDir(" + programName + ")" );

    string exe = getExecutable(programName);

    string dir    = dirnameString(exe);
    string parent = basenameString(dir);

    // Are we running from a directory called bin?
    if (parent.compare("bin") == 0) {
        //yes, so the directory above is the root
        string treeTop = dirnameString(dir) + "/";
        CARMA_CPTRACE(Trace::TRACE7, "Found bin; Treetop:" << treeTop);
        return treeTop;
    }

    // Are we running from a libtool .libs directory?
    if (parent.compare(".libs") == 0) {
        dir    = dirnameString(dir);
        parent = basenameString(dir);
        if (parent.compare("bin") == 0) {
            //yes, so the directory
            string treeTop = dirnameString(dir) + "/";
            CARMA_CPTRACE(Trace::TRACE7, "Found bin; Treetop:" << treeTop);
            return treeTop;
        }
    }

    // Work up the tree to see if there is a "carma" dir in it;
    // if so, it means you are in a build tree and this carma is the top of it
    while (!parent.empty()) {
        dir    = dirnameString(dir);
        parent = basenameString(dir);
        if (parent.compare("carma") == 0) {
            string treeTop = dirnameString(dir) + "/";
            CARMA_CPTRACE(Trace::TRACE7, "Walking up; Treetop:" << treeTop);
            return treeTop;
        }
    }

    // Trouble
    ostringstream os;
    os << "getRootDir(" << programName
       << ") failed; not a standard carma tree";
    throw CARMA_ERROR(os);

    return "neverGetToHere";
}


string
RuntimeDirs::getExecutableDir( const string & programName )
{
    const ScopedLogNdc ndc( "getExecutableDir(" + programName + ")" );

    return dirnameString(getExecutable(programName)) + "/";
}


string RuntimeDirs::canonicalize(const string& filename) {
    char realPath[PATH_MAX];
    // Removes symlinks and redundant '/'s
    char* r = realpath(filename.c_str(), realPath);
    if (r == NULL) {
        ostringstream o;
        o << "RuntimeDirs: can't convert file named \""
          << filename
          << "\" using realpath()" ;
        throw CARMA_ERROR(o);
    }
    //CARMA_CPTRACE(Trace::TRACE7, "Realpath addresses:" << hex
    //             << (unsigned int)realPath << "/"
    //             << (unsigned int)r << "/" << (unsigned int)rp) ;
    return realPath;
}


string
RuntimeDirs::getExecutable( const string & programName )
{
    const ScopedLogNdc ndc( "getExecutable(" + programName + ")" );

    string result;

    // Caching feature - only creates first time when gInitedOkay is false
    static ::pthread_mutex_t gGuard = PTHREAD_MUTEX_INITIALIZER;

    {
        ScopedLock< ::pthread_mutex_t > lock( gGuard );

        {
            static bool   gInitedOkay = false;
            static string gProgramName;
            // Full path to the binary, non-canonicalized
            static string gFullPath;
            static string gCanonicalized;

            if ( gInitedOkay == false ) {
                gProgramName = programName;
                gFullPath = findExecutable( gProgramName );
                gCanonicalized = canonicalize( gFullPath );

                gInitedOkay = true;
            }

            result = gCanonicalized;
        }
    }

    return result;
}

/*
 * Find the full path to the executable by running a glib routine
 * which is identical to the PATH search done by execvp().
 */
string
RuntimeDirs::findExecutable( const string & program )
{
    const ScopedLogNdc ndc( "findExecutable(" + program + ")" );
    char *path = g_find_program_in_path(program.c_str());

    if (path == NULL) {
        std::ostringstream oss;
        oss << "RuntimeDirs::findExecutable: unable to find "
            << program << " in PATH";
        throw CARMA_ERROR(oss.str());
    }

    // duplicate to a std::string, free the allocated value, and return
    const std::string s(path);
    free(path);
    return s;
}

string RuntimeDirs::basenameString(const string& inputPath)
{
    string path = trim(inputPath);
    if (path.empty())return path;
    string::size_type index = path.rfind('/');
    if (index == string::npos) return "";
    return path.substr(index+1);
}

string RuntimeDirs::dirnameString(const string& inputPath)
{
    string path = trim(inputPath);
    if (path.empty())return path;
    string::size_type index = path.rfind('/');
    if (index == string::npos) return "";
    return path.substr(0, index);
}

string RuntimeDirs::trim(const string& path)
{
    if (path.empty())return path;
    if (path.at(path.length()-1) == '/') {
        string trimmedPath = path;
        trimmedPath.resize(path.length()-1);
        return trimmedPath;
    }
    return path;

}
