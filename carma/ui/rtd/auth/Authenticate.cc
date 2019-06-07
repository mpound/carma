//----------------------------------------------------------------------
// User authentication class. Authentication is done with input/output
// to standard in/out.
//                        -Hemant Shukla (June 10, 1998)
//----------------------------------------------------------------------

#include <iostream>
#include <sstream>
#include <vector>

#include <sys/types.h>
#include <pwd.h>

#include <carma/util/Trace.h>
#include <carma/util/programLogging.h>

#include <carma/ui/rtd/auth/Authenticate.h>
#include <carma/ui/rtd/common/ReaderWithTimeout.h>
#include <carma/ui/rtd/common/ProtoBufUtil.h>
#include <carma/ui/rtd/common/WindowList.h>
#include <carma/ui/rtd/common/Version.h>

#include <carma/ui/rtd/common/RTD.pb.h>

#include <boost/algorithm/string.hpp>
#include <boost/algorithm/string/split.hpp>

using namespace carma::util;
using namespace carma::ui::rtd;

/**
 * Authenticate a user and store the short/full username and client version
 * for later use.
 */
enum AuthenticationStatus Authenticate::authenticate(const Version &version, const WindowList &windowList)
{
    // Give 3 minutes to login
    struct timeval tv;
    tv.tv_sec = 180;
    tv.tv_usec = 0;
    ReaderWithTimeout readerTMO(tv);

    CPTRACE(Trace::TRACE6, "Entering authentication...");

    std::string input("");
    while (readerTMO.getBytes(input)) {
        CPTRACE(Trace::TRACE6, "Authentication: clientRequest=" << input);

        // parse the protocol buffer
        rtdproto::AuthenticationRequest authreq;
        authreq.ParseFromString(input);

        this->shortname = authreq.username();

        const std::string clientVersion = authreq.version();
        const std::string windowName = authreq.window();

        // try to look up user's full name from passwd database
        {
            struct passwd *pw = getpwnam(this->shortname.c_str());
            if (pw != NULL) {
                this->fullname = pw->pw_gecos;
            } else {
                this->fullname = "";
            }
        }

        // check client version
        if (clientVersion != version.getLatestClientVersion()) {
            std::ostringstream oss;
            oss << "auth: client version is out of date "
                << "(client=" << clientVersion
                << ", latest=" << version.getLatestClientVersion() << ")";
            programLogErrorIfPossible(oss.str());

            rtdproto::AuthenticationReply authreply;
            authreply.set_code(rtdproto::AUTH_FAILURE);
            authreply.set_details("client version is out of date");
            serializeMessageToStdout(authreply);

            return AUTH_FAILURE;
        }

        // check the window list to make sure this window exists
        this->window = windowList.find(windowName);
        if (this->window == NULL) {
            std::ostringstream oss;
            oss << "auth: invalid window specified: " << windowName;
            programLogErrorIfPossible(oss.str());

            rtdproto::AuthenticationReply authreply;
            authreply.set_code(rtdproto::AUTH_FAILURE);
            authreply.set_details("invalid window specified");
            serializeMessageToStdout(authreply);

            return AUTH_FAILURE;
        }

        CPTRACE(Trace::TRACE6, "Authentication: success");
        CPTRACE(Trace::TRACE6, "shortname: " << this->shortname);
        CPTRACE(Trace::TRACE6, "fullname: " << this->fullname);
        CPTRACE(Trace::TRACE6, "clientversion: " << clientVersion);
        CPTRACE(Trace::TRACE6, "window: " << windowName);

        {
            rtdproto::AuthenticationReply authreply;
            authreply.set_code(rtdproto::AUTH_SUCCESS);
            authreply.set_details("success");
            serializeMessageToStdout(authreply);
        }

        return AUTH_SUCCESS;
    }

    // timeout or broken connection results in failure
    return AUTH_FAILURE;
}

std::string Authenticate::getShortName() const
{
    return this->shortname;
}

std::string Authenticate::getFullName() const
{
    return this->fullname;
}

Window *Authenticate::getWindow() const
{
    return this->window;
}
