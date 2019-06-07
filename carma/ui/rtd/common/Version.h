#ifndef CARMA_UI_RTD_VERSION_H
#define CARMA_UI_RTD_VERSION_H

/**
 *
 * @author Marc Pound
 *
 * $CarmaCopyright$
 *
 */


#include <string>

namespace carma {
namespace ui {
namespace rtd {

/**
 * Class to read and contain versioning info for C++ server and Java
 * client RTD programs.
 * @see conf/rtd/version.tab
 */

class Version {
public:
    /**
     * @param the file containing the version info
     */
    explicit Version( const std::string & file );
    // destructor
    virtual ~Version( );

    /**
     * @return the server version as Major.Minor[.Patch]
     */
    const char * getServerVersion() const;

    /**
     * @return the client version as Major.Minor[.Patch]
     */
    const char * getLatestClientVersion() const;

private:
    std::string server_;
    std::string client_;
};

}}} // End namespace carma::ui::rtd

#endif    // CARMA_UI_RTD_VERSION_H
