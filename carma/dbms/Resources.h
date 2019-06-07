#ifndef CARMA_DBMS_RESOURCES_H
#define CARMA_DBMS_RESOURCES_H

/**
 * @file
 * access of DBMS related resources for carma::dbms.
 *
 * @author: Dave Mehringer
 *
 * $CarmaCopyright$
 *
 */

#include <string>

namespace carma {
namespace dbms {

    typedef enum {
        DEFAULT_RDBMS,
        DEFAULT_DATASRC,
        DEFAULT_DBUSER,
        DEFAULT_DBNAME,
        DEFAULT_ODBCINI,
        DEFAULT_SOCKET,
        DEFAULT_PORT
    } Resource;

    std::string getResource(const Resource& resource);
}}

#endif
