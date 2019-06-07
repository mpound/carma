/** @file 
 * SAX Handlers for parsing IMR configuration files.
 * 
 * <dl><dt><b>Author </b></dt><dd>Andy Beard </dl>
 * $Revision: 1.15 $
 * $Date: 2013/04/09 22:48:02 $
 * $Id: ImrConfigHandlers.h,v 1.15 2013/04/09 22:48:02 abeard Exp $
 */

#ifndef CARMA_UTIL_IMRCONFIGHANDLERS_H
#define CARMA_UTIL_IMRCONFIGHANDLERS_H

#include <xercesc/sax/HandlerBase.hpp>

#include <map>
#include <vector>
#include <string>

namespace carma {
namespace util {

typedef enum {
    DATA,
    MONITOR,
    CONTROL,
    INTERFEROMETRY,
    CORRELATOR,
    OVRO,
    BIMA,
    SZA,
    MISC } SystemType;

typedef std::map<SystemType, int> SystemTypeTotalsMap;

/**
 * Structure to hold IMR server definition
 */
struct ServerConfig {
    std::string hostname;
    std::string name;
    std::string path;
    std::string args;
    std::string optargs;
    std::string directory;
    SystemType  system;
    int startup_priority; // Startup/shutdown priority
    bool critical;
};

typedef std::vector< ServerConfig > ServerVector;

/**
 * Structure to hold oad which embeds servers
 */
struct OADConfig {
    std::string hostname; 
    SystemType  system;
    ServerVector servers;
};

typedef std::vector< OADConfig > OadVector;

/**
 * Domain structure - holds it all
 */
typedef struct domainStruct {
    unsigned nServers;
    unsigned nCriticalServers;
    OadVector oads;
    SystemTypeTotalsMap nServersByType;
} domainType;

/**
 * Parse XML Configuration File and return a structure containing domain.
 */
domainType parseXmlConfig( const ::std::string & xmlFilename,
                           bool          validateOnly,
                           bool          verbose );
/**
 * Declares SAX Handlers for parsing XML Configuration files.
 */
class ImrConfigHandlers : public xercesc::HandlerBase {
public:
    
    /**
     * Constructor
     */
    ImrConfigHandlers( );

    /**
     * Destructor
     */
    ~ImrConfigHandlers( );

    /** 
     * Retrieve reference to vector containing servers.
     * This method retrieves all servers parsed from the input XML config file.
     * If no servers were retrieved, the vector will be empty.
     * @return Vector<server>
     */
    domainType getDomain( );

    /**
     * startDocument DocumentHandler overload
     */
    void startDocument( );

    /** 
     * startElement DocumentHandler overload
     */
    void startElement( const XMLCh * const name, 
                       xercesc::AttributeList & attributes );

    /**
     * endElement DocumentHandler overload
     */ 
    void endElement( const XMLCh * const name );

    /**
     * endDocument DocumentHandler overload
     */
    void endDocument( );
    
    /**
     * warning SAX ErrorHandler implementation
     */
    void warning( const xercesc::SAXParseException & exception );
    
    /**
     * error SAX ErrorHandler implementation
     */
    void error( const xercesc::SAXParseException & exception );

    /**
     * fatalError SAX ErrorHandler implementation
     */
    void fatalError( const xercesc::SAXParseException & exception );

private: 

    bool oadAlreadyExists( const std::string & oad );
    void appendExistingOad( );

    ServerConfig currentServer_;   // Current server
    OADConfig currentOad_;         // Current oad
    domainType domain_;          // Entire domain
    
}; // End class ImrConfigHandlers 
}} // End namespace carma::util;
#endif
