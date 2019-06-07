/** @file 
 * SAX Handlers for parsing IMR configuration files.
 * 
 * <dl><dt><b>Author </b></dt><dd>Andy Beard </dl>
 * $Revision: 1.20 $
 * $Date: 2014/11/05 18:58:24 $
 * $Id: ImrConfigHandlers.cc,v 1.20 2014/11/05 18:58:24 iws Exp $
 */

#include "carma/util/ImrConfigHandlers.h"

#include "carma/util/ErrorException.h"
#include "carma/util/IllegalArgumentException.h"
#include "carma/util/StrX.h"
#include "carma/util/xercesUtils.h"

#include <xercesc/parsers/SAXParser.hpp>
#include <xercesc/sax/AttributeList.hpp>
#include <xercesc/validators/DTD/DTDValidator.hpp>

#include <iostream>
#include <sstream>
#include <string>
#include <unistd.h>

using namespace std;
using namespace carma;
using namespace carma::util;
XERCES_CPP_NAMESPACE_USE

namespace {

    SystemType stringToSystemType( const ::std::string & sysString ) {
        if      ( sysString == "data" )           return DATA;
        else if ( sysString == "monitor" )        return MONITOR;
        else if ( sysString == "control" )        return CONTROL;
        else if ( sysString == "interferometry" ) return INTERFEROMETRY;
        else if ( sysString == "correlator" )     return CORRELATOR;
        else if ( sysString == "ovro" )           return OVRO;
        else if ( sysString == "bima" )           return BIMA;
        else if ( sysString == "sza" )            return SZA;
        else                                      return MISC; 
    }

    ::std::string systemTypeToString( const SystemType sysType ) {
        switch ( sysType ) {
            case MISC: return "misc";
            case DATA: return "data";
            case MONITOR: return "monitor";
            case CONTROL: return "control";
            case INTERFEROMETRY: return "interferometry";
            case CORRELATOR: return "correlator";
            case OVRO: return "ovro";
            case BIMA: return "bima";
            case SZA: return "sza";
            default: return "unknown";
        }
    }
    
    int stringToPriority( const ::std::string & priority ) {
        const int intpriority = ::atoi( priority.c_str() );
        if ( intpriority < 0 || intpriority > 9 ) {
            ostringstream msg;
            msg << intpriority << " is an invalid priority. "
                << "Startup priority must be between 0-9 inclusive.";
            throw CARMA_EXCEPTION( util::IllegalArgumentException, msg.str( ) );
        }

        return intpriority;
    }
            
    const SAXParser::ValSchemes validation = SAXParser::Val_Auto;

} // End namespace <unnamed>


// -----------------------------------------------------------------------------
domainType carma::util::parseXmlConfig( 
                           const ::std::string & xmlFilename,
                           bool          validateOnly,
                           bool          verbose )
{
    domainType domain;

    try { // Initialize XML platform and parse input file...
        XMLPlatformUtils::Initialize();
    } catch (...) {
        throw;
    }

    try {
        // Create DTD Validator on the heap - NOTE that the SAXParser
        // instance DELETES THE VALIDATOR!!!
        DTDValidator * validator = new DTDValidator( ); // Does NOT leak
        SAXParser parser( validator );
        ImrConfigHandlers handler;

        // Reuse the parser as the validators ErrorReporter.
        validator->setErrorReporter( &parser );

        parser.setDocumentHandler( &handler );
        parser.setErrorHandler( &handler );
        parser.setValidationScheme( validation );
        parser.parse( xmlFilename.c_str( ) );

        // Check to see if any errors were encountered while parsing
        if ( parser.getErrorCount( ) ) {
            ostringstream msg;
            msg << parser.getErrorCount( ) << " error" 
                << (parser.getErrorCount() > 1 ? "s" : "")
                << " encountered while parsing " << xmlFilename << ". "
                << " Fix above errors and try again." << endl;
            throw CARMA_EXCEPTION( ErrorException, msg.str( ) );
        }
    
        domain = handler.getDomain();

    } catch (...) {
        XMLPlatformUtils::Terminate( );
        throw;
    } 
        
    XMLPlatformUtils::Terminate();
        
    if ( verbose ) { // Spit out summary stats and server details

        OadVector::const_iterator oad = domain.oads.begin( ); 
        for ( ; oad != domain.oads.end( ); ++oad ) {

            ServerVector::const_iterator server = oad->servers.begin( );
            for ( ; server != oad->servers.end( ); ++server ) {

                cout << " Server            : " << server->name << endl;
                cout << "  hostname         : " << server->hostname << endl;
                cout << "  path             : " << server->path << endl;
                cout << "  directory        : " << server->directory << endl;
                cout << "  args             : " << server->args << endl;
                cout << "  optargs          : " << server->optargs << endl;
                cout << "  system           : " 
                    << systemTypeToString( server->system ) << endl;
                cout << "  startup-priority : " 
                    << server->startup_priority << endl;
                cout << "  critical         : " << boolalpha
                    << server->critical << noboolalpha << endl;
                cout << endl;
            } // loop over servers
        } // loop over oads

        ostringstream summary;

        summary << xmlFilename << " contains " << domain.nServers << " ";

        if ( domain.nServers > 1 ) {
            summary << "servers ";
        } else {
            summary << "server ";
        }

        summary << "(" << domain.nCriticalServers << " critical) on "
            << domain.oads.size() << " ";

        if ( domain.oads.size() > 1 ) {
            summary << "oads.";
        } else {
            summary << "oad.";
        }

        cout << summary.str( ) << endl;

    } // if verbose

    if ( validateOnly ) {
        cout << xmlFilename << " is valid XML." << endl;
    }

    return domain;

}

// -----------------------------------------------------------------------------
ImrConfigHandlers::ImrConfigHandlers() 
{
    // Nothing here
}

// -----------------------------------------------------------------------------
ImrConfigHandlers::~ImrConfigHandlers()
{
    // Nor here
}

// -----------------------------------------------------------------------------
domainType ImrConfigHandlers::getDomain() 
{
    return domain_;
}

// -----------------------------------------------------------------------------
void ImrConfigHandlers::startDocument()
{
    // Nothing
}

// -----------------------------------------------------------------------------
void 
ImrConfigHandlers::startElement( const XMLCh * const name, 
                                 AttributeList & attributes)
{
    const AutoXMLString xmlElementName( XMLString::transcode( name ) );
    const string elementName( xmlElementName.getString( ) );
    const unsigned len = attributes.getLength( );

    if ( elementName.compare( "oad" ) == 0 ) {

        // Get attribute, set hostname
        for (unsigned i = 0; i < len; i++) {

            const AutoXMLString xmlAttributeName( 
                XMLString::transcode( attributes.getName( i ) ) );
            const string attributeName( xmlAttributeName.getString( ) );
            
            const AutoXMLString xmlAttributeValue(
                XMLString::transcode( attributes.getValue( i ) ) );
            const string attributeValue( xmlAttributeValue.getString( ) );

            if ( attributeName.compare( "hostname" ) == 0) {
                currentOad_.servers.clear();
                string host = attributeValue;
                for ( string::size_type i = 0; i < host.size( ); ++i) {
                    host[i] = tolower( host[i] );
                }
                if ( host == "localhost" ) {
                    const int len = 255;
                    char localhostname[len+1];
                    gethostname(localhostname, len);
                    host = localhostname;
                }
                currentOad_.hostname    = host;
                currentServer_.hostname = host;
            } else if ( attributeName.compare( "system" ) == 0 ) {
                currentOad_.system = stringToSystemType( attributeValue ); 
            }
        }
    } else if ( elementName.compare( "server" ) == 0 ) {

        // Set other attributes...
        for (unsigned i = 0; i < len; i++) {

            const AutoXMLString xmlAttributeName( 
                XMLString::transcode( attributes.getName( i ) ) );
            const string attributeName( xmlAttributeName.getString( ) );

            const AutoXMLString xmlAttributeValue(
                XMLString::transcode( attributes.getValue( i ) ) );
            const string attributeValue( xmlAttributeValue.getString( ) );

            if ( attributeName.compare( "name" ) == 0 ) {
                currentServer_.name = attributeValue;
            } else if ( attributeName.compare( "path" ) == 0 ) {
                currentServer_.path = attributeValue;
            } else if ( attributeName.compare( "directory" ) == 0 ) {
                currentServer_.directory = attributeValue;
            } else if ( attributeName.compare( "args" ) == 0 ) {
                currentServer_.args = attributeValue;
            } else if ( attributeName.compare( "optargs" ) == 0 ) {
                currentServer_.optargs = attributeValue;
            } else if ( attributeName.compare( "system" ) == 0 ) {
                if ( attributeValue != "unknown" )  {
                    currentServer_.system = 
                        stringToSystemType( attributeValue ); 
                } else {
                    currentServer_.system = currentOad_.system;
                }
            } else if ( attributeName.compare( "startup-priority" ) == 0 ) {
                currentServer_.startup_priority = 
                    stringToPriority( attributeValue );
            } else if ( attributeName.compare( "critical" ) == 0 ) {
                currentServer_.critical = ( attributeValue == "true" );
            } 
        } // Loop over attributes
    } // if element 
}

// -----------------------------------------------------------------------------
void ImrConfigHandlers::endElement( const XMLCh * const name )
{
    const AutoXMLString xmlElementName( XMLString::transcode( name ) );
    const string elementName( xmlElementName.getString( ) );
    
    // Either add the server to the servers_ vector or the oad to the oad vec.
    if ( elementName.compare( "oad" ) == 0 ) {
        // Add the oad to the domain list
        if ( oadAlreadyExists( currentOad_.hostname ) ) {
            // Add the current oad to the pre-existing one.
            appendExistingOad();
        } else  {
            // Add the new oad to the domain.
            domain_.oads.push_back(currentOad_);
        }
    } else if ( elementName.compare( "server" ) == 0) {
        currentOad_.servers.push_back( currentServer_ );
        domain_.nServersByType[ currentServer_.system ] =+ 1;
    } 
}

// -----------------------------------------------------------------------------
void ImrConfigHandlers::endDocument()
{
    // Sum up the number of servers and number of critical servers
    unsigned int count = 0;
    unsigned int critCount = 0;

    vector<OADConfig>::iterator oad = domain_.oads.begin( ); 
    for ( ; oad != domain_.oads.end( ); ++oad ) {
        count += oad->servers.size();
        vector<ServerConfig>::iterator server = oad->servers.begin( );
        for ( ; server != oad->servers.end( ); ++server ) {
            if ( server->critical ) {
                ++critCount;  
            }
        } // End iterate over servers
    } // End iterator over oads
    domain_.nServers = count;
    domain_.nCriticalServers = critCount;
}

// -----------------------------------------------------------------------------
void ImrConfigHandlers::error(const SAXParseException& e)
{
    // TODO: Make CARMA specific
    cerr << "Error parsing file " << StrX(e.getSystemId())
        << ", line " << e.getLineNumber()
        << ", char " << e.getColumnNumber()
        << endl << " Message: " << StrX(e.getMessage()) << endl;
}

// -----------------------------------------------------------------------------
void ImrConfigHandlers::fatalError(const SAXParseException& e)
{
    // TODO: Make CARMA specific
    cerr << "\nFatal Error at file " << StrX(e.getSystemId())
        << ", line " << e.getLineNumber()
        << ", char " << e.getColumnNumber()
        << "\n  Message: " << StrX(e.getMessage()) << endl;
}

// -----------------------------------------------------------------------------
void ImrConfigHandlers::warning(const SAXParseException& e)
{
    // TODO: Make CARMA specific
    cerr << "\nWarning at file " << StrX(e.getSystemId())
        << ", line " << e.getLineNumber()
        << ", char " << e.getColumnNumber()
        << "\n  Message: " << StrX(e.getMessage()) << endl;
}

// -----------------------------------------------------------------------------
bool ImrConfigHandlers::oadAlreadyExists(const std::string & oad)
{
    vector<OADConfig>::iterator iOad;
    for (iOad = domain_.oads.begin(); iOad < domain_.oads.end(); iOad++) {
        if ( iOad->hostname == oad ) 
            return true;
    }
    return false;
}

// -----------------------------------------------------------------------------
void ImrConfigHandlers::appendExistingOad( )
{
    vector<OADConfig>::iterator oad;
    for (oad = domain_.oads.begin(); oad < domain_.oads.end(); oad++) {
        if ( oad->hostname == currentOad_.hostname )
            // Append the servers to the existing oad.
            oad->servers.insert(
                oad->servers.end(),
                currentOad_.servers.begin(),
                currentOad_.servers.end() );
    } 
}
