/*
 * @usage tTelemFile tmconfig=antenna/bima/telemetry.xml
 * @key tmconfig "antenna/bima/telemetry.xml" s Default telemetry description.
 * @logger DEFAULT_FACILITY carma.bima.TelemetryCheck
 */


// C++ Includes...
#include <iostream>
#include <ostream>

// Xerces includes
#include <xercesc/parsers/SAXParser.hpp>
#include <xercesc/validators/DTD/DTDValidator.hpp>

// CARMA Includes...
#include "carma/antenna/bima/TelemetryConfigHandler.h"
#include "carma/util/Program.h"

using namespace std;
using namespace carma::util;
using namespace carma::antenna::bima;

int Program::main()
{
  try
  {
    ostringstream oss;
    oss << Program::getConfDir() << "/" << getStringParameter( "tmconfig" );
    TelemetryConfigHandler tmConfig( true ); // Tell handler to be verbose...

    XMLPlatformUtils::Initialize();
    DTDValidator *validator = new DTDValidator;
    SAXParser *parser = new SAXParser( validator );

    validator->setErrorReporter( parser );

    parser->setDocumentHandler( &tmConfig );
    parser->setErrorHandler( &tmConfig );
    parser->parse( oss.str().c_str() );
 
    if ( parser->getErrorCount() )
     cerr << "************************************************************"
          << "* " << parser->getErrorCount() << " error"
          << (parser->getErrorCount() > 1 ? "s" : "") << " encountered while parsing. "
          << endl
          << "* Fix above errors and try again."
          << "************************************************************" << endl;
  }
  catch ( const carma::util::BaseException & be )
  {
    cerr << be.what() << endl;
  }

  exit(1);
}

