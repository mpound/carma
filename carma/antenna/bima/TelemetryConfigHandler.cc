/** @file 
 * SAX Handler for parsing BIMA Telemetry configuration file.
 * 
 * <dl><dt><b>Author </b></dt><dd>Colby Gutierrez-Kraybill</dl>
 * $Revision: 1.16 $
 * $Date: 2011/12/21 22:56:33 $
 * $Id: TelemetryConfigHandler.cc,v 1.16 2011/12/21 22:56:33 mpound Exp $
 */

#include "carma/antenna/bima/TelemetryConfigHandler.h"
#include <algorithm>

using namespace std;
using namespace carma::util;
using namespace carma::antenna::bima;

unsigned short TelemetryInfo::_global_sem_num = 0;

// -----------------------------------------------------------------------------
TelemetryConfigHandler::TelemetryConfigHandler( bool check ) 
{
  _check = check;
  _version = string("(not set)");
  // Nothing here
}

// -----------------------------------------------------------------------------
TelemetryConfigHandler::~TelemetryConfigHandler()
{
  // Nor here
}

// -----------------------------------------------------------------------------
void TelemetryConfigHandler::startDocument()
{
  // Nothing
}

// -----------------------------------------------------------------------------
void TelemetryConfigHandler::startElement( const XMLCh* const name,
    AttributeList& attributes )
{
  unsigned int attrLen, i;
  char *elName = XMLString::transcode(name);
  char *attrName;
  char *attrValue;
  static carma::canbus::msgType currentType = 0;

  attrLen = attributes.getLength();

  CPTRACE( Trace::TRACE6, " startElement: " << elName << " attrLen: " << attrLen );

  if ( strcmp( elName, "telemetry" ) == 0 )
  {
    for ( i = 0; i < attrLen; i++ )
    {
      attrName = XMLString::transcode( attributes.getName(i) );
      attrValue = XMLString::transcode( attributes.getValue(i) );

      if ( strcmp( "version", attrName ) == 0 )
	_version = string( attrValue );

      XMLString::release( &attrName );
      XMLString::release( &attrValue );
    }
  }
  else if ( strcmp( elName, "packet" ) == 0 )
  {
    string *packetName = NULL;

    for ( i = 0; i < attrLen; i++ )
    {
      attrName = XMLString::transcode( attributes.getName(i) );
      attrValue = XMLString::transcode( attributes.getValue(i) );

      if ( strcmp( "name", attrName ) == 0 ) 
	packetName = new string( attrValue );

      if ( strcmp( "type", attrName ) == 0 )
      {
	long lType = strtol( attrValue, (char **)0, 16 );
	currentType = (carma::canbus::msgType)lType;
	_info[currentType] = new TelemetryInfo();
      }

      XMLString::release( &attrName );
      XMLString::release( &attrValue );
    }

    _info[currentType]->setPacketName( (char *)packetName->c_str() );

    CPTRACE( Trace::TRACE7,
	"TelemetryConfigHandler, adding '" << packetName->c_str()
	<< "' msgId: 0x" << hex << currentType );
  }
  else if ( strcmp( elName, "variable" ) == 0 )
  {
    string *name, *size, *seq, *addr, *comment;

    name = size = seq = addr = comment = NULL;

    for ( i = 0; i < attrLen; i++ )
    {
      attrName = XMLString::transcode( attributes.getName(i) );
      attrValue = XMLString::transcode( attributes.getValue(i) );

      CPTRACE( Trace::TRACE7, "attrName: " << attrName
	  << " attrValue: " << attrValue );

      if ( strcmp( "name", attrName ) == 0 )
	name = new string( attrValue );

      if ( strcmp( "size", attrName ) == 0 )
	size = new string( attrValue );

      if ( strcmp( "seq", attrName ) == 0 )
	seq = new string( attrValue );

      if ( strcmp( "addr", attrName ) == 0 )
	addr = new string( attrValue );

      if ( strcmp( "comment", attrName ) == 0 )
	comment = new string( attrValue );

      XMLString::release( &attrName );
      XMLString::release( &attrValue );
    }

    CPTRACE( Trace::TRACE7, "Converting seq, addr, size to number values" );
    int seqNum = atoi( seq->c_str() );
    short shortAddr = strtol( addr->c_str(), (char **)0, 16 );
    int intSize = atoi( size->c_str() );

    if ( _check && shortAddr != 0 )
    {
      vector<short>::iterator vi = find(_addrCheck.begin(), _addrCheck.end(), shortAddr);

      if ( vi != _addrCheck.end() )
	cerr << "WARNING, Address defined more than once in XML file, "
	  << "name: " << *name << " addr: 0x" << hex << shortAddr << endl;
      else
	_addrCheck.push_back( shortAddr );
    }

    CPTRACE( Trace::TRACE7, "Inserting 0x" << hex << currentType << " into _info" );
    _info[currentType]->setMsgId( currentType );

    CPTRACE( Trace::TRACE7, "Setting data values in _info[0x" << hex << currentType << "]" );
    if ( name != NULL && comment != NULL )
      _info[currentType]->set( seqNum, (char *)name->c_str(),
	  (char *)comment->c_str(), shortAddr, intSize );
    else
      throw CARMA_ERROR( "What the hell?? name or comment were null!" );

    CPTRACE( Trace::TRACE7, 
	"Adding Telemetry variable: '" << name->c_str()
	<< "' -- '" << comment->c_str() << "'" ); 
  }

  XMLString::release( &elName );
}


// -----------------------------------------------------------------------------
void TelemetryConfigHandler::endElement(const XMLCh* const name)
{
  char *elName = XMLString::transcode(name);

  XMLString::release(&elName);
}

// -----------------------------------------------------------------------------
void TelemetryConfigHandler::endDocument()
{
  // Nasty hack to get around a multiple instance problem that would be
  // best avoided via having TelemetryClient become a singleton
  // 0x100 is one of the can message types for the telemetry...
  // if that every changes, this will be a hard ass bug to find... :)
  _info[0x100]->resetGlobalNum();
}

// -----------------------------------------------------------------------------
void TelemetryConfigHandler::error(const SAXParseException& e)
{
  // TODO: Make CARMA specific
  cerr << "Error parsing file " << StrX(e.getSystemId())
    << ", line " << e.getLineNumber()
    << ", char " << e.getColumnNumber()
    << endl << " Message: " << StrX(e.getMessage()) << endl;
}

// -----------------------------------------------------------------------------
void TelemetryConfigHandler::fatalError(const SAXParseException& e)
{
  // TODO: Make CARMA specific
  cerr << "\nFatal Error at file " << StrX(e.getSystemId())
    << ", line " << e.getLineNumber()
    << ", char " << e.getColumnNumber()
    << "\n  Message: " << StrX(e.getMessage()) << endl;
}

// -----------------------------------------------------------------------------
void TelemetryConfigHandler::warning(const SAXParseException& e)
{
  // TODO: Make CARMA specific
  cerr << "\nWarning at file " << StrX(e.getSystemId())
    << ", line " << e.getLineNumber()
    << ", char " << e.getColumnNumber()
    << "\n  Message: " << StrX(e.getMessage()) << endl;
}

// -----------------------------------------------------------------------------
string TelemetryConfigHandler::getTelemetryVersion()
{
  return _version;
}
