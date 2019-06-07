/** @file
 * Application for writing taylored packets to a CANbus - Based on Andy's
 * original application canSniffer.
 *
 * <dl><dt><b>Author </b></dt><dd>Colby Gutierrez-Kraybill</dl>
 * $Revision: 1.3 $
 * $Date: 2012/07/25 17:51:26 $
 * $Id: canpacket.cc,v 1.3 2012/07/25 17:51:26 abeard Exp $
 */

#include <iostream>
#include <sstream>

#include <stdlib.h>
#include <errno.h>
#include <climits>

#include "carma/util/Program.h"
#include "carma/util/Trace.h"
#include "carma/canbus/Utilities.h"
#include "carma/canbus/InetCan.h"
#include "carma/canbus/JanzCanIo.h"

using namespace std;
using namespace carma::util;
using namespace carma::canbus;

/**
 * @version $Revision: 1.3 $
 *
 * @description
 * \n Application for sending arbitrary can packets formed based on
 * command-line input.
 *
 * @usage canPacket mbus=(0-0xf) canbus=(0-1) [eng=(0-1)] [api=(0-255)] [node=(0-511)] [msgid=(0-1023)] [hcontent=<up to 8 bytes in hex>] [bcontent=<raw binary format in 010101... format>]
 *
 * @key mbus     0   i The Janz CAN/Dio card id.
 * @key canbus   0   i Canbus to send command on (0 or 1).
 * @key eng     -1   i Send  application (0) or engineering (1) message.
 * @key api      0   i Send particular api or boardType (if eng=1) (default all). 
 * @key node     0   i Send to particular node or serial number (if eng=1) (default all).
 * @key msgid    -1  i Send with a particular msgtype (default all).
 * @key hcontent  "nan"  s Content of packet in hexidecimal format.  If full 8 bytes not supplied, remaining content will be set to zero values.
 * @key overip   f   b Send via CAN over IP
 * @key host     localhost s Default to localhost
 * @key bcontent "nan"  s Content of packet in binary format.  If full 64 bits are not supplied, remaining content will be set to zero values.
 * @key format @noDefault s Format for values in sprintf format e.g. %f%c%c%c%c\n
 *\t  You do not have to specify all byte positions in the outbound packet that will be used.\n
 *\t  Any unspecified values will be padded as zeros.\n
 *\t    Available formatting strings:\n
 *\t      %g  - 8 byte double\n
 *\t      %f  - 4 byte float\n
 *\t      %ud - 4 byte unsigned int\n
 *\t      %d  - 4 byte int\n
 *\t      %uh - 2 byte unsigned short\n
 *\t      %h  - 2 byte short\n
 *\t      %c  - 1 byte unsigned char\n
 * @key values @noDefault s Values used for filling in packet based on the format given. You do not have to specify all bytes that will be in the outbound packet.  Any unspecified bytes will be padded as zeros.  %c (or unsigned char) values may be specified with hexidecimal notation e.g. 0xff or using a character representation, e.g. P
 *
 * @logger DEFAULT_FACILITY carma.canbus.canpacket
 */
int Program::main()
{

  try
  {
    int mbus = getIntParameter( "mbus" );
    int canbus = getIntParameter( "canbus" );
    int clEng = getIntParameter( "eng" );
    int clApi = getIntParameter( "api" ); // clXXX indicates command line
    int clNode = getIntParameter( "node" );
    int clMsgid = getIntParameter( "msgid" );
    string clhContent;
    string clbContent;
    string clFormat;
    string clValues;

    if ( parameterWasSpecified( "hcontent" ) )
      clhContent = getStringParameter( "hcontent" );

    if ( parameterWasSpecified( "bcontent" ) )
    {
      CPTRACE( Trace::TRACE2, " bcontent:" << clbContent );
      cerr << "bcontent not yet implemented" << endl;
      return EXIT_FAILURE;
      
      // and once implemented...
      if ( parameterWasSpecified( "hcontent" ) )
      {
	cerr << "'hcontent' and 'bcontent' are mutually exclusive parameters" << endl;
	cerr << "Usage: " << getUsageString() << endl;
	return EXIT_FAILURE;
      }
      else
	clbContent = getStringParameter( "bcontent" );
    }

    if ( parameterWasSpecified( "format" ) )
    {
      if ( parameterWasSpecified( "hcontent" ) || parameterWasSpecified( "bcontent" ) )
      {
	cerr << "'format' cannot be used with the hcontent or bcontent parameters" << endl;
	cerr << "Usage: " << getUsageString() << endl;
	return EXIT_FAILURE;
      }
      else
      {
	if ( ! parameterWasSpecified( "values" ) )
	{
	  cerr << "'values' must be specified in conjunction with format" << endl;
	  cerr << "Usage: " << getUsageString() << endl;
	  return EXIT_FAILURE;
	}
	clFormat = getStringParameter( "format" );
      }
    }

    if ( parameterWasSpecified( "values" ) )
    {
      if ( ! parameterWasSpecified( "format" ) )
      {
	cerr << "'format' parameter must be specified with 'values'" << endl;
	cerr << "Usage: " << getUsageString() << endl;
	return EXIT_FAILURE;
      }
      else
	clValues = getStringParameter( "values" );
    }


    unsigned char content[8] = { 0,0,0,0,0,0,0,0 };

    // Validate input parameters...
    if (!((mbus >= 0 && mbus <= 0xf) && (canbus == 0 || canbus == 1) && 
	  (clEng >= -1 && clEng <= 1) &&  (clApi >= 0 && clApi <= 255) && 
	  (clNode >= 0 && clNode <= 511) && 
	  (clMsgid >= -1 && clMsgid <= 1023))) {
      // Parameters invalid...    
      cerr << "Usage: " << getUsageString() << endl;
      return EXIT_FAILURE;
    } 


    ostringstream os;
    os << "/dev/dpm_" << hex << mbus << dec << canbus << ends;
    // Instatiate InetCan instead of CanIo to use CanOverIP
    CanIo * can;
    if ( parameterWasSpecified( "overip" ) )
    {
      string theHost(getStringParameter( "host" ));
      can = new InetCan( theHost );
    }
    else
    {
      can = new JanzCanIo( os.str().c_str() , true );
    }

    canbus::Message msg;
    msg.setBusId(extractBusId(os.str().c_str())); 
    msg.setId( createId( false, clApi, clNode, clMsgid ) );
    vector<byteType> data;

    CPTRACE( Trace::TRACE2, "Grabbing content..." );

    if ( parameterWasSpecified( "hcontent" ) )
    {
      CPTRACE( Trace::TRACE2, " hcontent: " << clhContent );

      // strip off leading 0x if given
      int hi = clhContent.find( "0x" );

      if ( hi == 0 )
	clhContent.erase( 0, 2 );

      int i, ci = clhContent.size();
      for ( i = 0; i < ci; i+=2 )
      {
	unsigned char h = toupper(clhContent[i]);
	h = (h >= 'A') ? h - 'A' + 10 : h - '0';

	unsigned char l = 0;

	if ( (i + 1) < ci )
	{
	  l = toupper(clhContent[i+1]);
	  l = (l >= 'A') ? l - 'A' + 10 : l - '0';
	}

	content[(int)(i/2)] = (h << 4) + l;
      }

      for ( int i = 0; i < 8; i++ )
	uByteToData( data, content[i] );
    }

    if ( parameterWasSpecified( "format" ) )
    {
      CPTRACE( Trace::TRACE2, " format:" << clFormat );

      // Tease out formattings
      unsigned int bytes = 0, nextf = 0, nextv = 0, pos = 0, fmt = 0;
      string fmts, vals;

      while ( bytes < 8 )
      {
	CPTRACE( Trace::TRACE2, "bytes: " << bytes );

	CPTRACE( Trace::TRACE2, " nextf: " << nextf );
	pos = clFormat.find( '%', nextf );
	if ( pos != string::npos )
	{
	  pos++;
	  CPTRACE( Trace::TRACE2, "  pos: " << pos );
	  fmt = clFormat.find( '%', pos );
	  if ( fmt == string::npos )
	    fmt = clFormat.length();
	  CPTRACE( Trace::TRACE2, "  fmt: " << fmt );
	  fmts = clFormat.substr( pos, fmt-pos );
	  nextf = fmt;

	  CPTRACE( Trace::TRACE2, " nextv: " << nextv );
	  pos = clValues.find( ',', nextv );
	  if ( pos == string::npos )
            pos = clValues.length();

	  if ( pos < nextv ) // not enough values specified
	    throw "Not enough values for number of specs in format";

	  CPTRACE( Trace::TRACE2, "  pos: " << pos );
	  vals = clValues.substr( nextv, pos-nextv );
	  nextv = pos + 1;

	  if ( fmts == "g" )
	  {
	    if ( bytes != 0 )
	      throw "Not enough room to put in double value";

	    bytes = 8;

	    char **endp = NULL;
	    double d = strtod( vals.c_str(), endp );
	    if ( endp != NULL )
	    {
	      ostringstream os;
	      os << "Error converting double at: '" << *endp << "'";
	      throw os.str().c_str();
	    }
	    else if ( errno == ERANGE )
	    {
	      if ( d == 0 )
		throw "Error double conversion underflow";
	      else
		throw "Error double conversion overflow";
	    }

	    doubleToData( data, d );
	    cout << " Format string: %" << fmts << ": " << d << endl;
	    CPTRACE( Trace::TRACE2, "  Adding double: " << d );

	  } // double

	  if ( fmts == "f" )
	  {
	    if ( bytes + 4 > 8 )
	      throw "Not enough room to put in float value";

	    bytes += 4;

	    char **endp = NULL;
	    float f = strtof( vals.c_str(), endp );
	    if ( endp != NULL )
	    {
	      ostringstream os;
	      os << "Error converting float at: '" << *endp << "'";
	      throw os.str().c_str();
	    }
	    else if ( errno == ERANGE )
	    {
	      if ( f == 0 )
		throw "Error float conversion underflow";
	      else
		throw "Error float conversion overflow";
	    }

	    floatToData( data, f );
	    cout << " Format string: %" << fmts << ": " << f << endl;
	    CPTRACE( Trace::TRACE2, "  Adding float: " << f );

	  } // float

	  if ( fmts == "d" )
	  {
	    if ( bytes + 4 > 8 )
	      throw "Not enough room to put in signed long value";

	    bytes += 4;

	    char **endp = NULL;
	    long l = strtol( vals.c_str(), endp, 10 );
	    if ( endp != NULL )
	    {
	      ostringstream os;
	      os << "Error converting long at: '" << *endp << "'";
	      throw os.str().c_str();
	    }
	    else if ( errno == ERANGE )
	    {
	      if ( l == LONG_MIN )
		throw "Error long conversion underflow";
	      else
		throw "Error long conversion overflow";
	    }
	    else if ( errno == EINVAL )
	    {
	      throw "Given base contains unsupported value or no digits seen";
	    }

	    sLongToData( data, l );
	    cout << " Format string: %" << fmts << ": " << l << endl;
	    CPTRACE( Trace::TRACE2, "  Adding long: " << l );

	  } // long

	  if ( fmts == "ud" )
	  {
	    if ( bytes + 4 > 8 )
	      throw "Not enough room to put in signed long value";

	    bytes += 4;

	    char **endp = NULL;
	    unsigned long l = strtoul( vals.c_str(), endp, 10 );
	    if ( endp != NULL )
	    {
	      ostringstream os;
	      os << "Error converting unsigned long at: '" << *endp << "'";
	      throw os.str().c_str();
	    }
	    else if ( errno == ERANGE )
	    {
	      if ( static_cast<long>(l) == LONG_MIN )
		throw "Error unsigned long conversion underflow";
	      else
		throw "Error unsigned long conversion overflow";
	    }
	    else if ( errno == EINVAL )
	    {
	      throw "Given base contains unsupported value or no digits seen";
	    }

	    uLongToData( data, l );
	    cout << " Format string: %" << fmts << ": " << l << endl;
	    CPTRACE( Trace::TRACE2, "  Adding unsigned long: " << l );

	  } // unsigned long

	  if ( fmts == "h" )
	  {
	    if ( bytes + 2 > 8 )
	      throw "Not enough room to put in short value";

	    bytes += 2;

	    char **endp = NULL;
	    short s = static_cast<short>(strtoul( vals.c_str(), endp, 10 ));
	    if ( endp != NULL )
	    {
	      ostringstream os;
	      os << "Error converting short at: '" << *endp << "'";
	      throw os.str().c_str();
	    }
	    else if ( errno == ERANGE )
	    {
	      throw "Error short conversion underflow/overflow";
	    }
	    else if ( errno == EINVAL )
	    {
	      throw "Given base contains unsupported value or no digits seen";
	    }

	    sShortToData( data, s );
	    cout << " Format string: %" << fmts << ": " << s << endl;
	    CPTRACE( Trace::TRACE2, "  Adding short: " << s );

	  } // short

	  if ( fmts == "uh" )
	  {
	    if ( bytes + 2 > 8 )
	      throw "Not enough room to put in unsigned short value";

	    bytes += 2;

	    char **endp = NULL;
	    unsigned short s = static_cast<unsigned short>(strtoul( vals.c_str(), endp, 10 ));
	    if ( endp != NULL )
	    {
	      ostringstream os;
	      os << "Error converting short at: '" << *endp << "'";
	      throw os.str().c_str();
	    }
	    else if ( errno == ERANGE )
	    {
	      throw "Error short conversion underflow/overflow";
	    }
	    else if ( errno == EINVAL )
	    {
	      throw "Given base contains unsupported value or no digits seen";
	    }

	    uShortToData( data, s );
	    cout << " Format string: %" << fmts << ": " << s << endl;
	    CPTRACE( Trace::TRACE2, "  Adding unsigned short: " << s );

	  } // unsigned short

	  if ( fmts == "c" )
	  {
	    if ( bytes + 1 > 8 )
	      throw "Not enough room to put in char value";

	    bytes += 1;

	    // Check if it's in hex format
	    const char *vc = NULL;
	    unsigned char c = '\0';
	    if ( vals.find( "0x", 0 ) != string::npos )
	    {
	      int a = strtol( vals.substr( 2, vals.length()-2 ).c_str(), (char**)NULL, 16 );
	      c = static_cast<unsigned char>(a);
	    }
	    else
	    {
	      vc = vals.c_str();
	      c = static_cast<unsigned char>(*vc);
	    }

	    uByteToData( data, c );
	    cout << " Format string: %" << fmts << ": ";
	    if ( ::isprint(c) )
	    {
	      cout << c;
	    }
	    else
	    {
	      cout << "0x" << hex << (int)c;
	    }
	    cout << endl;

	  } // unsigned short
	}
	else // No more formatting, pad out data
	{
	  bytes++;
	  uByteToData( data, 0x00 );
	  CPTRACE( Trace::TRACE2, "  Padding with: 0x00" );
	}

      } // next format/value tokens 
    } // while bytes < 8 

    CPTRACE( Trace::TRACE2, "  setData(...)");
    msg.setData( data );

    CPTRACE( Trace::TRACE2, "  postMessage(...)");
    can->postMessage( msg );
  }
  catch (std::exception &ex)
  {
    cerr << ex.what() << endl;
    return EXIT_FAILURE;
  } 
  catch ( const char *errcc )
  {
    cerr << errcc << endl;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
} // End Program::main()
