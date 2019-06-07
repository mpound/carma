
/**@file
 * Class definition for TelemetryCommand on the BIMA antennas.
 *
 * <dl><dt><b>Author </b></dt><dd>Colby Gutierrez-Kraybill </dl>
 * $Revision: 1.6 $
 * $Date: 2005/08/15 03:18:49 $
 * $Id: TelemetryCommand.h,v 1.6 2005/08/15 03:18:49 colby Exp $
 */


#ifndef CARMA_BIMA_TELEMETRYCOMMAND_H
#define CARMA_BIMA_TELEMETRYCOMMAND_H

#include <vector>

#include "carma/canbus/Types.h"

#define TELEMETRYIPQ  "telemetry.ipq"

#define IFIPQ  "if.ipq"
#define TELEMETRYCAN    c
#define TELEMETRYLONG   l
#define TELEMETRYSHORT  s


namespace carma
{
  namespace antenna
    {
      namespace bima
	{
	  class TelemetryCommand
	    {

	    public:
	      typedef union { char c; short s; long l; float f; double d; unsigned char bytes[8]; } PayloadType;

	    private:
	      unsigned short addr_;
	      PayloadType payload_;
	      unsigned char dataType_;
	      carma::canbus::msgType msgType_;

	    public:

	      void setAddr( unsigned short addr ) { addr_ = addr; }

	      void setPayload( char   payload ) { payload_.c = payload; }
	      void setPayload( short  payload ) { payload_.s = payload; }
	      void setPayload( long   payload ) { payload_.l = payload; }
	      void setPayload( float  payload ) { payload_.f = payload; }
	      void setPayload( double payload ) { payload_.d = payload; }
	      void setPayload( std::vector<carma::canbus::byteType> &payload )
		{
		  payload_.bytes[0] = payload[0];
		  payload_.bytes[1] = payload[1];
		  payload_.bytes[2] = payload[2];
		  payload_.bytes[3] = payload[3];
		  payload_.bytes[4] = payload[4];
		  payload_.bytes[5] = payload[5];
		  payload_.bytes[6] = payload[6];
		  payload_.bytes[7] = payload[7];
		}

	      void setDataType( unsigned char dataType ) { dataType_ = dataType; }
	      void setMsgType( carma::canbus::msgType msgType ) { msgType_ = msgType; }

	      void set( unsigned short addr, char payload, time_t timer = 0 )
		{
		  setAddr( addr );
		  setDataType( 'c' );
		  setPayload( payload );
		}

	      void set( unsigned short addr, short payload )
		{
		  setAddr( addr );
		  setDataType( 's' );
		  setPayload( payload );
		}

	      void set( unsigned short addr, long payload )
		{
		  setAddr( addr );
		  setDataType( 'l' );
		  setPayload( payload );
		}

	      void set( unsigned short addr, float payload )
		{
		  setAddr( addr );
		  setDataType( 'f' );
		  setPayload( payload );
		}

	      void set( unsigned short addr, double payload )
		{
		  setAddr( addr );
		  setDataType( 'd' );
		  setPayload( payload );
		}

	      void set( std::vector<carma::canbus::byteType> payload )
		{
		  setDataType( 'v' );
		  setPayload( payload );
		}

	      unsigned short getAddr( void ) { return addr_; }

	      void getPayload( char   &payload ) { payload = payload_.c; }
	      void getPayload( short  &payload ) { payload = payload_.s; }
	      void getPayload( long   &payload ) { payload = payload_.l; }
	      void getPayload( float  &payload ) { payload = payload_.f; }
	      void getPayload( double &payload ) { payload = payload_.d; }

	      void getPayload( std::vector<carma::canbus::byteType> &data )
		{
		  data.resize(0);
		  data.push_back( payload_.bytes[0] );
		  data.push_back( payload_.bytes[1] );
		  data.push_back( payload_.bytes[2] );
		  data.push_back( payload_.bytes[3] );
		  data.push_back( payload_.bytes[4] );
		  data.push_back( payload_.bytes[5] );
		  data.push_back( payload_.bytes[6] );
		  data.push_back( payload_.bytes[7] );
		}

	      unsigned char getDataType( void ) { return dataType_; }
	      unsigned char getMsgType( void ) { return msgType_; }

	    };
	}
    }
}



#endif // CARMA_BIMA_TELEMETRYCOMMAND_H
