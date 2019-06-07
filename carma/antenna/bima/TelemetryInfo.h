/**@file
 * 
 *
 * <dl><dt><b>Author </b></dt><dd>Colby Gutierrez-Kraybill</dl>
 * $Revision: 1.13 $
 * $Date: 2007/10/26 21:54:30 $
 * $Id: TelemetryInfo.h,v 1.13 2007/10/26 21:54:30 colby Exp $
 */

#ifndef CARMA_ANTENNA_BIMA_TELEMETRYINFO_H
#define CARMA_ANTENNA_BIMA_TELEMETRYINFO_H

#include <iostream>
#include <string>

#include "carma/canbus/Utilities.h"
#include "carma/util/Trace.h"

namespace carma
{
  namespace antenna
    {
      namespace bima
	{
	  class TelemetryInfo
	    {

	    public:
	      TelemetryInfo()
		{
		  for ( int i = 0; i < 7; i++ )
		    {
		      _name[i] = new std::string( "(unknown)" );
		      _comment[i] =
			new std::string( "This sub-telemetry id has no definition" );
		      _msgId = (carma::canbus::msgType)0;
		      _addr[i] = '\0';
		      _size[i] = 2; // default size in bytes
		      _len = 0;
                      _sem_num[i] = _global_sem_num++;
                      CPTRACE( carma::util::Trace::TRACE7, "Created TelemetryInfo instance: "
                               << "_name[" << i << "]: " << _name[i]->c_str()
                               << " _comment[" << i << "]: " << _comment[i]->c_str()
                               << " _msgId: 0x" << std::hex << _msgId
                               << " _addr[" << i << "]: " << _addr[i]
                               << " _size[" << i << "]: " << _size[i]
                               << " _len: " << _len
                               << " _sem_num: " << _sem_num[i] );
		    }
		}

	      ~TelemetryInfo()
		{
		}

	      // number of individual items in packet
	      int getLength()
		{
		  return _len;
		}

	      // size in bytes of this particular item
	      int getSize( int seq )
		{
		  return _size[seq];
		}

	      const char *getName( int seq )
		{
		  return _name[seq]->c_str();
		}
	      
	       unsigned short getAddr( int seq )
		{
		  return _addr[seq];
		}

               unsigned short getSemNum( int seq )
		{
		  return _sem_num[seq];
		}
     
	      void setMsgId( carma::canbus::msgType msgId )
		{
		  _msgId = msgId;
		}

	      void setPacketName( char *theName )
		{
		  _packetName = new std::string( theName );
		}

	      std::string getPacketName()
	      {
		return  std::string( *_packetName );
	      }

              void resetGlobalNum()
              {
                _global_sem_num = 0;
              }

	      void set( int sequenceNum, char *name, char *comment, short addr, int size )
		{
                  CPTRACE( carma::util::Trace::TRACE7, "TelemetryInfo.set("
                           << sequenceNum << ", " << name << ", " << comment << ", "
                           << addr << ", " << size << ")" );

		  if ( sequenceNum > -1 && sequenceNum < 8 )
		    {
                      CPTRACE( carma::util::Trace::TRACE7,
                               "_name[" << sequenceNum << "]"
                               << ": " << (char *)_name[sequenceNum]->c_str() );
   
		      if ( _name[sequenceNum] != (std::string *)NULL )
			delete _name[sequenceNum];

		      _name[sequenceNum] = new std::string(name);

                      CPTRACE( carma::util::Trace::TRACE7,
                               "_comment[" << sequenceNum << "]"
                               << ": " << (char *)_comment[sequenceNum]->c_str() );
   
		      if ( _comment[sequenceNum] != (std::string *)NULL )
			delete _comment[sequenceNum];

		      _comment[sequenceNum] = new std::string(comment);

		      _addr[sequenceNum] = addr;
		      _size[sequenceNum] = size;

		      if ( (sequenceNum+1) > _len )
			_len = (sequenceNum+1);

		    }
		  
		}

	      std::string *_packetName;
	      std::string *_name[8];
	      std::string *_comment[8];
	      carma::canbus::msgType _msgId;
	      unsigned short _addr[8];
	      int _size[8];
	      int _len;
              unsigned short _sem_num[8];
              static unsigned short _global_sem_num;
	      

	    }; // class TelemetryInfo
    
	} // namespace bima
    } // namespace antenna
} // namespace carma

#endif // CARMA_ANTENNA_BIMA_TELEMETRYINFO_H
