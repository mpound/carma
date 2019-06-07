#ifndef CARMA_DBMS_SYSLOGMESSAGE_H
#define CARMA_DBMS_SYSLOGMESSAGE_H


#include <iosfwd>
#include <sstream>
#include <string>
#include <cstring>

#include "carma/util/Time.h"

namespace carma
{
  namespace dbms
  {
    class SyslogMessage
    {
      public:

	static const size_t S_SHORT_LEN = 10;
	static const size_t S_STD_LEN = 80;
	static const size_t S_LONG_LEN = 1024;

	size_t setDate( double v )
	{
	  _mjd = v;
	  return sizeof( double );
	}
	double getDate() { return _mjd; };

	size_t setLog( const char *v )
	{
	  return setLog( (char *)v );
	}
	size_t setLog( char *v )
	{
	  size_t r = strnlen(v, S_STD_LEN)+1;
	  memset( _log, 0, r );
	  strncpy( _log, v, r );
	  _log[r] = '\0';
	  return r;
	}
	char *getLog() { return _log; };

	size_t setHost( const char *v )
	{
	  return setHost( (char *)v );
	}

	size_t setHost( char *v )
	{
	  size_t r = strnlen(v, S_STD_LEN)+1;
	  memset( _host, 0, r );
	  strncpy( _host, v, r );
	  _host[r] = '\0';
	  return r;
	}
	char *getHost() { return _host; };

	size_t setProgram( const char *v )
	{
	  return setProgram( (char *)v );
	}

	size_t setProgram( char *v )
	{
	  size_t r = strnlen(v, S_STD_LEN)+1;
	  memset( _program, 0, r );
	  strncpy( _program, v, r );
	  _program[r] = '\0';
	  return r;
	}
	char *getProgram() { return _program; };

	size_t setPrio( const char *v )
	{
	  return setPrio( (char *)v );
	}
	size_t setPrio( char *v )
	{
	  size_t r = strnlen(v, S_SHORT_LEN)+1;
	  memset( _prio, 0, r );
	  strncpy( _prio, v, r );
	  _prio[r] = '\0';
	  return r;
	}
	char *getCharPrio() { return _prio; };

	int getIntPrio()
	{
	  if ( ! strcmp( _prio, "DEBUG" ) )
	    return 25;
	  if ( ! strcmp( _prio, "INFO" ) )
	    return 30;
	  if ( ! strcmp( _prio, "NOTICE" ) )
	    return 40;
	  if ( ! strcmp( _prio, "WARN" ) )
	    return 50;
	  if ( ! strcmp( _prio, "ERROR" ) )
	    return 60;
	  if ( ! strcmp( _prio, "CRIT" ) )
	    return 70;
	  if ( ! strcmp( _prio, "ALERT" ) )
	    return 80;
	  if ( ! strcmp( _prio, "FATAL" ) )
	    return 90;

      return -1; // stifle warning
	};

	size_t setFullyQualifiedProgramName( const char *v )
	{
	  return setFullyQualifiedProgramName( (char *)v );
	}
	size_t setFullyQualifiedProgramName( char *v )
	{
	  size_t r = strnlen(v, S_STD_LEN)+1;
	  memset( _fullyQualifiedProgramName, 0, r );
	  strncpy( _fullyQualifiedProgramName, v, r );
	  _fullyQualifiedProgramName[r] = '\0';
	  return r;
	}
	char *getFullyQualifiedProgramName() {
	    return _fullyQualifiedProgramName; };

	size_t setThreadInfo( const char *v )
	{
	  return setThreadInfo( (char *)v );
	}
	size_t setThreadInfo( char *v )
	{
	  size_t r = strnlen(v, S_STD_LEN)+1;
	  memset( _threadInfo, 0, r );
	  strncpy( _threadInfo, v, r );
	  _threadInfo[r] = '\0';
//	  std::cout << "  v:{" << v << "}" << std::endl;
//	  std::cout << "  t:{" << _threadInfo << "}" << std::endl;
	  return r;
	}
	char *getThreadInfo() { return _threadInfo; };

	size_t setMessage( const char *v )
	{
	  return setMessage( (char *)v );
	}
	size_t setMessage( char *v )
	{
	  size_t r = strnlen(v, S_LONG_LEN)+1;
	  memset( _message, 0, r );
	  strncpy( _message, v, r );
	  _message[r] = '\0';
//	  std::cout << "  v:{" << v << "}" << std::endl;
//	  std::cout << "  m:{" << _message << "}" << std::endl;
	  return r;
	}
	char *getMessage() { return _message; };

	size_t length()
	{
	  size_t len = sizeof(double);
	  len += strnlen(_log,S_STD_LEN)+1;
	  len += strnlen(_host,S_STD_LEN)+1;
	  len += strnlen(_program,S_STD_LEN)+1;
	  len += strnlen(_prio,S_SHORT_LEN)+1;
	  len += strnlen(_fullyQualifiedProgramName,S_STD_LEN)+1;
	  len += strnlen(_threadInfo,S_STD_LEN)+1;
	  len += strnlen(_message,S_LONG_LEN)+1;

	  return len;
	}

	std::string toString() const
	{
	  std::ostringstream oss;
	  oss.precision(12);
	  oss
	    << "date:'"<< ::carma::util::Time::getDateTimeString(_mjd)
	    << "' log:'"<< _log
	    << "' host:'"<< _host
	    << "' program:'"<< _program
	    << "' prio:'"<< _prio
	    << "' fqpn:'"<< _fullyQualifiedProgramName
	    << "' threadInfo:'"<< _threadInfo
	    << "' message:'"<< _message << "'";

	  return oss.str();
	}

      private:
	// no wide characters/internationalization.
	// poop.  Add on 5 bytes to each, just
	// to give breathing room.
	double _mjd;
	char _log[S_STD_LEN+5];
	char _host[S_STD_LEN+5];
	char _program[S_STD_LEN+5];
	char _prio[S_SHORT_LEN+5];
	char _fullyQualifiedProgramName[S_STD_LEN+5];
	char _threadInfo[S_STD_LEN+5];
	char _message[S_LONG_LEN+5];

    }; // class SyslogMessage
  } // namespace dbms
} // namespace carma

namespace
{
  ::std::ostream& operator<<(::std::ostream &os, const ::carma::dbms::SyslogMessage *msg)
  {
    os << msg->toString();
    return os;
  }

  ::std::ostream& operator<<(::std::ostream &os, const ::carma::dbms::SyslogMessage &msg)
  {
    os << msg.toString();
    return os;
  }
}

#endif // CARMA_DBMS_SYSLOGMESSAGE_H
