#ifndef CARMA_DBMS_SYSLOGMMAPOPERATOR_H
#define CARMA_DBMS_SYSLOGMMAPOPERATOR_H

#include "carma/dbms/SyslogMMAPFile.h"
#include <string>
#include <map>

#include <sys/types.h>
#include <regex.h>

/**
 * @file
 * $Id: SyslogMMAPOperator.h,v 1.3 2008/04/23 21:40:41 abeard Exp $
 * @author Colby Gutierrez-Kraybill
 */

namespace carma
{
  namespace dbms
  {

    /**
     * This class is responsible for performing various
     * operations on the syslog cache.  Providing easy
     * to use access when filtering/searching.
     */
    class SyslogMMAPOperator
    {
      public:

	/**
	 * Constructor. 
	 * @param mmapFileName the mmap file to read from and write to.
	 * @param writer is this instantiation a writer to the file default false.
	 */
	SyslogMMAPOperator( SyslogMMAPFile &smapf );

	/** The destructor.  */
	~SyslogMMAPOperator();

	void setWayBackMachine( double mjd );
	void resetWayBackMachine();
	SyslogMessage *nextMessage();
	int messagesWaiting()
	{
	  int adj = 0;

	  if ( _pos > _cpos )
	    adj = _pos;

	  return ( (_cpos + adj) - _pos );
	};

	void setFilter( ::std::string &myFilter );
	::std::string::size_type instantSetSize() { return _setSize; };

	::std::string toString();

	bool hasFilter() { return _hasFilter; };
	void resetFilter();
	bool doesMatch( SyslogMessage *msg );

	::std::string getFilter() { return _filter; };

	typedef enum
	{
	  ALL,
	  PROGRAM,
	  LOGNAME,
	  NDC,
	  MESSAGE
	} fieldList;

      private:
	SyslogMMAPFile &_smapf;
	::std::string _filter;
	::std::string::size_type _setSize;
	::std::string _mjdOffsetString;
	::std::string _opString;
	bool _opGreater;
	::std::string _valString;
	::std::string _textString;
	::std::string _fieldString;
	int _fieldCheck;
	regex_t *_regex;

	int _prioInt;

	double _mjdOffset;
	long _cpos, _pos;
	bool _current;
	bool _hasFilter;
    };
  }
}

#endif // CARMA_DBMS_SYSLOGMMAPOPERATOR_H
