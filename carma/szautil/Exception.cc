#include "carma/szautil/Directives.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/LogStream.h"

using namespace std;
using namespace sza::util;

/**.......................................................................
 * Constructs an Error with a detailed message.
 * 
 * @param str string containing message.
 * @param filename where exception originated.
 * @param lineNumber where exception occurred.
 */
Exception::Exception(string str, const char* fileName, const int lineNumber, bool doReport) 
#if 0
  : ErrorException(str, fileName, lineNumber) 
#endif
{
#if !(0)
  message_ = str;
#endif

  if(doReport) 
    report((std::string)str);
}; 

/**.......................................................................
 * Constructs an Error with a detailed message.
 * 
 * @param os ostringstream containing message
 * @param filename where exception originated.
 * @param lineNumber exception occurred on.
 */
Exception::Exception(ostringstream& os, const char * fileName, 
		     const int lineNumber, bool doReport) 
#if 0
  : ErrorException(os, fileName, lineNumber)
#endif
{
#if !(0)
  message_ = os.str();
#endif

  if(doReport) 
    report();
};

/**.......................................................................
 * Constructs an Error with a detailed message.  
 * 
 * @param ls LogStream containing message 
 * @param fileName where exception originated.  
 * @param lineNumber where exception occurred.
 */
Exception::Exception(LogStream& ls, 
		     const char* fileName, const int lineNumber, bool doReport) 
#if 0
  : ErrorException(ls.getMessage(), fileName, lineNumber)
#endif
{
#if !(0)
  message_ = ls.getMessage();
#endif

  if(doReport) 
    report();
};

/** 
 * Constructor with a log stream.
 */
Exception::Exception(LogStream* ls, 
	  const char* fileName, const int lineNumber, bool doReport) 
#if 0
  : ErrorException(ls->getMessage(), fileName, lineNumber)
#endif
{
#if !(0)
  message_ = ls->getMessage();
#endif

  if(doReport) 
    report();
};

/**.......................................................................
 * Destructor
 */
Exception::~Exception() {}

