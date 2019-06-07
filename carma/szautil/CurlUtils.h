// $Id: CurlUtils.h,v 1.1 2010/12/13 21:06:29 eml Exp $

#ifndef SZA_UTIL_CURLUTILS_H
#define SZA_UTIL_CURLUTILS_H

/**
 * @file CurlUtils.h
 * 
 * Tagged: Fri Aug 14 16:23:24 PDT 2009
 * 
 * @version: $Revision: 1.1 $, $Date: 2010/12/13 21:06:29 $
 * 
 * @author tcsh: username: Command not found.
 */
#include <sstream>
#include <curl/curl.h>

namespace sza {
  namespace util {

    class CurlUtils {
    public:

      /**
       * Constructor.
       */
      CurlUtils();

      /**
       * Destructor.
       */
      virtual ~CurlUtils();

      std::string getUrl(std::string url, bool printStats=false);

      std::string postUserPass(std::string url, std::string user, std::string pass, std::string challenge);

    protected:

      enum {
	OPTION_FALSE = 0,
	OPTION_TRUE  = 1
      };

      // A stream containing the last read information returned from
      // the URL fetch

      std::ostringstream lastRead_;

      // A function that will be called to handle data from a call to
      // 'perform'

      static size_t handleData(void* buffer, size_t size, size_t nmemb, void* userp);

      
      // Initialize the CURL interface

      CURL* initCurl();
      
      // Cleanup the CURL interface

      void cleanUp(CURL* ctx);

      // Perform the request specified, and return stats about it, if
      // printStats=true

      const CURLcode performRequest(CURL* ctx, bool printStats);

      // Set up a URL to fetch

      void setUrl(CURL* ctx, std::string  url);

    }; // End class CurlUtils 

  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_CURLUTILS_H
