#include "carma/szautil/CurlUtils.h"
#include "carma/szautil/Exception.h"

using namespace std;
using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
CurlUtils::CurlUtils() {}

/**.......................................................................
 * Destructor.
 */
CurlUtils::~CurlUtils() {}


/**.......................................................................
 * Initialize the CURL interface
 */
CURL* CurlUtils::initCurl()
{
  lastRead_.str("");

  std::ostringstream testStr;

  // global libcURL init

  curl_global_init( CURL_GLOBAL_ALL ) ;

  // create a context, sometimes known as a handle.
  // Think of it as a lookup table, or a source of config data.

  CURL* ctx = curl_easy_init() ;
  
  if(ctx == NULL) {
    ThrowError("Unable to initialize cURL interface");
  }
  
  // BEGIN: configure the handle:
  
  // no progress bar:

  curl_easy_setopt( ctx , CURLOPT_NOPROGRESS , OPTION_TRUE) ;

  // (sending response headers to stderr)

  // This next line causes a seg fault when trying to pass data to my user function
  //  curl_easy_setopt( ctx , CURLOPT_WRITEHEADER , stderr) ;

  // response content: same choices as headers
  // send it to stdout to prove that libcURL differentiates the two

  curl_easy_setopt( ctx , CURLOPT_WRITEFUNCTION, handleData);

  curl_easy_setopt( ctx , CURLOPT_WRITEDATA, (void*)this);

  return ctx;
}

/**.......................................................................
 * Cleanup the CURL interface
 */
void CurlUtils::cleanUp(CURL* ctx)
{
  // Cleanup
  
  curl_easy_cleanup(ctx);
  curl_global_cleanup();
}

/**.......................................................................
 * Perform the request specified, and return stats about it, if
 * printStats=true
 */
const CURLcode CurlUtils::performRequest(CURL* ctx, bool printStats)
{
  const CURLcode rc = curl_easy_perform( ctx ) ;

  if(printStats) {
    if(rc != CURLE_OK) {
      
      std::cerr << "Error from cURL: " << curl_easy_strerror( rc ) << std::endl ;
      std::cerr << "Error from cURL: " << std::endl;
      
    } else {
      
      // get some info about the xfer:

      double statDouble ;
      long statLong ;
      char* statString = NULL ;
    
      // known as CURLINFO_RESPONSE_CODE in later curl versions

      if(CURLE_OK == curl_easy_getinfo(ctx, CURLINFO_HTTP_CODE, &statLong)){
	COUT("Response code:  " << statLong);
      }
    
      if(CURLE_OK == curl_easy_getinfo(ctx, CURLINFO_CONTENT_TYPE, &statString)){
	COUT("Content type:   " << statString);
      }
    
      if(CURLE_OK == curl_easy_getinfo(ctx, CURLINFO_SIZE_DOWNLOAD, &statDouble)){
	COUT("Download size:  " << statDouble << " bytes");
      }
    
      if(CURLE_OK == curl_easy_getinfo(ctx, CURLINFO_SPEED_DOWNLOAD, &statDouble)){
	COUT("Download speed: " << statDouble << " bytes/sec");
      }
    
    }
  }

  return rc;
}

/**.......................................................................
 * Set up a URL to fetch
 */
void CurlUtils::setUrl(CURL* ctx, std::string  url)
{
  // Set up the target url

  curl_easy_setopt(ctx , CURLOPT_URL,  url.c_str());
}


std::string CurlUtils::getUrl(std::string url, bool printStats)
{
  CURL* ctx = initCurl();
  setUrl(ctx, url);
  const CURLcode rc = performRequest(ctx, printStats);
  cleanUp(ctx);

  return lastRead_.str();
}

std::string CurlUtils::postUserPass(std::string url, std::string user, std::string pass, std::string challenge)
{
  CURL* ctx = initCurl();
  setUrl(ctx, url);
  curl_easy_setopt(ctx, CURLOPT_POST, 1);

  std::ostringstream os;

  os << "Username=" << user << "&Password=" << pass << "&Challenge=" << challenge;

  curl_easy_setopt(ctx, CURLOPT_POSTFIELDS, os.str().c_str());
  const CURLcode rc = performRequest(ctx, true);
  cleanUp(ctx);

  return lastRead_.str();
}


size_t CurlUtils::handleData(void* buffer, size_t size, size_t nmemb, void* userp)
{
  CurlUtils* reader = (CurlUtils*)userp;
  reader->lastRead_ << (char*)buffer;
  return size*nmemb;
}
