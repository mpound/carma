#ifndef SZA_UTIL_ARCHIVERWRITER_H
#define SZA_UTIL_ARCHIVERWRITER_H

/**
 * @file ArchiverWriter.h
 * 
 * Tagged: Fri 27-Jan-06 14:31:33
 * 
 * @author Erik Leitch
 */
#include "carma/szaarrayutils/arraymap.h"

class Archiver;

namespace sza {
  namespace util {

    class ArchiverWriter {
    public:
      
      /**
       * Constructor.
       */
      ArchiverWriter() {};
      
      /**
       * Destructor.
       */
      virtual ~ArchiverWriter() {};
      
      virtual void closeArcfile() {};
      virtual void flushArcfile() {};
      virtual int writeIntegration() {return 0;};
      virtual bool isOpen() {return false;};

    }; // End class ArchiverWriter
    
  } // End namespace util
} // End namespace sza


#endif // End #ifndef SZA_UTIL_ARCHIVERWRITER_H
