#ifndef SZA_UTIL_MUTEXEXCEPTION_H
#define SZA_UTIL_MUTEXEXCEPTION_H

/**
 * @file MutexException.h
 * 
 * Tagged: Sat May 22 07:36:14 PDT 2004
 * 
 * @author Erik Leitch
 */
namespace sza {
  namespace util {
    
    class MutexException {
    public:
      
      /**
       * Constructor.
       */
      MutexException();
      
      /**
       * Destructor.
       */
      virtual ~MutexException();
      
    private:
    }; // End class MutexException
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_MUTEXEXCEPTION_H
