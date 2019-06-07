// $Id: SzaMiriadIo.h,v 1.1 2010/12/13 21:06:32 eml Exp $

#ifndef SZA_UTIL_SZAMIRIADIO_H
#define SZA_UTIL_SZAMIRIADIO_H

/**
 * @file SzaMiriadIo.h
 * 
 * Tagged: Mon Oct  3 17:20:31 PDT 2005
 * 
 * @version: $Revision: 1.1 $, $Date: 2010/12/13 21:06:32 $
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/MiriadIo.h"

namespace sza {
  namespace util {

    class SzaMiriadIo : public MiriadIo {
    public:

      /**
       * Constructor.
       */
      SzaMiriadIo();

      /**
       * Destructor.
       */
      virtual ~SzaMiriadIo();

    private:
    }; // End class SzaMiriadIo

  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_SZAMIRIADIO_H
