// $Id: UranusModelReader.h,v 1.1 2010/12/13 21:06:33 eml Exp $

#ifndef SZA_UTIL_URANUSMODELREADER_H
#define SZA_UTIL_URANUSMODELREADER_H

/**
 * @file UranusModelReader.h
 * 
 * Tagged: Wed Oct 29 17:07:08 PDT 2008
 * 
 * @version: $Revision: 1.1 $, $Date: 2010/12/13 21:06:33 $
 * 
 * @author username: Command not found.
 */
#include "carma/szautil/Length.h"
#include "carma/szautil/ModelReaderNew.h"

namespace sza {
  namespace util {

    class UranusModelReader : public ModelReaderNew {
    public:

      /**
       * Constructors.
       */
      UranusModelReader();
      UranusModelReader(std::string dir, std::string fileName, std::vector<Frequency> freqs);

      /**
       * Destructor.
       */
      virtual ~UranusModelReader();

      void readRecord(InputStream* stream);

      Temperature calcTemp(Frequency& freq);

    private:

      static Length equatPhysDiam_;
      static Length polarPhysDiam_;
			      
    }; // End class UranusModelReader

  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_URANUSMODELREADER_H
