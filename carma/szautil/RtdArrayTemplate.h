// $Id: RtdArrayTemplate.h,v 1.1 2013/07/10 15:28:30 eml Exp $

#ifndef SZA_UTIL_RTDARRAYTEMPLATE_H
#define SZA_UTIL_RTDARRAYTEMPLATE_H

/**
 * @file RtdArrayTemplate.h
 * 
 * Tagged: Tue Aug 24 16:09:09 PDT 2010
 * 
 * @version: $Revision: 1.1 $, $Date: 2013/07/10 15:28:30 $
 * 
 * @author username: Command not found.
 */
#include "carma/szautil/NetDat.h"
#include "carma/szautil/DataType.h"

#include "carma/szaarrayutils/arraytemplate.h"

namespace sza {

  namespace array {
    class NetBuf;
  }

  namespace util {

    class RtdArrayTemplate : public NetDat {
    public:

      /**
       * Constructor.
       */
      RtdArrayTemplate();
      RtdArrayTemplate(ArrayTemplate* arrayTemplate);

      /**
       * Destructor.
       */
      virtual ~RtdArrayTemplate();

      void serialize();
      void deserialize(const std::vector<unsigned char>& bytes);
      void deserialize(const unsigned char* bytes);
      ArrayTemplate* getArrayTemplate();

      void print();
      void constructAsciiTemplate();
      void compressAsciiTemplate();

      void writeToFile();
      
    public:

      ArrayTemplate* arrayTemplate_;
      bool external_;
      std::string asciiTemplate_;
      std::map<unsigned, RegBlockTemp*> regTempMap_;

      void addTo(std::vector<unsigned char>& arr, unsigned char* ptr, unsigned nbyte);

      void setTo(ArrayTemplate* arrayTemplate);

    }; // End class RtdArrayTemplate

  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_RTDARRAYTEMPLATE_H
