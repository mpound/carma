// $Id: NetVar.h,v 1.3 2013/07/09 17:12:00 eml Exp $

#ifndef SZA_UTIL_NETVAR_H
#define SZA_UTIL_NETVAR_H

/**
 * @file carma/szautil/NetDat.h
 * 
 * Tagged: Wed Jul  6 13:31:47 PDT 2005
 * 
 * @version: $Revision: 1.3 $, $Date: 2013/07/09 17:12:00 $
 * 
 * @author username: Command not found.
 */
#include "carma/szautil/DataType.h"

#include "carma/szautil/NetDat.h"

namespace sza {
  namespace util {

    class NetStruct;

    class NetVar : public NetDat {
    public:

      /**
       * Constructor.
       */
      NetVar(sza::util::DataType::Type type, void* vPtr, unsigned nEl, bool convert=false);

      // Constructor for resizeable vectors

      NetVar(sza::util::DataType::Type type, void* vPtr, bool convert=false);

      /**
       * Destructor.
       */
      virtual ~NetVar();

      /**
       * Copy constructor
       */
      NetVar(const NetVar& netVar);
      NetVar(NetVar& netVar);

      /**
       * Deserialize data into this object
       */
      void deserialize(const std::vector<unsigned char>& bytes);

      unsigned nEl();

      void checkSize(const std::vector<unsigned char>& bytes);

      unsigned char* getPtr();

      unsigned size();

      bool isResizeable();

    public:

      // The type of this variable

      sza::util::DataType::Type type_;

      // True if this object is resizeable

      bool resizeable_;

      // Pointer to where the start of the memory resides for this
      // variable

      void* vPtr_;

      // Number of elements in this variable

      unsigned nEl_;

      // Size in bytes of each data type

      unsigned nBytePerEl_;

      // True if we should convert to network byte order on serialization

      bool convert_;

      /**
       * Serialize the data for this variable
       */
      void serialize();

      /**
       * Deserialize data into this object
       */
      void deserialize(const unsigned char* bytes);

      /**
       * Resize
       */
      void prependNel(unsigned char* dest);
      void parseNel(const unsigned char* src);
      void setNel(unsigned nEl);
      unsigned sizeOfPrefix();
      void resize();
      bool convert();

    }; // End class NetVar

  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_NETVAR_H
