// $Id: NetDat.h,v 1.2 2011/06/08 18:40:13 eml Exp $

#ifndef SZA_UTIL_NETDAT_H
#define SZA_UTIL_NETDAT_H

/**
 * @file carma/szautil/NetDat.h
 * 
 * Tagged: Wed Jul  6 13:41:09 PDT 2005
 * 
 * @version: $Revision: 1.2 $, $Date: 2011/06/08 18:40:13 $
 * 
 * @author username: Command not found.
 */
#include <vector>

namespace sza {
  namespace util {

    class NetStruct;
    class NetUnion;

    class NetDat {
    public:

      struct Info {
	NetDat* datPtr_;
	bool alloc_;

	Info() {
	  datPtr_ = 0;
	  alloc_ = false;
	};

	Info(NetDat* datPtr, bool alloc) {
	  datPtr_ = datPtr;
	  alloc_  = alloc;
	};

	Info(const Info& info) {
	  datPtr_ = info.datPtr_;
	  alloc_  = info.alloc_;
	};
      };

      /**
       * Constructor.
       */
      NetDat();

      /**
       * Constructor.
       */
      NetDat(const NetDat& netDat);
      NetDat(NetDat& netDat);
      NetDat& operator=(const NetDat& netDat);
      NetDat& operator=(NetDat& netDat);

      /**
       * Destructor.
       */
      virtual ~NetDat();

      /**
       * Return size in bytes
       */
      virtual unsigned size();

      /**
       * Return a reference to serialized data
       */
      virtual std::vector<unsigned char>& getSerializedData();
      virtual std::vector<unsigned char>& getSerializedDataNoResize();

      /**
       * Fill an external pointer with serialized data
       */
      virtual void packSerializedData(unsigned char* bytes);

      /**
       * Deserialize data into this object
       */
      virtual void deserialize(const std::vector<unsigned char>& bytes);
      
      /**
       * Return the maximum size (for variable sized objects) in bytes
       */
      unsigned maxSize();

    protected:

      friend class NetStruct;
      friend class NetUnion;

      /**
       * Memory for the serialized version of this data
       */
      std::vector<unsigned char> bytes_;

      /**
       * Size of this object in bytes
       */
      unsigned size_;

      /**
       * Resize the underlying byte array
       */
      virtual void resize(unsigned size);

      // Recalculate the size of the underlying byte array and resize

      virtual void resize();

      /**
       * Return a reference to internal data
       */
      unsigned char* const getSerializedDataPtr();

      /**
       * Serialize the data
       */
      virtual void serialize();

      /**
       * Private deserialization method
       */
      virtual void deserialize(const unsigned char* bytes)=0;

      /**
       * Check an array size against ours
       */
      virtual void checkSize(const std::vector<unsigned char>& bytes);

    }; // End class NetDat

  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_NETDAT_H
