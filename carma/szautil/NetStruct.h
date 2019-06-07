// $Id: NetStruct.h,v 1.2 2011/06/08 18:40:13 eml Exp $

#ifndef SZA_UTIL_NETSTRUCT_H
#define SZA_UTIL_NETSTRUCT_H

/**
 * @file carma/szautil/NetDat.h
 * 
 * Tagged: Wed Jul  6 15:09:03 PDT 2005
 * 
 * @version: $Revision: 1.2 $, $Date: 2011/06/08 18:40:13 $
 * 
 * @author username: Command not found.
 */
#include "carma/szautil/NetDat.h"
#include "carma/szautil/DataType.h"

namespace sza {
  namespace util {

    class NetStruct : public NetDat {
    public:

      /**
       * Constructor.
       */
      NetStruct();

      /**
       * Constructor.
       */
      NetStruct(const NetStruct& netStruct);
      NetStruct(NetStruct& netStruct);
      void operator=(const NetStruct& netStruct);
      void operator=(NetStruct& netStruct);

      /**
       * Destructor.
       */
      virtual ~NetStruct();

      /**
       * Add a member to the internal vector of members
       */
      virtual void addMember(NetDat* netDat, bool alloc=false);

      void addVar(bool& val);
      void addVar(unsigned char& val);
      void addVar(char& val);
      void addVar(short& val);
      void addVar(unsigned short& val);
      void addVar(int& val);
      void addVar(unsigned int& val);
      void addVar(float& val);
      void addVar(double& val);

      void addVar(bool* val, unsigned nEl);
      void addVar(unsigned char* val, unsigned nEl);
      void addVar(char* val, unsigned nEl);
      void addVar(short* val, unsigned nEl);
      void addVar(unsigned short* val, unsigned nEl);
      void addVar(int* val, unsigned nEl);
      void addVar(unsigned int* val, unsigned nEl);
      void addVar(float* val, unsigned nEl);
      void addVar(double* val, unsigned nEl);

      void addVar(std::string& val);
      void addVar(std::vector<unsigned char>& val);

      /**
       * Add a variable to the internal vector of members
       */
      void addVar(sza::util::DataType::Type type, void* vPtr, unsigned nEl);
      void addVar(sza::util::DataType::Type type, void* vPtr);

      /**
       * De-serialize data into this struct
       */
      void deserialize(const std::vector<unsigned char>& bytes);
      void deserialize(const unsigned char* bytes);

      /**
       * Return the size of this object
       */
      unsigned size();

    private:

      // A vector of objects contained in this class

      std::vector<NetDat::Info> members_;

      void resize();

      /**
       * Serialize the data in this struct
       */
      void serialize();

      /**
       * Check the size of an array against our size
       */
      void checkSize(const std::vector<unsigned char>& bytes);

    }; // End class NetStruct

  } // End namespace util
} // End namespace sza


#define NETSTRUCT_UCHAR(name) \
addVar(sza::util::DataType::UCHAR, (void*)&name, 1)

#define NETSTRUCT_UCHAR_ARR(name, nEl) \
addVar(sza::util::DataType::UCHAR, (void*)name, nEl)

#define NETSTRUCT_CHAR(name) \
addVar(sza::util::DataType::CHAR, (void*)&name, 1)

#define NETSTRUCT_CHAR_ARR(name, nEl) \
addVar(sza::util::DataType::CHAR, (void*)name, nEl)

#define NETSTRUCT_BOOL(name) \
addVar(sza::util::DataType::BOOL, (void*)&name, 1)

#define NETSTRUCT_BOOL_ARR(name, nEl) \
addVar(sza::util::DataType::BOOL, (void*)name, nEl)

#define NETSTRUCT_USHORT(name) \
addVar(sza::util::DataType::USHORT, (void*)&name, 1)

#define NETSTRUCT_USHORT_ARR(name, nEl) \
addVar(sza::util::DataType::USHORT, (void*)name, nEl)

#define NETSTRUCT_SHORT(name) \
addVar(sza::util::DataType::SHORT, (void*)&name, 1)

#define NETSTRUCT_SHORT_ARR(name, nEl) \
addVar(sza::util::DataType::SHORT, (void*)name, nEl)

#define NETSTRUCT_UINT(name) \
addVar(sza::util::DataType::UINT, (void*)&name, 1)

#define NETSTRUCT_UINT_ARR(name, nEl) \
addVar(sza::util::DataType::UINT, (void*)name, nEl)

#define NETSTRUCT_INT(name) \
addVar(sza::util::DataType::INT, (void*)&name, 1)

#define NETSTRUCT_INT_ARR(name, nEl) \
addVar(sza::util::DataType::INT, (void*)name, nEl)

#define NETSTRUCT_ULONG(name) \
addVar(sza::util::DataType::ULONG, (void*)&name, 1)

#define NETSTRUCT_ULONG_ARR(name, nEl) \
addVar(sza::util::DataType::ULONG, (void*)name, nEl)

#define NETSTRUCT_LONG(name) \
addVar(sza::util::DataType::LONG, (void*)&name, 1)

#define NETSTRUCT_LONG_ARR(name, nEl) \
addVar(sza::util::DataType::LONG, (void*)name, nEl)

#define NETSTRUCT_FLOAT(name) \
addVar(sza::util::DataType::FLOAT, (void*)&name, 1)

#define NETSTRUCT_FLOAT_ARR(name, nEl) \
addVar(sza::util::DataType::FLOAT, (void*)name, nEl)

#define NETSTRUCT_DOUBLE(name) \
addVar(sza::util::DataType::DOUBLE, (void*)&name, 1)

#define NETSTRUCT_DOUBLE_ARR(name, nEl) \
addVar(sza::util::DataType::DOUBLE, (void*)name, nEl)

#define NETSTRUCT_UCHAR_VEC(name) \
addVar(sza::util::DataType::UCHAR, (void*)&name)

#define NETSTRUCT_STRING(name) \
addVar(sza::util::DataType::STRING, (void*)&name)

#endif // End #ifndef SZA_UTIL_NETSTRUCT_H
