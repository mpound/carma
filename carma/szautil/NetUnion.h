// $Id: NetUnion.h,v 1.4 2013/07/09 17:12:00 eml Exp $

#ifndef SZA_UTIL_NETUNION_H
#define SZA_UTIL_NETUNION_H

/**
 * @file carma/szautil/NetDat.h
 * 
 * Tagged: Tue Jun 28 15:51:41 PDT 2005
 * 
 * @version: $Revision: 1.4 $, $Date: 2013/07/09 17:12:00 $
 * 
 * @author username: Command not found.
 */
#include <map>
#include "carma/szautil/DataType.h"

#include "carma/szautil/NetDat.h"

namespace sza {
  namespace util {

    class NetUnion : public NetDat {
    public:

      static const unsigned NETUNION_UNKNOWN = 0;

      /**
       * Constructor.
       */
      NetUnion();

      /**
       * Conunionor.
       */
      NetUnion(const NetUnion& netUnion);
      NetUnion(NetUnion& netUnion);
      void operator=(const NetUnion& netUnion);
      void operator=(NetUnion& netUnion);

      /**
       * Destructor.
       */
      virtual ~NetUnion();

      /**
       * Register a member
       */
      void addMember(unsigned id, NetDat* member=0, bool alloc=false);

      /**
       * Add a variable to the internal vector of members
       */
      void addVar(unsigned id, sza::util::DataType::Type type, 
		  void* vPtr, unsigned nEl, bool convert=false);

      // Method for vectors

      void addVar(unsigned id, sza::util::DataType::Type type, 
		  void* vPtr, bool convert=false);

      /**
       * Add just an id
       */
      void addCase(unsigned id);

      /**
       * Get a member by id
       */
      NetDat* const findMember(unsigned id);

      bool memberIsValid(unsigned id);

      /**
       * Find a member
       */
      NetDat* const getMember(unsigned id);

      /**
       * Set the internal id to the requested member
       */
      virtual void setTo(unsigned id);

      /**
       * Return the message type
       */
      unsigned getType();

      /**
       * Return the size of the member associated with this id
       */
      unsigned sizeOf(unsigned id);

      /**
       * Return the current size of this object
       */
      unsigned size();

      /**
       * Return the maximum size of this object
       */
      unsigned maxSize();

      /**
       * De-serialize data into this struct
       */
      virtual void deserialize(const std::vector<unsigned char>& bytes);
      virtual void deserializeNativeOrder(const std::vector<unsigned char>& bytes);
      void deserialize(const unsigned char* bytes, unsigned size);
      void deserializeNativeOrder(const unsigned char* bytes, unsigned size);
      void deserialize(const unsigned char* bytes);

    public:

      // A map of members contained in this class

      std::map<unsigned int, NetDat::Info> members_;

      // The member currently selected

      unsigned int id_;

      /**
       * Check the size of an array against our size
       */
      void checkSize(const std::vector<unsigned char>& bytes);

      /**
       * Check the size of an array against our size
       */
      void checkSize(const std::vector<unsigned char>& bytes, unsigned id);

      /**
       * Recalculate and resize this object
       */
      virtual void resize();

      /**
       * Serialize the data in this struct
       */
      void serialize();

    protected:

      void addVar(unsigned id, bool& val, bool convert=false);
      void addVar(unsigned id, unsigned char& val, bool convert=false);
      void addVar(unsigned id, char& val, bool convert=false);
      void addVar(unsigned id, short& val, bool convert=false);
      void addVar(unsigned id, unsigned short& val, bool convert=false);
      void addVar(unsigned id, int& val, bool convert=false);
      void addVar(unsigned id, unsigned int& val, bool convert=false);
      void addVar(unsigned id, float& val, bool convert=false);
      void addVar(unsigned id, double& val, bool convert=false);

      void addVar(unsigned id, bool* val, unsigned nEl, bool convert=false);
      void addVar(unsigned id, unsigned char* val, unsigned nEl, bool convert=false);
      void addVar(unsigned id, char* val, unsigned nEl, bool convert=false);
      void addVar(unsigned id, short* val, unsigned nEl, bool convert=false);
      void addVar(unsigned id, unsigned short* val, unsigned nEl, bool convert=false);
      void addVar(unsigned id, int* val, unsigned nEl, bool convert=false);
      void addVar(unsigned id, unsigned int* val, unsigned nEl, bool convert=false);
      void addVar(unsigned id, float* val, unsigned nEl, bool convert=false);
      void addVar(unsigned id, double* val, unsigned nEl, bool convert=false);

      void addVar(unsigned id, std::string& val, bool convert=false);
      void addVar(unsigned id, std::vector<unsigned char>& val, bool convert=false);

    }; // End class NetUnion

  } // End namespace util
} // End namespace sza

#define NETUNION_UCHAR(id, name) \
addVar(id, sza::util::DataType::UCHAR, (void*)&name, 1)

#define NETUNION_UCHAR_ARR(id, name, nEl) \
addVar(id, sza::util::DataType::UCHAR, (void*)name, nEl)

#define NETUNION_CHAR(id, name) \
addVar(id, sza::util::DataType::CHAR, (void*)&name, 1)

#define NETUNION_CHAR_ARR(id, name, nEl) \
addVar(id, sza::util::DataType::CHAR, (void*)name, nEl)

#define NETUNION_BOOL(id, name) \
addVar(id, sza::util::DataType::BOOL, (void*)&name, 1)

#define NETUNION_BOOL_ARR(id, name, nEl) \
addVar(id, sza::util::DataType::BOOL, (void*)name, nEl)

#define NETUNION_USHORT(id, name) \
addVar(id, sza::util::DataType::USHORT, (void*)&name, 1)

#define NETUNION_USHORT_ARR(id, name, nEl) \
addVar(id, sza::util::DataType::USHORT, (void*)name, nEl)

#define NETUNION_SHORT(id, name) \
addVar(id, sza::util::DataType::SHORT, (void*)&name, 1)

#define NETUNION_SHORT_ARR(id, name, nEl) \
addVar(id, sza::util::DataType::SHORT, (void*)name, nEl)

#define NETUNION_UINT(id, name) \
addVar(id, sza::util::DataType::UINT, (void*)&name, 1)

#define NETUNION_UINT_ARR(id, name, nEl) \
addVar(id, sza::util::DataType::UINT, (void*)name, nEl)

#define NETUNION_INT(id, name) \
addVar(id, sza::util::DataType::INT, (void*)&name, 1)

#define NETUNION_INT_ARR(id, name, nEl) \
addVar(id, sza::util::DataType::INT, (void*)name, nEl)

#define NETUNION_ULONG(id, name) \
addVar(id, sza::util::DataType::ULONG, (void*)&name, 1)

#define NETUNION_ULONG_ARR(id, name, nEl) \
addVar(id, sza::util::DataType::ULONG, (void*)name, nEl)

#define NETUNION_LONG(id, name) \
addVar(id, sza::util::DataType::LONG, (void*)&name, 1)

#define NETUNION_LONG_ARR(id, name, nEl) \
addVar(id, sza::util::DataType::LONG, (void*)name, nEl)

#define NETUNION_FLOAT(id, name) \
addVar(id, sza::util::DataType::FLOAT, (void*)&name, 1)

#define NETUNION_FLOAT_ARR(id, name, nEl) \
addVar(id, sza::util::DataType::FLOAT, (void*)name, nEl)

#define NETUNION_DOUBLE(id, name) \
addVar(id, sza::util::DataType::DOUBLE, (void*)&name, 1)

#define NETUNION_DOUBLE_ARR(id, name, nEl) \
addVar(id, sza::util::DataType::DOUBLE, (void*)name, nEl)

#define NETUNION_UCHAR_VEC(id, name) \
addVar(id, sza::util::DataType::UCHAR, (void*)&name)

#endif // End #ifndef SZA_UTIL_NETUNION_H
