// $Id: SzaMonitorSystemReg.h,v 1.1 2012/08/14 22:03:11 eml Exp $

#ifndef SZA_ANTENNA_CORBA_SZAMONITORSYSTEMREG_H
#define SZA_ANTENNA_CORBA_SZAMONITORSYSTEMREG_H

/**
 * @file SzaMonitorSystemReg.h
 * 
 * Tagged: Fri Aug 12 17:24:16 PDT 2011
 * 
 * @version: $Revision: 1.1 $, $Date: 2012/08/14 22:03:11 $
 * 
 * @author username: Command not found.
 */
#include <string>

#include "carma/szautil/DataType.h"

namespace sza {

  namespace util {
    class ArrayDataFrameManager;
  }

  namespace antenna {
    namespace corba {

      class SzaMonitorSystemReg {
      public:

	/**
	 * Constructor.
	 */
	SzaMonitorSystemReg(std::string regmapName, std::string boardName, std::string blockName, void* data);

	SzaMonitorSystemReg(const SzaMonitorSystemReg& reg);

	SzaMonitorSystemReg(SzaMonitorSystemReg& reg);

	/**
	 * Destructor.
	 */
	virtual ~SzaMonitorSystemReg();

	void operator=(const SzaMonitorSystemReg& reg);

	void operator=(SzaMonitorSystemReg& reg);

	void initialize(sza::util::ArrayDataFrameManager* adfm);
	
	void pack(sza::util::ArrayDataFrameManager* adfm);

      public:

	unsigned byteOffset_;
	sza::util::DataType::Type type_;
	unsigned nByte_;
	bool initialized_;
	unsigned char* data_;
	std::string regmapName_;
	std::string boardName_;
	std::string blockName_;


      }; // End class SzaMonitorSystemReg

    } // End namespace corba
  } // End namespace antenna
} // End namespace sza



#endif // End #ifndef SZA_ANTENNA_CORBA_SZAMONITORSYSTEMREG_H
