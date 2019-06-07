#ifndef SZA_UTIL_ANTENNADATAFRAME_H
#define SZA_UTIL_ANTENNADATAFRAME_H

/**
 * @file AntennaDataFrame.h
 * 
 * Tagged: Sun Mar 21 17:29:13 UTC 2004
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/AntNum.h"

namespace sza {
  namespace util {
      
      class AntennaDataFrame {
      public:
	
	/**
	 * Constructor
	 */
	AntennaDataFrame();

	/**
	 * Constructor
	 */
	AntennaDataFrame(const sza::util::AntNum& antNum);

	/**
	 * Destructor
	 */
	virtual ~AntennaDataFrame();

	/**
	 * Set the antenna id associated with this data frame.
	 */
	virtual void setAnt(unsigned int);

	/**
	 * Set the antenna id associated with this data frame.
	 */
	virtual void setAnt(sza::util::AntNum::Id antennaId);

	/**
	 * Set by reference
	 */
	virtual void setAnt(const sza::util::AntNum& antNum);
	
	/**
	 * Return the antenna id associated with this data frame.
	 */
	AntNum getAnt();
	
	/**
	 * Return the antenna id associated with this data frame.
	 */
	unsigned getAntIntId();

	/**
	 * Assignment operators
	 */
	void operator=(AntennaDataFrame& frame);

	/**
	 * Get a pointer to our internal data suitable for using as an
	 * external network buffer
	 */
	unsigned char* data();
	
      protected:
	
	sza::util::AntNum antNum_;
	
      }; // End class AntennaDataFrame
      
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_ANTENNADATAFRAME_H
