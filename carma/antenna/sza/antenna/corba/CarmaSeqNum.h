// $Id: CarmaSeqNum.h,v 1.2 2010/12/13 20:52:26 eml Exp $

#ifndef SZA_ANTENNA_CORBA_CARMASEQNUM_H
#define SZA_ANTENNA_CORBA_CARMASEQNUM_H

/**
 * @file CarmaSeqNum.h
 * 
 * Tagged: Fri Sep 11 15:24:35 PDT 2009
 * 
 * @version: $Revision: 1.2 $, $Date: 2010/12/13 20:52:26 $
 * 
 * @author username: Command not found.
 */
#include "carma/szautil/Mutex.h"

namespace sza {
  namespace antenna {
    namespace corba {

      class CarmaSeqNum {
      public:

	/**
	 * Constructor.
	 */
	CarmaSeqNum();

	/**
	 * Destructor.
	 */
	virtual ~CarmaSeqNum();

	void setSeq(unsigned seq, bool success);

	unsigned getSeq();
	bool getSuccess();

      private:

	unsigned seq_;
	bool success_;
	sza::util::Mutex guard_;

      }; // End class CarmaSeqNum

    } // End namespace corba
  } // End namespace antenna
} // End namespace sza



#endif // End #ifndef SZA_ANTENNA_CORBA_CARMASEQNUM_H
