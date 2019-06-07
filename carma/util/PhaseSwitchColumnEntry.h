/**
 * $Id: PhaseSwitchColumnEntry.h,v 1.2 2004/08/12 21:59:42 colby Exp $
 * 
 * PhaseSwitchColumnEntry.h - a class for abstracting phase switch column information.
 *
 * @author Colby Gutierrez-Kraybill
 * @version $Revision: 1.2 $ $Date: 2004/08/12 21:59:42 $
 *
 */

#ifndef CARMA_UTIL_PHASESWITCHCOLUMNENTRY_H
#define CARMA_UTIL_PHASESWITCHCOLUMNENTRY_H

// System Includes
#include <string.h>
#include <sstream>
#include <errno.h>

// CARMA Includes
#include "carma/util/BaseException.h"


#define PHASESWITCH_COLUMN_SLOT_LENGTH 1024  // Number of slots in phase switch sequence
#define PHASESWITCH_COLUMN_BYTE_LENGTH (PHASESWITCH_COLUMN_SLOT_LENGTH*2)/8  // In bytes, slots * 2bits/entry

#define NUM_ENG_COLUMNS          7

#define ENG_SEQ_1                0
#define ENG_SEQ_2                1
#define ENG_SEQ_3                2
#define ENG_SEQ_4                3
#define ENG_SEQ_5                4
#define ENG_SEQ_6                5
#define ENG_SEQ_7                6

namespace carma
{
  namespace util
  {

    /**
     * The purpose of the PhaseSwitchColumnEntry class is to present
     * an abstract representation of phase switch column information and
     * the ability to perform checks and creation of such entries.
     *
     * This class is currently not thread safe.
     */

    class PhaseSwitchColumnEntry
    {
    public:

      PhaseSwitchColumnEntry( unsigned char *columnData ) throw ( carma::util::BaseException );
      PhaseSwitchColumnEntry( void ) throw ( carma::util::BaseException );  // place holder

      ~PhaseSwitchColumnEntry() {};

      PhaseSwitchColumnEntry *createColumn( short psStepLength, short columnNum )
        throw ( carma::util::BaseException );
     
      unsigned char *getColumn();

      void compactEngColumn( short columnId );
      unsigned char getCRC();
      
    private:
      static unsigned char const crcTable[];
      static unsigned short const engPhaseSwitchColumns[PHASESWITCH_COLUMN_SLOT_LENGTH][NUM_ENG_COLUMNS];
//      static unsigned short const engPhaseSwitchColumns[2][1]={{0},{0}};
      unsigned char *columnData_;

    }; // class PhaseSwitchColumnEntry
  }; // util namespace
}; // carma namespace 


#endif // CARMA_UTIL_PHASESWITCHCOLUMNENTRY_H

