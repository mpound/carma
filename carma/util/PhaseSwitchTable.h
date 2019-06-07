/**
 * $Id: PhaseSwitchTable.h,v 1.5 2008/06/06 21:14:08 scott Exp $
 * 
 * PhaseSwitchTable.h - a class for handling phase switch table information
 *
 * @author Steve Scott
 * @version $Revision: 1.5 $ $Date: 2008/06/06 21:14:08 $
 *
 */

#ifndef CARMA_UTIL_PHASESWITCHTABLE_H
#define CARMA_UTIL_PHASESWITCHTABLE_H


// System Includes
#include <vector>

//#include <stdio.h>



namespace carma
{
  namespace util
  {

    /**
     * The  PhaseSwitchTable class provides a generic
     * way of reading, writing and retreiving specific PhaseSwitch 
     * information from a table that will be shared among all
     * the various subsystems. Only single state phase info is supported.
     * Columns are antennas/inputs, rows are temporal states.
     *
     * This class is currently not thread safe.
     */

    class PhaseSwitchTable {
    public:
        /*
        ** Constructor
        ** Creates an underlying table that is the carma default table
        ** (16x16) on construction, no matter what size was requested. 
        ** The table can be filled with other data as desired
        ** by using the setState() method.
        ** @param rows number of rows (rows represent temporal states)
        **  default = 16
        ** @param cols number of columns (columns represent antenna/inputs)
        **  default = 16
        */
        PhaseSwitchTable(int rows=16, int columns=16);

        virtual ~PhaseSwitchTable() ;
        
        /*
         * Get number of rows in table
         */
        int getNumRows();
        
        /*
         * Get number of columns in table
         */
        int getNumColumns();
         
        /*
        ** Get temporal states for a specific input
        ** @param columnId column index, starting at 0
        ** @return vector of bools representing phase states
        */
        std::vector<bool> getColumn(int columnId);
        /*
        ** Get states for a specific temporal index and input
        ** @param columnId column (antenna) index, starting at 0
        ** @param rowId row index (temporal slot), starting at 0
        */
        bool getState(int columnId, int rowId);
        /*
        ** Set states for a specific temporal index and input
        ** @param columnId column (antenna) index, starting at 0
        ** @param rowId row index (temporal slot), starting at 0
        ** @param state phase switch state for specified input and time
        */
        void setState(int columnId, int rowId, bool state);
        /*
        ** Get table state as a string to print out for debugging
        */
        std::string toString(bool header = true);
              
    private:
        int nRows_;
        int nCols_;
        // Array of pointers to vector<bool>'s (one per column)
        std::vector<bool>** psData_;  
        void checkRowCol(int row, int col);

    }; // class PhaseSwitchTable
  }; // util namespace
}; // carma namespace 

#endif // CARMA_UTIL_PHASESWITCHTABLE_H

