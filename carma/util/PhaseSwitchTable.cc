/**
 * $Id: PhaseSwitchTable.cc,v 1.7 2008/06/06 18:44:09 scott Exp $
 * 
 * PhaseSwitchTable.cc
 *
 */

#include <sstream>

#include "PhaseSwitchTable.h"
#include "carma/util/ErrorException.h"
#include "carma/util/WalshFunction.h"

using namespace ::std;
using namespace carma;
using namespace carma::util;


PhaseSwitchTable::PhaseSwitchTable (const int rows, const int cols)
{
    nRows_ = rows;
    nCols_ = cols;
    
    psData_ = new vector<bool>*[cols];
    for (int i = 0; i < cols; i++ ) {
        psData_[i] = new vector<bool>(rows);
    }
    // Default to standard carma table; if not big enough, just exit
    if ((rows < 16) || (cols < 16)) return;
    WalshFunction w(16);
    // //w.dump(false);
    // Standard table does not use the 0 function (DC), replacing with 15
    for (int c=0; c<16; c++) {
        // swap first and last columns
        int from = c;
        if (c == 0)  from =15; 
        if (c == 15) from = 0;
        for (int r=0; r < 16; r++) psData_[c]->at(r) = w.getValue(from, r);
    }
}

PhaseSwitchTable::~PhaseSwitchTable()
{
    delete []psData_;
}

int PhaseSwitchTable::getNumRows()
{
    return nRows_;
}

int PhaseSwitchTable::getNumColumns()
{
    return nCols_;
}

vector<bool> PhaseSwitchTable::getColumn(const int colId) 
{

    if ((colId < 0) || (colId >= nCols_)) {
        ostringstream errMsg;
        errMsg << "Requested colId (" << colId
               << " must be between 0 and " << nCols_;
        throw CARMA_ERROR(errMsg);
    }
    vector<bool> ps = *psData_[colId];
    return ps;
}

void PhaseSwitchTable::checkRowCol(const int colId, const int rowId) 
{
    bool err = false;
    ostringstream errMsg;
    if ((colId < 0) || (colId >= nCols_)) {
        err = true;
        errMsg << "Requested colId (" << colId
               << ") must be between 0 and " << nCols_-1;
        throw CARMA_ERROR(errMsg);
    }

    if ((rowId < 0) || (rowId >= nRows_)) {
        if (err) {
            errMsg << "\nand requested";
        }
        else {
            errMsg << "Requested";
            err = true;
        }        
        errMsg << " rowId (" << rowId
               << ") must be between 0 and " << nRows_-1;
    }
    if (err) {           
        throw CARMA_ERROR(errMsg);
    }
}
bool PhaseSwitchTable::getState(const int colId, const int rowId) 
{
    checkRowCol(colId, rowId);
    bool ps = psData_[colId]->at(rowId);
    return ps;
}

void PhaseSwitchTable::setState(const int colId, const int rowId, const bool ps) 
{

    checkRowCol(colId, rowId);
    psData_[colId]->at(rowId) = ps;
}

string PhaseSwitchTable::toString(bool header)
{
    ostringstream o;
    if (header) o << "Ants across, time down\n";
     for (int r = 0; r < nRows_; r++) {
        for (int c = 0; c < nCols_; c++) {
            o << getState(c, r);
        }
        o << "\n";
    }
    return o.str();            
}


