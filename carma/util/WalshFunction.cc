
/**
 *
 * Implementation of the Walsh function class.
 *
 * @author: Steve Scott
 *
 * $Id: WalshFunction.cc,v 1.1 2005/05/19 01:24:08 scott Exp $
 *
 * $CarmaCopyright$
 *
 */

#include <iostream>
#include <iomanip>
#include <sstream>

using namespace std;

#include "carma/util/ErrorException.h"
#include "carma/util/WalshFunction.h"


using namespace std;
using namespace carma::util;


WalshFunction::~WalshFunction()
{
}

/*
 * This generates Rademacher function, and from those generates Walsh.
 */
WalshFunction::WalshFunction(int nStates): nStates_(nStates)
{
    int exponent = 0;
    int function = 2;
    for (int i=1; i < 8; i++) {
        if (function == nStates) {
            exponent = i;
            break;
        }
        function *= 2;
    }
    if (exponent == 0) {
        ostringstream o;
        o << "PhaseSwitching::generateWalsh(rows=" << nStates << "):"
          << " is not a power of 2 less than or equal to 256." ;
        throw CARMA_ERROR(o);
    } 
    
    if (nStates < 10) {
        width_ = 1;
    }
    else if (nStates < 100) {
        width_ = 2;
    }
    else {
        width_ = 3;
    }

    // Initialize first function to all ones
    for (int i = 0; i < nStates_; i++) {
        val_.push_back(1);
    }
    // Use 'difference method' to fill the rest
    for (int f = 1; f < nStates_; f++) {
        int q = f & 1; // is one for odd values, 0 for even
        int j = f/2;
        int exponent1 = j/2 + q;
        int exponent2 = j + q;
        // The m factors are (-1)**exponent
        int m1 = (exponent1 & 1)? -1 : 1;
        int m2 = (exponent2 & 1)? -1 : 1;
        for (int i = 0; i < nStates_; i++) {
            int a = getValueTriState(j, 2*i);
            int b = getValueTriState(j, 2*(i - nStates/2));
            int v =  m1 * (a + m2 * b);    
            if (v == 1)val_.push_back(true);
            else       val_.push_back(false);
        }
    }       
} 

    
int WalshFunction::getValueTriState(int function, int index) const {
    if (index >= nStates_) return 0;
    if (index < 0)         return 0;
    int v = val_[index + function * nStates_];
    if (v) return  1;
    else   return -1;
} 
   
bool WalshFunction::getValue(int function, int index) const {
    if (function >= nStates_ ||
        function < 0         ||
        index >= nStates_    ||
        index < 0 ) 
    {
        ostringstream o;
        o << "WalshFunction::getValue(func=" << function
          << ", index=" << index << ") has func or index out of range[0,"
          << (nStates_ - 1) << "]";
        throw CARMA_ERROR(o); 
    }
    return val_[index + function * nStates_];
}
    
void WalshFunction::dump(bool whiteSpace) const {
    for (int f = 0; f < nStates_; f++) {
        ostringstream o; 
        o << "W" << f;
        cout << setw(width_+1) << o.str() << ":";
        for (int j = 0; j < nStates_; j++) {
            if (whiteSpace) cout << " ";
            char x = '0';
            if (getValue(f, j)) x = '1';
            cout << x;
        }
        cout << endl;
    }
}
 
void WalshFunction::graph() const {
    for (int f = 0; f < nStates_; f++) {
        cout << setw(2) << f << ":";
        for (int j = 0; j < nStates_; j++) {
            char x = '_';
            if (getValue(f, j)) x = '-';
            cout << x;
        }
        cout << endl;
    }
}
 
