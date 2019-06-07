
#ifndef CARMA_UTIL_WALSHFUNCTION_H
#define CARMA_UTIL_WALSHFUNCTION_H


/**
 * @file
 *
 * Walsh functions
 *
 * @author: Steve Scott
 *
 * $Id: WalshFunction.h,v 1.2 2008/06/06 00:00:07 scott Exp $
 *
 * $CarmaCopyright$
 *
 */

#include <vector>

   
namespace carma {
    namespace util {

/**
 * Class to generate and manipulate Walsh functions.
 *
 * Walsh Functions:
 * The reference used is Applications of Walsh and Related Functions, 
 * by K.G. Beauchamp. We generate Walsh functions from 
 * the difference equation, cf p. 20.
 *
 */  
  
class WalshFunction
{
 
public:
    /**
     * Constructor, generates a WalshSequence
     * @nStates number of states in the sequence 
     * Must be a power of two; not to exceed 256
     * @throws ErrorException when rows are not power of two
     */
    WalshFunction(int nStates);
    
    /**
     * Destructor
     */
    virtual ~WalshFunction();
    
    /** 
     * Get value
     * @param function Walsh function number (start at 0)
     * @param index of value within the function (start at 0)
     * @throws ErrorException if function or index is out of range
     */
    bool getValue(int function, int value) const; 

    /** 
     * Dump the values to stdout
     * @param whitespace - put a blank character in between values
     */
    void dump(bool whitespace = true) const; 

    /** 
     * Graph the values to stdout, looks like __--__--
     */
    void graph() const; 
   
private:
    // Hide default constructor
    WalshFunction();
    int nStates_ ;
    std::vector<bool> val_;
    int width_;
    // The generation algorithm requires a function that returns +1, -1
    // and zero when the index is out of range
    int getValueTriState(int function, int value) const; 
};


} }  // End namespace carma::util 



#endif  // CARMA_UTIL_WALSHFUNCTION_H









