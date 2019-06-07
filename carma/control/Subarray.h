
#ifndef CARMA_CONTROL_SUBARRAY_H
#define CARMA_CONTROL_SUBARRAY_H

/**
 * @file
 *
 * Carma subarray.
 *
 * @author: Steve Scott
 *
 * $Id: Subarray.h,v 1.2 2005/05/03 04:58:19 abeard Exp $
 *
 * $CarmaCopyright$
 *
 */
 



namespace carma {
namespace control {

class Subarray { 

public:
    Subarray();
    virtual ~Subarray();
    static std::string makeName(const std::string& name, int number);
 
private:

};



} }  // End namespace carma/control


#endif // End of conditional include guard

