// $Id: Pressure.h,v 1.1 2010/12/13 21:06:31 eml Exp $

#ifndef SZA_UTIL_PRESSURE_H
#define SZA_UTIL_PRESSURE_H

/**
 * @file Pressure.h
 * 
 * Tagged: Fri Nov 14 17:07:29 PST 2008
 * 
 * @version: $Revision: 1.1 $, $Date: 2010/12/13 21:06:31 $
 * 
 * @author username: Command not found.
 */
#include "carma/szautil/ConformableQuantity.h"

namespace sza {
  namespace util {

    class Pressure : public ConformableQuantity {
    public:
      
      class MilliBar {};

      /**
       * Constructor.
       */
      Pressure();
      Pressure(const MilliBar& unit, double mBar);

      /**
       * Destructor.
       */
      virtual ~Pressure();

      void setMilliBar(double mBar);

      double milliBar();

      friend std::ostream& operator<<(std::ostream& os, Pressure& pressure);

    private:

      double mBar_;

    }; // End class Pressure

  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_PRESSURE_H
