// $Id: Percent.h,v 1.1 2010/12/13 21:06:31 eml Exp $

#ifndef SZA_UTIL_PERCENT_H
#define SZA_UTIL_PERCENT_H

/**
 * @file Percent.h
 * 
 * Tagged: Mon Aug 17 11:01:48 PDT 2009
 * 
 * @version: $Revision: 1.1 $, $Date: 2010/12/13 21:06:31 $
 * 
 * @author tcsh: username: Command not found.
 */
#include "carma/szautil/ConformableQuantity.h"

namespace sza {
  namespace util {

    class Percent : public ConformableQuantity {
    public:

      class Max1 {};
      class Max100 {};

      /**
       * Constructor.
       */
      Percent();
      Percent(const Max1& unit, double percent);
      Percent(const Max100& unit, double percent);

      /**
       * Destructor.
       */
      virtual ~Percent();

      void setPercentMax1(double percent);
      void setPercentMax100(double percent);

      double percentMax1();
      double percentMax100();

    private:

      double percentMax1_;

    }; // End class Percent

  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_PERCENT_H
