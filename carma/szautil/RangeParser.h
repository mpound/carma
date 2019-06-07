// $Id: RangeParser.h,v 1.1 2013/11/19 23:54:10 eml Exp $

#ifndef SZA_UTIL_RANGEPARSER_H
#define SZA_UTIL_RANGEPARSER_H

/**
 * @file RangeParser.h
 * 
 * Tagged: Thu Nov  7 13:13:38 PST 2013
 * 
 * @version: $Revision: 1.1 $, $Date: 2013/11/19 23:54:10 $
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/String.h"

#include <vector>

namespace sza {
  namespace util {

    class RangeParser {
    public:

      std::vector<unsigned> extractIndexRange(sza::util::String& antStr, unsigned lowestValid, unsigned highestValid,
					      unsigned baseIndex=0, bool actualIndex=true);

      unsigned parseIndexExpression(sza::util::String& str, 
				    unsigned baseIndex,   unsigned actualIndex, 
				    unsigned lowestValid, unsigned highestValid);

      void parseIndexOperands(sza::util::String& str, unsigned& op1, unsigned& op2, std::string op,
			      unsigned baseIndex,   unsigned actualIndex, 
			      unsigned lowestValid, unsigned highestValid);

      void addIndex(std::vector<unsigned>& indices, unsigned index, unsigned lowestValid, unsigned highestValid);

      unsigned firstEvenIndex(unsigned lowestValid, unsigned highestValid);
      unsigned firstOddIndex(unsigned lowestValid, unsigned highestValid);

    }; // End class RangeParser

  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_RANGEPARSER_H
