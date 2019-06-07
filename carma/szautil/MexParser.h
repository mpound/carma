#ifndef SZA_MATLAB_MEXPARSER_H
#define SZA_MATLAB_MEXPARSER_H

/**
 * @file MexParser.h
 * 
 * Tagged: Wed May  4 23:26:43 PDT 2005
 * 
 * @author Erik Leitch
 */
#include "mex.h"
#include "matrix.h"

namespace sza {
  namespace util {
    
    class MexParser {
    public:
      
      /**
       * Constructor.
       */
      MexParser(const mxArray*);
      
      /**
       * Destructor.
       */
      virtual ~MexParser();

      // Return the dimensionality of an mxArray

      int* getDimensions();

      int getNumberOfDimensions();

      void printDimensions();

    private:
      
      mxArray* array_;

    }; // End class MexParser
    
  } // End namespace util
} // End namespace sza

#endif // End #ifndef SZA_MATLAB_MEXPARSER_H
