#ifndef CARMA_UTIL_BASELINEINDICES_H
#define CARMA_UTIL_BASELINEINDICES_H

#include <utility>


namespace carma {
namespace util {


::std::pair< int, int > inputNoPairForBaselineIndex( int index );
::std::pair< int, int > inputNoPairForBaselineIndexWithAutos( int index );

int baselineIndexForInputNos( int inputNo1, int inputNo2 );
int baselineIndexForInputNosWithAutos( int inputNo1, int inputNo2 );

int baselineCountForMaxInputNo( int maxInputNo );
int baselineCountForMaxInputNoWithAutos( int maxInputNo );


}  // namespace carma::util
}  // namespace carma

#endif
