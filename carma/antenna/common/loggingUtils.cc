
#include "carma/antenna/common/loggingUtils.h"

#include "carma/antenna/common/RxTypeInfo.h"
#include "carma/util/programLogging.h"
#include "carma/util/ScopedLogNdc.h" 

#include <string>

using namespace carma::antenna::common;
using namespace carma::util;
using namespace std;

void 
carma::antenna::common::logInfoWithRxNdc( 
    const RxTypeInfo & info, 
    const string & msg )
{
    const ScopedLogNdc ndc( info.rxAsString( ), "(", ")" );

    programLogInfoIfPossible( msg );

}
