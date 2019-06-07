#include "carma/util/IoLock.h"

using namespace carma;
using namespace carma::util;

// Initialize static variables

PthreadMutex IoLock::cerrMutex_;
