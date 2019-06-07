#include "carma/services/OpacityModel.h"

#include "carma/util/Trace.h"

using namespace carma::services;
using namespace carma::util;

namespace { 

    const Trace::TraceLevel TRACE_CTOR_DTOR = Trace::TRACE5;

} // End namespace <unnamed>

OpacityModel::OpacityModel( ) 
{
    CARMA_CPTRACE( TRACE_CTOR_DTOR, "OpacityModel() - C'tor." );
}

OpacityModel::~OpacityModel( ) 
{
    CARMA_CPTRACE( TRACE_CTOR_DTOR, "~OpacityModel() - D'tor." );
}
