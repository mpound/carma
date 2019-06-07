#include "carma/services/Interpolator.h"

#include "carma/util/ErrorException.h" 
//#include "carma/util/programLogging.h"
#include "carma/util/PthreadMutex.h"
#include "carma/util/ScopedLock.h"
#include <string>
#include <sstream>

using namespace std;
using namespace carma::util;
using namespace carma::services;

namespace {

::pthread_mutex_t gMasterGuard = PTHREAD_MUTEX_INITIALIZER;

typedef ScopedLock< ::pthread_mutex_t > MasterGuardLockType;


}  // namespace < anonymous >


Interpolator::Interpolator(
          const vector<double> & xvalue,
          const vector<double> & yvalue,
          const interpolationType type )
:
xval_(xvalue),
yval_(yvalue),
answer_(0.0)
{
    size_ = xvalue.size();
    size_t sy = yvalue.size();
    if ( size_ != sy ) {
    ostringstream os;
    os << " Input vector size mismatch [" 
        << size_
        << ","
        << sy
        << "]"
        ;
    throw CARMA_ERROR( os.str() );
    }
    this->setInterpType( type );
    this->initialize();
}

Interpolator::~Interpolator()
{
    freeAllocations();
}

void
Interpolator::freeAllocations() {
    freeGslAllocations();
    freeDoubleAllocations();
}

void
Interpolator::freeGslAllocations() 
{
    switch ( interpType_ ) {
    case LINEAR: 
    case POLYNOMIAL: 
        ::gsl_interp_free( interp_ );
        break;
    default:
    case CSPLINE: 
    case CSPLINE_PERIODIC:
    case AKIMA: 
    case AKIMA_PERIODIC:
        ::gsl_spline_free( spline_ );
        break;
    }
    ::gsl_interp_accel_free( acc_ );
#if 0
    if ( interp_ != 0 ) {
    programLogNotice(" Freeing interp");
    ::gsl_interp_free( interp_ );
    }
    if ( spline_ != 0 ) {
    programLogNotice(" Freeing spline ");
    ::gsl_spline_free( spline_ );
    }
    if ( acc_ != 0 ) {
    programLogNotice(" Freeing acc ");
    ::gsl_interp_accel_free( acc_ );
    }
#endif
}

void
Interpolator::freeDoubleAllocations() {
    delete [] x_;
    delete [] y_;
}


void Interpolator::initialize() 
{
    int status;
    x_ = new double[size_];
    y_ = new double[size_];
    for( size_t i = 0; i < size_; i++ ) {
        x_[i] = xval_[i];
        y_[i] = yval_[i];
    }

    switch ( interpType_ ) {
    case LINEAR: 
    case POLYNOMIAL: 
        status = ::gsl_interp_init( interp_, x_, y_, size_ );
        break;
    default:
    case CSPLINE: 
    case CSPLINE_PERIODIC:
    case AKIMA: 
    case AKIMA_PERIODIC:
        status = ::gsl_spline_init( spline_, x_, y_, size_  );
        break;
    }
}
    
void
Interpolator::setInterpType( interpolationType type )
{

    acc_ = ::gsl_interp_accel_alloc();
    const ::gsl_interp_type * itype;

    switch ( type ) {

    /** Linear interpolation */
    case LINEAR: 
            itype   = ::gsl_interp_linear;
            interp_ = ::gsl_interp_alloc( itype, size_ );
        break;

    /** Polynomial interpolation. The number of terms in the 
     * interpolating polynomial is equal to the number of points.
         */
    case POLYNOMIAL: 
            itype   = ::gsl_interp_polynomial;
            interp_ = ::gsl_interp_alloc( itype, size_ );
        break;

    default:
    /** Cubic spline with natural boundary conditions. */
    case CSPLINE: 
            itype   = ::gsl_interp_cspline;
            spline_ = ::gsl_spline_alloc( itype, size_ );
        break;

    /** Cubic spline with periodic boundary conditions. */
    case CSPLINE_PERIODIC:
            itype   = ::gsl_interp_cspline_periodic;
            spline_ = ::gsl_spline_alloc( itype, size_ );
        break;

    /** Non-rounded Akima spline with natural conditions. */
    case AKIMA: 
            itype   = ::gsl_interp_akima;
            spline_ = ::gsl_spline_alloc( itype, size_ );
        break;

    /** Non-rounded Akima spline with periodic boundary conditions. */
    case AKIMA_PERIODIC:
            itype   = ::gsl_interp_akima_periodic;
            spline_ = ::gsl_spline_alloc( itype, size_ );
        break;
    }

    interpType_ = type;

}

double 
Interpolator::evaluate( double x ) 
{
    MasterGuardLockType lock( gMasterGuard );
    string name;  
    int status;
    switch ( interpType_ ) {
    case LINEAR: 
    case POLYNOMIAL: 
        status = ::gsl_interp_eval_e( interp_, x_, y_, x, acc_, &answer_ );
            name = "gsl_interp_eval_e";
        break;
    default:
    case CSPLINE: 
    case CSPLINE_PERIODIC:
    case AKIMA: 
    case AKIMA_PERIODIC:
        status = ::gsl_spline_eval_e( spline_, x, acc_, &answer_ );
            name = "gsl_spline_eval_e";
        break;
    }

    ostringstream os;
    switch (status) {

    case GSL_SUCCESS:
        break;

    case GSL_EDOM:
                os << "Domain error trying to evaluate " << name << " at "
                   << x;
        throw CARMA_ERROR( os.str() );
        break;

    default:
                os << "Unexpected error " << status 
                   << " trying to evaluate " << name << " at "
                   << x;
        throw CARMA_ERROR( os.str() );
        break;
    }

    return answer_;
}
